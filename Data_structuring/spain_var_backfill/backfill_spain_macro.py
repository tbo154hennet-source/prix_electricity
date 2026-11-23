#!/usr/bin/env python3
"""Backfill monthly Spanish electricity series for a macro VAR (2010-2014 by default).

Price: OMIE Spain monthly means from a transparent, secondary open-data mirror.
Demand and generation: REE REData monthly peninsular data (via the public API).

The script never silently replaces unavailable observations with zero.
Prices and output are EUR/MWh and GWh/month, respectively.

Run: python backfill_spain_macro.py --start-year 2010 --end-year 2014
     python backfill_spain_macro.py --price-file omie_es_price_2010_2014.csv --price-only
"""
from __future__ import annotations

import argparse
import csv
import io
import json
import re
import sys
import time
from collections import defaultdict
from datetime import date, datetime
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen

PRICE_URL = "https://raw.githubusercontent.com/tiagofelicia/dados-energia/main/data/agregados/omie_es_mensal.csv"
REE_BASE = "https://apidatos.ree.es/es/datos"
PENINSULAR = {"geo_trunc": "electric_system", "geo_limit": "peninsular", "geo_ids": "8741"}


def download(url: str, *, retries: int = 3, timeout: float = 25) -> bytes:
    last_error = None
    for attempt in range(retries):
        try:
            request = Request(url, headers={"User-Agent": "Mozilla/5.0 SpainMacroVARResearch/1.0", "Accept": "application/json,text/csv,text/plain,*/*"})
            with urlopen(request, timeout=timeout) as res:
                data = res.read()
                if not data or data.lstrip()[:50].lower().startswith(b"<!doctype html"):
                    raise RuntimeError(f"Unexpected HTML/empty response from {url}")
                return data
        except (HTTPError, URLError, TimeoutError, RuntimeError) as exc:
            last_error = exc
            if isinstance(exc, HTTPError) and exc.code in (401, 403, 404):
                break
            if attempt + 1 < retries:
                time.sleep(0.5 * (attempt + 1))
    raise RuntimeError(f"Could not retrieve {url}: {last_error}")


def read_price_csv(blob: bytes | str) -> dict[str, float]:
    content = blob.decode("utf-8-sig") if isinstance(blob, bytes) else blob.lstrip("\ufeff")
    table = csv.DictReader(io.StringIO(content))
    if not {"mes", "es_medio"}.issubset(table.fieldnames or []):
        raise ValueError("Price CSV needs columns 'mes' and 'es_medio' (OMIE Spain monthly mirror format)")
    out = {}
    for row in table:
        raw = row["es_medio"].strip().replace(",", ".")
        if not raw:
            continue
        month, year = row["mes"].strip().split("/")
        key = f"{int(year):04d}-{int(month):02d}"
        value = float(raw)
        if key in out:
            raise ValueError(f"Duplicate price for {key}")
        out[key] = value
    return out


def ree_url(category: str, widget: str, year: int, *, scope: str = "peninsular") -> str:
    params = {"start_date": f"{year}-01-01T00:00", "end_date": f"{year}-12-31T23:59", "time_trunc": "month"}
    if scope == "peninsular":
        params.update(PENINSULAR)
    elif scope != "national":
        raise ValueError("scope must be peninsular or national")
    return f"{REE_BASE}/{category}/{widget}?{urlencode(params)}"


def get_ree_entries(payload: dict) -> list[tuple[str, dict[str, float]]]:
    """Read standard REData 'included[*].attributes.values' responses.

    Each returned series must have at most one observation per calendar month.
    """
    if payload.get("errors"):
        raise ValueError(f"REE returned errors: {payload['errors']}")
    extracted = []
    for item in payload.get("included", []):
        attr = item.get("attributes", {})
        values = attr.get("values")
        if not isinstance(values, list) or not values:
            continue
        name = str(attr.get("title") or attr.get("name") or attr.get("description") or item.get("id") or "unknown")
        monthly = {}
        for row in values:
            if not isinstance(row, dict) or row.get("value") is None:
                continue
            dt = str(row.get("datetime", ""))
            if not re.match(r"^\d{4}-\d{2}", dt):
                raise ValueError(f"Unrecognized REE observation datetime: {dt}")
            key = dt[:7]
            if key in monthly:
                raise ValueError(f"Duplicate REE month {key} in series {name}; expected time_trunc=month")
            monthly[key] = float(row["value"])
        if monthly:
            extracted.append((name, monthly))
    if not extracted:
        raise ValueError("No monthly numeric series in REE JSON 'included'; check API response")
    return extracted


def infer_monthly_gwh(values: dict[str, float], label: str) -> tuple[dict[str, float], str]:
    """REE endpoints can change the magnitude of displayed numbers; infer conservatively."""
    positive = sorted(x for x in values.values() if x > 0)
    if not positive:
        raise ValueError(f"No positive monthly values for {label}")
    median = positive[len(positive) // 2]
    if 1_000_000 <= median <= 100_000_000:
        return {m: v / 1000.0 for m, v in values.items()}, "MWh -> GWh"
    if 1_000 <= median < 100_000:
        return dict(values), "GWh (already)"
    raise ValueError(f"Unexpected monthly size in {label}: median={median:g}; check REE units")


def extract_demand(payload: dict) -> tuple[dict[str, float], str]:
    entries = get_ree_entries(payload)
    # Demand 'evolucion' commonly supplies a single series, but explicitly reject forecasts.
    eligible = [(name, vals) for name, vals in entries if
                not any(term in name.casefold() for term in ("previst", "programad", "corregid", "estimad"))]
    named = [(name, vals) for name, vals in eligible if "demanda" in name.casefold()]
    if len(named) == 1:
        chosen = named[0]
    elif len(eligible) == 1:
        chosen = eligible[0]
    else:
        raise ValueError(f"Ambiguous REE demand series: {[name for name, _ in entries]}")
    series, unit = infer_monthly_gwh(chosen[1], f"demand ({chosen[0]})")
    return series, f"{chosen[0]} | {unit}"


def extract_generation(payload: dict) -> tuple[dict[str, float], str]:
    entries = get_ree_entries(payload)
    aggregate_titles = {
        "total", "generacion total", "generación total", "generación renovable",
        "generacion renovable", "generación no renovable", "generacion no renovable",
        "renovable", "no renovable", "renovables", "no renovables",
    }
    leaves = [(name, vals) for name, vals in entries if name.casefold().strip() not in aggregate_titles]
    if not leaves:
        raise ValueError(f"No generation technologies in response: {[x[0] for x in entries]}")
    by_month = defaultdict(float)
    for name, vals in leaves:
        for month, value in vals.items():
            by_month[month] += value
    out, unit = infer_monthly_gwh(dict(by_month), "total generation")
    return out, f"sum({len(leaves)} technology series) | {unit}"


def months(start_year: int, end_year: int) -> list[str]:
    if start_year > end_year:
        raise ValueError("start_year after end_year")
    return [f"{year:04d}-{month:02d}" for year in range(start_year, end_year + 1) for month in range(1, 13)]


def make_rows(prices: dict[str, float], loads: dict[str, float] | None, gens: dict[str, float] | None,
              start_year: int, end_year: int, *, price_only: bool = False) -> list[dict]:
    result = []
    for m in months(start_year, end_year):
        if m not in prices:
            raise ValueError(f"Missing market price for {m}; no interpolation")
        if not price_only and (loads is None or gens is None or m not in loads or m not in gens):
            raise ValueError(f"Missing REE demand/generation for {m}; no interpolation")
        result.append({
            "date": f"{m}-01",
            "load_gwh": "" if price_only else round(loads[m], 5),
            "gen_tot_gwh": "" if price_only else round(gens[m], 5),
            "spot_price_eur_mwh": round(prices[m], 5),
            "proxy_supply": 0,
            "proxy_demand": 0,
            "proxy_observed": 0,  # 0 = unavailable before instrument start; NOT a measured zero
        })
    return result


def save_rows(rows: list[dict], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as out:
        writer = csv.DictWriter(out, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--start-year", type=int, default=2010)
    ap.add_argument("--end-year", type=int, default=2014)
    ap.add_argument("--scope", choices=("peninsular", "national"), default="peninsular")
    ap.add_argument("--price-file", type=Path, help="Cached OMIE Spanish monthly CSV (mes,es_medio)")
    ap.add_argument("--price-only", action="store_true", help="Build a price-only output without querying REE")
    ap.add_argument("--ree-json-dir", type=Path, help="Use saved REE JSON: load_2010.json and generation_2010.json, etc.")
    ap.add_argument("--output", type=Path, default=Path("spain_macro_electricity_backfill.csv"))
    args = ap.parse_args(argv)
    price_blob = args.price_file.read_bytes() if args.price_file else download(PRICE_URL)
    prices = read_price_csv(price_blob)
    all_load, all_gen = {}, {}
    if not args.price_only:
        for yr in range(args.start_year, args.end_year + 1):
            out = {}
            for label, category, widget, extractor in [
                ("load", "demanda", "evolucion", extract_demand),
                ("generation", "generacion", "estructura-generacion", extract_generation),
            ]:
                if args.ree_json_dir:
                    data = (args.ree_json_dir / f"{label}_{yr}.json").read_bytes()
                else:
                    url = ree_url(category, widget, yr, scope=args.scope)
                    print(f"GET {label} {yr}: {url}", file=sys.stderr)
                    data = download(url)
                try:
                    values, provenance = extractor(json.loads(data.decode("utf-8-sig")))
                except Exception as exc:
                    raise ValueError(f"Could not parse REE {label} {yr}: {exc}") from exc
                print(f"{label} {yr}: {len(values)} months ({provenance})", file=sys.stderr)
                out[label] = values
            all_load.update(out["load"])
            all_gen.update(out["generation"])
    rows = make_rows(prices, all_load, all_gen, args.start_year, args.end_year, price_only=args.price_only)
    save_rows(rows, args.output)
    print(f"OK: {len(rows)} months ({rows[0]['date']} to {rows[-1]['date']}) -> {args.output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        raise SystemExit(1)
