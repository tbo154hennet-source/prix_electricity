#!/usr/bin/env python3
"""Download Spanish wind speed and wind capacity, 2010-2015, to test a backfill
of the monthly macro supply instrument (Fore_Wind is only observed from 2015).

Same method as day_ahead/daily_temp/extend_tot_2026/wind_solar_plants_2026.ipynb:
hourly `wind_speed_100m` from the Open-Meteo archive (ERA5) at the 211 wind
farms of df_wind_spain_big.csv (80% of the WRI-estimated wind generation).

Outputs (in --out-dir):
  points/wind_<i>.csv          hourly wind speed per farm (cache, resumable)
  wind_capacity_monthly.csv    capacity of the 211 farms in service each month
                               (WRI commissioning_year) + REE monthly wind
                               generation (peninsular, 'Eolica') for validation
  wind_monthly_2010_2015.csv   monthly aggregates:
     ws_mean          simple mean over farms of hourly wind speed (km/h)
     ws_capw          mean weighted by the capacity in service that month
     ws3_capw         capacity-weighted mean of ws^3 (wind power ~ v^3)
     wind_pot_mw      sum_i cap_i,t * ws_i,h^3 / mean(ws^3), averaged over hours
                      (potential wind output index, MW-scale)
     cap_db_mw        capacity of the farms in service (WRI)
     ree_wind_gwh     REE monthly wind generation (validation / splice target)

Python >= 3.10, standard library only.
Run:  python download_wind_2010_2015.py
      (re-run to resume after an Open-Meteo rate limit: finished farms are cached)
"""
from __future__ import annotations

import argparse
import csv
import json
import sys
import time
from collections import defaultdict
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import urlopen

ROOT = Path(__file__).resolve().parents[2]
FARMS = ROOT / "day_ahead" / "daily_temp" / "outputs" / "df_wind_spain_big.csv"
OPENMETEO = "https://archive-api.open-meteo.com/v1/archive"
REE = "https://apidatos.ree.es/es/datos/generacion/estructura-generacion"
PENINSULAR = {"geo_trunc": "electric_system", "geo_limit": "peninsular", "geo_ids": "8741"}


def get_json(url: str, timeout: float = 120) -> dict:
    with urlopen(url, timeout=timeout) as res:
        return json.loads(res.read().decode("utf-8"))


def read_farms(path: Path) -> list[dict]:
    farms = []
    with path.open(encoding="utf-8") as f:
        for i, row in enumerate(csv.DictReader(f)):
            year = row.get("commissioning_year", "").strip()
            farms.append({
                "id": i,
                "name": row["name"],
                "lat": float(row["latitude"]),
                "lon": float(row["longitude"]),
                "cap": float(row["capacity_mw"]),
                # Missing commissioning year: assumed in service over the whole period.
                "since": int(float(year)) if year else None,
            })
    return farms


def download_farm(farm: dict, start: str, end: str, dest: Path) -> None:
    params = {"latitude": farm["lat"], "longitude": farm["lon"], "start_date": start,
              "end_date": end, "hourly": "wind_speed_100m", "timezone": "GMT"}
    url = f"{OPENMETEO}?{urlencode(params)}"
    wait = 60
    while True:
        try:
            data = get_json(url)
            break
        except HTTPError as exc:
            body = exc.read().decode("utf-8", "replace")
            if exc.code == 429 or "limit" in body.lower():
                if "daily" in body.lower():
                    raise SystemExit(f"Open-Meteo daily limit reached ({body.strip()}). Re-run tomorrow: cached farms are kept.")
                print(f"  rate limit ({body.strip()[:120]}), pause {wait}s", file=sys.stderr)
                time.sleep(wait)
                wait = min(wait * 2, 900)
                continue
            raise RuntimeError(f"HTTP {exc.code} for farm {farm['id']}: {body[:200]}")
        except (URLError, TimeoutError) as exc:
            print(f"  network error ({exc}), retry in 30s", file=sys.stderr)
            time.sleep(30)
    hourly = data["hourly"]
    tmp = dest.with_suffix(".tmp")
    with tmp.open("w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["time", "wind_speed_100m"])
        w.writerows(zip(hourly["time"], hourly["wind_speed_100m"]))
    tmp.replace(dest)


def capacity_in_service(farms: list[dict], year: int) -> list[float]:
    return [f["cap"] if f["since"] is None or f["since"] <= year else 0.0 for f in farms]


def ree_wind(start_year: int, end_year: int) -> dict[str, float]:
    out = {}
    for year in range(start_year, end_year + 1):
        params = {"start_date": f"{year}-01-01T00:00", "end_date": f"{year}-12-31T23:59",
                  "time_trunc": "month", **PENINSULAR}
        data = get_json(f"{REE}?{urlencode(params)}")
        series = [i for i in data["included"] if i["attributes"]["title"].casefold() in ("eólica", "eolica")]
        if len(series) != 1:
            raise ValueError(f"REE {year}: wind series not found")
        for v in series[0]["attributes"]["values"]:
            out[v["datetime"][:7]] = v["value"] / 1000.0  # MWh -> GWh
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--start", default="2010-01-01")
    ap.add_argument("--end", default="2015-12-31")
    ap.add_argument("--farms", type=Path, default=FARMS)
    ap.add_argument("--out-dir", type=Path, default=Path(__file__).resolve().parent)
    ap.add_argument("--cache-dir", type=Path, help="hourly per-farm cache (~250 MB; default OUT_DIR/points)")
    ap.add_argument("--sleep", type=float, default=3.0, help="pause between farms (s)")
    args = ap.parse_args(argv)

    farms = read_farms(args.farms)
    point_dir = args.cache_dir or args.out_dir / "points"
    point_dir.mkdir(parents=True, exist_ok=True)
    print(f"{len(farms)} farms, {sum(f['cap'] for f in farms):.0f} MW", file=sys.stderr)

    # 1. Wind speed, one cached file per farm -------------------------------
    for farm in farms:
        dest = point_dir / f"wind_{farm['id']:03d}.csv"
        if dest.exists():
            continue
        print(f"farm {farm['id'] + 1}/{len(farms)} {farm['name']} ({farm['lat']}, {farm['lon']})", file=sys.stderr)
        download_farm(farm, args.start, args.end, dest)
        time.sleep(args.sleep)

    # 2. Capacity + REE wind generation ------------------------------------
    y0, y1 = int(args.start[:4]), int(args.end[:4])
    ree = ree_wind(y0, y1)
    months = [f"{y:04d}-{m:02d}" for y in range(y0, y1 + 1) for m in range(1, 13)]
    cap_by_year = {y: capacity_in_service(farms, y) for y in range(y0, y1 + 1)}
    with (args.out_dir / "wind_capacity_monthly.csv").open("w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["date", "cap_db_mw", "n_farms_in_service", "ree_wind_gwh"])
        for m in months:
            caps = cap_by_year[int(m[:4])]
            w.writerow([f"{m}-01", round(sum(caps), 1), sum(c > 0 for c in caps), ree.get(m, "")])

    # 3. Monthly aggregation -------------------------------------------------
    ws = {}
    for farm in farms:
        with (point_dir / f"wind_{farm['id']:03d}.csv").open(encoding="utf-8") as f:
            ws[farm["id"]] = [(r["time"], float(r["wind_speed_100m"]) if r["wind_speed_100m"] not in ("", "None") else None)
                              for r in csv.DictReader(f)]
    times = [t for t, _ in ws[farms[0]["id"]]]
    if any([t for t, _ in ws[f["id"]]] != times for f in farms):
        raise ValueError("Farms do not share the same hourly time index")

    # Normalise ws^3 by its 2010-2015 all-farm mean, so wind_pot_mw is in MW-of-capacity units.
    all_cubes = [v ** 3 for s in ws.values() for _, v in s if v is not None]
    cube_norm = sum(all_cubes) / len(all_cubes)

    acc = defaultdict(lambda: defaultdict(float))
    for h, t in enumerate(times):
        m = t[:7]
        caps = cap_by_year[int(m[:4])]
        a = acc[m]
        for farm, cap in zip(farms, caps):
            v = ws[farm["id"]][h][1]
            if v is None:
                a["missing"] += 1
                continue
            a["ws_sum"] += v
            a["n"] += 1
            if cap > 0:
                a["ws_capw"] += cap * v
                a["ws3_capw"] += cap * v ** 3
                a["cap_h"] += cap
                a["pot"] += cap * v ** 3 / cube_norm
        a["hours"] += 1

    with (args.out_dir / "wind_monthly_2010_2015.csv").open("w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["date", "ws_mean", "ws_capw", "ws3_capw", "wind_pot_mw", "cap_db_mw", "ree_wind_gwh", "hours", "missing_obs"])
        for m in months:
            a = acc[m]
            if not a["hours"]:
                raise ValueError(f"No hourly data for {m}")
            w.writerow([f"{m}-01", round(a["ws_sum"] / a["n"], 4), round(a["ws_capw"] / a["cap_h"], 4),
                        round(a["ws3_capw"] / a["cap_h"], 1), round(a["pot"] / a["hours"], 1),
                        round(sum(cap_by_year[int(m[:4])]), 1), ree.get(m, ""), int(a["hours"]), int(a["missing"])])
    print(f"OK: {len(months)} months -> {args.out_dir / 'wind_monthly_2010_2015.csv'}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
