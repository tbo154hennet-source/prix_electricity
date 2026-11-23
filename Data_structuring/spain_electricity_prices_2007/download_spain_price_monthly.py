#!/usr/bin/env python3
"""Historical day-ahead electricity prices, Spain (monthly), from 2007.

2007-2009: REE annual reports, VOLUME-WEIGHTED monthly prices (not directly
           comparable to arithmetic OMIE prices). These are packaged in
           ree_monthly_weighted_2007_2009.csv and can be refreshed from PDFs.
2010 onward: OMIE Spanish bidding zone, arithmetic monthly price, via the
             openly available Tiago Felicia mirror of OMIE-derived data.

Research-safe output: separate columns. Explicit --combine creates a mixed-
definition price series for exploratory work only.

Requirements: pip install pandas requests pdfplumber
"""
from __future__ import annotations
import argparse
import re
import sys
from io import BytesIO, StringIO
from pathlib import Path
import pandas as pd
import requests

URL_OMIE_MIRROR = (
    "https://raw.githubusercontent.com/tiagofelicia/dados-energia/main/"
    "data/agregados/omie_es_mensal.csv"
)
URL_REE_REPORT = {
    2007: "https://www.ree.es/sites/default/files/downloadable/inf_sis_elec_ree_2007_v3.pdf",
    2008: "https://www.ree.es/sites/default/files/downloadable/inf_sis_elec_ree_2008_v4.pdf",
    2009: "https://www.ree.es/sites/default/files/downloadable/inf_sis_elec_ree_2009.pdf",
}
MONTHS = {
    "Enero": 1, "Febrero": 2, "Marzo": 3, "Abril": 4,
    "Mayo": 5, "Junio": 6, "Julio": 7, "Agosto": 8,
    "Septiembre": 9, "Setiembre": 9, "Octubre": 10,
    "Noviembre": 11, "Diciembre": 12,
}
HERE = Path(__file__).resolve().parent


def get_binary(url: str, timeout: int = 45) -> bytes:
    resp = requests.get(url, timeout=timeout, headers={"User-Agent": "Mozilla/5.0 (academic-research; contact site for access)"})
    resp.raise_for_status()
    if not resp.content:
        raise ValueError("Empty download: " + url)
    return resp.content


def parse_mirror_csv(raw: bytes) -> pd.DataFrame:
    """Read Spanish monthly OMIE price, reject dubious schema and duplicated dates."""
    frame = pd.read_csv(BytesIO(raw), sep=None, engine="python")
    # The mirror's published file uses 'mes' such as '01/2010', and 'es_medio'.
    if not {"mes", "es_medio"}.issubset(frame.columns):
        raise ValueError(f"Unexpected mirror CSV columns: {list(frame.columns)}")
    dates = pd.to_datetime(frame["mes"].astype(str), format="%m/%Y", errors="coerce")
    prices = pd.to_numeric(frame["es_medio"].astype(str).str.replace(",", ".", regex=False), errors="coerce")
    output = pd.DataFrame({"date": dates, "price_omie_arithmetic_eur_mwh": prices})
    output = output.dropna(subset=["date", "price_omie_arithmetic_eur_mwh"])
    if output.empty or output.date.duplicated().any():
        raise ValueError("Missing, duplicate or unparsable monthly prices")
    if not output.price_omie_arithmetic_eur_mwh.between(-1000, 10000).all():
        raise ValueError("Prices have implausible scale; check units")
    return output.sort_values("date").reset_index(drop=True)


def parse_ree_report_text(page_text: str, year: int) -> pd.DataFrame:
    """Parse table 'Energía y precios medios ponderados en el mercado diario'.

    Table rows: Enero 9.856 10,00 47,20 84,79
                month volume min weighted-monthly-mean max
    Extract only table immediately following that exact heading.
    """
    norm = " ".join(page_text.replace("\u00a0", " ").split())
    title = re.search(r"Energ[ií]a y precios medios ponderados en el mercado diario", norm, re.IGNORECASE)
    if title is None:
        raise ValueError(f"REE {year}: monthly day-ahead table title not found")
    section = norm[title.end():title.end() + 4200]
    # Stop before following chart or following market statistics section.
    section = re.split(r"(?:Mercado diario\. Precio medio|Precio medio ponderado diario|\(\*\) Incluye bombeo)", section, maxsplit=1)[0]
    month_names = "|".join(MONTHS)
    pattern = re.compile(rf"\b({month_names})\s+([\d.]+)\s+([\d.,-]+)\s+([\d.,-]+)\s+([\d.,-]+)", re.IGNORECASE)
    records = []
    for m in pattern.finditer(section):
        name, volume, minimum, mean, maximum = m.groups()
        mn = int(MONTHS[next(k for k in MONTHS if k.lower() == name.lower())])
        val = float(mean.replace(".", "").replace(",", "."))
        if not 0 <= val <= 500:
            raise ValueError(f"REE {year}-{mn:02d}: suspect mean price {val}")
        records.append({"date": pd.Timestamp(year, mn, 1), "price_ree_weighted_eur_mwh": val})
    df = pd.DataFrame(records)
    if len(df) != 12 or len(set(df.date)) != 12:
        raise ValueError(f"REE {year}: expected 12 unique months; got {len(df)}")
    return df.sort_values("date")


def fetch_ree_pdf(year: int) -> pd.DataFrame:
    import pdfplumber
    content = get_binary(URL_REE_REPORT[year], timeout=90)
    if not content.startswith(b"%PDF"):
        raise ValueError("Download is not a PDF")
    with pdfplumber.open(BytesIO(content)) as pdf:
        # Search full PDF to find the unique table, not hard-coded page numbers.
        for page in pdf.pages:
            page_text = page.extract_text() or ""
            if re.search(r"Energ[ií]a y precios medios ponderados en el mercado diario", page_text, re.IGNORECASE):
                try:
                    return parse_ree_report_text(page_text, year)
                except ValueError:
                    pass
    raise ValueError(f"Could not extract the 12 monthly prices from REE {year} report")


def load_ree_offline(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path, parse_dates=["date"])
    if len(df) != 36 or df.date.duplicated().any():
        raise ValueError("2007-2009 REE bundled CSV should have 36 unique months")
    return df[["date", "price_ree_weighted_eur_mwh"]]


def build_output(ree: pd.DataFrame, omie: pd.DataFrame,
                 start: int, end: int, combine: bool = False) -> pd.DataFrame:
    months = pd.date_range(f"{start}-01-01", f"{end}-12-01", freq="MS")
    out = pd.DataFrame({"date": months}).merge(ree, how="left", on="date").merge(omie, how="left", on="date")
    out["price_source"] = "missing"
    out.loc[out.price_ree_weighted_eur_mwh.notna(), "price_source"] = "REE_weighted"
    out.loc[out.price_omie_arithmetic_eur_mwh.notna(), "price_source"] = "OMIE_arithmetic_mirror"
    if combine:
        out["spot_price_eur_mwh_MIXED_DEFINITION"] = (
            out.price_omie_arithmetic_eur_mwh.combine_first(out.price_ree_weighted_eur_mwh)
        )
    return out


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--start", type=int, default=2007, help="First calendar year (default: 2007)")
    parser.add_argument("--end", type=int, default=2025, help="Last full calendar year (default: 2025)")
    parser.add_argument("--output", default="spain_electricity_monthly_2007_2025.csv")
    parser.add_argument("--refresh-ree", action="store_true", help="Re-download and parse REE PDFs instead of using bundled verified REE table")
    parser.add_argument("--combine", action="store_true", help="Make exploratory combined price (MIXES volume-weighted and arithmetic means)")
    parser.add_argument("--mirror-file", type=Path, help="Use a previously downloaded OMIE mirror CSV (offline mode)")
    args = parser.parse_args()
    if args.start < 2007 or args.end < args.start:
        parser.error("Years must satisfy 2007 <= start <= end")
    if args.start > 2009:
        ree = pd.DataFrame(columns=["date", "price_ree_weighted_eur_mwh"])
    elif args.refresh_ree:
        tables = []
        for y in (2007, 2008, 2009):
            print(f"Reading REE annual report {y}...", file=sys.stderr)
            tables.append(fetch_ree_pdf(y))
        ree = pd.concat(tables, ignore_index=True)
    else:
        ree = load_ree_offline(HERE / "ree_monthly_weighted_2007_2009.csv")
    if args.end >= 2010:
        print("Reading OMIE-derived monthly data (2010 onward)...", file=sys.stderr)
        raw = args.mirror_file.read_bytes() if args.mirror_file else get_binary(URL_OMIE_MIRROR)
        omie = parse_mirror_csv(raw)
    else:
        omie = pd.DataFrame(columns=["date", "price_omie_arithmetic_eur_mwh"])
    output = build_output(ree, omie, args.start, args.end, args.combine)
    missing = output.loc[output.price_source.eq("missing"), "date"].dt.strftime("%Y-%m").tolist()
    output.to_csv(args.output, index=False, date_format="%Y-%m-%d", float_format="%.4f")
    print(f"Saved {len(output)} months to {args.output}; missing: {len(missing)}")
    if missing:
        print("Missing months:", ", ".join(missing[:24]), file=sys.stderr)
        return 2
    if args.combine:
        print("CAUTION: Combined series mixes volume-weighted (2007-09) and arithmetic (2010+) definitions.")
    else:
        print("RESEARCH SAFE: prices are kept in separate columns due to different weighting conventions.")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
