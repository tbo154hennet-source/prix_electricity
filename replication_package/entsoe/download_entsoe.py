"""Download all ENTSO-E inputs used by the replication package.

Set ENTSOE_API_KEY in the environment, then select one dataset or use
``--dataset all``. Existing files are skipped unless ``--overwrite`` is used.
"""

from __future__ import annotations

import argparse
import os
import time
from collections.abc import Callable, Iterable
from pathlib import Path

import pandas as pd
from entsoe import EntsoePandasClient


TZ = "Europe/Brussels"
COUNTRIES = ("ES", "FR", "PT")
RENEWABLE_COUNTRIES = ("ES", "FR", "PT", "BE", "CH", "DE", "GB", "IT_NORD")
BORDER_PAIRS = (
    ("ES", "FR"),
    ("ES", "PT"),
    ("FR", "CH"),
    ("FR", "DE_AT_LU"),
    ("FR", "GB"),
    ("FR", "IT_NORD"),
)


def periods(start: pd.Timestamp, end: pd.Timestamp, frequency: str) -> Iterable[tuple[pd.Timestamp, pd.Timestamp]]:
    starts = pd.date_range(start=start, end=end, freq=frequency)
    for left in starts:
        if frequency == "MS":
            right = left + pd.offsets.MonthBegin(1)
        elif frequency == "YS":
            right = left + pd.offsets.YearBegin(1)
        elif frequency == "7D":
            right = left + pd.Timedelta(days=7)
        else:
            raise ValueError(f"Unsupported frequency: {frequency}")
        if left < end:
            yield left, min(right, end)


def as_frame(value: pd.Series | pd.DataFrame, name: str) -> pd.DataFrame:
    if isinstance(value, pd.Series):
        return value.to_frame(name=name)
    if isinstance(value, pd.DataFrame):
        return value
    raise TypeError(f"Expected pandas Series/DataFrame, got {type(value)!r}")


def country_destination(root: Path, dataset: str, country: str, left: pd.Timestamp) -> Path:
    """Return the legacy layout expected by the retained processing scripts."""
    lower = {"ES": "esp", "FR": "fr", "PT": "pt"}.get(country, country)
    label_month = left.strftime("%Y-%m")
    label_year = left.strftime("%Y")
    if dataset == "day_ahead_prices":
        return root / "day_ahead_prices" / lower / f"{label_month}.parquet"
    if dataset == "load":
        return root / "Generation_tot" / lower / "Load_final" / f"{label_month}{country}.parquet"
    if dataset == "load_forecast":
        return root / "Generation_tot" / lower / "Load_forecast" / f"{label_month}{country}.parquet"
    if dataset == "generation":
        return root / "Generation_tot" / lower / "by_fuel" / f"{label_month}{country}.parquet"
    if dataset == "generation_forecast":
        return root / "Generation_tot" / lower / "Generation_forecast" / f"{label_year}{country}.parquet"
    if dataset == "generation_per_plant":
        return root / "Generation_by_plant" / lower / f"{left.strftime('%Y-%m-%d')}.parquet"
    if dataset == "wind_solar_forecast":
        if country in {"ES", "FR", "PT"}:
            return root / "Wind_solar_forecast" / country / "day_ahead" / f"{label_year}{country}.parquet"
        return root / "Wind_solar_forecast" / "other_EU" / country / f"{label_year}{country}.parquet"
    if dataset == "intraday_wind_solar_forecast":
        subdir = "intradaily" if country == "ES" else "intraday"
        return root / "Wind_solar_forecast" / country / subdir / f"{label_year}{country}.parquet"
    if dataset == "unavailability":
        return root / "Unavailabilities_plants" / "ENTSOE_unavailabilities" / country / f"{label_year}.parquet"
    if dataset == "withdrawn_unavailability":
        return root / "Unavailabilities_plants" / "ENTSOE_dismissed_unavailabilities" / country / f"{label_year}.parquet"
    raise ValueError(f"No destination mapping for {dataset}")


def border_destination(
    root: Path,
    dataset: str,
    pair: tuple[str, str],
    direction: tuple[str, str],
    year: int,
) -> Path:
    folder = {
        "cross_border_flows": "actual_flows",
        "scheduled_exchanges": "scheduled_exchanges/API_pairs",
        "net_transfer_capacity_dayahead": "NTC",
    }[dataset]
    pair_name = f"{pair[0]}_{pair[1]}"
    direction_name = f"{direction[0]}_{direction[1]}_{year}.parquet"
    return root / "cross_border_flows" / folder / pair_name / direction_name

def save_query(
    destination: Path,
    query: Callable[[], pd.Series | pd.DataFrame],
    *,
    overwrite: bool,
    retries: int,
    pause: float,
) -> None:
    if destination.exists() and not overwrite:
        print(f"skip {destination}")
        return
    destination.parent.mkdir(parents=True, exist_ok=True)
    for attempt in range(1, retries + 1):
        try:
            frame = as_frame(query(), destination.stem)
            frame.to_parquet(destination, index=True)
            print(f"wrote {destination} ({len(frame):,} rows)")
            time.sleep(pause)
            return
        except Exception as exc:  # API/network exceptions differ by endpoint.
            if attempt == retries:
                raise
            delay = pause * (2 ** (attempt - 1))
            print(f"retry {attempt}/{retries}: {destination}: {exc}")
            time.sleep(delay)


def download_country_series(
    client: EntsoePandasClient,
    root: Path,
    dataset: str,
    start: pd.Timestamp,
    end: pd.Timestamp,
    overwrite: bool,
    retries: int,
    pause: float,
) -> None:
    specifications: dict[str, tuple[tuple[str, ...], str, Callable[..., object]]] = {
        "day_ahead_prices": (COUNTRIES[:2], "MS", client.query_day_ahead_prices),
        "load": (COUNTRIES[:2], "MS", client.query_load),
        "load_forecast": (COUNTRIES[:2], "MS", client.query_load_forecast),
        "generation": (COUNTRIES[:2], "MS", client.query_generation),
        "generation_forecast": (COUNTRIES[:2], "YS", client.query_generation_forecast),
        "wind_solar_forecast": (RENEWABLE_COUNTRIES, "YS", client.query_wind_and_solar_forecast),
        "intraday_wind_solar_forecast": (COUNTRIES[:2], "YS", client.query_intraday_wind_and_solar_forecast),
        "generation_per_plant": (COUNTRIES, "7D", client.query_generation_per_plant),
        "unavailability": (COUNTRIES, "YS", client.query_unavailability_of_generation_units),
        "withdrawn_unavailability": (("ES", "PT"), "YS", client.query_withdrawn_unavailability_of_generation_units),
    }
    countries, frequency, method = specifications[dataset]
    dataset_start = max(start, pd.Timestamp("2018-01-01", tz=TZ)) if dataset == "intraday_wind_solar_forecast" else start
    for country in countries:
        for left, right in periods(dataset_start, end, frequency):
            destination = country_destination(root, dataset, country, left)
            kwargs: dict[str, object] = {"start": left, "end": right}
            if dataset == "generation":
                kwargs.update(psr_type=None)
            elif dataset == "generation_per_plant":
                kwargs.update(psr_type=None, include_eic=False)
            elif dataset in {"wind_solar_forecast", "intraday_wind_solar_forecast"}:
                kwargs.update(psr_type=None)
            elif dataset == "unavailability":
                kwargs.update(docstatus=None, periodstartupdate=None, periodendupdate=None)
            save_query(
                destination,
                lambda c=country, kw=kwargs, fn=method: fn(c, **kw),
                overwrite=overwrite,
                retries=retries,
                pause=pause,
            )


def download_borders(
    client: EntsoePandasClient,
    root: Path,
    dataset: str,
    start: pd.Timestamp,
    end: pd.Timestamp,
    overwrite: bool,
    retries: int,
    pause: float,
) -> None:
    method_by_dataset = {
        "cross_border_flows": client.query_crossborder_flows,
        "scheduled_exchanges": client.query_scheduled_exchanges,
        "net_transfer_capacity_dayahead": client.query_net_transfer_capacity_dayahead,
    }
    method = method_by_dataset[dataset]
    pairs = (("ES", "FR"),) if dataset == "cross_border_flows" else BORDER_PAIRS
    for country_from, country_to in pairs:
        for origin, destination_country in ((country_from, country_to), (country_to, country_from)):
            for left, right in periods(start, end, "YS"):
                destination = border_destination(
                    root,
                    dataset,
                    (country_from, country_to),
                    (origin, destination_country),
                    left.year,
                )
                kwargs: dict[str, object] = {}
                if dataset == "scheduled_exchanges":
                    kwargs["dayahead"] = True
                save_query(
                    destination,
                    lambda a=origin, b=destination_country, s=left, e=right, fn=method, kw=kwargs: fn(a, b, start=s, end=e, **kw),
                    overwrite=overwrite,
                    retries=retries,
                    pause=pause,
                )


DATASETS = (
    "day_ahead_prices",
    "load",
    "load_forecast",
    "generation",
    "generation_forecast",
    "wind_solar_forecast",
    "intraday_wind_solar_forecast",
    "generation_per_plant",
    "unavailability",
    "withdrawn_unavailability",
    "cross_border_flows",
    "scheduled_exchanges",
    "net_transfer_capacity_dayahead",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dataset", choices=("all", *DATASETS), default="all")
    parser.add_argument("--output", type=Path, default=Path("data/entsoe"))
    parser.add_argument("--start", default="2015-01-01")
    parser.add_argument("--end", default="2025-10-01", help="Exclusive end date")
    parser.add_argument("--overwrite", action="store_true")
    parser.add_argument("--retries", type=int, default=4)
    parser.add_argument("--pause", type=float, default=1.0)
    parser.add_argument("--continue-on-error", action="store_true")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    api_key = os.environ.get("ENTSOE_API_KEY")
    if not api_key:
        raise SystemExit("Set ENTSOE_API_KEY before calling the ENTSO-E API.")
    start = pd.Timestamp(args.start, tz=TZ)
    end = pd.Timestamp(args.end, tz=TZ)
    if start >= end:
        raise SystemExit("--start must be earlier than --end")
    client = EntsoePandasClient(api_key=api_key)
    selected = DATASETS if args.dataset == "all" else (args.dataset,)
    failures: list[str] = []
    for dataset in selected:
        print(f"\n[{dataset}]")
        try:
            if dataset in {"cross_border_flows", "scheduled_exchanges", "net_transfer_capacity_dayahead"}:
                download_borders(client, args.output, dataset, start, end,
                                 args.overwrite, args.retries, args.pause)
            else:
                download_country_series(client, args.output, dataset, start, end,
                                        args.overwrite, args.retries, args.pause)
        except Exception as exc:
            if not args.continue_on_error:
                raise
            failures.append(f"{dataset}: {exc}")
            print(f"FAILED {dataset}: {exc}")
    if failures:
        raise SystemExit("Some datasets failed:\n- " + "\n- ".join(failures))


if __name__ == "__main__":
    main()


