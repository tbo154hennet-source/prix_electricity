#!/usr/bin/env python3
"""Download hourly temperature and precipitation at the 100 Spanish demand points, 2010-2015, to extend the
monthly demand weather (temperature_demand, precipitation_demand of the hourly micro file, observed from 2015)
back to 2010 for the out-of-sample synthetic decomposition (VAR robustness on proxies).

Same points and weights as day_ahead/daily_temp/compare_temperature.R (demand_points_corrected.csv, `factor`):
temperature_demand = mean_i(temperature_2m_i * factor_i), precipitation_demand likewise; heat/cold excess with
T_base = 18 as in day_ahead/esp/prepare_regress_niveau_hourly_ter.R. 2015 overlaps the micro file for validation.

Output: demand_weather_monthly_2010_2015.csv (month, temperature_demand, precipitation_demand,
        heat_excess_demand, cold_excess_demand); points/ caches one CSV per point (resumable).
Run:  python -I download_demand_weather_2010_2015.py
"""
from __future__ import annotations

import csv
import json
import time
from collections import defaultdict
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import urlopen

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
POINTS = ROOT / "day_ahead" / "daily_temp" / "outputs" / "demand_points_corrected.csv"
OPENMETEO = "https://archive-api.open-meteo.com/v1/archive"
T_BASE = 18.0


def fetch(lat: float, lon: float) -> dict:
    params = {"latitude": lat, "longitude": lon, "start_date": "2010-01-01", "end_date": "2015-12-31",
              "hourly": "temperature_2m,precipitation", "timezone": "GMT"}
    for attempt in range(8):
        try:
            with urlopen(OPENMETEO + "?" + urlencode(params), timeout=180) as res:
                return json.loads(res.read().decode("utf-8"))
        except HTTPError as e:
            wait = 65 if e.code == 429 else 20
            print(f"  HTTP {e.code}, retry in {wait}s", flush=True)
            time.sleep(wait)
        except URLError:
            time.sleep(30)
    raise RuntimeError(f"failed {lat},{lon}")


def main() -> None:
    cache = HERE / "points"
    cache.mkdir(exist_ok=True)
    with POINTS.open(encoding="utf-8") as f:
        # points without a consumption weight are dropped, as na.rm = TRUE does in compare_temperature.R
        points = [(float(r["latitude"]), float(r["longitude"]), float(r["factor"]))
                  for r in csv.DictReader(f) if r["factor"].strip()]
    temp_sum, prec_sum, heat_sum, cold_sum = (defaultdict(float) for _ in range(4))
    hours = defaultdict(set)
    for i, (lat, lon, w) in enumerate(points):
        path = cache / f"point_{i}.csv"
        if not path.exists():
            data = fetch(lat, lon)["hourly"]
            with path.open("w", newline="") as f:
                wr = csv.writer(f)
                wr.writerow(["time", "temperature_2m", "precipitation"])
                wr.writerows(zip(data["time"], data["temperature_2m"], data["precipitation"]))
            print(f"point {i + 1}/{len(points)} ({lat:.3f}, {lon:.3f})", flush=True)
            time.sleep(2)
        with path.open() as f:
            for r in csv.DictReader(f):
                if r["temperature_2m"] in ("", "None") or r["precipitation"] in ("", "None"):
                    continue
                t, p = float(r["temperature_2m"]), float(r["precipitation"])
                key = r["time"]
                # weighted means over points (same convention as compare_temperature.R: mean of value * factor)
                temp_sum[key] += t * w; prec_sum[key] += p * w
                hours[key].add(i)
    n = len(points)
    monthly = defaultdict(lambda: defaultdict(float))
    for key in temp_sum:
        t = temp_sum[key] / n; p = prec_sum[key] / n
        m = monthly[key[:7]]
        m["temperature_demand"] += t; m["precipitation_demand"] += p
        m["heat_excess_demand"] += max(t - T_BASE, 0.0); m["cold_excess_demand"] += max(T_BASE - t, 0.0)
        m["hours"] += 1
    out = HERE / "demand_weather_monthly_2010_2015.csv"
    cols = ["temperature_demand", "precipitation_demand", "heat_excess_demand", "cold_excess_demand"]
    with out.open("w", newline="") as f:
        wr = csv.writer(f)
        wr.writerow(["month"] + cols + ["hours"])
        for month in sorted(monthly):
            m = monthly[month]
            wr.writerow([month] + [round(m[c] / m["hours"], 5) for c in cols] + [int(m["hours"])])
    print(f"OK: {len(monthly)} months -> {out}")


if __name__ == "__main__":
    main()
