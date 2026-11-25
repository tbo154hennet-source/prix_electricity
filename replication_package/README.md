# Replication package

This directory is the minimal replication package for the figures and tables
used by `DT_relecture.tex`. It keeps the maintained macro pipeline, the
non-macro scripts that directly create manuscript artifacts, and only the
intermediate scripts or data needed by those producers.

## Data

`data/replication_data.zip` contains two layers:

- `data/entsoe/`: the archived ENTSO-E API extracts used in the project;
- analysis-ready inputs in their expected project-relative directories
  (`day_ahead/outputs`, `supply_demand_curve/outputs`,
  `Intra_day_micro/outputs`, and `VAR_analysis/data`).

The archive (rebuilt on 2026-11-25 after the correction of the Spanish
wind/solar forecast extracts and of the local-slope window) contains 2,261 files:
2,193 ENTSO-E extracts plus the compact raw/analysis-ready inputs, the external
series of `data/external/` and the inventory. Its uncompressed size is about
759 MB (724 MiB); the ZIP is 477,355,156 bytes (455.2 MiB).

Extract the archive **into this directory**, preserving paths. On PowerShell:

```powershell
Expand-Archive -LiteralPath data/replication_data.zip -DestinationPath .
```

The archive is deliberately not tracked by Git. Verify it with
`data/CHECKSUMS.txt`. `DATA_MANIFEST.csv` identifies the required datasets and
`FIGURE_MANIFEST.csv` maps all 39 active manuscript artifacts (and the numbers
of the hydro-arbitrage paragraph) to their retained producer. After extraction, `data/ARCHIVE_CONTENTS.csv` gives every archived
path and byte size.

## R environment

`R_REQUIREMENTS.txt` lists the packages used by the retained R/R Markdown
scripts. The default macro inference also requires `varexternal` 0.2.0; its
GPL-2 source archive is bundled and can be installed with:

```powershell
Rscript -e "remotes::install_local('vendor/varexternal-master.zip', upgrade='never')"
```

Set `VAR_USE_MSW_INFERENCE=false` only if point estimates without the MSW
inference bands are sufficient.

## Run

From this directory:

```powershell
Rscript main.R --check-only
Rscript main.R --stage=all
```
<!-- cd C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab\replication_package
& "C:\INSEE\R\R-4.6.1\bin\Rscript.exe" main.R --stage=all -->

Available stages are `descriptive`, `elasticity`, `intraday`, and `macro`.
Several stages can be comma-separated. Generated artifacts are written below
`output/`; no Overleaf directory is required.

The active map is retained as `day_ahead/daily_temp/
wind_solar_plants.ipynb`.
Two active supplementary tables retain their direct producers under
`VAR_analysis/supplementary/`; these two producers are marked
`generated-manual` in `FIGURE_MANIFEST.csv`. Two static manuscript images have
no producer in the repository and are marked `reference-only`.

External series (`data/external/`): the Alessandri-Gazzani gas supply shocks;
the monthly hydro reservoir stock of Red Electrica (apidatos.ree.es, endpoint
`generacion/reservas-hidroelectricas`); and ERA5 weather from the Open-Meteo
archive for 2010-2015 (wind speed at the wind farms, demand-weighted temperature
and precipitation), downloaded by the scripts of
`Data_structuring/spain_wind_backfill/` and `Data_structuring/spain_weather_backfill/`.
They extend the robustness proxies back to 2010.

The macro stage runs every VAR stage of `VAR_analysis/main.R` (Spain, including
the Bayesian proxy benchmark and its robustness, and France 2010-2025), then
`VAR_analysis/supplementary/hydro_arbitrage.R` for the hydro paragraph. It takes
about an hour with the default number of posterior draws.

The default run uses the archived ENTSO-E extracts. The ENTSO-E API call is
present but commented in `main.R`, as the data are already bundled.

## Re-download ENTSO-E data

Install the small Python environment and expose a fresh API token:

```powershell
python -m pip install -r entsoe/requirements.txt
$env:ENTSOE_API_KEY = "your-token"
python entsoe/download_entsoe.py --dataset all --output data/entsoe
```

The downloader is restartable: existing Parquet files are skipped unless
`--overwrite` is supplied. It covers day-ahead prices, actual load and
generation, generation forecasts, generation by plant, wind/solar forecasts,
generation-unit unavailability (including withdrawn notices), and the border
flows, scheduled exchanges, and day-ahead NTC series used here.

The API token that appeared in the historical scripts is not included. It
should be revoked and replaced because it was committed in plain text.

## Scope decisions

- Robustness plots actively referenced by the manuscript are kept. Slide-only,
  exploratory, and deprecated outputs are excluded unless they are an input to
  a retained producer.
- `reference_outputs/` contains all 39 active manuscript artifacts for visual
  and tabular comparison.
