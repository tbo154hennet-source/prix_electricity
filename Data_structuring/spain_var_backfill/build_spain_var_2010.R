# Central Spanish monthly VAR dataset, 2010-01 to 2025-09.
#
# VAR variables (spot_price, load_tot, gen_tot, IPI, HICP, ffr, spot_gas):
#   2015-01 onwards: raw levels of the benchmark
#     (VAR_analysis_fr/VAR_output/IV_data_esp_elas_sd_2026.parquet).
#   2010-2014: other sources, rescaled by the mean ratio benchmark / source over
#   2015-01 -> 2016-12 so that the series are continuous:
#     spot_price  OMIE Spain monthly mean        (backfill_spain_macro.py)
#     load_tot    REE peninsular demand, avg MW  (backfill_spain_macro.py)
#     gen_tot     REE peninsular generation, MW  (backfill_spain_macro.py)
#     spot_gas    TTF monthly mean (gas_daily.xlsx) from 2012, IMF "Natural
#                 gas, Europe" in EUR/MWh (FRED PNGASEUUSDM / EXUSEU) chained
#                 before (mean ratio TTF / IMF over 2012-2014)
#     IPI         INE IPI15382
#     HICP        FRED CP0000ESM086NEST
#     ffr         FRED ECBMRRFR (levels, no rescaling)
#   then the transformations of seasonal_adjustment_quint_sd.R on 2010-2025:
#   spot prices deflated by HICP, X-13 (auto transform, td/easter AIC test),
#   log(x + 1).
# Every other column (micro instruments, weather, wind, ...) is copied from
# VAR_analysis/data/IV_data_esp_log_elas_sd_2026.parquet and is NA before 2015:
# the instruments are not observed there.
#
# Usage (from the repo root):
#   python Data_structuring/spain_var_backfill/backfill_spain_macro.py \
#     --start-year 2010 --end-year 2016 \
#     --output Data_structuring/spain_var_backfill/spain_electricity_2010_2016.csv
#   Rscript Data_structuring/spain_var_backfill/build_spain_var_2010.R

library(dplyr)
library(arrow)
library(seasonal)

# The x13binary copy in the user library is blocked by group policy on INSEE
# machines; the one shipped in the INSEE R library is allowed.
insee_x13 <- file.path(R.home(), "library", "x13binary", "bin")
if (!nzchar(Sys.getenv("X13_PATH")) && dir.exists(insee_x13)) {
  Sys.setenv(X13_PATH = insee_x13)
}

root <- Sys.getenv("PRIX_ELEC_ROOT", "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab")
backfill_dir <- file.path(root, "Data_structuring", "spain_var_backfill")
raw_path <- file.path(
  root, "depreciated", "VAR_analysis_fr", "VAR_output",
  "IV_data_esp_elas_sd_2026.parquet"
)
benchmark_path <- file.path(
  root, "VAR_analysis", "data", "IV_data_esp_log_elas_sd_2026.parquet"
)
electricity_path <- file.path(backfill_dir, "spain_electricity_2010_2016.csv")
output_path <- file.path(
  root, "VAR_analysis", "data", "IV_data_esp_log_elas_sd_2010.parquet"
)
diagnostics_path <- file.path(backfill_dir, "splice_diagnostics_2010.csv")

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2025-09-01")
overlap <- as.Date(c("2015-01-01", "2016-12-01"))
dates <- seq(start_date, end_date, by = "month")
var_levels <- c("spot_price", "load_tot", "gen_tot", "IPI", "HICP", "spot_gas")

month_floor <- function(x) as.Date(format(as.Date(x), "%Y-%m-01"))

fred <- function(id) {
  data <- read.csv(paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id,
    "&cosd=2009-01-01"
  ))
  names(data) <- c("observation_date", id)
  data$observation_date <- as.Date(data$observation_date)
  data[[id]] <- suppressWarnings(as.numeric(data[[id]]))
  data
}

# Benchmark raw levels -------------------------------------------------------
raw <- as.data.frame(read_parquet(raw_path)) %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  filter(observation_date <= end_date) %>%
  select(observation_date, all_of(var_levels), ffr)

# Backfill sources -------------------------------------------------------------
electricity <- read.csv(electricity_path) %>%
  mutate(observation_date = as.Date(date))
hours <- as.numeric(difftime(
  seq(min(electricity$observation_date), by = "month",
      length.out = nrow(electricity) + 1L)[-1],
  electricity$observation_date,
  units = "hours"
))
electricity <- electricity %>%
  mutate(
    spot_price_src = spot_price_eur_mwh,
    load_tot_src = load_gwh * 1000 / hours,
    gen_tot_src = gen_tot_gwh * 1000 / hours
  ) %>%
  select(observation_date, spot_price_src, load_tot_src, gen_tot_src)

fred_series <- Reduce(
  function(a, b) merge(a, b, by = "observation_date", all = TRUE),
  lapply(c("CP0000ESM086NEST", "ECBMRRFR", "PNGASEUUSDM", "EXUSEU"), fred)
) %>%
  mutate(
    HICP_src = CP0000ESM086NEST,
    ffr_src = ECBMRRFR,
    # USD/MMBtu -> EUR/MWh
    imf_gas_src = PNGASEUUSDM / EXUSEU / 0.293071
  ) %>%
  select(observation_date, HICP_src, ffr_src, imf_gas_src)

ine <- jsonlite::fromJSON(
  "https://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/IPI15382?nult=250"
)$Data
ipi <- data.frame(
  observation_date = month_floor(as.POSIXct(
    ine$Fecha / 1000, origin = "1970-01-01", tz = "Europe/Madrid"
  )),
  IPI_src = ine$Valor
)

ttf <- readxl::read_excel(file.path(root, "gas_daily.xlsx"))
names(ttf)[1:2] <- c("time", "ttf")
ttf <- ttf %>%
  mutate(
    ttf = as.numeric(gsub(",", ".", ttf)),
    observation_date = month_floor(time)
  ) %>%
  filter(is.finite(ttf)) %>%
  group_by(observation_date) %>%
  summarise(ttf_src = mean(ttf), .groups = "drop")

sources <- data.frame(observation_date = dates) %>%
  left_join(electricity, by = "observation_date") %>%
  left_join(fred_series, by = "observation_date") %>%
  left_join(ipi, by = "observation_date") %>%
  left_join(ttf, by = "observation_date")

# Chain IMF gas onto TTF over 2012-2014, before splicing on the benchmark.
ttf_window <- sources$observation_date >= as.Date("2012-01-01") &
  sources$observation_date < as.Date("2015-01-01") &
  is.finite(sources$ttf_src)
imf_to_ttf <- mean(
  sources$ttf_src[ttf_window] / sources$imf_gas_src[ttf_window]
)
message(
  "TTF months 2012-2014: ", sum(ttf_window),
  "; corr(TTF, IMF Europe gas) = ",
  round(cor(sources$ttf_src[ttf_window], sources$imf_gas_src[ttf_window]), 3)
)
sources$spot_gas_src <- ifelse(
  is.finite(sources$ttf_src), sources$ttf_src,
  sources$imf_gas_src * imf_to_ttf
)

# Splice -----------------------------------------------------------------------
extended <- data.frame(observation_date = dates) %>%
  left_join(raw, by = "observation_date") %>%
  left_join(sources, by = "observation_date")
in_overlap <- extended$observation_date >= overlap[[1]] &
  extended$observation_date <= overlap[[2]]
pre_2015 <- extended$observation_date < as.Date("2015-01-01")

diagnostics <- list()
for (variable in c(var_levels, "ffr")) {
  source_name <- paste0(variable, "_src")
  ratio <- extended[[variable]][in_overlap] / extended[[source_name]][in_overlap]
  scale <- if (variable == "ffr") 1 else mean(ratio)
  if (any(!is.finite(extended[[source_name]][pre_2015]))) {
    stop("Missing 2010-2014 source values for ", variable)
  }
  extended[[variable]][pre_2015] <- extended[[source_name]][pre_2015] * scale
  diagnostics[[variable]] <- data.frame(
    variable = variable,
    scale = scale,
    ratio_sd = sd(ratio),
    correlation = cor(
      extended[[variable]][in_overlap], extended[[source_name]][in_overlap]
    )
  )
}
diagnostics <- bind_rows(diagnostics)
diagnostics$imf_to_ttf <- imf_to_ttf
write.csv(diagnostics, diagnostics_path, row.names = FALSE)
print(diagnostics)

extended <- extended %>% select(observation_date, all_of(var_levels), ffr)
if (any(!is.finite(as.matrix(extended[, -1])))) {
  stop("The extended dataset still has missing values.")
}

# Transformations of seasonal_adjustment_quint_sd.R ---------------------------
extended <- extended %>%
  mutate(across(contains("spot"), ~ . / HICP * 100))

for (variable in c("HICP", "spot_price", "spot_gas", "load_tot", "gen_tot")) {
  fit <- seas(
    ts(extended[[variable]], start = c(2010, 1), frequency = 12),
    transform.function = "auto",
    regression.aictest = c("td", "easter")
  )
  extended[[variable]] <- as.numeric(final(fit))
}
for (variable in var_levels) {
  extended[[paste0(variable, "_log")]] <- log(extended[[variable]] + 1)
}

# Central dataset ---------------------------------------------------------------
benchmark <- as.data.frame(read_parquet(benchmark_path)) %>%
  mutate(observation_date = as.Date(observation_date))
central <- data.frame(observation_date = dates) %>%
  left_join(benchmark, by = "observation_date")
central$month <- as.integer(format(central$observation_date, "%m"))
central$year <- as.integer(format(central$observation_date, "%Y"))

# The VAR series come from the spliced, re-adjusted data on the whole sample.
# The gas level stays the benchmark one from 2015 so that the Iberian wedge
# (spot_gas - spot_gas_capped) is unchanged; before 2015 there was no cap.
replaced <- c(
  setdiff(var_levels, "spot_gas"), "ffr",
  paste0(var_levels, "_log")
)
central[replaced] <- extended[match(central$observation_date, extended$observation_date), replaced]
central$spot_gas[pre_2015] <- extended$spot_gas[pre_2015]
central$spot_gas_capped[pre_2015] <- extended$spot_gas[pre_2015]
central$spot_gas_capped_log[pre_2015] <- extended$spot_gas_log[pre_2015]

write_parquet(central, output_path)
message(
  "Saved ", output_path, " (", nrow(central), " months, ",
  ncol(central), " columns)"
)
