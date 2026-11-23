# French monthly VAR dataset, 2010-01 to 2025-09.
#
# VAR variables (spot_price, load_tot, gen_forecast, IPI, HICP, ffr, spot_gas):
#   2015-01 onwards: raw levels of the benchmark
#     (Data_structuring/output/IV_data_fr_new.parquet).
#   2010-2014: public sources (download_france_sources.R), rescaled by the mean
#   ratio benchmark / source over 2015-01 -> 2016-12:
#     spot_price    EPEX Spot France day-ahead, monthly time mean of hourly prices
#     load_tot      RTE gross consumption, GWh -> average MW
#     gen_forecast  RTE total production, GWh -> average MW (the benchmark uses
#                   the ENTSO-E day-ahead generation forecast, not observed
#                   before 2015)
#     spot_gas      TTF monthly mean (gas_price_formatted.parquet) from 2012,
#                   IMF "Natural gas, Europe" in EUR/MWh chained before
#     IPI           INSEE IPI manufacturing, SA-WDA, base 2021
#     HICP          FRED CP0000FRM086NEST
#     ffr           FRED ECBMRRFR (levels, no rescaling)
#   then the transformations of seasonal_adjustment_fr_sd.R on 2010-2025:
#   spot prices deflated by HICP, X-13 (auto transform, td/easter AIC test) on
#   HICP, spot_price, spot_gas, load_tot, gen_forecast, then log(x + 1).
#
# Supply proxies (wind). NA where unobserved: they must not be read as zero
# shocks (VAR_analysis/R/00_utils.R, proxy_instrument()).
#   wind_era5_surprise  2010-: capacity_t x (CF_t - calendar-month mean CF), GW,
#                       CF = ERA5 100 m wind through a power curve, weighted by
#                       regional capacity, calibrated on the RTE load factor
#   wind_rte_surprise   2012-: same with the RTE observed wind load factor
#   wind_era5_cf_log    2010-: log ERA5 CF, calendar-month anomaly
#   wind_rte_cf_log     2012-: log RTE CF, calendar-month anomaly
#   *_tadj              the four above, residuals on the Eurostat heating and
#                       cooling degree-day anomalies (wind is milder weather)
#   Fore_Wind_log       2015-: benchmark proxy (ENTSO-E day-ahead wind forecast,
#                       log level, calendar-month anomaly), copied unchanged
# Demand proxy: temperature_discomfort_log, log monthly mean |T| at the benchmark
# point, calendar-month anomaly over 2010-2025.
# Every other benchmark column is copied from
# VAR_analysis/data/IV_data_fr_log_elas_sd_2026.parquet and is NA before 2015.
#
# Usage (from the repo root):
#   Rscript Data_structuring/france_var_backfill/download_france_sources.R
#   Rscript Data_structuring/france_var_backfill/build_france_var_2010.R

library(dplyr)
library(arrow)
library(seasonal)

# The x13binary copy in the user library is blocked by group policy on INSEE
# machines; the one shipped in the INSEE R library is allowed.
insee_x13 <- file.path(R.home(), "library", "x13binary", "bin")
if (!nzchar(Sys.getenv("X13_PATH")) && dir.exists(insee_x13)) {
  Sys.setenv(X13_PATH = insee_x13)
}

root <- Sys.getenv(
  "PRIX_ELEC_ROOT",
  "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
)
backfill_dir <- file.path(root, "Data_structuring", "france_var_backfill")
source_dir <- file.path(backfill_dir, "sources")
raw_path <- file.path(root, "Data_structuring", "output", "IV_data_fr_new.parquet")
benchmark_path <- file.path(
  root, "VAR_analysis", "data", "IV_data_fr_log_elas_sd_2026.parquet"
)
gas_path <- file.path(root, "day_ahead", "outputs", "gas_price_formatted.parquet")
output_path <- file.path(
  root, "VAR_analysis", "data", "IV_data_fr_log_elas_sd_2010.parquet"
)
diagnostics_path <- file.path(backfill_dir, "splice_diagnostics_2010.csv")
wind_diagnostics_path <- file.path(backfill_dir, "wind_proxy_diagnostics_2010.csv")

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2025-09-01")
overlap <- as.Date(c("2015-01-01", "2016-12-01"))
dates <- seq(start_date, end_date, by = "month")
var_levels <- c("spot_price", "load_tot", "gen_forecast", "IPI", "HICP", "spot_gas")
x13_vars <- c("HICP", "spot_price", "spot_gas", "load_tot", "gen_forecast")

read_source <- function(name) {
  data <- read.csv(file.path(source_dir, name), check.names = FALSE)
  if ("observation_date" %in% names(data)) {
    data$observation_date <- as.Date(data$observation_date)
  }
  data
}
month_floor <- function(x) as.Date(format(as.Date(x), "%Y-%m-01"))
calendar_month <- function(x) as.integer(format(x, "%m"))
# Calendar-month anomaly computed on the observed months only.
monthly_anomaly <- function(x, date) {
  month <- calendar_month(date)
  x - ave(x, month, FUN = function(v) mean(v, na.rm = TRUE))
}
hours_in_month <- function(date) {
  next_month <- seq(min(date), by = "month", length.out = length(date) + 1L)[-1]
  as.numeric(difftime(
    as.POSIXct(format(next_month), tz = "Europe/Paris"),
    as.POSIXct(format(date), tz = "Europe/Paris"),
    units = "hours"
  ))
}

# Benchmark raw levels -------------------------------------------------------
raw <- as.data.frame(read_parquet(raw_path)) %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  filter(observation_date >= as.Date("2015-01-01"), observation_date <= end_date) %>%
  select(observation_date, all_of(var_levels), ffr, temperature_discomfort)

# Backfill sources -------------------------------------------------------------
epex <- read_source("epex_fr_monthly.csv") %>%
  transmute(observation_date, spot_price_src = spot_price_epex, epex_hours = hours)

rte <- read_source("rte_equilibre_mensuel.csv") %>%
  transmute(
    observation_date = as.Date(paste0(mois, "-01")),
    production_totale, consommation_brute
  ) %>%
  arrange(observation_date)
rte_hours <- hours_in_month(rte$observation_date)
rte <- rte %>%
  transmute(
    observation_date,
    load_tot_src = consommation_brute * 1000 / rte_hours,
    gen_forecast_src = production_totale * 1000 / rte_hours
  )

fred_series <- read_source("fred_monthly.csv") %>%
  transmute(
    observation_date,
    HICP_src = CP0000FRM086NEST,
    ffr_src = ECBMRRFR,
    # USD/MMBtu -> EUR/MWh
    imf_gas_src = PNGASEUUSDM / EXUSEU / 0.293071
  )

ipi <- read_source("insee_ipi.csv")

ttf <- as.data.frame(read_parquet(gas_path)) %>%
  mutate(observation_date = month_floor(time)) %>%
  filter(is.finite(spot_gas)) %>%
  group_by(observation_date) %>%
  summarise(ttf_src = mean(spot_gas), .groups = "drop")

temperature <- read_source("temperature_point_monthly.csv") %>%
  select(observation_date, temperature_discomfort_src)

sources <- data.frame(observation_date = dates) %>%
  left_join(epex, by = "observation_date") %>%
  left_join(rte, by = "observation_date") %>%
  left_join(fred_series, by = "observation_date") %>%
  left_join(ipi, by = "observation_date") %>%
  left_join(ttf, by = "observation_date") %>%
  left_join(temperature, by = "observation_date")

pre_2015 <- sources$observation_date < as.Date("2015-01-01")
if (any(sources$epex_hours[pre_2015] < 24 * 28)) {
  stop("Incomplete EPEX months before 2015.")
}

# Chain IMF gas onto TTF over 2012-2014, before splicing on the benchmark.
ttf_window <- sources$observation_date >= as.Date("2012-01-01") &
  sources$observation_date < as.Date("2015-01-01") &
  is.finite(sources$ttf_src)
imf_to_ttf <- mean(sources$ttf_src[ttf_window] / sources$imf_gas_src[ttf_window])
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

log_diff <- function(x) diff(log(x))
diagnostics <- list()
for (variable in c(var_levels, "ffr", "temperature_discomfort")) {
  source_name <- paste0(variable, "_src")
  benchmark <- extended[[variable]][in_overlap]
  source <- extended[[source_name]][in_overlap]
  ratio <- benchmark / source
  # Same series for the rate (levels) and the temperature (same Open-Meteo point).
  scale <- if (variable %in% c("ffr", "temperature_discomfort")) 1 else mean(ratio)
  if (any(!is.finite(extended[[source_name]][pre_2015]))) {
    stop("Missing 2010-2014 source values for ", variable)
  }
  extended[[variable]][pre_2015] <- extended[[source_name]][pre_2015] * scale
  diagnostics[[variable]] <- data.frame(
    variable = variable,
    scale = scale,
    ratio_sd = sd(ratio),
    correlation_levels = cor(benchmark, source),
    correlation_monthly_changes = if (variable == "ffr") NA_real_ else
      cor(log_diff(benchmark), log_diff(source))
  )
}
diagnostics <- bind_rows(diagnostics)
diagnostics$imf_to_ttf <- imf_to_ttf
write.csv(diagnostics, diagnostics_path, row.names = FALSE)
print(diagnostics)

extended <- extended %>%
  select(observation_date, all_of(var_levels), ffr, temperature_discomfort)
if (any(!is.finite(as.matrix(extended[, -1])))) {
  stop("The extended dataset still has missing values.")
}

# Transformations of seasonal_adjustment_fr_sd.R ------------------------------
extended <- extended %>%
  mutate(across(contains("spot"), ~ . / HICP * 100))
for (variable in x13_vars) {
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
extended$temperature_discomfort_log <- monthly_anomaly(
  log(extended$temperature_discomfort + 1), extended$observation_date
)

# Wind supply proxies ----------------------------------------------------------
# Installed capacity: year-end values interpolated to mid-month.
parc <- read_source("rte_parc_national.csv")
capacity <- data.frame(observation_date = dates) %>%
  mutate(
    year = as.integer(format(observation_date, "%Y")),
    month = calendar_month(observation_date),
    previous = parc$parc_installe_eolien[match(year - 1L, parc$annee)],
    current = parc$parc_installe_eolien[match(year, parc$annee)],
    capacity_mw = previous + (current - previous) * (month - 0.5) / 12
  )
if (any(!is.finite(capacity$capacity_mw))) stop("Missing wind capacity.")

# ERA5 load factor, regional capacity weights (2012 shares before 2012).
points <- read_source("era5_wind_points.csv")
regional <- read_source("rte_parc_regional.csv") %>%
  filter(code_insee_region %in% points$code_insee_region) %>%
  select(annee, code_insee_region, parc_installe_eolien)
points_per_region <- table(points$code_insee_region)
era5 <- read_source("era5_wind_monthly.csv") %>%
  left_join(points[, c("point", "code_insee_region")], by = "point") %>%
  mutate(annee = pmax(as.integer(format(observation_date, "%Y")), min(regional$annee))) %>%
  left_join(regional, by = c("annee", "code_insee_region")) %>%
  mutate(weight = parc_installe_eolien /
           as.numeric(points_per_region[as.character(code_insee_region)])) %>%
  group_by(observation_date) %>%
  summarise(
    cf_era5_raw = sum(weight * cf_power_curve) / sum(weight),
    points = n(),
    .groups = "drop"
  )
if (any(era5$points != nrow(points))) stop("ERA5 points missing in some months.")

rte_cf <- read_source("rte_fc_eolien_mensuel.csv") %>%
  transmute(observation_date = as.Date(paste0(mois, "-01")), cf_rte = fc_moyen_eolien / 100)

wind <- capacity %>%
  select(observation_date, capacity_mw) %>%
  left_join(era5, by = "observation_date") %>%
  left_join(rte_cf, by = "observation_date")
if (any(!is.finite(wind$cf_era5_raw))) stop("ERA5 load factor missing.")

calibration <- lm(cf_rte ~ cf_era5_raw, data = wind)
wind$cf_era5 <- predict(calibration, newdata = wind)
wind <- wind %>%
  mutate(
    wind_era5_surprise = capacity_mw *
      monthly_anomaly(cf_era5, observation_date) / 1000,
    wind_rte_surprise = capacity_mw *
      monthly_anomaly(cf_rte, observation_date) / 1000,
    wind_era5_cf_log = monthly_anomaly(log(cf_era5), observation_date),
    wind_rte_cf_log = monthly_anomaly(log(cf_rte), observation_date)
  )

# Windy winter months are mild in France (westerly flows), calm ones cold and
# anticyclonic: the wind proxies load on heating demand. The *_tadj variants
# are their residuals on the national degree-day anomalies (Eurostat).
degree_days <- read_source("eurostat_degree_days.csv")
wind <- wind %>%
  left_join(degree_days, by = "observation_date") %>%
  mutate(
    hdd_anomaly = monthly_anomaly(hdd, observation_date),
    cdd_anomaly = monthly_anomaly(cdd, observation_date)
  )
if (any(!is.finite(wind$hdd_anomaly) | !is.finite(wind$cdd_anomaly))) {
  stop("Missing Eurostat degree days.")
}
temperature_adjusted <- function(proxy) {
  model <- lm(stats::reformulate(c("hdd_anomaly", "cdd_anomaly"), proxy), data = wind)
  adjusted <- rep(NA_real_, nrow(wind))
  adjusted[as.integer(names(stats::residuals(model)))] <- stats::residuals(model)
  adjusted
}
for (proxy in c("wind_era5_surprise", "wind_rte_surprise", "wind_era5_cf_log", "wind_rte_cf_log")) {
  wind[[paste0(proxy, "_tadj")]] <- temperature_adjusted(proxy)
}

# Central dataset ---------------------------------------------------------------
benchmark <- as.data.frame(read_parquet(benchmark_path)) %>%
  mutate(observation_date = as.Date(observation_date))
central <- data.frame(observation_date = dates) %>%
  left_join(benchmark, by = "observation_date")
central$month <- calendar_month(central$observation_date)
central$year <- as.integer(format(central$observation_date, "%Y"))

replaced <- c(
  var_levels, "ffr", paste0(var_levels, "_log"),
  "temperature_discomfort", "temperature_discomfort_log"
)
central[replaced] <- extended[match(central$observation_date, extended$observation_date), replaced]
wind_columns <- c(
  "capacity_mw", "cf_era5", "cf_rte",
  "wind_era5_surprise", "wind_rte_surprise", "wind_era5_cf_log", "wind_rte_cf_log",
  paste0(c("wind_era5_surprise", "wind_rte_surprise", "wind_era5_cf_log", "wind_rte_cf_log"), "_tadj"),
  "hdd", "cdd", "hdd_anomaly", "cdd_anomaly"
)
central[wind_columns] <- wind[match(central$observation_date, wind$observation_date), wind_columns]
# The benchmark file stores unobserved values as 0: restore NA before 2015.
central$Fore_Wind_log[central$observation_date < as.Date("2015-01-01")] <- NA_real_

# Wind diagnostics --------------------------------------------------------------
pair_cor <- function(x, y) {
  keep <- is.finite(central[[x]]) & is.finite(central[[y]])
  data.frame(x = x, y = y, months = sum(keep), correlation = cor(central[[x]][keep], central[[y]][keep]))
}
wind_diagnostics <- bind_rows(
  pair_cor("cf_era5", "cf_rte"),
  pair_cor("wind_era5_surprise", "wind_rte_surprise"),
  pair_cor("wind_era5_cf_log", "wind_rte_cf_log"),
  pair_cor("wind_era5_surprise", "Fore_Wind_log"),
  pair_cor("wind_rte_surprise", "Fore_Wind_log"),
  pair_cor("wind_era5_cf_log", "Fore_Wind_log"),
  pair_cor("wind_era5_surprise", "hdd_anomaly"),
  pair_cor("wind_era5_cf_log", "hdd_anomaly"),
  pair_cor("wind_rte_surprise", "hdd_anomaly"),
  pair_cor("Fore_Wind_log", "hdd_anomaly"),
  pair_cor("temperature_discomfort_log", "hdd_anomaly")
)
wind_diagnostics$calibration_intercept <- coef(calibration)[[1]]
wind_diagnostics$calibration_slope <- coef(calibration)[[2]]
wind_diagnostics$calibration_r2 <- summary(calibration)$r.squared
write.csv(wind_diagnostics, wind_diagnostics_path, row.names = FALSE)
print(wind_diagnostics)

write_parquet(central, output_path)
message("Saved ", output_path, " (", nrow(central), " months, ", ncol(central), " columns)")
