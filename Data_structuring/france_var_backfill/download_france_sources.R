# Download the public sources used to extend the French monthly VAR to 2010.
#
# Every file is written to Data_structuring/france_var_backfill/sources/ as a
# small monthly (or annual) CSV, then read by build_france_var_2010.R.
#
#   epex_fr_monthly.csv          EPEX Spot France day-ahead, hourly prices
#                                2009-2016 (ewoken/epex-spot-data archive),
#                                monthly time mean in Europe/Paris months
#   rte_equilibre_mensuel.csv    RTE / ODRE: national monthly production and
#                                gross consumption (GWh), 2007-
#   rte_fc_eolien_mensuel.csv    RTE / ODRE: national monthly wind load factor
#                                (%), 2012-
#   rte_parc_national.csv        RTE / ODRE: installed wind capacity at year end
#                                (MW), 2001-
#   rte_parc_regional.csv        RTE / ODRE: idem by region, 2012-
#   era5_wind_points.csv         the 12 onshore wind points and their region
#   era5_wind_monthly.csv        ERA5 100 m wind (Open-Meteo archive), hourly,
#                                through a generic power curve, monthly means
#                                by point, 2010-01 to 2025-12
#   temperature_point_monthly.csv  Open-Meteo hourly 2 m temperature at the
#                                point that enters the 2015- French data (see
#                                README), monthly mean of |T|, 2010-2016
#   fred_monthly.csv             FRED: HICP France, ECB MRO rate, IMF Europe gas
#                                price (USD/MMBtu), USD/EUR
#   insee_ipi.csv                INSEE IPI manufacturing, SA-WDA, base 2021
#                                (DBnomics)
#   eurostat_degree_days.csv     Eurostat nrg_chdd_m: monthly heating and
#                                cooling degree days, France
#
# Open-Meteo answers are cached outside the repository (large hourly files):
# OPEN_METEO_CACHE, by default C:/Users/DBT3KD/Documents/france_var_backfill_cache.
#
# Usage (from the repo root):
#   Rscript Data_structuring/france_var_backfill/download_france_sources.R

library(jsonlite)

root <- Sys.getenv(
  "PRIX_ELEC_ROOT",
  "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
)
source_dir <- file.path(root, "Data_structuring", "france_var_backfill", "sources")
cache_dir <- Sys.getenv(
  "OPEN_METEO_CACHE",
  "C:/Users/DBT3KD/Documents/france_var_backfill_cache"
)
dir.create(source_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

write_source <- function(data, name) {
  path <- file.path(source_dir, name)
  write.csv(data, path, row.names = FALSE)
  message("Saved ", path, " (", nrow(data), " rows)")
}

paris_month <- function(time) {
  as.Date(format(time, "%Y-%m-01", tz = "Europe/Paris"))
}

# EPEX Spot France --------------------------------------------------------------
epex <- do.call(rbind, lapply(2009:2016, function(year) {
  hourly <- fromJSON(sprintf(
    "https://ewoken.github.io/epex-spot-data/data/%d.json", year
  ))
  # startDate carries its UTC offset (+01:00 / +02:00).
  start <- as.POSIXct(
    sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", hourly$startDate),
    format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"
  )
  if (anyNA(start)) stop("Unparsed EPEX timestamps in ", year)
  data.frame(start = start, price = as.numeric(hourly$price_euros_mwh))
}))
epex <- epex[!duplicated(epex$start), , drop = FALSE]
epex$observation_date <- paris_month(epex$start)
epex_monthly <- do.call(rbind, lapply(
  split(epex, epex$observation_date),
  function(month) {
    data.frame(
      observation_date = month$observation_date[[1]],
      spot_price_epex = mean(month$price, na.rm = TRUE),
      hours = sum(is.finite(month$price))
    )
  }
))
write_source(epex_monthly, "epex_fr_monthly.csv")

# RTE / ODRE --------------------------------------------------------------------
odre <- function(dataset) {
  url <- sprintf(
    "https://odre.opendatasoft.com/api/explore/v2.1/catalog/datasets/%s/exports/csv?delimiter=%%3B",
    dataset
  )
  data <- read.csv(url, sep = ";", check.names = FALSE, fileEncoding = "UTF-8-BOM")
  data[, !grepl("^geo_", names(data)), drop = FALSE]
}
by_first_column <- function(data) data[order(data[[1]]), , drop = FALSE]
write_source(by_first_column(odre("equilibre-national-mensuel-prod-conso-brute")), "rte_equilibre_mensuel.csv")
write_source(by_first_column(odre("fc-tc-nationaux-mensuels-eolien-solaire")), "rte_fc_eolien_mensuel.csv")
write_source(by_first_column(odre("parc-national-annuel-prod-eolien-solaire")), "rte_parc_national.csv")
write_source(by_first_column(odre("parc-regional-annuel-prod-eolien-solaire")), "rte_parc_regional.csv")

# Open-Meteo --------------------------------------------------------------------
open_meteo_hourly <- function(name, latitude, longitude, variable, start, end,
                              model = NULL) {
  path <- file.path(cache_dir, paste0(name, ".csv"))
  if (file.exists(path)) return(read.csv(path))
  query <- c(
    latitude = latitude, longitude = longitude,
    start_date = start, end_date = end,
    hourly = variable, timezone = "GMT", wind_speed_unit = "ms"
  )
  if (!is.null(model)) query <- c(query, models = model)
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    paste(names(query), query, sep = "=", collapse = "&")
  )
  for (attempt in 1:20) {
    answer <- tryCatch(fromJSON(url), error = function(e) e)
    if (!inherits(answer, "error")) break
    # Free tier: 600 weighted calls a minute, 5000 an hour; one multi-year
    # hourly request weighs several hundred calls.
    message("Open-Meteo refused ", name, " (", conditionMessage(answer), "); waiting 90 s")
    Sys.sleep(90)
  }
  if (inherits(answer, "error")) stop("Open-Meteo download failed for ", name)
  data <- data.frame(time = answer$hourly$time, value = as.numeric(answer$hourly[[variable]]))
  write.csv(data, path, row.names = FALSE)
  Sys.sleep(60)
  data
}

monthly_from_hourly <- function(hourly, fun) {
  time <- as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC")
  month <- paris_month(time)
  values <- fun(hourly$value)
  aggregate(
    list(value = values, hours = is.finite(values)),
    list(observation_date = month),
    function(x) if (is.logical(x)) sum(x) else mean(x, na.rm = TRUE)
  )
}

# Generic onshore power curve (cut-in 3 m/s, rated 12 m/s, cut-out 25 m/s).
power_curve <- function(speed) {
  ifelse(
    speed < 3 | speed >= 25, 0,
    ifelse(speed >= 12, 1, (speed^3 - 27) / (12^3 - 27))
  )
}

# One or two points in the onshore wind area of each region holding at least
# 1 % of French capacity; Hauts-de-France and Grand Est get two points each.
wind_points <- data.frame(
  point = c(
    "hdf_aisne", "hdf_artois", "ge_champagne", "ge_meuse", "occitanie_aude",
    "centre_beauce", "naq_poitou", "bretagne", "pdl_loire_atl",
    "normandie_caux", "bfc_yonne", "aura_haute_loire"
  ),
  code_insee_region = c(32, 32, 44, 44, 76, 24, 75, 53, 52, 28, 27, 84),
  latitude = c(49.85, 50.45, 48.75, 48.85, 43.25, 48.15, 46.10, 48.30, 47.20, 49.70, 47.70, 45.20),
  longitude = c(3.20, 2.20, 4.20, 5.40, 2.85, 1.55, 0.10, -2.90, -1.50, 0.70, 3.80, 3.90)
)
write_source(wind_points, "era5_wind_points.csv")

wind_monthly <- do.call(rbind, lapply(seq_len(nrow(wind_points)), function(i) {
  point <- wind_points[i, ]
  hourly <- open_meteo_hourly(
    paste0("era5_wind100m_", point$point), point$latitude, point$longitude,
    "wind_speed_100m", "2010-01-01", "2025-12-31", model = "era5"
  )
  speed <- monthly_from_hourly(hourly, identity)
  cf <- monthly_from_hourly(hourly, power_curve)
  data.frame(
    point = point$point,
    observation_date = speed$observation_date,
    wind_speed_100m = speed$value,
    cf_power_curve = cf$value,
    hours = cf$hours
  )
}))
write_source(wind_monthly, "era5_wind_monthly.csv")

# The 2015- French hourly data keep the first of the 49 population points
# (distinct(datetime) in day_ahead/fr/prepare_regress_day_ahead_fr_new.R).
temperature_hourly <- open_meteo_hourly(
  "temperature_2m_point_50.17875_4.077917", 50.17875, 4.077917,
  "temperature_2m", "2010-01-01", "2016-12-31"
)
temperature_monthly <- monthly_from_hourly(temperature_hourly, abs)
names(temperature_monthly)[2] <- "temperature_discomfort_src"
write_source(temperature_monthly, "temperature_point_monthly.csv")

# FRED and INSEE ----------------------------------------------------------------
fred <- function(id) {
  data <- read.csv(paste0(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id, "&cosd=2009-01-01"
  ))
  names(data) <- c("observation_date", id)
  data[[id]] <- suppressWarnings(as.numeric(data[[id]]))
  data
}
fred_series <- Reduce(
  function(a, b) merge(a, b, by = "observation_date", all = TRUE),
  lapply(c("CP0000FRM086NEST", "ECBMRRFR", "PNGASEUUSDM", "EXUSEU"), fred)
)
write_source(fred_series, "fred_monthly.csv")

# Eurostat national heating / cooling degree days (JRC, population-weighted).
degree_days <- fromJSON(
  "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nrg_chdd_m?geo=FR&format=JSON&lang=EN"
)
indicator_index <- unlist(degree_days$dimension$indic_nrg$category$index)
time_index <- unlist(degree_days$dimension$time$category$index)
n_time <- length(time_index)
dd_value <- function(indicator) {
  position <- indicator_index[[indicator]] * n_time + unname(time_index)
  # Missing cells are absent from the JSON and come back as NA.
  as.numeric(unlist(degree_days$value)[as.character(position)])
}
write_source(
  data.frame(
    observation_date = paste0(names(time_index), "-01"),
    hdd = dd_value("HDD"),
    cdd = dd_value("CDD")
  ),
  "eurostat_degree_days.csv"
)

ipi <- fromJSON(
  "https://api.db.nomics.world/v22/series/INSEE/IPI-2021/M.BDM_EUR.IPI.SO.A21-C.INDICE.FM.SO.CVS-CJO.2021?observations=1&format=json"
)$series$docs
write_source(
  data.frame(
    observation_date = paste0(unlist(ipi$period), "-01"),
    IPI_src = as.numeric(unlist(ipi$value))
  ),
  "insee_ipi.csv"
)
