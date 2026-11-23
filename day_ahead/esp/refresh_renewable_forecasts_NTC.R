# Refresh the Spanish renewable-forecast columns of the hourly day-ahead files after the ENTSO-E re-download
# (2026-11-20; the January 2026 day-ahead and intraday wind/solar files were corrupt in 2020, 2022, 2024-2025).
# Equivalent to the left_join on renewable_forecasts_esp.parquet in prepare_regress_niveau_hourly_ter.R (line ~213):
# the columns coming from that file are dropped and joined again by datetime, nothing else changes.
# Run after day_ahead/esp/build_renewable_forecast_revision_bis.R.
library(arrow)
library(dplyr)

root_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
renewable_forecasts <- read_parquet(file.path(root_dir, "renewable_forecasts_esp.parquet"))
renewable_columns <- setdiff(names(renewable_forecasts), "__index_level_0__")

for (file in c("price_indispo_merge_esp.parquet", "price_indispo_merge_esp_NTC.parquet")) {
  path <- file.path(root_dir, "day_ahead/outputs/esp", file)
  hourly <- read_parquet(path)
  order <- names(hourly)
  refreshed <- hourly %>%
    dplyr::select(-dplyr::any_of(renewable_columns)) %>%
    left_join(renewable_forecasts, by = c("datetime" = "__index_level_0__")) %>%
    dplyr::select(dplyr::all_of(order))
  stopifnot(nrow(refreshed) == nrow(hourly), identical(refreshed$datetime, hourly$datetime))
  message(file, ": Fore_Wind changed in ", round(mean(abs(refreshed$Fore_Wind - hourly$Fore_Wind) > 1, na.rm = TRUE), 3),
          " of hours; NA ", sum(is.na(hourly$Fore_Wind)), " -> ", sum(is.na(refreshed$Fore_Wind)))
  write_parquet(refreshed, path)
}
