# France, hourly price elasticity of demand with the day-ahead wind forecast as a pure supply shifter
# (Step 1 of the fast-track identification of the working paper; Spanish counterpart:
# supply_demand_curve/estimate_elasticity/estimate_elas_standard_IV.R, row "Supply", Lag 0).
# log(load) on log(price + 1) instrumented by Fore_Wind, prices >= 10 EUR, Driscoll-Kraay (24 h) standard errors.
# Output: estimate_elasticity/outputs/elasticity_standard_fr.csv, read by VAR_analysis (stage france2010).
# Usage (repo root): Rscript supply_demand_curve/estimate_elasticity/fr/estimate_elas_standard_IV_fr.R
suppressPackageStartupMessages({library(arrow); library(dplyr); library(fixest)})

root <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
hourly_dir <- file.path(root, "day_ahead", "outputs")
d <- read_parquet(file.path(hourly_dir, "fr", "price_indispo_merge_fr_NTC.parquet")) %>%
  distinct(datetime, .keep_all = TRUE)
# outage columns are missing when no outage is declared (same rule as fr/build_french_proxies.R)
capacity <- grep("Capacity", names(d), value = TRUE)
d[capacity] <- lapply(d[capacity], function(x) ifelse(is.na(x), 0, x))
# hourly French load (ENTSO-E actual total load), day_ahead/fr/prepare_regress_day_ahead_fr_new.R
load <- read_parquet(file.path(hourly_dir, "fr_generation_day_ahead.parquet")) %>%
  select(datetime, load_tot) %>% distinct(datetime, .keep_all = TRUE)
d <- inner_join(d, load, by = "datetime") %>%
  filter(price >= 10, is.finite(load_tot), load_tot > 0) %>% as.data.frame()
d$creneau <- factor(d$hour); d$weekday <- factor(format(d$datetime, "%u"))
d$year <- factor(d$year); d$month <- factor(d$month)

controls <- c("Fore_Solar", "spot_gas", "Published.Predicted.Unavailable.Capacity",
              "Unpublished.Predicted.Unavailable.Capacity", "cbf_fr", "relative_humidity_2m", "precipitation",
              "cold_excess_instrument", "heat_excess_instrument")
# first row: same fixed effects as the Spanish standard IV (central value); second: with weekday effects
fixed_effects <- c("year + month + hour" = "year + month + creneau",
                   "year + month + weekday + hour" = "year + month + weekday + creneau")
rows <- lapply(names(fixed_effects), function(fe) {
  m <- feols(as.formula(paste0("log(load_tot) ~ ", paste(controls, collapse = " + "), " | ", fixed_effects[[fe]],
                               " | log(price + 1) ~ Fore_Wind")),
             data = d, vcov = DK(24) ~ datetime, notes = FALSE)
  fs <- summary(m, stage = 1, vcov = DK(24) ~ datetime)
  data.frame(Country = "France", Model = "Supply", Instrument = "Fore_Wind", FE = fe,
             Coef = unname(coef(m)["fit_log(price + 1)"]), Se = unname(se(m)["fit_log(price + 1)"]),
             IVF = unname((coef(fs)["Fore_Wind"] / se(fs)["Fore_Wind"])^2), N = nobs(m), Lag = 0,
             Start = format(min(d$datetime), "%Y-%m-%d"), End = format(max(d$datetime), "%Y-%m-%d"))
})
result <- do.call(rbind, rows)
print(result, digits = 3)
write.csv(result, file.path(root, "supply_demand_curve", "estimate_elasticity", "outputs", "elasticity_standard_fr.csv"),
          row.names = FALSE)
