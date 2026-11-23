# Wind-proxy supply-shock estimation for France and Spain.
# Plotting is deliberately kept in supply_demand/irf_plot.Rmd.
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(MASS); library(vars)
})
root <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
out <- file.path(root, "VAR_analysis_fr", "VAR_output")
save_dir <- file.path(out, "plot_irfs_rds", "wind_supply")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
p <- 12L; irhor <- 40L

pick <- function(data, candidates, label, country) {
  ans <- candidates[candidates %in% names(data)][1]
  if (is.na(ans)) stop("Missing ", label, " for ", country, ": ",
    paste(candidates, collapse = ", "))
  ans
}
estimate_wind_supply <- function(data, generation, wind_proxy, country) {
  variables <- c("spot_price_log", "load_tot_log", generation,
    "IPI_log", "HICP_log", "ffr", "spot_gas_log")
  required <- unique(c("observation_date", variables, wind_proxy))
  if (length(missing <- setdiff(required, names(data))))
    stop("Missing inputs for ", country, ": ", paste(missing, collapse = ", "))
  sample <- data %>%
   dplyr::select(all_of(required)) %>%
    mutate(observation_date = as.Date(observation_date)) %>%
    arrange(observation_date) %>%
    filter(if_all(-observation_date, is.finite))
  x <- ts(as.matrix(sample[, variables, drop = FALSE]), frequency = 12)
  model <- vars::VAR(x, p = p, type = "const")
  u <- residuals(model); sigma <- cov(u)
  wind <- sample[[wind_proxy]][-seq_len(p)]
  gamma <- matrix(drop(crossprod(wind, u)), ncol = 1)
  sigma_a <- gamma %*%
    MASS::ginv(t(gamma) %*% MASS::ginv(sigma) %*% gamma) %*% t(gamma)
  # Price is first, so the price impact is normalized to one.
  theta <- matrix(sigma_a[1, ] / sigma_a[1, 1], ncol = 1,
    dimnames = list(variables, "Wind supply"))
  ma <- vars::Phi(model, nstep = irhor - 1L)
  irf <- do.call(rbind, lapply(seq_len(irhor), function(hh) {
    data.frame(Horizon = hh - 1L, Variable = variables,
      Response = drop(ma[, , hh] %*% theta), Country = country,
      Shock = "Wind supply")
  }))
  list(theta_0 = theta, irf = irf, var_model = model,
    dates = sample$observation_date[-seq_len(p)], wind_proxy = wind_proxy,
    variables = variables)
}

france <- read_parquet(file.path(out, "IV_data_fr_log_elas_sd_2026.parquet"))
generation_france <- pick(france, c("gen_forecast_log", "gen_tot_log"),
  "generation", "France")
wind_france <- pick(france, c("Fore_Wind_log", "wind_forecast_log"),
  "wind proxy", "France")
wind_supply_france <- estimate_wind_supply(
  france, generation_france, wind_france, "France")

spain <- read_parquet(file.path(out, "IV_data_esp_log_elas_sd_2026.parquet"))
generation_spain <- pick(spain, c("gen_tot_log", "gen_forecast_log"),
  "generation", "Spain")
wind_spain <- pick(spain, c("wind_forecast_log", "Fore_Wind_log"),
  "wind proxy", "Spain")
wind_supply_spain <- estimate_wind_supply(
  spain, generation_spain, wind_spain, "Spain")

saveRDS(wind_supply_france,
  file.path(save_dir, "irf_wind_supply_france.rds"))
saveRDS(wind_supply_spain,
  file.path(save_dir, "irf_wind_supply_spain.rds"))
message("Wind-supply estimates saved in: ", save_dir)
