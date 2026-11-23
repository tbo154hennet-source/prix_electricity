# French Mertens-Ravn identification and cumulative contributions.
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(ggplot2); library(gt)
  library(MASS); library(tidyr); library(vars)
})
root <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
out <- file.path(root, "VAR_analysis_fr", "VAR_output")
hd_dir <- file.path(out, "plot_irfs_rds", "diff", "hd")
table_dir <- file.path(out, "tables")
figure_dir <- file.path(out, "figures")
latex_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
invisible(lapply(c(hd_dir, table_dir, figure_dir, latex_dir), dir.create,
  recursive = TRUE, showWarnings = FALSE))
p <- 12L; irhor <- 40L
# Micro estimate used in the earlier French/Spanish specifications.
demand_price_elasticity <- -0.29
d <- read_parquet(file.path(out, "IV_data_fr_log_elas_sd_2026.parquet"))
d$observation_date <- as.Date(d$observation_date)
d <- arrange(d, observation_date)
pick <- function(candidates, label) {
  ans <- candidates[candidates %in% names(d)][1]
  if (is.na(ans)) stop("Missing ", label, ": ", paste(candidates, collapse = ", "))
  ans
}
generation <- pick(c("gen_forecast_log", "gen_tot_log"), "generation")
z_demand <- pick(c("temperature_discomfort", "temp_excess_instrument"),
  "temperature proxy")
z_supply <- pick(c("Fore_Wind_log", "wind_forecast_log"), "wind proxy")
demand_vars <- c("load_tot_log", "spot_price_log", generation,
  "IPI_log", "HICP_log", "ffr", "spot_gas_log")
needed <- unique(c("observation_date", demand_vars, z_demand, z_supply))
if (length(miss <- setdiff(needed, names(d))))
  stop("Missing columns: ", paste(miss, collapse = ", "))
d <- d %>% select(all_of(needed)) %>% filter(if_all(-observation_date, is.finite))
if (nrow(d) <= p) stop("Not enough complete observations for VAR(12).")
inv <- function(x) {
  ans <- try(solve(x), silent = TRUE)
  if (inherits(ans, "try-error")) MASS::ginv(x) else ans
}

# Joint correlated-proxy identification, matching first_var_estimation_bis().
# The demand impact vector is normalized to Load = 1 and its price response is
# fixed at the independently estimated micro elasticity (-0.29), not zero.
x <- ts(as.matrix(d[, demand_vars, drop = FALSE]), frequency = 12)
fit <- vars::VAR(x, p = p, type = "const")
u <- residuals(fit); sigma <- cov(u)
z <- as.matrix(d[, c(z_demand, z_supply), drop = FALSE])[-seq_len(p), ]
gamma <- t(crossprod(z, u)); n <- ncol(x)
selector <- rbind(diag(2), matrix(0, n - 2, 2))
theta_d <- gamma %*% MASS::ginv(t(selector) %*% gamma) %*%
  c(1, demand_price_elasticity)
restriction_supply <- selector
restriction_supply[, 1] <- MASS::ginv(sigma) %*% theta_d
theta_s <- gamma %*% MASS::ginv(t(restriction_supply) %*% gamma) %*%
  c(0, 1)
theta <- cbind(Demand = theta_d, Supply = theta_s)
rownames(theta) <- demand_vars
sigma_a <- gamma %*%
  MASS::ginv(t(gamma) %*% MASS::ginv(sigma) %*% gamma) %*% t(gamma)
tc <- t(theta) %*% theta
D <- inv(tc) %*% t(theta) %*% sigma_a %*% theta %*% inv(tc)
phi <- matrix(c(1, 0, -theta[1, 2], 1), nrow = 2) %*%
  t(selector) %*% gamma
left <- t(phi) %*% inv(D)
shocks <- inv(t(left) %*% left) %*% t(left) %*%
  t(gamma) %*% MASS::ginv(sigma) %*% t(u)
rownames(shocks) <- c("Demand", "Supply")

# Historical decomposition from the VAR moving-average representation.
ma <- vars::Phi(fit, nstep = irhor - 1)

# French impulse-response functions for the jointly identified shocks.
irf_array <- array(NA_real_, c(irhor, n, 2),
  dimnames = list(Horizon = 0:(irhor - 1), Variable = demand_vars,
    Shock = c("Demand", "Supply")))
for (hh in seq_len(irhor)) {
  irf_array[hh, , ] <- ma[, , hh] %*% theta
}
irf_france <- as.data.frame.table(irf_array,
  responseName = "Response", stringsAsFactors = FALSE) %>%
  mutate(Horizon = as.integer(as.character(Horizon)),
    Variable = case_when(
      Variable == "load_tot_log" ~ "Electricity load",
      Variable == "spot_price_log" ~ "Electricity price",
      Variable == generation ~ "Electricity generation",
      Variable == "IPI_log" ~ "Industrial production",
      Variable == "HICP_log" ~ "HICP",
      Variable == "ffr" ~ "Policy rate",
      Variable == "spot_gas_log" ~ "Gas price",
      TRUE ~ as.character(Variable)))

irf_france_plot <- ggplot(irf_france,
    aes(x = Horizon, y = Response, colour = Shock)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey55") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(Demand = "#CB181D", Supply = "#08519C")) +
  labs(title = "France: impulse responses",
    subtitle = "Joint Mertens-Ravn identification",
    x = "Months after shock", y = "Response", colour = "Shock") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

irf_france_plot

ggsave(file.path(figure_dir, "irf_mertens_ravn_france.png"),
  irf_france_plot, width = 10, height = 11, dpi = 300)

hd <- array(0, c(nrow(u), n, 2),
  dimnames = list(NULL, demand_vars, c("Demand", "Supply")))
for (tt in seq_len(nrow(u))) {
  horizons <- 0:(min(irhor, tt) - 1)
  for (hh in seq_along(horizons)) {
    response <- ma[, , horizons[hh] + 1] %*% theta
    hd[tt, , ] <- hd[tt, , ] +
      response %*% diag(shocks[, tt - horizons[hh]], 2)
  }
}
dates <- d$observation_date[-seq_len(p)]
mr_france <- list(hd = hd, dates = dates, theta_0 = theta,
  structural_shocks = shocks,
  demand_price_elasticity = demand_price_elasticity,
  demand_proxy = z_demand, supply_proxy = z_supply, var_model = fit)
saveRDS(mr_france, file.path(hd_dir, "hd_mertens_ravn_france.rds"))

periods <- list(
  price_up = as.Date(c("2021-01-01", "2022-06-01")),
  inflation_up = as.Date(c("2021-07-01", "2022-12-01")),
  price_down = as.Date(c("2022-07-01", "2023-12-01")),
  inflation_down = as.Date(c("2023-01-01", "2023-12-01")))
quarter <- function(date) paste0(format(date, "%Y"), "Q",
  ((as.integer(format(date, "%m")) - 1L) %/% 3L) + 1L)
period_label <- function(period) paste(quarter(period[1]), quarter(period[2]))
sum_hd <- function(variable, period) {
  keep <- dates >= period[1] & dates <= period[2]
  apply(hd[keep, variable, , drop = FALSE], 3, sum, na.rm = TRUE)
}
mean_price <- function(period) {
  keep <- d$observation_date >= period[1] & d$observation_date <= period[2]
  mean(exp(d$spot_price_log[keep]), na.rm = TRUE)
}
make_block <- function(regime, inflation_period, price_period) {
  inflation <- sum_hd("HICP_log", inflation_period)
  price <- sum_hd("spot_price_log", price_period)
  months <- sum(dates >= inflation_period[1] & dates <= inflation_period[2])
  tibble::tibble(
    Regime = regime,
    Section = c("Inflation", "Inflation", "Elec Price", "Elec Price"),
    Period = c(rep(period_label(inflation_period), 2),
      rep(period_label(price_period), 2)),
    Measure = c("log-points", "points", "log-points", "euros"),
    Demand = c(inflation["Demand"],
      100 * (exp(inflation["Demand"])^(12 / months) - 1),
      price["Demand"], mean_price(price_period) * price["Demand"]),
    Supply = c(inflation["Supply"],
      100 * (exp(inflation["Supply"])^(12 / months) - 1),
      price["Supply"], mean_price(price_period) * price["Supply"]))
}
contributions_mr_france <- bind_rows(
  make_block("Energy crisis", periods$inflation_up, periods$price_up),
  make_block("Post-Crisis Normalization", periods$inflation_down,
    periods$price_down))
# readr::write_csv(contributions_mr_france,
#   file.path(table_dir, "cumulative_contributions_mr_france.csv"))

# Re-estimate Spain with the same micro demand-elasticity restriction as France.
data_spain <- read_parquet(
  file.path(out, "IV_data_esp_log_elas_sd_2026.parquet"))
data_spain$observation_date <- as.Date(data_spain$observation_date)
data_spain <- arrange(data_spain, observation_date)
generation_spain <- c("gen_tot_log", "gen_forecast_log")[
  c("gen_tot_log", "gen_forecast_log") %in% names(data_spain)][1]
temperature_spain <- c("temperature_discomfort", "temp_excess_instrument")[
  c("temperature_discomfort", "temp_excess_instrument") %in%
    names(data_spain)][1]
wind_spain <- c("wind_forecast_log", "Fore_Wind_log")[
  c("wind_forecast_log", "Fore_Wind_log") %in% names(data_spain)][1]
if (any(is.na(c(generation_spain, temperature_spain, wind_spain))))
  stop("Missing Spanish generation, temperature, or wind variable.")

vars_spain <- c("load_tot_log", "spot_price_log", generation_spain,
  "IPI_log", "HICP_log", "ffr", "spot_gas_log")
needed_spain <- c("observation_date", vars_spain,
  temperature_spain, wind_spain)
if (length(miss <- setdiff(needed_spain, names(data_spain))))
  stop("Missing Spanish columns: ", paste(miss, collapse = ", "))
data_spain <- data_spain %>% select(all_of(needed_spain)) %>%
  filter(if_all(-observation_date, is.finite))

x_spain <- ts(as.matrix(data_spain[, vars_spain, drop = FALSE]),
  frequency = 12)
fit_spain <- vars::VAR(x_spain, p = p, type = "const")
u_spain <- residuals(fit_spain)
sigma_spain <- cov(u_spain)
z_spain <- as.matrix(data_spain[,
  c(temperature_spain, wind_spain), drop = FALSE])[-seq_len(p), ]
gamma_spain <- t(crossprod(z_spain, u_spain))
n_spain <- ncol(x_spain)
selector_spain <- rbind(diag(2), matrix(0, n_spain - 2, 2))

# Demand impact: load = 1 and price = the external micro estimate (-0.29).
theta_d_spain <- gamma_spain %*%
  MASS::ginv(t(selector_spain) %*% gamma_spain) %*%
  c(1, demand_price_elasticity)
restriction_s_spain <- selector_spain
restriction_s_spain[, 1] <- MASS::ginv(sigma_spain) %*% theta_d_spain
theta_s_spain <- gamma_spain %*%
  MASS::ginv(t(restriction_s_spain) %*% gamma_spain) %*% c(0, 1)
theta_spain <- cbind(Demand = theta_d_spain, Supply = theta_s_spain)
rownames(theta_spain) <- vars_spain

sigma_a_spain <- gamma_spain %*%
  MASS::ginv(t(gamma_spain) %*% MASS::ginv(sigma_spain) %*%
    gamma_spain) %*% t(gamma_spain)
tc_spain <- t(theta_spain) %*% theta_spain
D_spain <- inv(tc_spain) %*% t(theta_spain) %*% sigma_a_spain %*%
  theta_spain %*% inv(tc_spain)
phi_spain <- matrix(c(1, 0, -theta_spain[1, 2], 1), nrow = 2) %*%
  t(selector_spain) %*% gamma_spain
left_spain <- t(phi_spain) %*% inv(D_spain)
shocks_spain <- inv(t(left_spain) %*% left_spain) %*%
  t(left_spain) %*% t(gamma_spain) %*% MASS::ginv(sigma_spain) %*%
  t(u_spain)
rownames(shocks_spain) <- c("Demand", "Supply")

ma_spain <- vars::Phi(fit_spain, nstep = irhor - 1)
hd_spain <- array(0, c(nrow(u_spain), n_spain, 2),
  dimnames = list(NULL, vars_spain, c("Demand", "Supply")))
for (tt in seq_len(nrow(u_spain))) {
  horizons <- 0:(min(irhor, tt) - 1)
  for (hh in seq_along(horizons)) {
    response <- ma_spain[, , horizons[hh] + 1] %*% theta_spain
    hd_spain[tt, , ] <- hd_spain[tt, , ] +
      response %*% diag(shocks_spain[, tt - horizons[hh]], 2)
  }
}
dates_spain <- data_spain$observation_date[-seq_len(p)]
mr_spain <- list(hd = hd_spain, dates = dates_spain,
  theta_0 = theta_spain, structural_shocks = shocks_spain,
  demand_price_elasticity = demand_price_elasticity,
  demand_proxy = temperature_spain, supply_proxy = wind_spain,
  var_model = fit_spain)
saveRDS(mr_spain, file.path(hd_dir, "hd_mertens_ravn_spain_micro_elas.rds"))

sum_hd_spain <- function(variable, period) {
  keep <- dates_spain >= period[1] & dates_spain <= period[2]
  apply(hd_spain[keep, variable, , drop = FALSE], 3, sum, na.rm = TRUE)
}
mean_price_spain <- function(period) {
  keep <- data_spain$observation_date >= period[1] &
    data_spain$observation_date <= period[2]
  mean(exp(data_spain$spot_price_log[keep]), na.rm = TRUE)
}
make_block_spain <- function(regime, inflation_period, price_period) {
  inflation <- sum_hd_spain("HICP_log", inflation_period)
  price <- sum_hd_spain("spot_price_log", price_period)
  months <- sum(dates_spain >= inflation_period[1] &
    dates_spain <= inflation_period[2])
  tibble::tibble(
    Regime = regime,
    Section = c("Inflation", "Inflation", "Elec Price", "Elec Price"),
    Period = c(rep(period_label(inflation_period), 2),
      rep(period_label(price_period), 2)),
    Measure = c("log-points", "points", "log-points", "euros"),
    Demand = c(inflation["Demand"],
      100 * (exp(inflation["Demand"])^(12 / months) - 1),
      price["Demand"], mean_price_spain(price_period) * price["Demand"]),
    Supply = c(inflation["Supply"],
      100 * (exp(inflation["Supply"])^(12 / months) - 1),
      price["Supply"], mean_price_spain(price_period) * price["Supply"]))
}
contributions_mr_spain <- bind_rows(
  make_block_spain("Energy crisis", periods$inflation_up, periods$price_up),
  make_block_spain("Post-Crisis Normalization", periods$inflation_down,
    periods$price_down))
# readr::write_csv(contributions_mr_spain,
#   file.path(table_dir, "cumulative_contributions_mr_spain.csv"))

table_keys <- c("Regime", "Section", "Period", "Measure")
contributions_mr_countries <- contributions_mr_spain %>%
  rename(`Spain Demand` = Demand, `Spain Supply` = Supply) %>%
  left_join(contributions_mr_france %>%
      rename(`France Demand` = Demand, `France Supply` = Supply),
    by = table_keys)
readr::write_csv(contributions_mr_countries,
  file.path(table_dir, "cumulative_contributions_mr_spain_france.csv"))

gt_contributions_mr_countries <- contributions_mr_countries %>%
  gt(groupname_col = "Regime") %>%
  tab_spanner("Mertens and Ravn - Spain",
    columns = c(`Spain Demand`, `Spain Supply`)) %>%
  tab_spanner("Mertens and Ravn - France",
    columns = c(`France Demand`, `France Supply`)) %>%
  cols_label(Section = "", Period = "Period", Measure = "",
    `Spain Demand` = "Demand", `Spain Supply` = "Supply",
    `France Demand` = "Demand", `France Supply` = "Supply") %>%
  fmt_number(columns = c(`Spain Demand`, `Spain Supply`,
    `France Demand`, `France Supply`), decimals = 4) %>%
  tab_source_note(md(paste0(
    "*Both countries are re-estimated with correlated wind-supply and ",
    "temperature-demand proxies. Demand price elasticity fixed at ",
    demand_price_elasticity, ".*"))) %>%
  tab_style(cell_text(weight = "bold"), cells_row_groups()) %>%
  opt_row_striping() %>% opt_table_outline()
gtsave(gt_contributions_mr_countries,
  file.path(latex_dir, "cumulative_contributions_mr_spain_france.tex"))
print(gt_contributions_mr_countries)
print(irf_france_plot)
