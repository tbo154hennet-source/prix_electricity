# Identified VAR historical decompositions with/without spot_gas_log.
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(ggplot2); library(gt)
  library(MASS); library(tidyr); library(vars)
})
root <- 'Z:/prix_electricite_gitlab'
out <- file.path(root, 'VAR_analysis_fr', 'VAR_output')
dirs <- c(hd = file.path(out, 'plot_irfs_rds', '1var', 'hd'),
  tables = file.path(out, 'tables'), figures = file.path(out, 'figures'),
  latex_tables = file.path(root, 'images', 'latex_tables'))
lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
source(file.path(root, 'VAR_analysis_fr', 'auxiliary', 'auxiliary_control_bis.R'))
p <- 12L
irhor <- 40L
demand_vars <- c('load_tot_log', 'spot_price_log', 'gen_tot_log',
  'IPI_log', 'HICP_log', 'ffr')
supply_vars <- c('spot_price_log', 'load_tot_log', 'gen_tot_log',
  'IPI_log', 'HICP_log', 'ffr')
gas_var <- 'spot_gas_log'
proxies <- c(Demand = 'delta_demand', Supply = 'delta_supply')
raw <- read_parquet(file.path(out, 'IV_data_esp_log_elas_sd_2026.parquet'))
raw[['observation_date']] <- as.Date(raw[['observation_date']])
raw <- arrange(raw, observation_date)
# Reproduce the PCA instruments used in compute_IRFs_with_load.R.
proxy_source <- c('delta_s_tot', 'delta_d_tot')
if (any(!proxy_source %in% names(raw)))
  stop('Missing PCA proxy inputs: delta_s_tot and/or delta_d_tot.')
x_proxy <- scale(cbind(s = raw[['delta_s_tot']], d = raw[['delta_d_tot']]),
  center = TRUE, scale = FALSE)
pca_proxy <- prcomp(x_proxy)
raw[['delta_supply']] <- pca_proxy[['x']][, 1]
raw[['delta_demand']] <- pca_proxy[['x']][, 2]
needed <- unique(c('observation_date', demand_vars, supply_vars, gas_var, proxies))
missing <- setdiff(needed, names(raw))
if (length(missing)) stop('Missing columns: ', paste(missing, collapse = ', '))
# Same sample in both variants, including gas availability.
d <- dplyr::filter(dplyr::select(raw, all_of(needed)),
  if_all(-observation_date, is.finite))
if (nrow(d) <= p) stop('Not enough complete observations for VAR(12).')

fit_hd <- function(data, variables, proxy_name) {
  x <- ts(as.matrix(data[, variables, drop = FALSE]), frequency = 12)
  z <- ts(data[[proxy_name]], frequency = 12)
  model <- vars::VAR(x, p = p, type = 'const')
  u <- residuals(model)
  sigma <- cov(u)
  gamma <- t(t(z[-seq_len(p)]) %*% u)
  sigma_a <- gamma %*%
    ginv(t(gamma) %*% ginv(sigma) %*% gamma) %*% t(gamma)
  theta <- matrix(sigma_a[1, ] / sigma_a[1, 1], ncol = 1L)
  rownames(theta) <- variables
  phi <- ginv(theta) %*% gamma
  st <- t(theta) %*% theta
  dm <- solve(st) %*% t(theta) %*% sigma_a %*% theta %*% solve(st)
  di <- try(solve(dm), silent = TRUE)
  if (inherits(di, 'try-error')) di <- ginv(dm)
  right <- t(gamma) %*% ginv(sigma) %*% t(u)
  left <- t(phi) %*% di
  shock <- solve(t(left) %*% left) %*% t(left) %*% right
  coefs <- do.call(cbind, lapply(coef(model), function(z) z[, 1]))
  lambda <- get_lambda_0(MARep(t(coefs), p, irhor), theta, irhor)
  hd <- matrix(0, nrow(u), length(variables))
  for (t in seq_len(nrow(u))) {
    horizons <- seq_len(min(irhor, t))
    shock_dates <- t - horizons + 1L
    for (j in seq_along(variables))
      hd[t, j] <- sum(lambda[horizons, j, 1] * shock[1, shock_dates])
  }
  colnames(hd) <- variables
  list(hd = hd, dates = data[['observation_date']][-seq_len(p)],
    structural_shock = drop(shock), theta_0 = theta, var_model = model)
}

# Impose the impact vector identified with gas on a VAR whose dynamics exclude
# gas. This is an anchored counterfactual decomposition, not a new SVAR.
fit_hd_anchored <- function(data, variables, anchor_theta) {
  x <- ts(as.matrix(data[, variables, drop = FALSE]), frequency = 12)
  model <- vars::VAR(x, p = p, type = 'const')
  u <- residuals(model)
  sigma <- cov(u)
  theta <- anchor_theta[variables, , drop = FALSE]
  denominator <- drop(t(theta) %*% ginv(sigma) %*% theta)
  if (!is.finite(denominator) ||
      abs(denominator) <= sqrt(.Machine$double.eps))
    stop('The gas-identified impact vector cannot anchor the no-gas VAR.')
  shock <- drop((t(theta) %*% ginv(sigma) %*% t(u)) / denominator)
  coefs <- do.call(cbind, lapply(coef(model), function(z) z[, 1]))
  lambda <- get_lambda_0(MARep(t(coefs), p, irhor), theta, irhor)
  hd <- matrix(0, nrow(u), length(variables))
  for (t in seq_len(nrow(u))) {
    horizons <- seq_len(min(irhor, t))
    shock_dates <- t - horizons + 1L
    for (j in seq_along(variables))
      hd[t, j] <- sum(lambda[horizons, j, 1] * shock[shock_dates])
  }
  colnames(hd) <- variables
  list(hd = hd, dates = data[['observation_date']][-seq_len(p)],
    structural_shock = shock, theta_0 = theta, var_model = model)
}

estimate_spec <- function(include_gas) {
  extra <- if (include_gas) gas_var else character()
  dem <- fit_hd(d, c(demand_vars, extra), proxies[['Demand']])
  sup <- fit_hd(d, c(supply_vars, extra), proxies[['Supply']])
  variables <- intersect(colnames(sup[['hd']]), colnames(dem[['hd']]))
  hd <- array(NA_real_, c(nrow(sup[['hd']]), length(variables), 2L),
    dimnames = list(NULL, variables, c('Supply', 'Demand')))
  hd[, , 'Supply'] <- sup[['hd']][, variables, drop = FALSE]
  hd[, , 'Demand'] <- dem[['hd']][, variables, drop = FALSE]
  list(hd = hd, dates = sup[['dates']], supply = sup, demand = dem)
}
including_gas <- estimate_spec(TRUE)
electricity_specific <- estimate_spec(FALSE)
anchored_demand <- fit_hd_anchored(
  d, demand_vars, including_gas[['demand']][['theta_0']])
anchored_supply <- fit_hd_anchored(
  d, supply_vars, including_gas[['supply']][['theta_0']])
anchored_variables <- intersect(
  colnames(anchored_supply[['hd']]), colnames(anchored_demand[['hd']]))
anchored_hd <- array(NA_real_,
  c(nrow(anchored_supply[['hd']]), length(anchored_variables), 2L),
  dimnames = list(NULL, anchored_variables, c('Supply', 'Demand')))
anchored_hd[, , 'Supply'] <-
  anchored_supply[['hd']][, anchored_variables, drop = FALSE]
anchored_hd[, , 'Demand'] <-
  anchored_demand[['hd']][, anchored_variables, drop = FALSE]
anchored_model <- list(hd = anchored_hd,
  dates = anchored_supply[['dates']], supply = anchored_supply,
  demand = anchored_demand)

models <- list(
  'Electricity Specific' = electricity_specific,
  'No-gas dynamics / Gas-identified shocks' = anchored_model,
  'Including Gas' = including_gas)
saveRDS(models, file.path(dirs[['hd']], 'hd_supply_demand_with_without_gas.rds'))

periods <- list(
  price_up = as.Date(c('2021-01-01', '2022-06-01')),
  # Align the inflation episode with the acceleration phase around the
  # observed HICP peak (2022-07), using complete calendar quarters.
  inflation_up = as.Date(c('2021-04-01', '2022-09-01')),
  price_down = as.Date(c('2022-07-01', '2023-12-01')),
  # Capture the decline from the HICP peak to the post-peak trough
  # (2023-06), rather than starting after most disinflation had occurred.
  inflation_down = as.Date(c('2022-07-01', '2023-06-01')))
quarter <- function(x) paste0(format(x, '%Y'), 'Q',
  ((as.integer(format(x, '%m')) - 1L) %/% 3L) + 1L)
period_label <- function(x) paste(quarter(x[1]), quarter(x[2]))
sum_hd <- function(model, variable, period) {
  keep <- model[['dates']] >= period[1] & model[['dates']] <= period[2]
  apply(model[['hd']][keep, variable, , drop = FALSE], 3, sum, na.rm = TRUE)
}

make_table <- function(model, specification) {
  iu <- sum_hd(model, 'HICP_log', periods[['inflation_up']])
  id <- sum_hd(model, 'HICP_log', periods[['inflation_down']])
  pu <- sum_hd(model, 'spot_price_log', periods[['price_up']])
  pd <- sum_hd(model, 'spot_price_log', periods[['price_down']])
  count <- function(period) sum(model[['dates']] >= period[1] &
    model[['dates']] <= period[2])
  mean_price <- function(period) {
    keep <- d[['observation_date']] >= period[1] &
      d[['observation_date']] <= period[2]
    mean(exp(d[['spot_price_log']][keep]), na.rm = TRUE)
  }
  block <- function(regime, inflation, price, ip, pp) {
    n <- count(ip)
    tibble(Regime = regime,
      Section = c('Inflation', 'Inflation', 'Elec Price', 'Elec Price'),
      Period = c(rep(period_label(ip), 2), rep(period_label(pp), 2)),
      Measure = c('log-points', 'annualized percent', 'log-points', 'euros'),
      Demand = c(inflation[['Demand']],
        100 * (exp(inflation[['Demand']])^(12 / n) - 1),
        price[['Demand']], mean_price(pp) * price[['Demand']]),
      Supply = c(inflation[['Supply']],
        100 * (exp(inflation[['Supply']])^(12 / n) - 1),
        price[['Supply']], mean_price(pp) * price[['Supply']]))
  }
  bind_rows(
    block('Energy crisis', iu, pu, periods[['inflation_up']],
      periods[['price_up']]),
    block('Post-Crisis Normalization', id, pd, periods[['inflation_down']],
      periods[['price_down']])
  ) %>% mutate(Specification = specification, .before = Demand)
}

table_long <- bind_rows(lapply(names(models),
  function(spec) make_table(models[[spec]], spec)))
contribution_table <- table_long %>%
  pivot_wider(id_cols = c(Regime, Section, Period, Measure),
    names_from = Specification, values_from = c(Demand, Supply),
    names_glue = '{Specification} - {.value}') %>%
  dplyr::select(Regime, Section, Period, Measure,
    'Electricity Specific - Demand', 'Electricity Specific - Supply',
    'No-gas dynamics / Gas-identified shocks - Demand',
    'No-gas dynamics / Gas-identified shocks - Supply',
    'Including Gas - Demand', 'Including Gas - Supply')
readr::write_csv(contribution_table, file.path(dirs[['tables']],
  'cumulative_contributions_with_without_gas.csv'))

gt_table <- contribution_table %>%
  gt(groupname_col = 'Regime') %>%
  cols_label(Section = '', Measure = '', Period = 'Period') %>%
  tab_spanner(label = 'Electricity Specific',
    columns = c('Electricity Specific - Demand',
      'Electricity Specific - Supply')) %>%
  tab_spanner(label = 'No-gas dynamics / Gas-identified shocks',
    columns = c(
      'No-gas dynamics / Gas-identified shocks - Demand',
      'No-gas dynamics / Gas-identified shocks - Supply')) %>%
  tab_spanner(label = 'Including Gas',
    columns = c('Including Gas - Demand', 'Including Gas - Supply')) %>%
  cols_label(
    'Electricity Specific - Demand' = 'Demand',
    'Electricity Specific - Supply' = 'Supply',
    'No-gas dynamics / Gas-identified shocks - Demand' = 'Demand',
    'No-gas dynamics / Gas-identified shocks - Supply' = 'Supply',
    'Including Gas - Demand' = 'Demand',
    'Including Gas - Supply' = 'Supply') %>%
  fmt_number(columns = where(is.numeric), decimals = 4) %>%
  tab_style(cell_text(weight = 'bold'), cells_row_groups()) %>%
  opt_row_striping() %>% opt_table_outline()
gtsave(gt_table, file.path(dirs[['tables']],
  'cumulative_contributions_with_without_gas.tex'))

# Publication table: retain only the benchmark specification with gas.
including_gas_table <- contribution_table %>%
  dplyr::select(Regime, Section, Period, Measure,
    'Including Gas - Demand', 'Including Gas - Supply')

including_gas_gt <- including_gas_table %>%
  gt(groupname_col = 'Regime') %>%
  cols_label(Section = '', Measure = '', Period = 'Period') %>%
  tab_spanner(label = 'Including Gas',
    columns = c('Including Gas - Demand', 'Including Gas - Supply')) %>%
  cols_label(
    'Including Gas - Demand' = 'Demand',
    'Including Gas - Supply' = 'Supply') %>%
  fmt_number(columns = where(is.numeric), decimals = 4) %>%
  tab_style(cell_text(weight = 'bold'), cells_row_groups()) %>%
  opt_row_striping() %>%
  opt_table_outline()

gtsave(including_gas_gt, file.path(dirs[['latex_tables']],
  'cumulative_contributions.tex'))

plot_specs <- c('Electricity Specific', 'Including Gas')
plot_data <- bind_rows(lapply(plot_specs, function(spec) {
  model <- models[[spec]]
  as.data.frame(model[['hd']][, 'HICP_log', , drop = FALSE][, 1, ]) %>%
    mutate(Date = model[['dates']], Specification = spec) %>%
    pivot_longer(c(Supply, Demand), names_to = 'Shock',
      values_to = 'Contribution')
})) %>%
  mutate(Specification = factor(Specification,
    levels = c('Including Gas', 'Electricity Specific')),
    Legend = paste(Shock, Specification, sep = ' - '))

# Pale wide bars are behind narrow dark electricity-specific bars.
hd_plot <- ggplot() +
  geom_col(data = filter(plot_data, Specification == 'Including Gas'),
    aes(Date, Contribution, fill = Legend), position = 'stack', width = 25) +
  geom_col(data = filter(plot_data, Specification == 'Electricity Specific'),
    aes(Date, Contribution, fill = Legend), position = 'stack', width = 12) +
  scale_fill_manual(values = c(
    'Demand - Including Gas' = '#FCAE91',
    'Supply - Including Gas' = '#9ECAE1',
    'Demand - Electricity Specific' = '#CB181D',
    'Supply - Electricity Specific' = '#08519C'), name = NULL) +
  labs(title = 'Historical decomposition of HICP',
    subtitle = 'Light: Including Gas | Dark: Electricity Specific',
    x = NULL, y = 'Contribution (log-points)') +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'top', panel.grid.minor = element_blank())
ggsave(file.path(dirs[['figures']],
  'historical_decomposition_with_without_gas.png'),
  hd_plot, width = 12, height = 6, dpi = 300)

print(gt_table)
print(hd_plot)



# ============================================================
# SIMPLER TABLE: INCLUDING GAS ONLY
# ============================================================

contribution_table_including_gas <- table_long %>%
  dplyr::filter(Specification == "Including Gas") %>%
  dplyr::select(
    Regime,
    Section,
    Period,
    Measure,
    Demand,
    Supply
  )

# Save CSV
readr::write_csv(
  contribution_table_including_gas,
  file.path(
    dirs[["tables"]],
    "cumulative_contributions_including_gas.csv"
  )
)

# GT table
gt_table_including_gas <- contribution_table_including_gas %>%
  gt(groupname_col = "Regime") %>%
  cols_label(
    Section = "",
    Measure = "",
    Period = "Period",
    Demand = "Demand",
    Supply = "Supply"
  ) %>%
  fmt_number(
    columns = c(Demand, Supply),
    decimals = 4
  ) %>%
  tab_style(
    cell_text(weight = "bold"),
    cells_row_groups()
  ) %>%
  opt_row_striping() %>%
  opt_table_outline()

# Save LaTeX
gtsave(
  gt_table_including_gas,
  file.path(
    dirs[["tables"]],
    "cumulative_contributions_including_gas.tex"
  )
)

# ============================================================
# SIMPLER TABLE: INCLUDING GAS ONLY
# ============================================================

# ============================================================
# COMPACT TABLE: INCLUDING GAS ONLY
# ============================================================

contribution_table_including_gas <- table_long %>%
  dplyr::filter(Specification == "Including Gas") %>%
  dplyr::select(
    Regime,
    Section,
    Period,
    Measure,
    Demand,
    Supply
  ) %>%
  dplyr::mutate(
    Measure = dplyr::recode(
      Measure,
      "annualized percent" = "points"
    )
  )


# ============================================================
# COMPACT TABLE: INCLUDING GAS ONLY
# ============================================================

contribution_table_including_gas <- table_long %>%
  dplyr::filter(Specification == "Including Gas") %>%
  dplyr::select(
    Regime,
    Section,
    Period,
    Measure,
    Demand,
    Supply
  ) %>%
  dplyr::mutate(
    Measure = dplyr::recode(
      Measure,
      "annualized percent" = "points"
    )
  ) %>%
  dplyr::group_by(Regime, Section) %>%
  dplyr::mutate(
    Period_display = dplyr::if_else(
      dplyr::row_number() == 1,
      Period,
      ""
    ),
    Section_display = dplyr::if_else(
      dplyr::row_number() == 1,
      Section,
      ""
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    Regime,
    Section = Section_display,
    Period = Period_display,
    Measure,
    Demand,
    Supply
  )


# ============================================================
# GT TABLE
# ============================================================

gt_table_including_gas <- contribution_table_including_gas %>%
  gt(
    groupname_col = "Regime"
  ) %>%
  
  cols_label(
    Section = "",
    Period = "Period",
    Measure = "",
    Demand = "Demand",
    Supply = "Supply"
  ) %>%
  
  fmt_number(
    columns = c(Demand, Supply),
    decimals = 4
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Section,
      rows = Section != ""
    )
  ) %>%
  
  cols_align(
    align = "left",
    columns = c(Section, Period, Measure)
  ) %>%
  
  cols_align(
    align = "right",
    columns = c(Demand, Supply)
  ) %>%
  
  opt_row_striping() %>%
  opt_table_outline()

# Save LaTeX
gtsave(
  gt_table_including_gas,
  file.path(
    "Z:/Overleaf_elec/images/latex_tables",
    "cumulative_contributions.tex"
  )
)
