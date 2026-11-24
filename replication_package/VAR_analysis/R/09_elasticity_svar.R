# Central identification: Bayesian SVAR on the electricity supply- and demand-curve slopes, identified by monthly
# physical proxies and sign restrictions on the slopes.
#
# Structural block on the reduced-form innovations of the monthly VAR, netted of the gas-price innovation
# (the TTF gas price is treated as exogenous to Spanish electricity shocks within the month):
#   supply curve  u_load = eps_s * u_price + v_s     adverse supply shock e_s = -v_s / sd(v_s)
#   demand curve  u_load = eps_d * u_price + v_d     demand shock          e_d =  v_d / sd(v_d)
# Orthogonality of e_s and e_d maps eps_s into eps_d, so the VAR likelihood is flat along that set
# (Baumeister and Hamilton, 2015, 2019). In the central specification the position along the set is informed by
#   - the signs eps_s > 0 and eps_d < 0 only (no prior on the slope values),
#   - two monthly physical proxies (attach_physical_proxies): available non-fossil energy (realised wind speed and
#     6-month cumulated precipitation, orthogonalised on temperature) loads on the supply shock only, and
#     temperature-discomfort degree-days (orthogonalised on wind and hydro) on the demand shock only
#     (likelihood ratios of the two exclusion restrictions, as in a two-proxy SVAR).
# The micro elasticities (hourly IV supply slope 0.237; daily local-projection demand slope -0.02 to -0.05) are
# used as validation. Robustness: the same proxies with slope priors centred on them, and the former central
# specification with the synthetic hourly curve shifts aggregated to months (Kilian, 2024) and slope priors.
# The reduced form uses the Minnesota / sum-of-coefficients / single-unit-root prior with hyperparameters at the
# posterior mode of the marginal likelihood (Giannone, Lenza and Primiceri, 2015). No sign restriction is imposed
# on the responses.

# ----------------------------------------------------------------------------------------------------
# 1. Kilian-aggregated block proxies from the hourly curve-shift regressions
# ----------------------------------------------------------------------------------------------------
elasticity_block_instruments <- function() {
  list(
    all = c("Fore_Wind", "Fore_Solar", "Published.Predicted.Unavailable.Capacity",
            "cold_excess_demand", "heat_excess_demand", "cbf_es"),
    supply = c("Fore_Wind", "Fore_Solar", "Published.Predicted.Unavailable.Capacity", "cbf_es"),
    demand = c("cold_excess_demand", "heat_excess_demand")
  )
}

build_block_kilian_proxies <- function(path) {
  if (!file.exists(path)) stop("High-frequency proxy data not found: ", path)
  inst <- elasticity_block_instruments()
  hourly <- as.data.frame(arrow::read_parquet(
    path,
    col_select = dplyr::all_of(c("datetime", "load_tot", inst$all, "fit_supply_unmatched", "fit_demand_unmatched"))
  ))
  hourly <- hourly[!duplicated(hourly$datetime), , drop = FALSE]
  hourly <- hourly[order(hourly$datetime), , drop = FALSE]
  for (column in c("cbf_es", "Published.Predicted.Unavailable.Capacity")) {
    hourly[[column]][!is.finite(hourly[[column]])] <- mean(hourly[[column]], na.rm = TRUE)
  }
  x <- as.matrix(hourly[, inst$all])
  # The fitted curve shifts are linear in the six instruments: recover the coefficients exactly.
  recover <- function(y) {
    ok <- is.finite(y) & rowSums(is.finite(x)) == ncol(x)
    fit <- stats::lm.fit(x[ok, , drop = FALSE], y[ok])
    list(beta = fit$coefficients, r2 = 1 - sum(fit$residuals^2) / sum((y[ok] - mean(y[ok]))^2))
  }
  supply_curve <- recover(hourly$fit_supply_unmatched)
  demand_curve <- recover(hourly$fit_demand_unmatched)
  if (min(supply_curve$r2, demand_curve$r2) < 0.999) {
    stop("Curve shifts are not linear in the instrument set (R2 = ",
         round(min(supply_curve$r2, demand_curve$r2), 4), ").")
  }
  block <- function(beta, columns) drop(x[, columns, drop = FALSE] %*% beta[columns])
  hourly$supply_block <- block(supply_curve$beta, inst$supply)
  hourly$demand_block <- block(demand_curve$beta, inst$demand)

  # Auction-day panel, load weighted.
  hourly$day <- as.Date(format(hourly$datetime, "%Y-%m-%d", tz = "Europe/Madrid"))
  weight <- ifelse(is.finite(hourly$load_tot), hourly$load_tot, 0)
  load_day <- rowsum(weight, hourly$day)
  # Full synthetic curve shifts (all six instruments); missing hours are filled from the exact linear map.
  complete_rows <- rowSums(is.finite(x)) == ncol(x)
  full_shift <- function(y, beta) {
    fill <- !is.finite(y) & complete_rows
    y[fill] <- drop(x[fill, , drop = FALSE] %*% beta)
    y
  }
  hourly$supply_shift <- full_shift(hourly$fit_supply_unmatched, supply_curve$beta)
  hourly$demand_shift <- full_shift(hourly$fit_demand_unmatched, demand_curve$beta)
  day_mean <- function(v) as.numeric(rowsum(weight * ifelse(is.finite(v), v, 0), hourly$day)) / as.numeric(load_day)
  daily <- data.frame(
    day = as.Date(rownames(load_day)),
    load = as.numeric(load_day),
    supply_block = day_mean(hourly$supply_block),
    demand_block = day_mean(hourly$demand_block),
    supply_shift = day_mean(hourly$supply_shift),
    demand_shift = day_mean(hourly$demand_shift),
    wind = day_mean(hourly$Fore_Wind)
  )
  daily <- daily[order(daily$day), , drop = FALSE]
  days <- nrow(daily)

  # Daily surprise: residual on seven own lags, weekday dummies, annual Fourier terms and a trend.
  day_of_year <- as.numeric(format(daily$day, "%j"))
  weekday <- factor(format(daily$day, "%u"))
  deterministic <- cbind(
    1, seq_len(days) / days, stats::model.matrix(~ weekday)[, -1, drop = FALSE],
    sapply(1:3, function(k) sin(2 * pi * k * day_of_year / 365.25)),
    sapply(1:3, function(k) cos(2 * pi * k * day_of_year / 365.25))
  )
  lag_value <- function(z, k) c(rep(NA_real_, k), head(z, -k))
  surprise <- function(z) {
    regressors <- cbind(deterministic, sapply(1:7, function(k) lag_value(z, k)))
    ok <- rowSums(is.finite(cbind(z, regressors))) == ncol(regressors) + 1L
    out <- rep(NA_real_, length(z))
    out[ok] <- stats::lm.fit(regressors[ok, , drop = FALSE], z[ok])$residuals
    out
  }

  # Kilian (2024) aggregation with load weights: a day-k innovation enters the current monthly average with
  # the remaining load share of the month and the next month with the elapsed share.
  month <- as.Date(format(daily$day, "%Y-%m-01"))
  kilian <- function(z) {
    z0 <- ifelse(is.finite(z), z, 0)
    pieces <- lapply(split(seq_len(days), month), function(index) {
      share <- daily$load[index] / sum(daily$load[index])
      current <- rev(cumsum(rev(share)))
      c(current = sum(z0[index] * current), carry = sum(z0[index] * (1 - current)))
    })
    current <- vapply(pieces, `[[`, numeric(1), "current")
    carry <- vapply(pieces, `[[`, numeric(1), "carry")
    data.frame(observation_date = as.Date(names(pieces)), value = current + c(0, head(carry, -1L)))
  }
  anomaly <- function(value, dates) value - stats::ave(value, format(dates, "%m"), FUN = mean)

  supply_surprise <- kilian(surprise(daily$supply_block))
  demand_surprise <- kilian(surprise(daily$demand_block))
  supply_level <- kilian(daily$supply_block)
  demand_level <- kilian(daily$demand_block)
  syn_supply_surprise <- kilian(surprise(daily$supply_shift))
  syn_demand_surprise <- kilian(surprise(daily$demand_shift))
  syn_supply_level <- kilian(daily$supply_shift)
  syn_demand_level <- kilian(daily$demand_shift)
  wind_surprise <- kilian(surprise(daily$wind))
  out <- data.frame(
    observation_date = supply_surprise$observation_date,
    k_S_block_surp = supply_surprise$value,
    k_D_block_surp = demand_surprise$value,
    k_S_block_lvl = anomaly(supply_level$value, supply_level$observation_date),
    k_D_block_lvl = anomaly(demand_level$value, demand_level$observation_date),
    # synthetic proxies: full supply and demand curve shifts
    k_S_syn_surp = syn_supply_surprise$value,
    k_D_syn_surp = syn_demand_surprise$value,
    k_S_syn_lvl = anomaly(syn_supply_level$value, syn_supply_level$observation_date),
    k_D_syn_lvl = anomaly(syn_demand_level$value, syn_demand_level$observation_date),
    # day-ahead wind forecast alone (robustness)
    k_S_wind_surp = wind_surprise$value
  )
  attr(out, "coefficients") <- rbind(supply_curve = supply_curve$beta, demand_curve = demand_curve$beta)
  out
}

attach_block_proxies <- function(country_data, path) {
  stopifnot(country_data$country == "Spain")
  proxies <- build_block_kilian_proxies(path)
  matched <- match(as.Date(country_data$data$observation_date), proxies$observation_date)
  for (column in setdiff(names(proxies), "observation_date")) {
    country_data$data[[column]] <- proxies[[column]][matched]
  }
  country_data$block_proxy_coefficients <- attr(proxies, "coefficients")
  country_data
}

# Benchmark monthly proxies: load-weighted monthly LEVEL of the synthetic supply and demand curve shifts,
# residualised on calendar-month and year fixed effects. Identification comes from within-year deviations of a
# month from its usual seasonal level (e.g. January 2015 windier than the average January and than the rest of
# 2015); the year effects remove the interannual swings (2021-2023 energy crisis). Built from the hourly file with
# the re-downloaded day-ahead forecasts (supply_demand_curve/build/patch_new_day_ahead_forecasts.R) when present.
benchmark_hourly_path <- function(cfg) {
  # The hourly curve-shift file is rebuilt on the re-downloaded ENTSO-E forecasts
  # (supply_demand_curve/build/no_price_conditioning.R after day_ahead/esp/refresh_renewable_forecasts_NTC.R).
  cfg$data$spain_high_frequency_proxies
}

attach_synthetic_level_proxies <- function(country_data, path) {
  hourly <- as.data.frame(arrow::read_parquet(
    path, col_select = c("datetime", "load_tot", "fit_supply_unmatched", "fit_demand_unmatched")))
  hourly <- hourly[!duplicated(hourly$datetime), , drop = FALSE]
  month <- format(hourly$datetime, "%Y-%m-01", tz = "Europe/Madrid")
  weight <- ifelse(is.finite(hourly$load_tot), hourly$load_tot, 0)
  monthly <- function(v) {
    ok <- is.finite(v)
    tapply(weight[ok] * v[ok], month[ok], sum) / tapply(weight[ok], month[ok], sum)
  }
  supply <- monthly(hourly$fit_supply_unmatched); demand <- monthly(hourly$fit_demand_unmatched)
  data <- country_data$data
  key <- format(as.Date(data$observation_date), "%Y-%m-01")
  s <- as.numeric(supply[key]); d <- as.numeric(demand[key])
  rows <- which(is.finite(s) & is.finite(d))
  dates <- as.Date(data$observation_date[rows])
  calendar <- factor(format(dates, "%m")); year <- factor(format(dates, "%Y"))
  for (name in c("syn_S_lvl_cy", "syn_D_lvl_cy")) data[[name]] <- NA_real_
  data$syn_S_lvl_cy[rows] <- stats::resid(stats::lm(s[rows] ~ calendar + year))
  data$syn_D_lvl_cy[rows] <- stats::resid(stats::lm(d[rows] ~ calendar + year))
  data$syn_S_lvl <- s; data$syn_D_lvl <- d  # raw monthly levels, for the out-of-sample decomposition
  country_data$data <- data
  country_data
}

# Residual of a monthly series on calendar-month and year fixed effects over its observed months (NA elsewhere).
calendar_year_residual <- function(x, dates) {
  out <- rep(NA_real_, length(x)); rows <- which(is.finite(x))
  calendar <- factor(format(dates[rows], "%m")); year <- factor(format(dates[rows], "%Y"))
  out[rows] <- stats::resid(stats::lm(x[rows] ~ calendar + year))
  out
}

# Monthly weather from 2010: realised wind speed at 100 m at the wind farms (ERA5; Open-Meteo backfill before
# 2015, wind_speed_100m of the VAR data after, same farms and capacity weights) and demand-weighted temperature
# and precipitation (Open-Meteo backfill at the demand points before 2015, hourly micro file after).
monthly_weather_2010 <- function(country_data, cfg) {
  dates <- as.Date(country_data$data$observation_date); key <- format(dates, "%Y-%m")
  wind_bf <- utils::read.csv(cfg$data$wind_backfill_2010)
  wind_bf_value <- wind_bf$ws_capw[match(key, substr(wind_bf$date, 1, 7))]
  wind <- ifelse(is.finite(country_data$data$wind_speed_100m), country_data$data$wind_speed_100m, wind_bf_value)

  hourly <- as.data.frame(arrow::read_parquet(cfg$data$spain_high_frequency_proxies,
                                              col_select = c("datetime", "temperature_demand", "precipitation_demand")))
  hourly <- hourly[!duplicated(hourly$datetime) & is.finite(hourly$temperature_demand), , drop = FALSE]
  month <- format(hourly$datetime, "%Y-%m", tz = "GMT")
  agg <- function(v) tapply(v, month, mean, na.rm = TRUE)
  micro <- data.frame(month = names(agg(hourly$temperature_demand)),
                      heat = as.numeric(agg(pmax(hourly$temperature_demand - 18, 0))),
                      cold = as.numeric(agg(pmax(18 - hourly$temperature_demand, 0))),
                      precipitation = as.numeric(agg(hourly$precipitation_demand)))
  bf <- utils::read.csv(cfg$data$demand_weather_backfill_2010)
  bf <- data.frame(month = bf$month, heat = bf$heat_excess_demand, cold = bf$cold_excess_demand,
                   precipitation = bf$precipitation_demand)
  pick <- function(col) {
    m <- micro[[col]][match(key, micro$month)]
    ifelse(is.finite(m), m, bf[[col]][match(key, bf$month)])
  }
  data.frame(observation_date = dates, wind = wind, heat = pick("heat"), cold = pick("cold"),
             precipitation = pick("precipitation"))
}

# Robustness proxies observed from 2010 (the benchmark proxies start in 2015):
#   wind_S_lvl_cy: realised wind speed, net of calendar-month and year fixed effects (supply proxy);
#   syn_*_oos_cy:  synthetic supply and demand shifts, out of sample before 2015 -- the monthly shifts are projected
#                  on calendar months and weather (wind, heat and cold excess, precipitation) over 2015-2025 and
#                  the projection is applied to the 2010-2014 weather; observed shifts from 2015.
attach_proxies_2010 <- function(country_data, cfg) {
  data <- country_data$data
  dates <- as.Date(data$observation_date)
  weather <- monthly_weather_2010(country_data, cfg)
  data$wind_S_lvl_cy <- calendar_year_residual(weather$wind, dates)
  calendar <- factor(format(dates, "%m"))
  design <- data.frame(calendar = calendar, weather[, c("wind", "heat", "cold", "precipitation")])
  fit_rows <- is.finite(data$syn_S_lvl) & is.finite(data$syn_D_lvl) & stats::complete.cases(design)
  r2 <- c()
  for (curve in c("S", "D")) {
    y <- data[[paste0("syn_", curve, "_lvl")]]
    model <- stats::lm(y ~ ., data = cbind(y = y, design)[fit_rows, ])
    r2[curve] <- summary(model)$r.squared
    predicted <- rep(NA_real_, nrow(data)); ok <- stats::complete.cases(design)
    predicted[ok] <- stats::predict(model, newdata = design[ok, ])
    spliced <- ifelse(is.finite(y), y, predicted)
    data[[paste0("syn_", curve, "_oos_cy")]] <- calendar_year_residual(spliced, dates)
  }
  message(sprintf("Out-of-sample synthetic decomposition: weather R2 supply %.2f, demand %.2f", r2["S"], r2["D"]))
  country_data$proxy_2010_r2 <- r2
  country_data$data <- data
  country_data
}

# Benchmark proxy SVAR (one proxy per shock, OLS VAR, MSW weak-IV robust sets), both shocks normalised on the
# electricity price.
fit_benchmark_msw <- function(country_data, cfg) {
  list(
    Supply = fit_proxy_svar(country_data, "syn_S_lvl_cy", "spot_price_log", "Supply", cfg, use_inference = TRUE),
    Demand = fit_proxy_svar(country_data, "syn_D_lvl_cy", "spot_price_log", "Demand", cfg, use_inference = TRUE)
  )
}

# Benchmark of the working paper: proxy SVAR on a Bayesian reduced form. The VAR (p lags, log levels) is estimated
# with the GLP Minnesota prior (hyperparameters at the mode of the marginal likelihood); each shock is identified by
# its monthly proxy (impact vector = covariance between the proxy and the reduced-form residuals, normalised on the
# electricity price, or on the gas price for the Alessandri-Gazzani gas-supply shock). For each posterior draw of
# (B, Sigma), the proxy moments z_t u_t are reweighted by a block Bayesian bootstrap (Dirichlet weights on blocks of
# `block` months), so that the bands reflect both reduced-form uncertainty and the sampling uncertainty of the
# proxy covariance, which matters with weak proxies. Point models (posterior mean of B and Sigma, unweighted
# moments) feed the historical decompositions and the policy counterfactuals.
add_hydro_storage <- function(country_data, path) {
  if (!file.exists(path)) stop("Hydro reserves not found: ", path)
  hydro <- utils::read.csv(path)
  dates <- as.Date(paste0(hydro$month, "-01"))
  country_data$data$hydro_storage_log <- log(hydro$total_mwh)[match(country_data$data$observation_date, dates)]
  country_data$variables <- c(country_data$variables, "hydro_storage_log")
  country_data
}

bayesian_proxy_benchmark <- function(country_data, cfg, proxies = c(Supply = "syn_S_lvl_cy", Demand = "syn_D_lvl_cy"),
                                     name = "Benchmark: monthly proxy SVAR, Bayesian reduced form") {
  set.seed(cfg$seed + 7L)
  variables <- country_data$variables; n <- length(variables); p <- cfg$elasticity$p_central
  horizon <- max(cfg$elasticity$horizon, cfg$horizon - 1L)
  sample <- finite_complete(country_data$data, unique(c("observation_date", variables)))
  levels <- as.matrix(sample[, variables])
  post <- reduced_form_posterior(levels, p, "glp")
  t_obs <- nrow(post$Y); k <- ncol(post$X); lx <- t(chol(post$xtx_inv))
  proxies <- c(proxies, "Gas supply" = country_data$gas_supply_proxy)
  norm <- c(Supply = "spot_price_log", Demand = "spot_price_log", "Gas supply" = "spot_gas_log")
  z <- sapply(proxies, function(proxy) proxy_instrument(sample, proxy, p)$z)
  block <- round(5 * t_obs^0.25)
  dates <- sample$observation_date[-seq_len(p)]

  identify <- function(b, sigma, weights = rep(1, t_obs)) {
    u <- post$Y - post$X %*% b; colnames(u) <- variables
    ma <- elas_ma_matrices(b[-1, , drop = FALSE], n, p, horizon)
    models <- lapply(names(proxies), function(shock) {
      gamma <- matrix(drop(crossprod(weights * z[, shock], u)), ncol = 1L, dimnames = list(variables, NULL))
      theta <- gamma / gamma[norm[[shock]], 1]; colnames(theta) <- shock
      list(gamma = gamma, theta = theta)
    })
    names(models) <- names(proxies)
    list(u = u, ma = ma, models = models)
  }
  block_weights <- function() {
    id <- (seq_len(t_obs) - 1L + sample.int(block, 1L) - 1L) %/% block + 1L
    w <- stats::rexp(max(id))[id]
    w / mean(w)
  }

  draws <- cfg$elasticity$draws
  irf <- lapply(proxies, function(x) array(NA_real_, c(horizon + 1L, n, draws), dimnames = list(0:horizon, variables, NULL)))
  keep_draws <- unique(round(seq(1, draws, length.out = min(draws, cfg$elasticity$stored_draws))))
  stored <- list()
  for (r in seq_len(draws)) {
    sigma <- solve(stats::rWishart(1, post$nu, solve(post$S))[, , 1])
    b <- post$B + lx %*% matrix(stats::rnorm(k * n), k, n) %*% chol(sigma)
    weights <- block_weights()
    id <- identify(b, sigma, weights)
    for (shock in names(proxies)) for (hh in 0:horizon) irf[[shock]][hh + 1L, , r] <- id$ma[, , hh + 1L] %*% id$models[[shock]]$theta
    if (r %in% keep_draws) stored[[length(stored) + 1L]] <- list(B = b, sigma = sigma, weights = weights)
  }

  # Point models in the format of fit_proxy_svar(), for 05_contributions.R and 06_counterfactuals.R
  sigma_mean <- post$sigma_mean; dimnames(sigma_mean) <- list(variables, variables)
  make_models <- function(b, sigma, weights = rep(1, t_obs)) {
    id <- identify(b, sigma, weights)
    out <- lapply(names(proxies), function(shock) {
      theta <- id$models[[shock]]$theta; gamma <- id$models[[shock]]$gamma
      shocks <- matrix(single_proxy_shocks(id$u, sigma, gamma, theta), nrow = 1L, dimnames = list(shock, NULL))
      list(method = "External instrument, Bayesian reduced form", country = country_data$country,
           proxy = proxies[[shock]], norm_var = norm[[shock]], shock = shock, fit = list(p = p),
           sample = sample, variables = variables, residuals = id$u, sigma = sigma, dates = dates,
           gamma = gamma, theta = theta, shocks = shocks, ma = id$ma,
           irf = irf_from_theta(id$ma, theta, shock),
           hd = historical_decomposition(id$ma, theta, shocks, variables, shock))
    })
    names(out) <- names(proxies)
    out
  }
  point <- make_models(post$B, sigma_mean)
  chosen <- unique(round(seq(1, draws, length.out = min(draws, max(1L, as.integer(cfg$counterfactual_bootstrap))))))
  point$Supply$irf_draws <- lapply(chosen, function(r) {
    draw <- irf$Supply[seq_len(cfg$horizon), , r]
    draw / draw[1, "spot_price_log"]
  })
  list(spec = list(name = name, var_prior = "glp", p = p), variables = variables, posterior = post,
       irf_supply = irf$Supply, irf_demand = irf$Demand, irf_gas = irf[["Gas supply"]],
       point = point, stored = stored, make_models = make_models)
}

# Posterior quantiles of the gas-supply IRF of the benchmark, scaled to a `size` log-point impact rise in the gas price.
bayesian_proxy_gas_quantiles <- function(fit, size = 0.1, horizon = NULL) {
  draws <- fit$irf_gas
  for (r in seq_len(dim(draws)[3])) draws[, , r] <- size * draws[, , r] / draws[1, "spot_gas_log", r]
  if (!is.null(horizon)) draws <- draws[seq_len(min(dim(draws)[1], horizon + 1L)), , , drop = FALSE]
  q <- function(pr) as.vector(apply(draws, c(1, 2), stats::quantile, pr, na.rm = TRUE))
  data.frame(Specification = fit$spec$name, Shock = "Gas supply",
             Horizon = rep(as.integer(dimnames(draws)[[1]]), times = dim(draws)[2]),
             Variable = rep(dimnames(draws)[[2]], each = dim(draws)[1]),
             Median = q(0.5), Lower68 = q(0.16), Upper68 = q(0.84), Lower90 = q(0.05), Upper90 = q(0.95),
             stringsAsFactors = FALSE)
}

# Posterior bands for the inflation contributions of the Bayesian proxy benchmark (stored draws).
bayesian_proxy_contribution_bands <- function(fit, cfg, observed_data) {
  rows <- lapply(seq_along(fit$stored), function(i) {
    draw <- fit$stored[[i]]
    models <- try(fit$make_models(draw$B, draw$sigma, draw$weights), silent = TRUE)
    if (inherits(models, "try-error")) return(NULL)
    decomposition <- combine_three_proxy_hd(models$Supply, models$Demand, models[["Gas supply"]])
    table <- contribution_table_from_hd(decomposition, cfg$windows, "Spain", "posterior draw", observed_data)
    data.frame(Draw = i, table[, c("Episode", "Period", "Shock", "Cumulative_inflation_contribution_pp",
                                   "Cumulative_inflation_pp")])
  })
  draws <- dplyr::bind_rows(rows)
  draws$Shock[draws$Shock == "Total electricity"] <- "Total identified (electricity and gas)"
  draws |>
    dplyr::mutate(Share = 100 * Cumulative_inflation_contribution_pp / Cumulative_inflation_pp) |>
    dplyr::group_by(Episode, Period, Shock) |>
    dplyr::summarise(
      Median_pp = stats::median(Cumulative_inflation_contribution_pp),
      Lower68_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.16),
      Upper68_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.84),
      Lower90_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.05),
      Upper90_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.95),
      Median_share_percent = stats::median(Share),
      Lower90_share_percent = stats::quantile(Share, 0.05),
      Upper90_share_percent = stats::quantile(Share, 0.95),
      Observed_cumulative_inflation_pp = dplyr::first(Cumulative_inflation_pp),
      Draws = dplyr::n(), .groups = "drop")
}

# MSW point estimates and bands in the format of elasticity_quantiles(), scaled to a `size` log-point impact rise
# in the electricity price (the normalisation is fixed, so scaling the MSW sets is exact).
msw_quantiles <- function(benchmark, name, size = 0.1, horizon = NULL) {
  dplyr::bind_rows(lapply(names(benchmark), function(shock) {
    data <- as.data.frame(prepare_iv_plot_data(benchmark[[shock]]))
    if (!is.null(horizon)) data <- data[data$Horizon <= horizon, , drop = FALSE]
    data.frame(Specification = name, Shock = shock, Horizon = as.integer(data$Horizon),
               Variable = as.character(data$Variable), Median = size * data$Estimate,
               Lower68 = size * data$Lower68, Upper68 = size * data$Upper68,
               Lower90 = size * data$Lower90, Upper90 = size * data$Upper90, stringsAsFactors = FALSE)
  }))
}

# Monthly physical proxies, built at the VAR frequency instead of aggregating hourly curve shifts:
#   supply: available non-fossil energy = standardised anomalies of the realised wind speed at 100 m and of the
#           6-month cumulated precipitation (hydro inflows), orthogonalised on the cold and heat anomalies;
#   demand: anomaly of the temperature-discomfort degree-days (cold + heat excess), orthogonalised on the wind and
#           hydro anomalies.
# Anomalies are residuals on calendar-month dummies and a linear trend over the observed months. The "_ll"
# versions are further residualised on `ll_lags` lags of the VAR variables (Stock-Watson lead-lag exogeneity).
attach_physical_proxies <- function(country_data, ll_lags = 2L) {
  data <- country_data$data
  inputs <- c("wind_speed_100m", "precipitation", "cold_excess", "heat_excess")
  assert_columns(data, inputs, "Physical proxies")
  rows <- which(stats::complete.cases(data[, inputs, drop = FALSE]))
  calendar <- factor(format(as.Date(data$observation_date[rows]), "%m"))
  trend <- seq_along(rows)
  lag_value <- function(z, k) c(rep(NA_real_, k), head(z, -k))
  anomaly <- function(z) stats::resid(stats::lm(z ~ calendar + trend, na.action = stats::na.exclude))
  standardise <- function(z) (z - mean(z, na.rm = TRUE)) / stats::sd(z, na.rm = TRUE)
  orthogonalise <- function(z, w) stats::resid(stats::lm(z ~ w, na.action = stats::na.exclude))

  wind <- anomaly(data$wind_speed_100m[rows])
  rain <- anomaly(data$precipitation[rows])
  hydro <- rain + Reduce(`+`, lapply(1:5, function(k) lag_value(rain, k)))
  cold <- anomaly(data$cold_excess[rows]); heat <- anomaly(data$heat_excess[rows])
  supply <- orthogonalise(standardise(wind) + standardise(hydro), cbind(cold, heat))
  demand <- orthogonalise(anomaly(data$cold_excess[rows] + data$heat_excess[rows]), cbind(wind, hydro))

  lags <- do.call(cbind, lapply(seq_len(ll_lags), function(k) {
    out <- matrix(NA_real_, length(rows), length(country_data$variables))
    valid <- rows - k >= 1L
    out[valid, ] <- as.matrix(data[rows[valid] - k, country_data$variables, drop = FALSE])
    out
  }))
  lead_lag <- function(z) stats::resid(stats::lm(z ~ lags, na.action = stats::na.exclude))

  put <- function(name, value) { data[[name]] <<- NA_real_; data[[name]][rows] <<- as.numeric(value) }
  put("phys_S_wind", wind); put("phys_S_hydro", hydro); put("phys_D_degree_days", anomaly(data$cold_excess[rows] + data$heat_excess[rows]))
  put("phys_supply", supply); put("phys_demand", demand)
  put("phys_supply_ll", lead_lag(supply)); put("phys_demand_ll", lead_lag(demand))
  country_data$data <- data
  country_data
}

# Temperature shifts of the hourly supply and demand curves, aggregated to monthly anomalies (MW). Temperature moves
# both curves (cold: about -0.9 MW of supply per MW of demand in the hourly regressions), so a temperature proxy is
# not a pure demand shifter. Returns
#   - `temperature_contamination`: kappa = cov(phys_demand, supply shift) / cov(phys_demand, demand shift), the
#     loading of the physical demand proxy on the supply-curve shift per unit of demand-curve shift;
#   - `curve_demand`: the temperature demand-curve shift orthogonalised on the temperature supply-curve shift and on
#     the wind and hydro anomalies (a demand proxy purged of the supply effect of temperature).
attach_temperature_shifts <- function(country_data, path) {
  inst <- elasticity_block_instruments()
  hourly <- as.data.frame(arrow::read_parquet(
    path, col_select = dplyr::all_of(c("datetime", "load_tot", inst$all, "fit_supply_unmatched", "fit_demand_unmatched"))))
  hourly <- hourly[!duplicated(hourly$datetime), , drop = FALSE]
  for (column in c("cbf_es", "Published.Predicted.Unavailable.Capacity")) {
    hourly[[column]][!is.finite(hourly[[column]])] <- mean(hourly[[column]], na.rm = TRUE)
  }
  x <- as.matrix(hourly[, inst$all]); complete <- rowSums(is.finite(x)) == ncol(x)
  recover <- function(y) stats::lm.fit(x[complete, , drop = FALSE], y[complete])$coefficients
  beta_s <- recover(hourly$fit_supply_unmatched); beta_d <- recover(hourly$fit_demand_unmatched)
  temp <- inst$demand
  month <- as.Date(format(as.Date(format(hourly$datetime, "%Y-%m-%d", tz = "Europe/Madrid")), "%Y-%m-01"))
  weight <- ifelse(is.finite(hourly$load_tot) & complete, hourly$load_tot, 0)
  monthly_mean <- function(v) as.numeric(rowsum(weight * ifelse(complete, v, 0), month)) / as.numeric(rowsum(weight, month))
  monthly <- data.frame(observation_date = as.Date(rownames(rowsum(weight, month))),
                        supply = monthly_mean(drop(x[, temp] %*% beta_s[temp])),
                        demand = monthly_mean(drop(x[, temp] %*% beta_d[temp])))
  calendar <- factor(format(monthly$observation_date, "%m")); trend <- seq_len(nrow(monthly))
  anomaly <- function(z) stats::resid(stats::lm(z ~ calendar + trend, na.action = stats::na.exclude))

  data <- country_data$data
  matched <- match(as.Date(data$observation_date), monthly$observation_date)
  data$temp_S_shift <- anomaly(monthly$supply)[matched]
  data$temp_D_shift <- anomaly(monthly$demand)[matched]
  data$curve_demand <- stats::resid(stats::lm(temp_D_shift ~ temp_S_shift + phys_S_wind + phys_S_hydro, data = data,
                                              na.action = stats::na.exclude))
  ok <- is.finite(data$phys_demand) & is.finite(data$temp_S_shift) & is.finite(data$temp_D_shift)
  country_data$temperature_contamination <- stats::cov(data$phys_demand[ok], data$temp_S_shift[ok]) /
    stats::cov(data$phys_demand[ok], data$temp_D_shift[ok])
  country_data$temperature_curve_coefficients <- rbind(supply_curve = beta_s[temp], demand_curve = beta_d[temp])
  country_data$data <- data
  country_data
}

# ----------------------------------------------------------------------------------------------------
# 2. Minnesota / GLP prior (conjugate NIW with dummy observations)
# ----------------------------------------------------------------------------------------------------
# Self-contained lag matrix and moving-average helpers (regressors: const, lag-1 block, ..., lag-p block).
elas_lag_matrix <- function(levels, p) {
  levels <- as.matrix(levels); t_obs <- nrow(levels)
  cbind(const = 1, do.call(cbind, lapply(seq_len(p), function(lag) levels[(p + 1L - lag):(t_obs - lag), , drop = FALSE])))
}

elas_ma_matrices <- function(lag_coefficients, n, p, horizon) {
  a <- lapply(seq_len(p), function(lag) t(lag_coefficients[(lag - 1L) * n + seq_len(n), , drop = FALSE]))
  ma <- array(0, dim = c(n, n, horizon + 1L)); ma[, , 1] <- diag(n)
  for (hh in seq_len(horizon)) for (lag in seq_len(min(hh, p))) {
    ma[, , hh + 1L] <- ma[, , hh + 1L] + ma[, , hh + 1L - lag] %*% a[[lag]]
  }
  ma
}

glp_log_mvgamma <- function(a, n) n * (n - 1) / 4 * log(pi) + sum(lgamma(a + (1 - seq_len(n)) / 2))

# Log marginal density of Y = X B + U under p(B, Sigma) ~ |Sigma|^{-(n+1)/2}; ratios are proper.
glp_log_marginal <- function(Y, X) {
  n <- ncol(Y); nu <- nrow(Y) - ncol(X)
  xtx <- crossprod(X); b <- solve(xtx, crossprod(X, Y)); s <- crossprod(Y - X %*% b)
  -(n * nu / 2) * log(pi) - (n / 2) * as.numeric(determinant(xtx)$modulus) +
    glp_log_mvgamma(nu / 2, n) - (nu / 2) * as.numeric(determinant(s)$modulus)
}

glp_dummies <- function(levels, p, lambda, mu, delta, sigma, eps_const = 1e-4) {
  n <- ncol(levels); k <- 1 + n * p; prior_mean <- rep(1, n)
  ybar <- colMeans(levels[seq_len(p), , drop = FALSE])
  y_rows <- list(
    rbind(diag(prior_mean * sigma / lambda, n), matrix(0, n * (p - 1), n)),   # Minnesota
    diag(sigma, n),                                                            # residual covariance
    matrix(0, 1, n)                                                            # diffuse constant
  )
  x_rows <- list(
    cbind(0, kronecker(diag(seq_len(p), p), diag(sigma / lambda, n))),
    matrix(0, n, k),
    matrix(c(eps_const, rep(0, n * p)), 1)
  )
  if (is.finite(mu)) {        # sum of coefficients
    y_rows[[length(y_rows) + 1L]] <- diag(ybar * prior_mean / mu, n)
    x_rows[[length(x_rows) + 1L]] <- cbind(0, matrix(rep(diag(ybar * prior_mean / mu, n), p), n))
  }
  if (is.finite(delta)) {     # single unit root
    y_rows[[length(y_rows) + 1L]] <- matrix(ybar / delta, 1)
    x_rows[[length(x_rows) + 1L]] <- matrix(c(1 / delta, rep(ybar / delta, p)), 1)
  }
  list(Y = do.call(rbind, y_rows), X = do.call(rbind, x_rows))
}

glp_gamma_parameters <- function(mode, sd) {
  shape <- (2 + mode^2 / sd^2 + sqrt((4 + mode^2 / sd^2) * mode^2 / sd^2)) / 2
  c(shape = shape, rate = sqrt(shape) / sd)
}

glp_select_hyperparameters <- function(levels, p, type = c("glp", "minnesota"),
                                       lambda_grid = c(0.03, 0.05, 0.08, 0.1, 0.12, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1),
                                       mu_grid = c(0.02, 0.05, 0.1, 0.25, 0.5, 1, 2, 4),
                                       delta_grid = c(0.02, 0.05, 0.1, 0.25, 0.5, 1, 2, 4)) {
  type <- match.arg(type)
  X <- elas_lag_matrix(levels, p)
  Y <- levels[(p + 1):nrow(levels), , drop = FALSE]
  sigma <- apply(levels, 2, function(y) stats::sd(stats::resid(stats::lm(y[-1] ~ y[-length(y)]))))
  grid <- if (type == "glp") {
    expand.grid(lambda = lambda_grid, mu = mu_grid, delta = delta_grid)
  } else {
    data.frame(lambda = lambda_grid, mu = Inf, delta = Inf)
  }
  a <- glp_gamma_parameters(0.2, 0.4); b <- glp_gamma_parameters(1, 1)
  grid$log_ml <- apply(grid, 1, function(h) {
    d <- glp_dummies(levels, p, h[["lambda"]], h[["mu"]], h[["delta"]], sigma)
    glp_log_marginal(rbind(d$Y, Y), rbind(d$X, X)) - glp_log_marginal(d$Y, d$X)
  })
  grid$log_posterior <- grid$log_ml + stats::dgamma(grid$lambda, a[1], a[2], log = TRUE) +
    if (type == "glp") stats::dgamma(grid$mu, b[1], b[2], log = TRUE) + stats::dgamma(grid$delta, b[1], b[2], log = TRUE) else 0
  best <- grid[which.max(grid$log_posterior), , drop = FALSE]
  on_edge <- type == "glp" && (best$mu %in% range(mu_grid) || best$delta %in% range(delta_grid)) ||
    best$lambda %in% range(lambda_grid)
  if (on_edge) warning("GLP hyperparameters on the edge of the grid: ", paste(names(best), best, collapse = ", "))
  list(best = best, grid = grid, sigma = sigma)
}

reduced_form_posterior <- function(levels, p, prior = c("glp", "minnesota", "diffuse")) {
  prior <- match.arg(prior)
  X <- elas_lag_matrix(levels, p)
  Y <- levels[(p + 1):nrow(levels), , drop = FALSE]
  hyper <- NULL
  if (prior == "diffuse") {
    xs <- X; ys <- Y
  } else {
    selection <- glp_select_hyperparameters(levels, p, prior)
    hyper <- selection$best
    d <- glp_dummies(levels, p, hyper$lambda, hyper$mu, hyper$delta, selection$sigma)
    xs <- rbind(d$X, X); ys <- rbind(d$Y, Y)
  }
  xtx_inv <- solve(crossprod(xs))
  b_bar <- xtx_inv %*% crossprod(xs, ys)
  s <- crossprod(ys - xs %*% b_bar)
  nu <- nrow(ys) - ncol(xs)
  list(prior = prior, hyper = hyper, X = X, Y = Y, B = b_bar, S = s, nu = nu, xtx_inv = xtx_inv,
       sigma_mean = s / (nu - ncol(Y) - 1))
}

companion_root <- function(lag_coefficients, n, p) {
  companion <- matrix(0, n * p, n * p)
  companion[seq_len(n), ] <- t(lag_coefficients)
  if (p > 1L) companion[(n + 1):(n * p), seq_len(n * (p - 1))] <- diag(n * (p - 1))
  max(Mod(eigen(companion, only.values = TRUE)$values))
}

# ----------------------------------------------------------------------------------------------------
# 3. Structural mapping and posterior sampler
# ----------------------------------------------------------------------------------------------------
elasticity_vectors <- function(sigma, eps_s, eps_d, index, gas_netted) {
  n <- nrow(sigma)
  make <- function(eps) {
    a <- numeric(n); a[index$load] <- 1; a[index$price] <- -eps
    if (gas_netted) a[index$gas] <- -(sigma[index$load, index$gas] - eps * sigma[index$price, index$gas]) / sigma[index$gas, index$gas]
    a
  }
  a_s <- make(eps_s); a_d <- make(eps_d)
  sd_s <- sqrt(drop(t(a_s) %*% sigma %*% a_s)); sd_d <- sqrt(drop(t(a_d) %*% sigma %*% a_d))
  list(a_s = a_s, a_d = a_d, sd_s = sd_s, sd_d = sd_d,
       b_s = -drop(sigma %*% a_s) / sd_s,        # impact of a one-s.d. adverse supply shock
       b_d = drop(sigma %*% a_d) / sd_d)         # impact of a one-s.d. demand shock
}

netted_block <- function(sigma, index, gas_netted) {
  p_ <- index$price; l_ <- index$load; g_ <- index$gas
  if (!gas_netted) return(c(pp = sigma[p_, p_], ll = sigma[l_, l_], lp = sigma[l_, p_]))
  c(pp = sigma[p_, p_] - sigma[p_, g_]^2 / sigma[g_, g_],
    ll = sigma[l_, l_] - sigma[l_, g_]^2 / sigma[g_, g_],
    lp = sigma[l_, p_] - sigma[l_, g_] * sigma[p_, g_] / sigma[g_, g_])
}

implied_demand_slope <- function(eps_s, block) (eps_s * block[["lp"]] - block[["ll"]]) / (eps_s * block[["pp"]] - block[["lp"]])

truncated_normal_log <- function(x, parameters, side) {
  inside <- if (side == "positive") x > 0 else x < 0
  ifelse(inside, stats::dnorm(x, parameters[[1]], parameters[[2]], log = TRUE), -Inf)
}

# Log likelihood ratio of the exclusion restriction "z is correlated with u_L - kept_slope * u_P only" against an
# unrestricted regression of z on (u_L, u_P): (n / 2) [log(1 - R2_unrestricted) - log(1 - R2_restricted)].
# Vectorised over kept_slope. With several proxies the terms are added (independent proxy errors).
proxy_exclusion_loglik <- function(z, u_load, u_price, kept_slope) {
  vz <- sum(z^2)
  c_load <- sum(z * u_load); c_price <- sum(z * u_price)
  v_ll <- sum(u_load^2); v_pp <- sum(u_price^2); v_lp <- sum(u_load * u_price)
  r2_restricted <- (c_load - kept_slope * c_price)^2 / (vz * (v_ll - 2 * kept_slope * v_lp + kept_slope^2 * v_pp))
  gram <- matrix(c(v_ll, v_lp, v_lp, v_pp), 2)
  r2_unrestricted <- drop(t(c(c_load, c_price)) %*% solve(gram, c(c_load, c_price))) / vz
  (length(z) / 2) * (log(1 - r2_unrestricted) - log(1 - pmin(r2_restricted, 1 - 1e-12)))
}

# Same likelihood ratio for the restriction "z is uncorrelated with u_L - orth_slope * u_P": under the restriction z
# loads only on the part of (u_L, u_P) orthogonal to that combination. Vectorised over orth_slope.
proxy_orthogonality_loglik <- function(z, u_load, u_price, orth_slope) {
  vz <- sum(z^2)
  c_load <- sum(z * u_load); c_price <- sum(z * u_price)
  v_ll <- sum(u_load^2); v_pp <- sum(u_price^2); v_lp <- sum(u_load * u_price)
  w_w <- v_ll - 2 * orth_slope * v_lp + orth_slope^2 * v_pp      # <w, w>, w = u_L - m u_P
  p_w <- v_lp - orth_slope * v_pp                                 # <u_P, w>
  z_w <- c_load - orth_slope * c_price                            # <z, w>
  z_c <- c_price - p_w / w_w * z_w                                # <z, c>, c = u_P - <u_P,w>/<w,w> w
  c_c <- v_pp - p_w^2 / w_w                                       # <c, c>
  r2_restricted <- z_c^2 / (vz * c_c)
  gram <- matrix(c(v_ll, v_lp, v_lp, v_pp), 2)
  r2_unrestricted <- drop(t(c(c_load, c_price)) %*% solve(gram, c(c_load, c_price))) / vz
  (length(z) / 2) * (log(1 - r2_unrestricted) - log(1 - pmin(r2_restricted, 1 - 1e-12)))
}

# Pooled natural shocks. Each instrument x_j shifts both hourly curves (beta_s_j, beta_d_j). Restriction for one curve
# with slope eps: the curve shift v = u_L - eps u_P depends on the instruments only through the known index
# (S = sum_j beta_s_j x_j for supply, D = sum_j beta_d_j x_j for demand), against an unrestricted regression on all
# instruments. Log likelihood ratio (n / 2) [log RSS_unrestricted - log RSS_restricted], vectorised over eps (both
# residual sums of squares are quadratic in eps).
curve_system_loglik <- function(u_load, u_price, eps, restricted, unrestricted) {
  quadratic <- function(w) {
    pl <- drop(w %*% qr.solve(w, u_load)); pp <- drop(w %*% qr.solve(w, u_price))
    c(a = sum(u_load * (u_load - pl)), b = sum(u_load * (u_price - pp)), c = sum(u_price * (u_price - pp)))
  }
  rss <- function(q) q[["a"]] - 2 * eps * q[["b"]] + eps^2 * q[["c"]]
  (length(u_load) / 2) * (log(rss(quadratic(unrestricted))) - log(rss(quadratic(restricted))))
}

# Monthly anomalies of the six hourly instruments (load-weighted monthly means, residuals on calendar-month dummies
# and a trend) and the two curve-shift indices S = sum_j beta_s_j x_j and D = sum_j beta_d_j x_j (MW), with the
# coefficients of the hourly supply- and demand-curve regressions.
attach_curve_instruments <- function(country_data, path) {
  inst <- elasticity_block_instruments()
  hourly <- as.data.frame(arrow::read_parquet(
    path, col_select = dplyr::all_of(c("datetime", "load_tot", inst$all, "fit_supply_unmatched", "fit_demand_unmatched"))))
  hourly <- hourly[!duplicated(hourly$datetime), , drop = FALSE]
  for (column in c("cbf_es", "Published.Predicted.Unavailable.Capacity")) {
    hourly[[column]][!is.finite(hourly[[column]])] <- mean(hourly[[column]], na.rm = TRUE)
  }
  x <- as.matrix(hourly[, inst$all]); complete <- rowSums(is.finite(x)) == ncol(x)
  recover <- function(y) stats::lm.fit(x[complete, , drop = FALSE], y[complete])$coefficients
  beta_s <- recover(hourly$fit_supply_unmatched); beta_d <- recover(hourly$fit_demand_unmatched)
  month <- as.Date(format(as.Date(format(hourly$datetime, "%Y-%m-%d", tz = "Europe/Madrid")), "%Y-%m-01"))
  weight <- ifelse(is.finite(hourly$load_tot) & complete, hourly$load_tot, 0)
  load_month <- rowsum(weight, month)
  monthly <- sapply(inst$all, function(j) {
    as.numeric(rowsum(weight * ifelse(complete, x[, j], 0), month)) / as.numeric(load_month)
  })
  dates <- as.Date(rownames(load_month))
  calendar <- factor(format(dates, "%m")); trend <- seq_along(dates)
  anomalies <- apply(monthly, 2, function(z) stats::resid(stats::lm(z ~ calendar + trend, na.action = stats::na.exclude)))
  data <- country_data$data
  matched <- match(as.Date(data$observation_date), dates)
  names_ci <- curve_instrument_columns()
  for (k in seq_along(inst$all)) data[[names_ci[k]]] <- anomalies[matched, k]
  data$curve_S <- drop(anomalies %*% beta_s[inst$all])[matched]
  data$curve_D <- drop(anomalies %*% beta_d[inst$all])[matched]
  country_data$curve_coefficients <- rbind(supply_curve = beta_s[inst$all], demand_curve = beta_d[inst$all])
  country_data$data <- data
  country_data
}

curve_instrument_columns <- function() paste0("ci_", make.names(elasticity_block_instruments()$all))

fit_elasticity_bvar <- function(country_data, spec, cfg) {
  set.seed(cfg$seed)
  variables <- country_data$variables
  index <- list(price = match("spot_price_log", variables), load = match("load_tot_log", variables),
                gas = match("spot_gas_log", variables))
  if (any(vapply(index, is.na, logical(1)))) stop("Elasticity SVAR needs price, load and gas in the VAR.")
  n <- length(variables); p <- spec$p
  horizon <- max(cfg$elasticity$horizon, cfg$horizon - 1L)   # stored horizon (plots are truncated)
  sample <- finite_complete(country_data$data, unique(c("observation_date", variables)))
  levels <- as.matrix(sample[, variables])
  post <- reduced_form_posterior(levels, p, spec$var_prior)
  t_obs <- nrow(post$Y); k <- ncol(post$X)
  lx <- t(chol(post$xtx_inv))

  # Proxies: each loads on one structural shock only (exclusion restriction); common observed months.
  proxies <- spec$proxies
  z_raw <- lapply(proxies, function(proxy) sample[[proxy$column]][(p + 1):nrow(sample)])
  observed <- Reduce(`&`, lapply(z_raw, is.finite), init = rep(TRUE, t_obs))
  curve <- spec$curve_system
  if (!is.null(curve)) {
    curve_columns <- unique(c(curve$instruments, curve$supply_index, curve$demand_index, curve$supply_only))
    curve_raw <- as.matrix(sample[(p + 1):nrow(sample), curve_columns, drop = FALSE])
    observed <- observed & stats::complete.cases(curve_raw)
    curve_x <- scale(curve_raw[observed, , drop = FALSE], scale = FALSE)
    basis <- function(columns) curve_x[, columns, drop = FALSE]
    curve_bases <- list(
      supply_restricted = basis(c(curve$supply_index, curve$supply_only)),
      demand_restricted = basis(curve$demand_index),
      unrestricted = basis(c(curve$instruments, curve$supply_only)))
  }
  z_centered <- lapply(z_raw, function(z) z[observed] - mean(z[observed]))
  proposal_range <- if (is.null(spec$proposal_range)) cfg$elasticity$proposal_range else spec$proposal_range

  draws <- cfg$elasticity$draws; proposals <- cfg$elasticity$proposals
  irf_s <- irf_d <- array(NA_real_, c(horizon + 1L, n, draws), dimnames = list(0:horizon, variables, NULL))
  eps_s_draw <- eps_d_draw <- ess <- roots <- rep(NA_real_, draws)
  keep_draws <- unique(round(seq(1, draws, length.out = min(draws, cfg$elasticity$stored_draws))))
  stored <- list()
  for (r in seq_len(draws)) {
    sigma <- solve(stats::rWishart(1, post$nu, solve(post$S))[, , 1])
    b <- post$B + lx %*% matrix(stats::rnorm(k * n), k, n) %*% chol(sigma)
    roots[r] <- companion_root(b[-1, , drop = FALSE], n, p)
    if (identical(spec$identification, "mertens_ravn")) {
      # Mertens-Ravn (2013) on this reduced-form draw: the supply- and demand-shift proxies span the two shocks and
      # the supply shock traces the micro demand curve on impact (load / price = demand elasticity).
      u <- post$Y - post$X %*% b
      u <- scale(u[observed, , drop = FALSE], scale = FALSE)
      loads <- vapply(proxies, `[[`, "", "loads_on")
      gamma <- t(crossprod(cbind(z_centered[[which(loads == "supply")]], z_centered[[which(loads == "demand")]]), u))
      selector <- matrix(0, n, 2); selector[index$price, 1] <- 1; selector[index$load, 2] <- 1
      theta_s <- gamma %*% solve(t(selector) %*% gamma, c(1, spec$mr_demand_elasticity))
      restriction <- selector; restriction[, 1] <- solve(sigma, theta_s)
      theta_d <- gamma %*% solve(t(restriction) %*% gamma, c(0, 1))
      ma <- elas_ma_matrices(b[-1, , drop = FALSE], n, p, horizon)
      for (hh in 0:horizon) {
        irf_s[hh + 1L, , r] <- ma[, , hh + 1L] %*% theta_s
        irf_d[hh + 1L, , r] <- ma[, , hh + 1L] %*% theta_d
      }
      eps_s_draw[r] <- theta_d[index$load] / theta_d[index$price]; eps_d_draw[r] <- spec$mr_demand_elasticity
      ess[r] <- 1
      next
    }
    block <- netted_block(sigma, index, spec$gas_netted)
    if (is.null(spec$fixed_demand)) {
      eps_s <- stats::runif(proposals, proposal_range[[1]], proposal_range[[2]])
      eps_d <- implied_demand_slope(eps_s, block)
    } else {
      # Demand slope fixed (dogmatic prior at the hourly estimate): the supply slope follows from the
      # orthogonality of the two shocks, on each reduced-form draw (the mapping is symmetric).
      eps_d <- spec$fixed_demand
      eps_s <- implied_demand_slope(eps_d, block)
      if (!is.finite(eps_s) || eps_s <= 0) next
    }
    # Slope priors; without a prior only the signs eps_s > 0 and eps_d < 0 are imposed.
    log_weight <- (if (is.null(spec$prior_supply)) 0 else truncated_normal_log(eps_s, spec$prior_supply, "positive")) +
      (if (is.null(spec$prior_demand)) ifelse(eps_d < 0, 0, -Inf) else truncated_normal_log(eps_d, spec$prior_demand, "negative"))
    if (length(proxies) || !is.null(curve)) {
      u <- post$Y - post$X %*% b
      net <- function(column) {
        value <- u[, column]
        if (spec$gas_netted) value <- value - sigma[column, index$gas] / sigma[index$gas, index$gas] * u[, index$gas]
        value <- value[observed]; value - mean(value)
      }
      u_price <- net(index$price); u_load <- net(index$load)
      for (i in seq_along(proxies)) {
        # supply-shift proxy loads on v_s = u_L - eps_s u_P only; demand-shift proxy on v_d = u_L - eps_d u_P only
        kappa <- if (is.null(proxies[[i]]$contamination)) 0 else proxies[[i]]$contamination
        own <- if (proxies[[i]]$loads_on == "supply") eps_s else eps_d
        other <- if (proxies[[i]]$loads_on == "supply") eps_d else eps_s
        if (kappa == 0) {
          log_weight <- log_weight + proxy_exclusion_loglik(z_centered[[i]], u_load, u_price, own)
        } else {
          # Known contamination kappa = cov(z, v_other) / cov(z, v_own) (curve shifts in quantity units): z is
          # orthogonal to v_other - kappa v_own = (1 - kappa) u_L - (eps_other - kappa eps_own) u_P.
          if (abs(1 - kappa) < 1e-6) stop("Proxy contamination of 1 is not identified: ", proxies[[i]]$column)
          log_weight <- log_weight + proxy_orthogonality_loglik(z_centered[[i]], u_load, u_price,
                                                                (other - kappa * own) / (1 - kappa))
        }
      }
      if (!is.null(curve)) {
        log_weight <- log_weight +
          curve_system_loglik(u_load, u_price, eps_s, curve_bases$supply_restricted, curve_bases$unrestricted) +
          curve_system_loglik(u_load, u_price, eps_d, curve_bases$demand_restricted, curve_bases$unrestricted)
      }
    }
    finite <- is.finite(log_weight)
    if (!any(finite)) next
    weight <- exp(log_weight - max(log_weight[finite])); weight[!finite] <- 0
    ess[r] <- sum(weight)^2 / sum(weight^2)
    chosen <- sample.int(length(weight), 1L, prob = weight)
    vectors <- elasticity_vectors(sigma, eps_s[chosen], eps_d[chosen], index, spec$gas_netted)
    ma <- elas_ma_matrices(b[-1, , drop = FALSE], n, p, horizon)
    for (hh in 0:horizon) {
      irf_s[hh + 1L, , r] <- ma[, , hh + 1L] %*% vectors$b_s
      irf_d[hh + 1L, , r] <- ma[, , hh + 1L] %*% vectors$b_d
    }
    eps_s_draw[r] <- eps_s[chosen]; eps_d_draw[r] <- eps_d[chosen]
    if (r %in% keep_draws) stored[[length(stored) + 1L]] <- list(B = b, sigma = sigma, eps_s = eps_s[chosen])
  }
  keep <- is.finite(eps_s_draw)
  result <- list(
    spec = spec, variables = variables, hyper = post$hyper, roots = roots,
    eps_s = eps_s_draw[keep], eps_d = eps_d_draw[keep], ess = ess[keep],
    irf_supply = irf_s[, , keep, drop = FALSE], irf_demand = irf_d[, , keep, drop = FALSE],
    sample = sample, posterior = post, stored = stored
  )
  if (!identical(spec$identification, "mertens_ravn")) result$point <- elasticity_point_models(country_data, result, cfg)
  result
}

# Point models at the posterior mean of the reduced form and the posterior median supply slope, in the format of
# the single-shock proxy models, so that the historical decompositions and contribution tables reuse 05_contributions.R.
elasticity_models_at <- function(country_data, fit, B, sigma, eps_s, cfg) {
  variables <- fit$variables; spec <- fit$spec; p <- spec$p; post <- fit$posterior
  index <- list(price = match("spot_price_log", variables), load = match("load_tot_log", variables),
                gas = match("spot_gas_log", variables))
  dimnames(sigma) <- list(variables, variables)
  residuals <- post$Y - post$X %*% B; colnames(residuals) <- variables
  eps_d <- implied_demand_slope(eps_s, netted_block(sigma, index, spec$gas_netted))
  vectors <- elasticity_vectors(sigma, eps_s, eps_d, index, spec$gas_netted)
  ma <- elas_ma_matrices(B[-1, , drop = FALSE], length(variables), p, cfg$horizon - 1L)
  dates <- fit$sample$observation_date[-seq_len(p)]
  make_model <- function(impact, shock_unit, norm_index, shock_name) {
    theta <- matrix(impact / impact[norm_index], ncol = 1L, dimnames = list(variables, shock_name))
    shocks <- matrix(shock_unit * impact[norm_index], nrow = 1L, dimnames = list(shock_name, NULL))
    list(method = "Bayesian elasticity SVAR", country = country_data$country, shock = shock_name,
         norm_var = variables[norm_index], variables = variables, dates = dates, sample = fit$sample,
         residuals = residuals, sigma = sigma, theta = theta, shocks = shocks, ma = ma,
         irf = irf_from_theta(ma, theta, shock_name),
         hd = historical_decomposition(ma, theta, shocks, variables, shock_name))
  }
  shock_s <- -drop(residuals %*% vectors$a_s) / vectors$sd_s
  shock_d <- drop(residuals %*% vectors$a_d) / vectors$sd_d
  supply <- make_model(vectors$b_s, shock_s, index$price, "Supply")
  demand <- make_model(vectors$b_d, shock_d, index$load, "Demand")
  supply$elasticities <- demand$elasticities <- c(eps_s = eps_s, eps_d = eps_d)

  # Alessandri-Gazzani gas-supply shock identified on the same reduced form (external instrument, gas +1).
  gas <- NULL
  if (!is.null(country_data$gas_supply_proxy) && country_data$gas_supply_proxy %in% names(fit$sample)) {
    z_gas <- proxy_instrument(fit$sample, country_data$gas_supply_proxy, p)$z
    gamma <- matrix(drop(crossprod(z_gas, residuals)), ncol = 1L, dimnames = list(variables, NULL))
    theta_gas <- gamma / gamma[index$gas, 1]; colnames(theta_gas) <- "Gas supply"
    shocks_gas <- matrix(single_proxy_shocks(residuals, sigma, gamma, theta_gas), nrow = 1L,
                         dimnames = list("Gas supply", NULL))
    gas <- list(method = "External instrument on the elasticity-SVAR reduced form", country = country_data$country,
                shock = "Gas supply", norm_var = "spot_gas_log", variables = variables, dates = dates,
                sample = fit$sample, residuals = residuals, sigma = sigma, gamma = gamma, theta = theta_gas,
                shocks = shocks_gas, ma = ma, irf = irf_from_theta(ma, theta_gas, "Gas supply"),
                hd = historical_decomposition(ma, theta_gas, shocks_gas, variables, "Gas supply"))
  }
  list(supply = supply, demand = demand, gas_supply = gas, shock_supply = shock_s, shock_demand = shock_d,
       dates = dates)
}

# Point models at the posterior mean of the reduced form and the posterior median supply slope. The supply model
# also carries posterior draws of its price-normalised IRF for the counterfactual uncertainty.
elasticity_point_models <- function(country_data, fit, cfg) {
  post <- fit$posterior
  out <- elasticity_models_at(country_data, fit, post$B, post$sigma_mean, stats::median(fit$eps_s), cfg)
  n_draws <- dim(fit$irf_supply)[3]
  chosen <- unique(round(seq(1, n_draws, length.out = min(n_draws, max(1L, as.integer(cfg$counterfactual_bootstrap))))))
  out$supply$irf_draws <- lapply(chosen, function(r) {
    draw <- fit$irf_supply[seq_len(cfg$horizon), , r]
    draw / draw[1, "spot_price_log"]
  })
  out
}

# Posterior bands for the inflation contributions: historical decompositions recomputed on stored posterior draws.
elasticity_contribution_bands <- function(country_data, fit, cfg) {
  rows <- lapply(seq_along(fit$stored), function(i) {
    draw <- fit$stored[[i]]
    models <- try(elasticity_models_at(country_data, fit, draw$B, draw$sigma, draw$eps_s, cfg), silent = TRUE)
    if (inherits(models, "try-error") || is.null(models$gas_supply)) return(NULL)
    decomposition <- combine_three_proxy_hd(models$supply, models$demand, models$gas_supply)
    table <- contribution_table_from_hd(decomposition, cfg$windows, "Spain", "posterior draw", country_data$data)
    data.frame(Draw = i, table[, c("Episode", "Period", "Shock", "Cumulative_inflation_contribution_pp",
                                   "Cumulative_inflation_pp")])
  })
  draws <- dplyr::bind_rows(rows)
  draws$Shock[draws$Shock == "Total electricity"] <- "Total identified (electricity and gas)"
  draws |>
    dplyr::mutate(Share = 100 * Cumulative_inflation_contribution_pp / Cumulative_inflation_pp) |>
    dplyr::group_by(Episode, Period, Shock) |>
    dplyr::summarise(
      Median_pp = stats::median(Cumulative_inflation_contribution_pp),
      Lower68_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.16),
      Upper68_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.84),
      Lower90_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.05),
      Upper90_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.95),
      Median_share_percent = stats::median(Share),
      Lower90_share_percent = stats::quantile(Share, 0.05),
      Upper90_share_percent = stats::quantile(Share, 0.95),
      Observed_cumulative_inflation_pp = dplyr::first(Cumulative_inflation_pp),
      Draws = dplyr::n(), .groups = "drop")
}

# Bootstrap bands for the inflation contributions of the benchmark proxy SVAR (electricity supply, electricity
# demand, gas supply). Moving-block residual bootstrap: each replication re-estimates the OLS VAR on simulated data
# and re-identifies the three shocks with the resampled proxies; the historical decomposition is then computed on
# the observed data with the bootstrap coefficients, as with the posterior draws of the Bayesian SVAR.
benchmark_contribution_bands <- function(supply, demand, gas_supply, cfg, observed_data,
                                         draws = cfg$counterfactual_bootstrap) {
  variables <- supply$variables; p <- supply$fit$p; k <- length(variables)
  stopifnot(identical(as.Date(supply$dates), as.Date(demand$dates)),
            identical(as.Date(supply$dates), as.Date(gas_supply$dates)))
  observed <- as.matrix(supply$sample[, variables, drop = FALSE])
  datamat <- as.matrix(supply$fit$datamat)
  y_observed <- datamat[, variables, drop = FALSE]
  x_observed <- datamat[, setdiff(colnames(datamat), variables), drop = FALSE]
  models <- list(Supply = supply, Demand = demand, "Gas supply" = gas_supply)
  norm <- c(Supply = "spot_price_log", Demand = "spot_price_log", "Gas supply" = "spot_gas_log")
  instruments <- lapply(models, function(m) proxy_instrument(m$sample, m$proxy, p)$z)
  residuals <- sweep(supply$residuals, 2, colMeans(supply$residuals), "-")
  n <- nrow(residuals)
  block <- as.integer(cfg$counterfactual_block_size)
  if (!is.finite(block) || block <= 0L) block <- round(5 * n^0.25)
  set.seed(cfg$seed + 1789L)
  rows <- list(); attempts <- 0L
  while (length(rows) < draws && attempts < 4L * draws) {
    attempts <- attempts + 1L
    table <- try({
      index <- moving_block_indices(n, block)
      boot_data <- simulate_var_path(supply$fit, observed[seq_len(p), , drop = FALSE], residuals[index, , drop = FALSE])
      boot_fit <- vars::VAR(stats::ts(boot_data, frequency = 12), p = p, type = "const")
      boot_residuals <- stats::residuals(boot_fit)
      sigma <- stats::cov(boot_residuals)
      coefficients <- vars::Bcoef(boot_fit)[, colnames(x_observed), drop = FALSE]
      u_observed <- y_observed - x_observed %*% t(coefficients)
      ma <- vars::Phi(boot_fit, nstep = cfg$horizon - 1L)
      boot_models <- lapply(names(models), function(shock) {
        gamma <- matrix(drop(crossprod(instruments[[shock]][index], boot_residuals)), ncol = 1L,
                        dimnames = list(variables, NULL))
        if (abs(gamma[norm[[shock]], 1]) < sqrt(.Machine$double.eps)) stop("Weak normalisation")
        theta <- gamma / gamma[norm[[shock]], 1]; colnames(theta) <- shock
        shocks <- matrix(single_proxy_shocks(u_observed, sigma, gamma, theta), nrow = 1L)
        if (any(!is.finite(ma)) || max(abs(ma[, , dim(ma)[3]] %*% theta)) > 100) stop("Explosive draw")
        list(dates = supply$dates, variables = variables,
             hd = historical_decomposition(ma, theta, shocks, variables, shock))
      })
      decomposition <- combine_three_proxy_hd(boot_models[[1]], boot_models[[2]], boot_models[[3]])
      contribution_table_from_hd(decomposition, cfg$windows, "Spain", "bootstrap draw", observed_data)
    }, silent = TRUE)
    if (inherits(table, "try-error")) next
    rows[[length(rows) + 1L]] <- data.frame(Draw = length(rows) + 1L,
                                            table[, c("Episode", "Period", "Shock", "Cumulative_inflation_contribution_pp",
                                                      "Cumulative_inflation_pp")])
  }
  message("Benchmark contribution bootstrap: ", length(rows), " accepted draws out of ", attempts)
  draws_table <- dplyr::bind_rows(rows)
  draws_table$Shock[draws_table$Shock == "Total electricity"] <- "Total identified (electricity and gas)"
  draws_table |>
    dplyr::mutate(Share = 100 * Cumulative_inflation_contribution_pp / Cumulative_inflation_pp) |>
    dplyr::group_by(Episode, Period, Shock) |>
    dplyr::summarise(
      Median_pp = stats::median(Cumulative_inflation_contribution_pp),
      Lower68_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.16),
      Upper68_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.84),
      Lower90_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.05),
      Upper90_pp = stats::quantile(Cumulative_inflation_contribution_pp, 0.95),
      Median_share_percent = stats::median(Share),
      Lower90_share_percent = stats::quantile(Share, 0.05),
      Upper90_share_percent = stats::quantile(Share, 0.95),
      Observed_cumulative_inflation_pp = dplyr::first(Cumulative_inflation_pp),
      Draws = dplyr::n(), .groups = "drop")
}

# Pass-through of the gas-supply shock (gas price +100% on impact) to HICP and the electricity price.
benchmark_gas_passthrough <- function(gas_supply) {
  irf <- gas_supply$irf
  irf <- irf[irf$Horizon %in% c(0, 3, 6, 12, 24) & irf$Variable %in% c("spot_gas_log", "spot_price_log", "HICP_log", "IPI_log"), ]
  tidyr::pivot_wider(irf[, c("Horizon", "Variable", "Estimate")], names_from = Variable, values_from = Estimate)
}

# ----------------------------------------------------------------------------------------------------
# 4. Specifications, summaries and figures
# ----------------------------------------------------------------------------------------------------
elasticity_specs <- function(cfg, temperature_contamination = 0) {
  e <- cfg$elasticity
  supply <- function(column) list(column = column, loads_on = "supply")
  demand <- function(column) list(column = column, loads_on = "demand")
  physical <- list(supply("phys_supply"), demand("phys_demand"))
  synthetic <- list(supply(e$supply_proxy), demand(e$demand_proxy))
  # Central: monthly physical proxies (wind-hydro supply, degree-day demand) and the signs eps_s > 0, eps_d < 0
  # only. The micro elasticities are used to validate the posterior, not as priors.
  central <- list(name = "Central: physical monthly proxies, signs only", p = e$p_central, var_prior = "glp",
                  proxies = physical, gas_netted = TRUE, proposal_range = e$proposal_range_flat)
  # Former central specification: synthetic hourly curve shifts (Kilian surprises) and slope priors.
  synthetic_priors <- list(name = "Synthetic shifts and slope priors (former central)", p = e$p_central,
                           var_prior = "glp", prior_supply = e$prior_supply, prior_demand = e$prior_demand,
                           proxies = synthetic, gas_netted = TRUE)
  # fields are replaced, not merged (modifyList would merge the proxy lists); `drop` removes a field
  variant <- function(base, name, ..., drop = character(0)) {
    out <- base; changes <- list(...)
    for (field in names(changes)) out[field] <- list(changes[[field]])
    for (field in drop) out[field] <- list(NULL)
    out$name <- name
    out
  }
  alt <- function(name, ..., drop = character(0)) variant(central, name, ..., drop = drop)
  syn <- function(name, ..., drop = character(0)) variant(synthetic_priors, name, ..., drop = drop)
  with_priors <- list(prior_supply = e$prior_supply, prior_demand = e$prior_demand, proposal_range = e$proposal_range)
  list(
    central = central,
    # slope priors added to the central proxies
    physical_priors = do.call(alt, c(list("Physical monthly proxies and slope priors"), with_priors)),
    physical_micro = alt("Physical proxies, supply-slope prior on the hourly estimate",
                         prior_supply = e$prior_supply_micro, proposal_range = e$proposal_range),
    # VAR specification
    p6 = alt("GLP prior, p = 6", p = 6L),
    minnesota = alt("Minnesota-only VAR prior", var_prior = "minnesota"),
    diffuse = alt("Diffuse VAR prior", var_prior = "diffuse"),
    no_gas = alt("Innovations not netted of gas", gas_netted = FALSE),
    # physical proxies
    physical_ll = alt("Physical proxies residualised on VAR lags (lead-lag)",
                      proxies = list(supply("phys_supply_ll"), demand("phys_demand_ll"))),
    physical_supply_only = alt("Physical supply proxy only", proxies = list(supply("phys_supply"))),
    physical_demand_only = alt("Physical demand proxy only", proxies = list(demand("phys_demand"))),
    # supply effect of temperature: known loading of the demand proxy on the supply-curve shift (hourly curves)
    temperature_supply = alt("Physical proxies, temperature also shifts supply (kappa from hourly curves)",
                             proxies = list(supply("phys_supply"),
                                            c(demand("phys_demand"), contamination = temperature_contamination))),
    curve_demand = alt("Physical supply proxy and curve-based demand proxy (purged of temperature supply shift)",
                       proxies = list(supply("phys_supply"), demand("curve_demand"))),
    # pooled natural shocks: every instrument shifts both curves with the hourly coefficients
    curve_pooled = alt("Pooled natural shocks: both curve shifts of each instrument", proxies = list(),
                       curve_system = list(instruments = curve_instrument_columns(),
                                           supply_index = "curve_S", demand_index = "curve_D")),
    curve_pooled_hydro = alt("Pooled natural shocks and hydro (supply only)", proxies = list(),
                             curve_system = list(instruments = curve_instrument_columns(), supply_index = "curve_S",
                                                 demand_index = "curve_D", supply_only = "phys_S_hydro")),
    no_proxy = do.call(alt, c(list("Slope priors only (no proxy)", proxies = list()), with_priors)),
    # synthetic hourly curve shifts (former central specification and its variants)
    synthetic = synthetic_priors,
    benchmark = syn("Synthetic shifts, no slope priors (signs only)",
                    proposal_range = e$proposal_range_flat, drop = c("prior_supply", "prior_demand")),
    level_proxy = syn("Synthetic shifts in levels",
                      proxies = list(supply(sub("_surp$", "_lvl", e$supply_proxy)), demand(sub("_surp$", "_lvl", e$demand_proxy)))),
    demand_shift_only = syn("Synthetic demand shift only", proxies = list(demand(e$demand_proxy))),
    supply_shift_only = syn("Synthetic supply shift only", proxies = list(supply(e$supply_proxy))),
    temperature = syn("Temperature component of the demand shift only", proxies = list(demand(e$temperature_proxy))),
    wind = syn("Supply proxy: wind forecast", proxies = list(supply("k_S_wind_surp"))),
    wind_temperature = syn("Wind forecast (supply) and temperature (demand)",
                           proxies = list(supply("k_S_wind_surp"), demand(e$temperature_proxy))),
    synthetic_wedge = syn("Synthetic shifts and Iberian wedge (supply)",
                          proxies = list(supply(e$supply_proxy), supply("wedge_iber_exception"), demand(e$demand_proxy))),
    # identification robustness: Mertens-Ravn with the physical proxies and the micro demand elasticity
    mertens_ravn = alt("Mertens-Ravn: physical proxies and micro demand elasticity", identification = "mertens_ravn",
                       mr_demand_elasticity = read_micro_demand_elasticity(cfg), gas_netted = FALSE),
    # Robustness of the benchmark proxy SVAR (figures 14-15 of the working paper): demand slope fixed at the
    # hourly estimate (wind as a pure supply shock), with and without gas-netted innovations; Mertens-Ravn with
    # the benchmark proxies (monthly levels of the synthetic shifts, calendar-month and year fixed effects).
    fixed_micro = list(name = "Bayesian, demand elasticity fixed at the hourly estimate", p = e$p_central,
                       var_prior = "glp", proxies = list(), gas_netted = TRUE,
                       fixed_demand = read_micro_demand_elasticity(cfg)),
    fixed_micro_no_gas = list(name = "Bayesian, fixed demand elasticity, innovations not netted of gas",
                              p = e$p_central, var_prior = "glp", proxies = list(), gas_netted = FALSE,
                              fixed_demand = read_micro_demand_elasticity(cfg)),
    mertens_ravn_benchmark = alt("Mertens-Ravn (benchmark proxies, hourly demand elasticity)",
                                 identification = "mertens_ravn",
                                 proxies = list(supply("syn_S_lvl_cy"), demand("syn_D_lvl_cy")),
                                 mr_demand_elasticity = read_micro_demand_elasticity(cfg), gas_netted = FALSE)
  )
}

elasticity_quantiles <- function(fit, horizon = NULL) {
  variables <- fit$variables
  normalise <- function(draws, norm, size = 1) {
    for (r in seq_len(dim(draws)[3])) draws[, , r] <- size * draws[, , r] / draws[1, norm, r]
    draws
  }
  out <- list()
  for (shock in c("Supply", "Demand")) {
    # Both shocks scaled to a 10% (0.1 log point) impact rise in the electricity spot price
    draws <- normalise(if (shock == "Supply") fit$irf_supply else fit$irf_demand, "spot_price_log", size = 0.1)
    if (!is.null(horizon)) draws <- draws[seq_len(min(dim(draws)[1], horizon + 1L)), , , drop = FALSE]
    q <- function(pr) apply(draws, c(1, 2), stats::quantile, pr, na.rm = TRUE)
    med <- q(0.5); l68 <- q(0.16); u68 <- q(0.84); l90 <- q(0.05); u90 <- q(0.95)
    out[[shock]] <- data.frame(
      Specification = fit$spec$name, Shock = shock,
      Horizon = rep(as.integer(dimnames(draws)[[1]]), times = length(variables)),
      Variable = rep(variables, each = dim(draws)[1]),
      Median = as.vector(med), Lower68 = as.vector(l68), Upper68 = as.vector(u68),
      Lower90 = as.vector(l90), Upper90 = as.vector(u90), stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(out)
}

elasticity_summary_row <- function(fit) {
  s <- fit$irf_supply; d <- fit$irf_demand
  q <- function(x) stats::quantile(x, c(0.5, 0.05, 0.95), na.rm = TRUE)
  fmt <- function(x) { v <- q(x); sprintf("%.3f [%.3f, %.3f]", v[1], v[2], v[3]) }
  data.frame(
    Specification = fit$spec$name,
    VAR_prior = fit$spec$var_prior, Lags = fit$spec$p,
    Hyperparameters = if (is.null(fit$hyper)) "-" else
      sprintf("lambda = %.2f, mu = %s, delta = %s", fit$hyper$lambda, format(fit$hyper$mu), format(fit$hyper$delta)),
    Largest_root_median = round(stats::median(fit$roots), 4),
    Share_roots_above_1.05 = round(mean(fit$roots > 1.05), 3),
    Median_ESS = round(stats::median(fit$ess), 1),
    Supply_slope = fmt(fit$eps_s), Demand_slope = fmt(fit$eps_d),
    P_supply_signs = round(mean(s[1, "spot_price_log", ] > 0 & s[1, "load_tot_log", ] < 0 & s[1, "gen_tot_log", ] < 0), 3),
    P_demand_signs = round(mean(d[1, "spot_price_log", ] > 0 & d[1, "load_tot_log", ] > 0), 3),
    Supply_price_6m = fmt(s[7, "spot_price_log", ] / s[1, "spot_price_log", ]),
    Supply_HICP_12m = fmt(s[13, "HICP_log", ] / s[1, "spot_price_log", ]),
    Demand_HICP_6m = fmt(d[7, "HICP_log", ] / d[1, "load_tot_log", ]),
    Demand_HICP_12m = fmt(d[13, "HICP_log", ] / d[1, "load_tot_log", ]),
    stringsAsFactors = FALSE, check.names = FALSE)
}

elasticity_validation <- function(fit, country_data) {
  point <- fit$point
  data <- country_data$data[match(point$dates, country_data$data$observation_date), , drop = FALSE]
  correlation <- function(shock, column) {
    if (!column %in% names(data)) return(NA_real_)
    ok <- is.finite(data[[column]]); if (sum(ok) < 12) return(NA_real_)
    stats::cor(shock[ok], data[[column]][ok])
  }
  proxies <- c("Synthetic supply shift (Kilian surprise)" = "k_S_syn_surp",
               "Synthetic demand shift (Kilian surprise)" = "k_D_syn_surp",
               "Temperature component of the demand shift" = "k_D_block_surp",
               "Supply-instrument component of the supply shift" = "k_S_block_surp",
               "Wind forecast (Kilian surprise)" = "k_S_wind_surp",
               "Iberian wedge" = "wedge_iber_exception",
               "Benchmark PCA supply proxy" = "proxy_supply_pc",
               "Benchmark PCA demand proxy" = "proxy_demand_pc",
               "Alessandri-Gazzani gas instrument" = "gas_supply_gazzani",
               "Physical supply proxy (wind and hydro)" = "phys_supply",
               "Physical demand proxy (degree-days)" = "phys_demand")
  data.frame(
    Proxy = names(proxies),
    Corr_with_supply_shock = vapply(proxies, function(x) correlation(point$shock_supply, x), numeric(1)),
    Corr_with_demand_shock = vapply(proxies, function(x) correlation(point$shock_demand, x), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL)
}

plot_elasticity_irf <- function(quantiles, shock) {
  data <- quantiles[quantiles$Shock == shock, , drop = FALSE]
  order <- c("spot_price_log", "load_tot_log", "gen_tot_log", "IPI_log", "HICP_log", "ffr", "spot_gas_log")
  present <- c(intersect(order, unique(data$Variable)), setdiff(unique(data$Variable), order))
  data$Variable <- factor(variable_label(data$Variable), levels = variable_label(present))
  # Demand shocks in red tones to distinguish them from supply shocks (blue)
  band_fill <- if (shock == "Demand") "firebrick2" else "steelblue"
  line_col <- if (shock == "Demand") "darkred" else "navy"
  ggplot2::ggplot(data, ggplot2::aes(Horizon, Median)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower90, ymax = Upper90), fill = band_fill, alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower68, ymax = Upper68), fill = band_fill, alpha = 0.30) +
    ggplot2::geom_line(colour = line_col, linewidth = 0.9) +
    ggplot2::facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
    ggplot2::labs(x = "Months", y = NULL)
}

plot_elasticity_comparison <- function(quantiles, shock, variables = c("spot_price_log", "load_tot_log", "HICP_log", "ffr")) {
  data <- quantiles[quantiles$Shock == shock & quantiles$Variable %in% variables, , drop = FALSE]
  data$Variable <- factor(variable_label(data$Variable), levels = variable_label(variables))
  ggplot2::ggplot(data, ggplot2::aes(Horizon, Median, colour = Specification, fill = Specification)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = Lower68, ymax = Upper68), alpha = 0.06, colour = NA) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = "Months", y = NULL, colour = NULL, fill = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2), fill = ggplot2::guide_legend(ncol = 2))
}

# Posterior slopes of the central specification (signs only), of the same proxies with slope priors, and the
# priors themselves; dashed lines: micro estimates (hourly IV supply slope, daily local-projection demand slope).
plot_elasticity_slopes <- function(fit, cfg, fit_priors = NULL) {
  e <- cfg$elasticity
  levels <- c("Posterior: central (signs only)", "Posterior: with slope priors", "Slope prior")
  data <- rbind(
    data.frame(Slope = "Supply slope", Distribution = levels[1], value = fit$eps_s),
    data.frame(Slope = "Demand slope", Distribution = levels[1], value = fit$eps_d),
    data.frame(Slope = "Supply slope", Distribution = levels[3],
               value = stats::rnorm(4000, e$prior_supply[1], e$prior_supply[2])),
    data.frame(Slope = "Demand slope", Distribution = levels[3],
               value = pmin(stats::rnorm(4000, e$prior_demand[1], e$prior_demand[2]), 0)))
  if (!is.null(fit_priors)) {
    data <- rbind(data,
                  data.frame(Slope = "Supply slope", Distribution = levels[2], value = fit_priors$eps_s),
                  data.frame(Slope = "Demand slope", Distribution = levels[2], value = fit_priors$eps_d))
  }
  data$Distribution <- factor(data$Distribution, levels = intersect(levels, unique(data$Distribution)))
  micro <- data.frame(Slope = c("Supply slope", "Demand slope"), value = c(e$prior_supply_micro[1], e$prior_demand[1]))
  ggplot2::ggplot(data, ggplot2::aes(value, colour = Distribution, fill = Distribution)) +
    ggplot2::geom_density(alpha = 0.2, adjust = 1.2) +
    ggplot2::geom_vline(data = micro, ggplot2::aes(xintercept = value), linetype = "dashed", colour = "grey30") +
    ggplot2::facet_wrap(~ Slope, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, fill = NULL)
}

elasticity_output_cfg <- function(cfg) {
  out <- cfg
  out$table_dir <- file.path(cfg$output_dir, "elasticity", "tables")
  out$figure_dir <- file.path(cfg$output_dir, "elasticity", "figures")
  out$overleaf$enabled <- isTRUE(cfg$overleaf$enabled) && isTRUE(cfg$elasticity$export_overleaf)
  dir.create(out$table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(out$figure_dir, recursive = TRUE, showWarnings = FALSE)
  out
}

run_elasticity_stage <- function(country_data, cfg, get_model = function(name, builder) builder()) {
  ocfg <- elasticity_output_cfg(cfg)
  message("Elasticity SVAR outputs: ", dirname(ocfg$table_dir),
          if (ocfg$overleaf$enabled) " (Overleaf export on)" else " (Overleaf export off)")
  country_data <- attach_block_proxies(country_data, cfg$data$spain_high_frequency_proxies)
  country_data <- attach_physical_proxies(country_data)
  country_data <- attach_temperature_shifts(country_data, cfg$data$spain_high_frequency_proxies)
  country_data <- attach_curve_instruments(country_data, cfg$data$spain_high_frequency_proxies)
  country_data <- attach_synthetic_level_proxies(country_data, benchmark_hourly_path(cfg))
  message("Temperature contamination of the demand proxy (supply / demand shift): ",
          round(country_data$temperature_contamination, 3))
  coefficients <- country_data$block_proxy_coefficients
  save_csv(data.frame(Curve = rownames(coefficients), coefficients, check.names = FALSE), ocfg,
           "block_proxy_curve_coefficients.csv")

  specs <- elasticity_specs(cfg, country_data$temperature_contamination)
  tag <- sprintf("_d%d_h%d", cfg$elasticity$draws, cfg$elasticity$horizon)
  fits <- lapply(names(specs), function(key) {
    get_model(paste0("spain_elasticity_bvar_", key, tag), function() {
      message("Elasticity SVAR: ", specs[[key]]$name)
      fit_elasticity_bvar(country_data, specs[[key]], cfg)
    })
  })
  names(fits) <- names(specs)
  central <- fits$central

  quantiles <- dplyr::bind_rows(lapply(fits, elasticity_quantiles, horizon = cfg$elasticity$horizon))
  save_csv(quantiles, ocfg, "irfs_spain_elasticity_svar.csv")
  summary <- dplyr::bind_rows(lapply(fits, elasticity_summary_row))
  save_csv(summary, ocfg, "elasticity_svar_specifications.csv")
  save_csv(elasticity_validation(central, country_data), ocfg, "elasticity_svar_proxy_validation.csv")
  save_csv(data.frame(Supply_slope = central$eps_s, Demand_slope = central$eps_d), ocfg,
           "elasticity_svar_slope_posterior.csv")

  central_q <- quantiles[quantiles$Specification == central$spec$name, , drop = FALSE]
  save_plot(plot_elasticity_irf(central_q, "Supply"), ocfg, cfg$plot_names$spain_supply_elasticity,
            overleaf_dir = "macro", width = 14, height = 8)
  save_plot(plot_elasticity_irf(central_q, "Demand"), ocfg, cfg$plot_names$spain_demand_elasticity,
            overleaf_dir = "macro", width = 14, height = 8)
  save_plot(plot_elasticity_slopes(central, cfg, fits$physical_priors), ocfg, cfg$plot_names$spain_elasticity_slopes,
            overleaf_dir = "macro", width = 12, height = 5, theme_overrides = ggplot2::theme(legend.position = "bottom"))
  groups <- list(
    benchmark = c("central", "physical_priors", "synthetic", "benchmark"),
    robustness_identification = c("central", "diffuse", "no_gas", "physical_priors", "mertens_ravn"),
    robustness_proxies = c("central", "physical_ll", "physical_supply_only", "physical_demand_only"),
    robustness_temperature = c("central", "temperature_supply", "curve_demand"),
    pooled_curves = c("central", "temperature_supply", "curve_pooled", "curve_pooled_hydro"),
    robustness_synthetic = c("synthetic", "benchmark", "wind_temperature", "synthetic_wedge")
  )
  # Benchmark of the working paper: monthly proxy SVAR (levels of the synthetic shifts net of calendar-month and year
  # fixed effects) on a Bayesian (GLP) reduced form; main figures, and its comparison with the Bayesian SVAR with a
  # fixed demand elasticity and Mertens-Ravn (price, load, HICP, industrial production; +10% electricity price).
  benchmark_fit <- bayesian_proxy_benchmark(country_data, cfg)
  benchmark_name <- benchmark_fit$spec$name
  benchmark_q <- elasticity_quantiles(benchmark_fit, horizon = cfg$elasticity$horizon)
  save_csv(benchmark_q, ocfg, "irfs_spain_benchmark_bayesian_proxy.csv")
  save_plot(plot_elasticity_irf(benchmark_q, "Supply"), ocfg, "supply_benchmark_main_plot.png",
            overleaf_dir = "macro", width = 14, height = 8)
  save_plot(plot_elasticity_irf(benchmark_q, "Demand"), ocfg, "demand_benchmark_main_plot.png",
            overleaf_dir = "macro", width = 14, height = 8)
  gas_q <- bayesian_proxy_gas_quantiles(benchmark_fit, size = 0.1, horizon = cfg$elasticity$horizon)
  save_csv(gas_q, ocfg, "irfs_spain_benchmark_gas_supply.csv")
  save_plot(plot_elasticity_irf(gas_q, "Gas supply"), ocfg, "gas_supply_shock_benchmark_irf.png",
            overleaf_dir = "macro", width = 14, height = 8)
  # Same proxies on the OLS reduced form, with weak-instrument robust MSW sets (robustness; relevance statistics).
  benchmark_ols <- get_model("spain_benchmark_msw_synthetic_levels_cy", function() fit_benchmark_msw(country_data, cfg))
  ols_name <- "Proxy SVAR, OLS reduced form (MSW sets)"
  ols_q <- msw_quantiles(benchmark_ols, ols_name, size = 0.1, horizon = cfg$elasticity$horizon)
  save_csv(ols_q, ocfg, "irfs_spain_benchmark_ols_msw.csv")
  save_plot(plot_elasticity_irf(ols_q, "Supply"), ocfg, "supply_proxy_ols_msw.png", overleaf_dir = "macro", width = 14, height = 8)
  save_plot(plot_elasticity_irf(ols_q, "Demand"), ocfg, "demand_proxy_ols_msw.png", overleaf_dir = "macro", width = 14, height = 8)
  # Robustness: same benchmark with the stock of hydro reservoirs (REE, all reservoirs, log MWh) as an extra
  # endogenous variable, so that the shocks are identified net of the hydro storage dynamics.
  hydro_fit <- bayesian_proxy_benchmark(add_hydro_storage(country_data, cfg$data$hydro_reserves), cfg,
                                        name = "Benchmark with hydro reservoir storage")
  hydro_q <- elasticity_quantiles(hydro_fit, horizon = cfg$elasticity$horizon)
  save_csv(hydro_q, ocfg, "irfs_spain_benchmark_hydro_storage.csv")
  # Robustness on proxies observed from 2010: wind speed as the supply proxy, and the synthetic decomposition
  # applied out of sample before 2015 (same Bayesian proxy SVAR).
  country_2010 <- attach_proxies_2010(country_data, cfg)
  wind_fit <- bayesian_proxy_benchmark(country_2010, cfg, proxies = c(Supply = "wind_S_lvl_cy", Demand = "syn_D_lvl_cy"),
                                       name = "Benchmark, supply proxy: wind speed from 2010")
  oos_fit <- bayesian_proxy_benchmark(country_2010, cfg, proxies = c(Supply = "syn_S_oos_cy", Demand = "syn_D_oos_cy"),
                                      name = "Benchmark, synthetic proxies out of sample before 2015")
  wind_q <- elasticity_quantiles(wind_fit, horizon = cfg$elasticity$horizon)
  oos_q <- elasticity_quantiles(oos_fit, horizon = cfg$elasticity$horizon)
  save_csv(dplyr::bind_rows(wind_q, oos_q), ocfg, "irfs_spain_benchmark_proxies_2010.csv")
  comparison_keys <- c("fixed_micro", "fixed_micro_no_gas", "mertens_ravn_benchmark")
  # Identification robustness (<shock>_benchmark_robustness.png): fixed demand slope with/without gas netting,
  # Mertens-Ravn, OLS reduced form. Proxy and data robustness (<shock>_proxy_robustness.png): proxies observed
  # from 2010 and hydro reservoir storage as an extra VAR variable.
  comparison_sets <- list(
    benchmark_robustness = list(
      data = dplyr::bind_rows(benchmark_q, ols_q, quantiles[quantiles$Specification %in%
                                                   vapply(fits[comparison_keys], function(f) f$spec$name, ""), ]),
      levels = c(benchmark_name, vapply(fits[comparison_keys], function(f) f$spec$name, ""), ols_name)),
    proxy_robustness = list(
      data = dplyr::bind_rows(benchmark_q, wind_q, oos_q, hydro_q),
      levels = c(benchmark_name, wind_fit$spec$name, oos_fit$spec$name, hydro_fit$spec$name)))
  for (set in names(comparison_sets)) {
    comparison <- comparison_sets[[set]]$data
    comparison$Specification <- factor(comparison$Specification, levels = comparison_sets[[set]]$levels)
    # Median lines only for Mertens-Ravn and the OLS/MSW sets (much wider bands), so that the other bands stay readable
    no_band <- comparison$Specification %in% c(fits$mertens_ravn_benchmark$spec$name, ols_name)
    comparison$Lower68[no_band] <- NA_real_; comparison$Upper68[no_band] <- NA_real_
    for (shock in c("Supply", "Demand")) {
      save_plot(plot_elasticity_comparison(comparison, shock, c("spot_price_log", "load_tot_log", "HICP_log", "IPI_log")),
                ocfg, sprintf("%s_%s.png", tolower(shock), set), overleaf_dir = "macro",
                width = 14, height = 10, theme_overrides = ggplot2::theme(legend.position = "bottom",
                                                                          legend.text = ggplot2::element_text(size = 12)))
    }
  }

  for (group in names(groups)) {
    names_in <- vapply(fits[groups[[group]]], function(f) f$spec$name, "")
    selected <- quantiles[quantiles$Specification %in% names_in, , drop = FALSE]
    selected$Specification <- factor(selected$Specification, levels = names_in)
    # Mertens-Ravn bands are an order of magnitude wider: median line only, so that the other bands stay readable
    no_band <- selected$Specification %in% vapply(fits[intersect("mertens_ravn", names(fits))], function(f) f$spec$name, "")
    selected$Lower68[no_band] <- NA_real_; selected$Upper68[no_band] <- NA_real_
    for (shock in c("Supply", "Demand")) {
      save_plot(plot_elasticity_comparison(selected, shock), ocfg,
                sprintf("%s_elasticity_%s.png", tolower(shock), group), overleaf_dir = "macro",
                width = 14, height = 10, theme_overrides = ggplot2::theme(legend.position = "bottom", legend.text = ggplot2::element_text(size = 12)))
    }
  }

  # Historical decompositions, contributions and policy counterfactuals from the benchmark proxy SVAR, with the
  # Alessandri-Gazzani gas-supply shock identified on the same OLS reduced form (gas price +1 on impact).
  if (is.null(country_data$gas_supply_proxy)) stop("The gas-supply instrument is required for the energy contributions.")
  gas_ols <- get_model("spain_benchmark_msw_gas_supply", function()
    fit_proxy_svar(country_data, country_data$gas_supply_proxy, "spot_gas_log", "Gas supply", cfg,
                   use_inference = FALSE))
  run_relevance(list("Supply proxy: synthetic shift, monthly level (month and year FE)" = benchmark_ols$Supply,
                     "Demand proxy: synthetic shift, monthly level (month and year FE)" = benchmark_ols$Demand,
                     "Gas supply proxy: Alessandri-Gazzani 2025" = gas_ols),
                ocfg, csv_name = "proxy_relevance_spain_benchmark.csv", tex_name = "iv_relevance_spain_benchmark.tex")

  # Historical decompositions, contributions and policy counterfactuals from the benchmark (point models at the
  # posterior mean of the reduced form; posterior bands for the contributions and the wind counterfactual).
  point <- benchmark_fit$point
  contributions <- run_spain_contributions(point$Supply, point$Demand, point[["Gas supply"]], ocfg, country_data$data)
  save_csv(data.frame(Date = as.Date(point$Supply$dates), Supply_shock = point$Supply$shocks[1, ],
                      Demand_shock = point$Demand$shocks[1, ], Gas_supply_shock = point[["Gas supply"]]$shocks[1, ]),
           ocfg, "benchmark_structural_shocks.csv")
  save_csv(benchmark_gas_passthrough(point[["Gas supply"]]), ocfg, "benchmark_gas_supply_passthrough.csv")
  bands <- bayesian_proxy_contribution_bands(benchmark_fit, cfg, country_data$data)
  save_csv(bands, ocfg, "contributions_spain_benchmark_posterior_bands.csv")
  counterfactuals <- run_counterfactuals(point$Supply, ocfg,
                                         cache_tag = sprintf("_benchmark_bayes_proxy_d%d", cfg$elasticity$draws))
  invisible(list(fits = fits, summary = summary, contributions = contributions, bands = bands,
                 counterfactuals = counterfactuals))
}
