# Bridge Proxy-SVAR for Spanish electricity supply and demand shocks.
# Gazzani and Vicondoa (2020), Bridge Proxy-SVAR.
# https://economia.uc.cl/wp-content/uploads/2022/10/dt-533.pdf
# hourly signals -> informative daily SVAR innovations -> monthly means -> Proxy-SVAR.
# The monthly bridge is an unweighted average; no STL is applied to shocks.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyr)
  library(tidyquant)
  library(varexternal)
  library(vars)
})

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = '/', mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(current, 'VAR_analysis_fr')) &&
        dir.exists(file.path(current, 'supply_demand_curve'))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop('Project root not found.', call. = FALSE)
    current <- parent
  }
}

file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
start_dir <- if (length(file_arg)) dirname(sub('^--file=', '', file_arg[[1]])) else getwd()
project_root <- find_project_root(start_dir)

bridge_config <- list(
  high_frequency_file = file.path(
    project_root, 'supply_demand_curve', 'outputs', 'coef_reg_no_conditioning',
    'price_indispo_merge_esp_npc_all_final_2026.parquet'
  ),
  high_frequency_datetime = 'datetime',
  high_frequency_supply = 'fit_supply_unmatched',
  high_frequency_demand = 'fit_demand_unmatched',
  high_frequency_information = c(
    price = 'price',
    load = 'load_tot',
    generation = 'gen_tot',
    wind_forecast = 'Fore_Wind',
    solar_forecast = 'Fore_Solar',
    predicted_unavailability = 'Published.Predicted.Unavailable.Capacity',
    cross_border_flow = 'cbf_es',
    spot_gas = 'spot_gas',
    temperature = 'temperature_demand'
  ),
  ttf_symbol = 'TTF=F',
  ttf_start = as.Date('2015-01-01'),
  ttf_end = as.Date('2026-01-01'),
  ttf_refresh = FALSE,
  timezone = 'Europe/Madrid',
  minimum_intraday_observations = NULL,
  minimum_monthly_day_coverage = 0.80,
  daily_var_max_lags = 21L,
  daily_var_lag_criterion = 'AIC(n)',
  daily_annual_fourier_terms = 3L,
  daily_recursive_order = c(
    'supply_signal', 'demand_signal', 'price', 'load', 'generation',
    'wind_forecast', 'solar_forecast', 'predicted_unavailability',
    'cross_border_flow', 'spot_gas', 'temperature'
  ),
  monthly_var_file = file.path(
    project_root, 'VAR_analysis_fr', 'VAR_output',
    'IV_data_esp_log_elas_sd_2026.parquet'
  ),
  monthly_date = 'observation_date',
  var_columns = c('load_tot_log', 'spot_price_log', 'gen_tot_log',
                  'spot_gas_log', 'IPI_log', 'HICP_log', 'ffr'),
  var_lags = 12L,
  irf_horizon = 20L,
  confidence_level = 0.68,
  msw_confidence_levels = c(0.68, 0.90),
  msw_nw_lags = 0L,
  normalization_variable = c(supply = 'spot_price_log', demand = 'spot_price_log'),
  normalization_value = c(supply = -0.01, demand = 0.01),
  information_sufficiency_lags = 3L,
  lead_lag_horizon = 12L,
  output_directory = file.path(
    project_root, 'VAR_analysis_fr', 'VAR_output', 'bridge_proxy_svar'
  ),
  ttf_cache_file = file.path(
    project_root, 'VAR_analysis_fr', 'VAR_output', 'bridge_proxy_svar',
    'ttf_daily_close.parquet'
  )
)

check_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(label, ' is missing: ', paste(missing, collapse = ', '), call. = FALSE)
}

mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

assert_regular_months <- function(x) {
  index <- year(x) * 12L + month(x)
  if (length(index) > 1L && any(diff(index) != 1L)) {
    stop('The merged Bridge/VAR sample is not monthly and consecutive.', call. = FALSE)
  }
}

partial_f_test <- function(data, outcome, tested, other) {
  full <- lm(reformulate(c(tested, other), outcome), data = data)
  restricted <- lm(reformulate(other, outcome), data = data)
  comparison <- anova(restricted, full)
  data.frame(
    outcome = outcome, instrument = tested,
    partial_F = unname(comparison$F[2]),
    p_value = unname(comparison[['Pr(>F)']][2])
  )
}

information_sufficiency_test <- function(proxy, var_data, n_lags) {
  test_data <- data.frame(proxy = as.numeric(proxy))
  for (lag_number in seq_len(n_lags)) {
    for (variable in names(var_data)) {
      values <- var_data[[variable]]
      test_data[[paste0(variable, '_lag', lag_number)]] <-
        c(rep(NA_real_, lag_number), head(values, -lag_number))
    }
  }
  test_data <- test_data[complete.cases(test_data), , drop = FALSE]
  unrestricted <- lm(proxy ~ ., data = test_data)
  comparison <- anova(lm(proxy ~ 1, data = test_data), unrestricted)
  data.frame(
    F_statistic = unname(comparison$F[2]),
    p_value = unname(comparison[['Pr(>F)']][2]),
    numerator_df = unname(comparison$Df[2]),
    denominator_df = df.residual(unrestricted),
    observations = nrow(test_data)
  )
}

lead_lag_correlations <- function(residuals, proxies, max_offset) {
  sample_size <- nrow(residuals)
  output <- list()
  index <- 1L
  for (offset in seq.int(-max_offset, max_offset)) {
    if (offset >= 0L) {
      proxy_rows <- seq_len(sample_size - offset)
      residual_rows <- proxy_rows + offset
    } else {
      residual_rows <- seq_len(sample_size + offset)
      proxy_rows <- residual_rows - offset
    }
    for (proxy_name in colnames(proxies)) for (residual_name in colnames(residuals)) {
      output[[index]] <- data.frame(
        proxy = proxy_name, residual = residual_name,
        residual_offset = offset,
        correlation = cor(
          proxies[proxy_rows, proxy_name], residuals[residual_rows, residual_name],
          use = 'complete.obs'
        )
      )
      index <- index + 1L
    }
  }
  bind_rows(output)
}

moving_average_matrices <- function(var_model, horizon) {
  ar <- vars::Acoef(var_model)
  p <- length(ar)
  n <- nrow(ar[[1]])
  ma <- array(0, c(horizon + 1L, n, n))
  ma[1, , ] <- diag(n)
  if (horizon == 0L) return(ma)
  for (h in seq_len(horizon)) {
    for (lag_number in seq_len(min(p, h))) {
      ma[h + 1L, , ] <- ma[h + 1L, , ] +
        ar[[lag_number]] %*% ma[h - lag_number + 1L, , ]
    }
  }
  ma
}

identify_relative_impacts <- function(residuals, proxies, variables, values) {
  # Equation (16): relative impact responses are proportional to E[u_tau z_tau].
  gamma <- crossprod(residuals, proxies) / nrow(residuals)
  rownames(gamma) <- colnames(residuals)
  colnames(gamma) <- colnames(proxies)
  if (qr(gamma)$rank < ncol(gamma)) {
    stop('Supply and demand proxies do not jointly identify two shocks.', call. = FALSE)
  }
  impact <- gamma
  for (shock in colnames(proxies)) {
    variable <- variables[[shock]]
    denominator <- gamma[variable, shock]
    if (!is.finite(denominator) || abs(denominator) < sqrt(.Machine$double.eps)) {
      stop(shock, ' proxy cannot be normalized on ', variable, '.', call. = FALSE)
    }
    impact[, shock] <- gamma[, shock] / denominator * values[[shock]]
  }
  list(gamma = gamma, impact = impact)
}

compute_irfs <- function(var_model, impact, horizon) {
  ma <- moving_average_matrices(var_model, horizon)
  responses <- rownames(impact)
  shocks <- colnames(impact)
  irf <- array(
    NA_real_, c(horizon + 1L, length(responses), length(shocks)),
    dimnames = list(horizon = 0:horizon, response = responses, shock = shocks)
  )
  for (h in 0:horizon) irf[h + 1L, , ] <- ma[h + 1L, , ] %*% impact
  data <- expand.grid(
    horizon = 0:horizon, response = responses, shock = shocks,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  data$irf <- as.vector(irf)
  list(array = irf, data = data, ma = ma)
}

bootstrap_bridge_irfs <- function(var_data, var_model, proxy_matrix, config) {
  replications <- as.integer(config$bootstrap_replications)
  confidence_level <- config$confidence_level
  if (replications < 1L) stop('bootstrap_replications must be positive.', call. = FALSE)
  if (!is.finite(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    stop('confidence_level must lie strictly between zero and one.', call. = FALSE)
  }

  p <- config$var_lags
  sample_size <- nrow(var_data)
  residuals_original <- as.matrix(residuals(var_model))
  residuals_centered <- scale(residuals_original, center = TRUE, scale = FALSE)
  ar <- vars::Acoef(var_model)
  intercept <- vapply(var_model$varresult, function(equation) {
    unname(coef(equation)[['const']])
  }, numeric(1))
  response_names <- config$var_columns
  shock_names <- colnames(proxy_matrix)
  draws <- array(
    NA_real_,
    c(replications, config$irf_horizon + 1L,
      length(response_names), length(shock_names)),
    dimnames = list(
      replication = seq_len(replications),
      horizon = 0:config$irf_horizon,
      response = response_names,
      shock = shock_names
    )
  )

  set.seed(config$bootstrap_seed)
  successful <- 0L
  for (b in seq_len(replications)) {
    sampled_rows <- sample.int(nrow(residuals_centered), replace = TRUE)
    simulated <- matrix(NA_real_, sample_size, length(response_names),
                        dimnames = list(NULL, response_names))
    simulated[seq_len(p), ] <- as.matrix(var_data[seq_len(p), , drop = FALSE])
    for (tt in seq.int(p + 1L, sample_size)) {
      conditional_mean <- intercept
      for (lag_number in seq_len(p)) {
        conditional_mean <- conditional_mean +
          ar[[lag_number]] %*% simulated[tt - lag_number, ]
      }
      simulated[tt, ] <- conditional_mean + residuals_centered[sampled_rows[tt - p], ]
    }

    draw <- tryCatch({
      fitted_bootstrap <- vars::VAR(as.data.frame(simulated), p = p, type = 'const')
      residuals_bootstrap <- as.matrix(residuals(fitted_bootstrap))
      colnames(residuals_bootstrap) <- response_names
      proxies_bootstrap <- proxy_matrix[sampled_rows, , drop = FALSE]
      identification_bootstrap <- identify_relative_impacts(
        residuals_bootstrap, proxies_bootstrap,
        config$normalization_variable, config$normalization_value
      )
      compute_irfs(fitted_bootstrap, identification_bootstrap$impact,
                   config$irf_horizon)$array
    }, error = function(e) NULL)

    if (!is.null(draw) && all(is.finite(draw))) {
      successful <- successful + 1L
      draws[successful, , , ] <- draw
    }
  }

  if (successful < max(30L, ceiling(0.5 * replications))) {
    stop('Too few successful bootstrap replications: ', successful,
         ' out of ', replications, '.', call. = FALSE)
  }
  draws <- draws[seq_len(successful), , , , drop = FALSE]
  alpha <- 1 - confidence_level
  lower <- apply(draws, c(2, 3, 4), quantile, probs = alpha / 2,
                 na.rm = TRUE, names = FALSE)
  upper <- apply(draws, c(2, 3, 4), quantile, probs = 1 - alpha / 2,
                 na.rm = TRUE, names = FALSE)
  dimnames(lower) <- dimnames(draws)[2:4]
  dimnames(upper) <- dimnames(draws)[2:4]
  list(
    lower = lower, upper = upper, draws = draws,
    successful_replications = successful,
    requested_replications = replications,
    confidence_level = confidence_level
  )
}

extract_msw_bound <- function(svariv_result, confidence_level, bound,
                              response_names, max_horizon) {
  data <- as.data.frame(svariv_result$irfs) %>%
    mutate(
      variable = as.character(variable),
      confidence_numeric = as.numeric(as.character(confi_level)),
      horizon_zero = as.integer(horizon) - 1L
    ) %>%
    filter(
      confi_type == 'msw', type == bound,
      abs(confidence_numeric - confidence_level) < 1e-8,
      horizon_zero >= 0L, horizon_zero <= max_horizon,
      variable %in% response_names
    )
  output <- matrix(
    NA_real_, max_horizon + 1L, length(response_names),
    dimnames = list(horizon = 0:max_horizon, response = response_names)
  )
  for (row in seq_len(nrow(data))) {
    output[data$horizon_zero[row] + 1L, data$variable[row]] <- data$value[row]
  }
  if (anyNA(output)) {
    stop('SVARIV returned an incomplete ', bound, ' MSW band.', call. = FALSE)
  }
  output
}

compute_msw_bands <- function(var_data, proxy_matrix, config) {
  if (!requireNamespace('varexternal', quietly = TRUE)) {
    stop('Package varexternal is required for MSW confidence bands.', call. = FALSE)
  }
  response_names <- config$var_columns
  shock_names <- colnames(proxy_matrix)
  lower <- upper <- array(
    NA_real_,
    c(config$irf_horizon + 1L, length(response_names), length(shock_names)),
    dimnames = list(
      horizon = 0:config$irf_horizon,
      response = response_names,
      shock = shock_names
    )
  )
  raw_results <- vector('list', length(shock_names))
  names(raw_results) <- shock_names

  for (shock in shock_names) {
    normalization_index <- match(
      config$normalization_variable[[shock]], response_names
    )
    raw_results[[shock]] <- varexternal::SVARIV(
      ydata = as.matrix(var_data),
      z = as.numeric(proxy_matrix[, shock]),
      p = config$var_lags,
      confidence = config$msw_confidence_levels,
      NWlags = config$msw_nw_lags,
      norm = normalization_index,
      scale = config$normalization_value[[shock]],
      horizons = config$irf_horizon + 1L,
      ci_type = 'msw',
      print_wald = FALSE,
      instrument_name = shock
    )
    lower[, , shock] <- extract_msw_bound(
      raw_results[[shock]], config$confidence_level, 'lower',
      response_names, config$irf_horizon
    )
    upper[, , shock] <- extract_msw_bound(
      raw_results[[shock]], config$confidence_level, 'upper',
      response_names, config$irf_horizon
    )
  }
  bounded <- expand.grid(
    horizon = 0:config$irf_horizon,
    response = response_names,
    shock = shock_names,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  bounded$bounded <- as.vector(is.finite(lower) & is.finite(upper))
  list(
    lower = lower,
    upper = upper,
    raw_results = raw_results,
    bounded = bounded,
    confidence_level = config$confidence_level,
    confidence_levels = config$msw_confidence_levels,
    nw_lags = config$msw_nw_lags,
    method = 'MSW weak-IV-robust confidence sets via varexternal::SVARIV'
  )
}

load_ttf_daily_close <- function(config, calendar_dates) {
  refresh <- isTRUE(config$ttf_refresh)
  if (refresh || !file.exists(config$ttf_cache_file)) {
    downloaded <- tidyquant::tq_get(
      config$ttf_symbol,
      from = config$ttf_start,
      to = config$ttf_end
    )
    check_columns(downloaded, c('date', 'close'), 'Yahoo TTF data')
    downloaded <- downloaded %>%
      transmute(date = as.Date(date), close = as.numeric(close)) %>%
      filter(!is.na(date), is.finite(close), close > 0) %>%
      distinct(date, .keep_all = TRUE) %>%
      arrange(date)
    if (!nrow(downloaded)) stop('Yahoo returned no valid TTF close prices.', call. = FALSE)
    dir.create(dirname(config$ttf_cache_file), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(downloaded, config$ttf_cache_file)
  } else {
    downloaded <- arrow::read_parquet(config$ttf_cache_file) %>%
      transmute(date = as.Date(date), close = as.numeric(close)) %>%
      filter(!is.na(date), is.finite(close), close > 0) %>%
      arrange(date)
  }

  calendar <- data.frame(
    date = seq(min(calendar_dates), max(calendar_dates), by = 'day')
  ) %>%
    left_join(downloaded, by = 'date') %>%
    tidyr::fill(close, .direction = 'down')
  attr(calendar, 'first_market_date') <- min(downloaded$date)
  attr(calendar, 'last_market_date') <- max(downloaded$date)
  calendar
}

daily_deterministics <- function(dates, fourier_terms) {
  weekday <- factor(lubridate::wday(dates, week_start = 1L), levels = 1:7)
  output <- as.data.frame(model.matrix(~ weekday)[, -1L, drop = FALSE])
  day_index <- as.numeric(dates - min(dates))
  output$linear_trend <- as.numeric(scale(day_index))
  if (fourier_terms > 0L) {
    for (harmonic in seq_len(fourier_terms)) {
      angle <- 2 * pi * harmonic * lubridate::yday(dates) / 365.25
      output[[paste0('annual_sin_', harmonic)]] <- sin(angle)
      output[[paste0('annual_cos_', harmonic)]] <- cos(angle)
    }
  }
  as.matrix(output)
}

identify_daily_hf_shocks <- function(hourly, config) {
  system_names <- config$daily_recursive_order
  missing_system <- setdiff(system_names, names(hourly))
  if (length(missing_system)) {
    stop('Daily HF system is missing: ', paste(missing_system, collapse = ', '),
         call. = FALSE)
  }

  typical_count <- as.integer(round(median(dplyr::count(hourly, date)$n)))
  minimum_count <- config$minimum_intraday_observations
  if (is.null(minimum_count)) minimum_count <- max(1L, floor(0.75 * typical_count))
  daily <- hourly %>%
    group_by(date) %>%
    summarise(
      across(all_of(system_names), mean_or_na),
      n_supply = sum(is.finite(supply_signal)),
      n_demand = sum(is.finite(demand_signal)),
      n_intraday = n(),
      .groups = 'drop'
    ) %>%
    filter(n_intraday >= minimum_count, n_supply > 0L, n_demand > 0L) %>%
    arrange(date)

  # Stable daily transformations; every series is standardized before the HF-VAR.
  daily <- daily %>% mutate(
    spot_gas_level = spot_gas,
    price = asinh(price),
    load = log(pmax(load, 1)),
    generation = log(pmax(generation, 1)),
    wind_forecast = log1p(pmax(wind_forecast, 0)),
    solar_forecast = log1p(pmax(solar_forecast, 0)),
    predicted_unavailability = log1p(pmax(predicted_unavailability, 0)),
    cross_border_flow = asinh(cross_border_flow),
    spot_gas = log(pmax(spot_gas, .Machine$double.eps))
  ) %>% filter(if_all(all_of(system_names), is.finite))

  date_index <- year(daily$date) * 366L + yday(daily$date)
  if (nrow(daily) > 1L && any(diff(date_index) > 2L)) {
    warning('The daily HF-VAR sample contains gaps longer than one calendar day.')
  }
  if (nrow(daily) < 10L * config$daily_var_max_lags + 100L) {
    stop('Too few complete daily observations for the configured HF-VAR.', call. = FALSE)
  }

  unscaled <- as.matrix(dplyr::select(daily, all_of(system_names)))
  scaled <- scale(unscaled)
  if (any(!is.finite(scaled))) stop('A daily HF variable has zero or invalid variance.', call. = FALSE)
  scaled <- as.data.frame(scaled)
  colnames(scaled) <- system_names
  deterministics <- daily_deterministics(daily$date, config$daily_annual_fourier_terms)

  lag_selection <- vars::VARselect(
    scaled, lag.max = config$daily_var_max_lags,
    type = 'const', exogen = deterministics
  )$selection
  selected_lag <- unname(lag_selection[[config$daily_var_lag_criterion]])
  if (!length(selected_lag) || !is.finite(selected_lag)) {
    stop('Daily VAR lag criterion not found: ', config$daily_var_lag_criterion,
         call. = FALSE)
  }
  selected_lag <- max(1L, as.integer(selected_lag))
  daily_var <- vars::VAR(
    scaled, p = selected_lag, type = 'const', exogen = deterministics
  )
  reduced_residuals <- as.matrix(residuals(daily_var))
  covariance <- crossprod(reduced_residuals) / nrow(reduced_residuals)
  impact <- t(chol(covariance + diag(1e-10, nrow(covariance))))
  structural_shocks <- t(solve(impact, t(reduced_residuals)))
  colnames(structural_shocks) <- system_names
  shock_dates <- tail(daily$date, nrow(structural_shocks))

  shocks <- data.frame(
    date = shock_dates,
    supply = structural_shocks[, 'supply_signal'],
    demand = structural_shocks[, 'demand_signal'],
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      dplyr::select(daily, date, supply_signal, demand_signal,
                    spot_gas, spot_gas_level,
                    n_supply, n_demand, n_intraday),
      by = 'date'
    )

  serial_correlation <- bind_rows(lapply(c('supply', 'demand'), function(shock) {
    test <- Box.test(shocks[[shock]], lag = 30L, type = 'Ljung-Box')
    data.frame(
      shock = shock, lag = 30L,
      statistic = unname(test$statistic), p_value = test$p.value
    )
  }))
  list(
    shocks = shocks,
    inputs = daily,
    model = daily_var,
    reduced_residuals = reduced_residuals,
    impact = impact,
    lag_selection = lag_selection,
    selected_lag = selected_lag,
    serial_correlation = serial_correlation,
    shock_correlation = cor(dplyr::select(shocks, supply, demand)),
    scaling_center = attr(scale(unscaled), 'scaled:center'),
    scaling_scale = attr(scale(unscaled), 'scaled:scale'),
    typical_count = typical_count,
    minimum_count = minimum_count
  )
}

build_bridge_proxies <- function(config = bridge_config) {
  if (!file.exists(config$high_frequency_file)) {
    stop('High-frequency file not found: ', config$high_frequency_file, call. = FALSE)
  }
  hf_raw <- arrow::read_parquet(config$high_frequency_file)
  check_columns(
    hf_raw,
    c(config$high_frequency_datetime, config$high_frequency_supply,
      config$high_frequency_demand, unname(config$high_frequency_information)),
    'High-frequency proxy data'
  )
  hourly <- data.frame(
    date = as.Date(hf_raw[[config$high_frequency_datetime]], tz = config$timezone),
    supply_signal = as.numeric(hf_raw[[config$high_frequency_supply]]),
    demand_signal = as.numeric(hf_raw[[config$high_frequency_demand]])
  )
  for (new_name in names(config$high_frequency_information)) {
    source_name <- config$high_frequency_information[[new_name]]
    hourly[[new_name]] <- as.numeric(hf_raw[[source_name]])
  }
  hourly <- hourly %>% filter(!is.na(date))
  ttf_market <- load_ttf_daily_close(config, hourly$date)
  ttf_coverage <- data.frame(
    requested_start = min(hourly$date),
    requested_end = max(hourly$date),
    yahoo_start = attr(ttf_market, 'first_market_date'),
    yahoo_end = attr(ttf_market, 'last_market_date')
  )
  hourly <- hourly %>%
    left_join(
      dplyr::rename(ttf_market, ttf_close = close),
      by = 'date'
    ) %>%
    mutate(
      spot_gas_fallback = spot_gas,
      spot_gas = coalesce(ttf_close, spot_gas_fallback)
    ) %>%
    dplyr::select(-ttf_close)
  stage_one <- identify_daily_hf_shocks(hourly, config)

  monthly <- stage_one$shocks %>%
    mutate(bridge_month = floor_date(date, 'month')) %>%
    group_by(bridge_month) %>% summarise(
      n_valid_supply_days = sum(is.finite(supply)),
      n_valid_demand_days = sum(is.finite(demand)),
      supply = mean_or_na(supply), demand = mean_or_na(demand),
      spot_gas = mean_or_na(spot_gas_level),
      n_observed_days = n(), .groups = 'drop'
    ) %>% mutate(
      expected_calendar_days = days_in_month(bridge_month),
      supply_day_coverage = n_valid_supply_days / expected_calendar_days,
      demand_day_coverage = n_valid_demand_days / expected_calendar_days,
      spot_gas_log = log(spot_gas)
    ) %>% filter(
      supply_day_coverage >= config$minimum_monthly_day_coverage,
      demand_day_coverage >= config$minimum_monthly_day_coverage
    ) %>% arrange(bridge_month)
  if (!nrow(monthly)) stop('No monthly Bridge proxy passes the coverage rule.', call. = FALSE)
  list(
    daily = stage_one$shocks,
    monthly = monthly,
    stage_one = stage_one,
    ttf_coverage = ttf_coverage,
    typical_count = stage_one$typical_count,
    minimum_count = stage_one$minimum_count
  )
}

run_bridge_proxy_svar <- function(config = bridge_config, write_outputs = FALSE) {
  bridge <- build_bridge_proxies(config)
  if (!file.exists(config$monthly_var_file)) {
    stop('Monthly VAR file not found: ', config$monthly_var_file, call. = FALSE)
  }
  monthly_var <- arrow::read_parquet(config$monthly_var_file)
  monthly_non_bridge_columns <- setdiff(config$var_columns, 'spot_gas_log')
  check_columns(monthly_var, c(config$monthly_date, monthly_non_bridge_columns),
                'Monthly VAR data')
  monthly_var <- monthly_var %>% transmute(
    bridge_month = floor_date(as.Date(.data[[config$monthly_date]]), 'month'),
    across(all_of(monthly_non_bridge_columns), as.numeric)
  )
  if (anyDuplicated(monthly_var$bridge_month)) stop('Duplicate months in VAR data.', call. = FALSE)

  estimation_data <- monthly_var %>%
    inner_join(
      dplyr::select(bridge$monthly, bridge_month, supply, demand, spot_gas_log),
      by = 'bridge_month'
    ) %>%
    filter(if_all(all_of(c(config$var_columns, 'supply', 'demand')), is.finite)) %>%
    arrange(bridge_month)
  assert_regular_months(estimation_data$bridge_month)

  minimum_size <- length(config$var_columns) * config$var_lags + config$var_lags + 10L
  if (nrow(estimation_data) < minimum_size) {
    stop('Too few complete months: ', nrow(estimation_data),
         '; safety minimum: ', minimum_size, '.', call. = FALSE)
  }

  var_data <- as.data.frame(dplyr::select(estimation_data, all_of(config$var_columns)))
  var_model <- vars::VAR(var_data, p = config$var_lags, type = 'const')
  reduced_form_residuals <- as.matrix(residuals(var_model))
  colnames(reduced_form_residuals) <- config$var_columns
  #head(var_data)
  #dim(var_data)
  proxy_matrix_full <- estimation_data %>%
    dplyr::select(supply, demand) %>%
    as.matrix()
  colnames(proxy_matrix_full) <- c('supply', 'demand')
  proxy_matrix <- estimation_data %>%
    dplyr::select(supply, demand) %>%
    slice((config$var_lags + 1L):n()) %>% as.matrix()
  colnames(proxy_matrix) <- c('supply', 'demand')
  if (nrow(proxy_matrix) != nrow(reduced_form_residuals)) {
    stop('Proxy and VAR-residual samples are misaligned.', call. = FALSE)
  }

  identification <- identify_relative_impacts(
    reduced_form_residuals, proxy_matrix,
    config$normalization_variable, config$normalization_value
  )
  irfs <- compute_irfs(var_model, identification$impact, config$irf_horizon)
  msw <- compute_msw_bands(var_data, proxy_matrix_full, config)
  irfs$data$lower <- as.vector(msw$lower)
  irfs$data$upper <- as.vector(msw$upper)

  first_stage_data <- data.frame(
    reduced_form_residuals,
    supply = proxy_matrix[, 'supply'], demand = proxy_matrix[, 'demand'],
    check.names = FALSE
  )
  first_stage <- bind_rows(
    partial_f_test(first_stage_data, config$normalization_variable[['supply']],
                   'supply', 'demand'),
    partial_f_test(first_stage_data, config$normalization_variable[['demand']],
                   'demand', 'supply')
  )
  first_stage_all <- bind_rows(lapply(config$var_columns, function(outcome) {
    bind_rows(
      partial_f_test(first_stage_data, outcome, 'supply', 'demand'),
      partial_f_test(first_stage_data, outcome, 'demand', 'supply')
    )
  }))

  sufficiency_supply <- information_sufficiency_test(
    estimation_data$supply, var_data, config$information_sufficiency_lags
  ) %>% mutate(proxy = 'supply', .before = 1L)
  sufficiency_demand <- information_sufficiency_test(
    estimation_data$demand, var_data, config$information_sufficiency_lags
  ) %>% mutate(proxy = 'demand', .before = 1L)

  diagnostics <- list(
    sample = data.frame(
      start = min(estimation_data$bridge_month),
      end = max(estimation_data$bridge_month),
      monthly_observations = nrow(estimation_data),
      residual_observations = nrow(reduced_form_residuals),
      var_lags = config$var_lags,
      typical_intraday_count = bridge$typical_count,
      minimum_intraday_count = bridge$minimum_count
    ),
    daily_hf_var = data.frame(
      daily_start = min(bridge$stage_one$inputs$date),
      daily_end = max(bridge$stage_one$inputs$date),
      daily_observations = nrow(bridge$stage_one$inputs),
      selected_lag = bridge$stage_one$selected_lag,
      lag_criterion = config$daily_var_lag_criterion,
      identification = 'recursive: supply, demand, then information variables'
    ),
    ttf_coverage = bridge$ttf_coverage,
    daily_hf_lag_selection = data.frame(
      criterion = names(bridge$stage_one$lag_selection),
      selected_lag = as.integer(bridge$stage_one$lag_selection)
    ),
    daily_shock_correlation = bridge$stage_one$shock_correlation,
    daily_shock_serial_correlation = bridge$stage_one$serial_correlation,
    proxy_correlation = cor(proxy_matrix, use = 'complete.obs'),
    gamma_rank = qr(identification$gamma)$rank,
    first_stage = first_stage,
    first_stage_all_residuals = first_stage_all,
    information_sufficiency = bind_rows(sufficiency_supply, sufficiency_demand),
    lead_lag_correlations = lead_lag_correlations(
      reduced_form_residuals, proxy_matrix, config$lead_lag_horizon
    )
  )

  result <- list(
    method = 'Gazzani-Vicondoa Bridge Proxy-SVAR',
    config = config,
    daily_proxies = bridge$daily,
    monthly_proxies = bridge$monthly,
    daily_hf_inputs = bridge$stage_one$inputs,
    daily_hf_var_model = bridge$stage_one$model,
    daily_hf_reduced_residuals = bridge$stage_one$reduced_residuals,
    daily_hf_impact = bridge$stage_one$impact,
    estimation_data = estimation_data,
    var_model = var_model,
    reduced_form_residuals = reduced_form_residuals,
    gamma = identification$gamma,
    impact = identification$impact,
    irf = irfs$array,
    irf_lower = msw$lower,
    irf_upper = msw$upper,
    msw = msw[c('confidence_level', 'confidence_levels', 'nw_lags',
                'method', 'bounded')],
    svariv_msw_results = msw$raw_results,
    ma_matrices = irfs$ma,
    diagnostics = diagnostics
  )

  if (write_outputs) {
    dir.create(config$output_directory, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(
      bridge$daily,
      file.path(config$output_directory, 'daily_supply_demand_proxies.parquet')
    )
    arrow::write_parquet(
      bridge$monthly,
      file.path(config$output_directory, 'monthly_bridge_proxies.parquet')
    )
    write.csv(irfs$data,
              file.path(config$output_directory, 'bridge_proxy_svar_irfs.csv'),
              row.names = FALSE)
    write.csv(
      bind_rows(lapply(msw$raw_results, function(x) as.data.frame(x$irfs))),
      file.path(config$output_directory, 'bridge_proxy_svar_msw_irfs.csv'),
      row.names = FALSE
    )
    write.csv(
      msw$bounded,
      file.path(config$output_directory, 'bridge_proxy_svar_msw_bounded.csv'),
      row.names = FALSE
    )
    write.csv(first_stage,
              file.path(config$output_directory, 'bridge_proxy_first_stage.csv'),
              row.names = FALSE)
    write.csv(first_stage_all,
              file.path(config$output_directory, 'bridge_proxy_first_stage_all_residuals.csv'),
              row.names = FALSE)
    write.csv(diagnostics$information_sufficiency,
              file.path(config$output_directory, 'bridge_proxy_information_sufficiency.csv'),
              row.names = FALSE)
    write.csv(diagnostics$lead_lag_correlations,
              file.path(config$output_directory, 'bridge_proxy_lead_lag_correlations.csv'),
              row.names = FALSE)
    write.csv(diagnostics$daily_hf_var,
              file.path(config$output_directory, 'daily_hf_var_specification.csv'),
              row.names = FALSE)
    write.csv(diagnostics$daily_hf_lag_selection,
              file.path(config$output_directory, 'daily_hf_var_lag_selection.csv'),
              row.names = FALSE)
    write.csv(diagnostics$daily_shock_serial_correlation,
              file.path(config$output_directory, 'daily_hf_shock_serial_correlation.csv'),
              row.names = FALSE)
    write.csv(diagnostics$ttf_coverage,
              file.path(config$output_directory, 'ttf_close_coverage.csv'),
              row.names = FALSE)
    saveRDS(result, file.path(config$output_directory, 'bridge_proxy_svar_results.rds'))
  }

  message('Bridge Proxy-SVAR estimated from ', min(estimation_data$bridge_month),
          ' to ', max(estimation_data$bridge_month), '.')
  message('Stage I daily HF-VAR lag selected by ', config$daily_var_lag_criterion,
          ': ', bridge$stage_one$selected_lag, '.')
  message('Correlation of identified daily supply and demand shocks: ',
          round(bridge$stage_one$shock_correlation[1, 2], 6L), '.')
  message('Impact matrix:')
  print(round(identification$impact, 5L))
  message(round(100 * config$confidence_level),
          '% MSW weak-IV-robust confidence sets computed with SVARIV.')
  print(msw$bounded %>%
          group_by(shock) %>%
          summarise(bounded = sum(bounded), total = n(), .groups = 'drop'))
  message('Partial first-stage F statistics:')
  print(first_stage)
  message('Information-sufficiency tests (null: no predictability):')
  print(diagnostics$information_sufficiency)
  if (any(first_stage$partial_F < 10, na.rm = TRUE)) {
    warning('At least one normalized first stage has F < 10; treat the point IRFs as weak-proxy estimates.')
  }
  if (any(diagnostics$information_sufficiency$p_value < 0.05, na.rm = TRUE)) {
    warning('At least one Bridge proxy is predictable from lagged VAR variables; information sufficiency is rejected at 5%.')
  }
  invisible(result)
}

# Direct execution:
# Rscript VAR_analysis_fr/supply_demand/brridge_proxy_svar.R
# From RStudio:
# bridge_results <- run_bridge_proxy_svar()
direct_call <- sub('^--file=', '', commandArgs(FALSE))
if (FALSE && any(grepl('[/\\\\]brridge_proxy_svar\\.R$', direct_call, ignore.case = TRUE))) {
  bridge_results <- run_bridge_proxy_svar()
}
names(bridge_results)
# Superseded point-only plotting example retained for reference.
if (FALSE) {
library(dplyr)
library(tidyr)
library(ggplot2)

plot_irf <- function(irf_matrix, shock_name, color = "steelblue") {

  plot_data <- as.data.frame(irf_matrix)

  # Les noms des variables sont généralement dans les colonnes
  if (is.null(colnames(irf_matrix))) {
    colnames(plot_data) <- paste0("Variable_", seq_len(ncol(plot_data)))
  }

  plot_data$horizon <- seq_len(nrow(plot_data)) - 1

  plot_data <- plot_data %>%
    pivot_longer(
      cols = -horizon,
      names_to = "response",
      values_to = "irf"
    )

  ggplot(plot_data, aes(x = horizon, y = irf)) +
    geom_hline(
      yintercept = 0,
      color = "grey60",
      linewidth = 0.4
    ) +
    geom_line(
      color = color,
      linewidth = 0.8
    ) +
    facet_wrap(
      ~ response,
      scales = "free_y",
      ncol = 2
    ) +
    labs(
      title = paste("IRFs —", tools::toTitleCase(shock_name), "shock"),
      x = "Months",
      y = "Impulse response"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Vérifie d'abord l'ordre des chocs
dimnames(irfs$array)[[3]]

# Deux graphiques séparés
p_supply <- plot_irf(
  irfs$array[, , 1],
  "supply",
  "steelblue"
)

p_demand <- plot_irf(
  irfs$array[, , 2],
  "demand",
  "firebrick"
)

p_supply
p_demand

}

plot_irf_with_bands <- function(irf_matrix, lower_matrix, upper_matrix,
                                shock_name, color = 'steelblue',
                                confidence_level = 0.68) {
  if (is.null(irf_matrix) || is.null(lower_matrix) || is.null(upper_matrix)) {
    stop('IRF bands are missing. Re-estimate bridge_results with the updated run_bridge_proxy_svar().',
         call. = FALSE)
  }
  if (!identical(dim(irf_matrix), dim(lower_matrix)) ||
      !identical(dim(irf_matrix), dim(upper_matrix))) {
    stop('Point IRFs and confidence bands do not have matching dimensions.', call. = FALSE)
  }
  make_long <- function(x, value_name) {
    x <- as.matrix(x)
    response_names <- colnames(x)
    if (is.null(response_names)) response_names <- paste0('Variable_', seq_len(ncol(x)))
    data <- data.frame(
      horizon = rep(seq_len(nrow(x)) - 1L, times = ncol(x)),
      response = rep(response_names, each = nrow(x)),
      stringsAsFactors = FALSE
    )
    data[[value_name]] <- as.vector(x)
    data
  }
  plot_data <- make_long(irf_matrix, 'irf') %>%
    left_join(make_long(lower_matrix, 'lower'), by = c('horizon', 'response')) %>%
    left_join(make_long(upper_matrix, 'upper'), by = c('horizon', 'response')) %>%
    mutate(
      unbounded = !is.finite(lower) | !is.finite(upper),
      lower = ifelse(unbounded, NA_real_, lower),
      upper = ifelse(unbounded, NA_real_, upper)
    )
  unbounded_count <- sum(plot_data$unbounded)
  caption <- if (unbounded_count > 0L) {
    paste(unbounded_count, 'unbounded MSW sets are not shaded')
  } else {
    NULL
  }

  ggplot(plot_data, aes(x = horizon, y = irf)) +
    geom_hline(yintercept = 0, color = 'grey60', linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = color,
                alpha = 0.20, linewidth = 0) +
    geom_line(color = color, linewidth = 0.8) +
    facet_wrap(~ response, scales = 'free_y', ncol = 2) +
    labs(
      title = paste('IRFs -', tools::toTitleCase(shock_name), 'shock'),
      subtitle = paste0(round(100 * confidence_level), '% MSW weak-IV-robust bands'),
      caption = caption,
      x = 'Months', y = 'Impulse response'
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = 'bold'),
          panel.grid.minor = element_blank())
}

direct_call <- sub('^--file=', '', commandArgs(FALSE))
if (!length(commandArgs(TRUE)) &&
    any(grepl('[/\\\\]bridge_proxy_svar\\.R$', direct_call, ignore.case = TRUE))) {
  bridge_results <- run_bridge_proxy_svar(write_outputs = TRUE)
  p_supply <- plot_irf_with_bands(
    bridge_results$irf[, , 'supply'],
    bridge_results$irf_lower[, , 'supply'],
    bridge_results$irf_upper[, , 'supply'],
    'supply', 'steelblue', bridge_results$msw$confidence_level
  )
  p_demand <- plot_irf_with_bands(
    bridge_results$irf[, , 'demand'],
    bridge_results$irf_lower[, , 'demand'],
    bridge_results$irf_upper[, , 'demand'],
    'demand', 'firebrick', bridge_results$msw$confidence_level
  )
  # print(p_supply)
  # print(p_demand)
  # ggsave(file.path(bridge_config$output_directory, 'bridge_irf_supply.png'),
  #        p_supply, width = 7, height = 6, dpi = 300)
  # ggsave(file.path(bridge_config$output_directory, 'bridge_irf_demand.png'),
  #        p_demand, width = 7, height = 6, dpi = 300)
}

# IRF matrices: horizons x variables
irf_supply <- bridge_results$irf[, , "supply"]
irf_demand <- bridge_results$irf[, , "demand"]

# MSW confidence bands à 68 %
lower_supply <- bridge_results$irf_lower[, , "supply"]
upper_supply <- bridge_results$irf_upper[, , "supply"]

lower_demand <- bridge_results$irf_lower[, , "demand"]
upper_demand <- bridge_results$irf_upper[, , "demand"]

# Graphiques
p_supply <- plot_irf_with_bands(
  irf_matrix       = irf_supply,
  lower_matrix     = lower_supply,
  upper_matrix     = upper_supply,
  shock_name       = "supply",
  color            = "steelblue",
  confidence_level = bridge_results$msw$confidence_level
)

p_demand <- plot_irf_with_bands(
  irf_matrix       = irf_demand,
  lower_matrix     = lower_demand,
  upper_matrix     = upper_demand,
  shock_name       = "demand",
  color            = "firebrick",
  confidence_level = bridge_results$msw$confidence_level
)

print(p_supply)
print(p_demand)

# Facultatif : sauvegarde
# ggsave("bridge_irf_supply.png", p_supply, width = 7, height = 6, dpi = 300)
# ggsave("bridge_irf_demand.png", p_demand, width = 7, height = 6, dpi = 300)
