read_country_data <- function(path, country) {
  if (!file.exists(path)) stop("Input data not found: ", path)
  data <- as.data.frame(arrow::read_parquet(path))
  data$observation_date <- as.Date(data$observation_date)
  data <- data[order(data$observation_date), , drop = FALSE]

  generation <- pick_column(
    data,
    if (country == "Spain") c("gen_tot_log", "gen_forecast_log")
    else c("gen_forecast_log", "gen_tot_log"),
    "generation series", country
  )
  wind <- pick_column(
    data,
    if (country == "Spain") c("wind_forecast_log", "Fore_Wind_log")
    else c("Fore_Wind_log", "wind_forecast_log"),
    "wind proxy", country
  )

  variables <- c(
    "spot_price_log", "load_tot_log", generation,
    "IPI_log", "HICP_log", "ffr", "spot_gas_log"
  )
  assert_columns(data, c("observation_date", variables, wind), paste(country, "data"))

  if (country == "Spain") {
    assert_columns(data, c("delta_s_tot", "delta_d_tot"), "Spanish proxies")
    # The micro instruments start in 2015 while the VAR starts in 2010: the
    # PCA uses observed months only and the proxies stay NA before.
    observed <- is.finite(data$delta_s_tot) & is.finite(data$delta_d_tot)
    proxy_matrix <- scale(
      cbind(
        supply = data$delta_s_tot[observed],
        demand = data$delta_d_tot[observed]
      ),
      center = TRUE,
      scale = FALSE
    )
    pca <- stats::prcomp(proxy_matrix)
    data$proxy_supply_pc <- NA_real_
    data$proxy_demand_pc <- NA_real_
    data$proxy_supply_pc[observed] <- pca$x[, 1]
    data$proxy_demand_pc[observed] <- pca$x[, 2]

    if (stats::cor(data$proxy_supply_pc, data$delta_s_tot, use = "complete.obs") < 0) {
      data$proxy_supply_pc <- -data$proxy_supply_pc
    }
    if (stats::cor(data$proxy_demand_pc, data$delta_d_tot, use = "complete.obs") < 0) {
      data$proxy_demand_pc <- -data$proxy_demand_pc
    }

    assert_columns(data, c("spot_gas", "spot_gas_capped"), "Iberian wedge")
    raw_wedge <- data$spot_gas - data$spot_gas_capped
    data$wedge_iber_exception <- ifelse(
      is.finite(raw_wedge) & raw_wedge > 10,
      raw_wedge,
      0
    )
    supply_proxy <- "proxy_supply_pc"
    demand_proxy <- "proxy_demand_pc"
  } else {
    temperature <- pick_column(
      data,
      c("temperature_discomfort", "temperature_discomfort_log"),
      "temperature-demand proxy", country
    )
    supply_proxy <- wind
    demand_proxy <- temperature
  }

  list(
    country = country,
    data = data,
    variables = variables,
    generation = generation,
    wind_proxy = wind,
    supply_proxy = supply_proxy,
    demand_proxy = demand_proxy
  )
}

construct_kilian_monthly_proxies <- function(path) {
  if (!file.exists(path)) stop("High-frequency proxy data not found: ", path)
  high_frequency <- as.data.frame(arrow::read_parquet(
    path,
    col_select = c(
      "datetime", "load_tot",
      "fit_supply_unmatched", "fit_demand_unmatched"
    )
  ))
  assert_columns(
    high_frequency,
    c(
      "datetime", "load_tot",
      "fit_supply_unmatched", "fit_demand_unmatched"
    ),
    "High-frequency Spanish proxies"
  )
  high_frequency <- high_frequency[order(high_frequency$datetime), , drop = FALSE]
  if (anyDuplicated(high_frequency$datetime)) {
    # The source has a small number of exact replicated boundary-hour rows.
    # Collapse them before assigning timing weights so they are not counted as
    # distinct surprises.
    finite_mean <- function(x) {
      values <- x[is.finite(x)]
      if (length(values)) mean(values) else NA_real_
    }
    high_frequency <- high_frequency |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(
        dplyr::across(
          c(
            load_tot, fit_supply_unmatched,
            fit_demand_unmatched
          ),
          finite_mean
        ),
        .groups = "drop"
      ) |>
      as.data.frame()
  }
  if (any(!is.finite(high_frequency$load_tot)) ||
      any(high_frequency$load_tot <= 0)) {
    stop("Kilian aggregation requires finite, strictly positive hourly load.")
  }

  # Kilian (2024) assigns a surprise to the current and following monthly
  # averages according to when it occurs. The original equal-day weights are
  # generalized here to the load weights used for the Spanish monthly price:
  # remaining monthly load share today, elapsed load share next month.
  timezone <- attr(high_frequency$datetime, "tzone")
  if (is.null(timezone) || !nzchar(timezone)) timezone <- "Europe/Madrid"
  high_frequency$observation_date <- as.Date(format(
    high_frequency$datetime,
    "%Y-%m-01",
    tz = timezone
  ))
  monthly_rows <- split(
    seq_len(nrow(high_frequency)),
    high_frequency$observation_date
  )
  components <- lapply(monthly_rows, function(rows) {
    frame <- high_frequency[rows, , drop = FALSE]
    load_share <- frame$load_tot / sum(frame$load_tot)
    current_weight <- rev(cumsum(rev(load_share)))
    next_weight <- 1 - current_weight
    supply <- frame$fit_supply_unmatched
    demand <- frame$fit_demand_unmatched
    supply[!is.finite(supply)] <- 0
    demand[!is.finite(demand)] <- 0
    data.frame(
      observation_date = frame$observation_date[[1]],
      hourly_observations = nrow(frame),
      supply_current = sum(supply * current_weight),
      supply_next = sum(supply * next_weight),
      demand_current = sum(demand * current_weight),
      demand_next = sum(demand * next_weight)
    )
  })
  monthly <- do.call(rbind, components)
  rownames(monthly) <- NULL
  monthly <- monthly[order(monthly$observation_date), , drop = FALSE]
  month_index <- as.integer(format(monthly$observation_date, "%Y")) * 12L +
    as.integer(format(monthly$observation_date, "%m"))
  if (length(month_index) > 1L && any(diff(month_index) != 1L)) {
    stop("High-frequency proxy data do not cover consecutive months.")
  }
  monthly$supply_carry_in <- c(0, head(monthly$supply_next, -1L))
  monthly$demand_carry_in <- c(0, head(monthly$demand_next, -1L))
  monthly$delta_s_tot_kilian_raw <-
    monthly$supply_current + monthly$supply_carry_in
  monthly$delta_d_tot_kilian_raw <-
    monthly$demand_current + monthly$demand_carry_in

  monthly
}

attach_kilian_proxies <- function(country_data, path) {
  stopifnot(country_data$country == "Spain")
  monthly <- construct_kilian_monthly_proxies(path)
  matched <- match(country_data$data$observation_date, monthly$observation_date)
  # Months before the high-frequency data (the VAR starts earlier) stay NA.
  observed <- !is.na(matched)
  if (!any(observed) ||
      any(country_data$data$observation_date[!observed] >
            min(monthly$observation_date))) {
    stop("Kilian proxies have gaps inside the Spanish model sample.")
  }
  raw_columns <- c(
    "delta_s_tot_kilian_raw",
    "delta_d_tot_kilian_raw"
  )
  country_data$data[, raw_columns] <- NA_real_
  country_data$data[observed, raw_columns] <-
    monthly[matched[observed], raw_columns, drop = FALSE]
  calendar_month <- as.integer(format(
    country_data$data$observation_date,
    "%m"
  ))
  monthly_mean <- function(x) ave(x, calendar_month, FUN = function(v) mean(v, na.rm = TRUE))
  country_data$data$delta_s_tot_kilian <-
    country_data$data$delta_s_tot_kilian_raw -
    monthly_mean(country_data$data$delta_s_tot_kilian_raw)
  country_data$data$delta_d_tot_kilian <-
    country_data$data$delta_d_tot_kilian_raw -
    monthly_mean(country_data$data$delta_d_tot_kilian_raw)
  columns <- c("delta_s_tot_kilian", "delta_d_tot_kilian")
  if (any(!is.finite(as.matrix(country_data$data[observed, columns, drop = FALSE])))) {
    stop("The matched Kilian proxies contain non-finite values.")
  }

  proxy_matrix <- scale(
    cbind(
      supply = country_data$data$delta_s_tot_kilian[observed],
      demand = country_data$data$delta_d_tot_kilian[observed]
    ),
    center = TRUE,
    scale = FALSE
  )
  pca <- stats::prcomp(proxy_matrix)
  country_data$data$proxy_supply_kilian_pc <- NA_real_
  country_data$data$proxy_demand_kilian_pc <- NA_real_
  country_data$data$proxy_supply_kilian_pc[observed] <- pca$x[, 1]
  country_data$data$proxy_demand_kilian_pc[observed] <- pca$x[, 2]
  if (stats::cor(
      country_data$data$proxy_supply_kilian_pc,
      country_data$data$delta_s_tot_kilian,
      use = "complete.obs"
    ) < 0) {
    country_data$data$proxy_supply_kilian_pc <-
      -country_data$data$proxy_supply_kilian_pc
  }
  if (stats::cor(
      country_data$data$proxy_demand_kilian_pc,
      country_data$data$delta_d_tot_kilian,
      use = "complete.obs"
    ) < 0) {
    country_data$data$proxy_demand_kilian_pc <-
      -country_data$data$proxy_demand_kilian_pc
  }
  country_data$supply_proxy_kilian <- "proxy_supply_kilian_pc"
  country_data$demand_proxy_kilian <- "proxy_demand_kilian_pc"
  country_data$kilian_proxy_components <- monthly
  country_data
}
construct_daily_mixed_frequency_proxies <- function(path) {
  if (!file.exists(path)) stop("High-frequency proxy data not found: ", path)
  high_frequency <- as.data.frame(arrow::read_parquet(
    path,
    col_select = c(
      "datetime", "load_tot",
      "fit_supply_unmatched", "fit_demand_unmatched"
    )
  ))
  high_frequency <- high_frequency[order(high_frequency$datetime), , drop = FALSE]
  if (anyDuplicated(high_frequency$datetime)) {
    finite_mean <- function(x) {
      values <- x[is.finite(x)]
      if (length(values)) mean(values) else NA_real_
    }
    high_frequency <- high_frequency |>
      dplyr::group_by(datetime) |>
      dplyr::summarise(
        dplyr::across(
          c(
            load_tot, fit_supply_unmatched,
            fit_demand_unmatched
          ),
          finite_mean
        ),
        .groups = "drop"
      ) |>
      as.data.frame()
  }
  if (any(!is.finite(high_frequency$load_tot)) ||
      any(high_frequency$load_tot <= 0)) {
    stop("Daily mixed-frequency proxies require finite, positive hourly load.")
  }
  timezone <- attr(high_frequency$datetime, "tzone")
  if (is.null(timezone) || !nzchar(timezone)) timezone <- "Europe/Madrid"
  high_frequency$calendar_date <- as.Date(format(
    high_frequency$datetime,
    "%Y-%m-%d",
    tz = timezone
  ))
  high_frequency$observation_date <- as.Date(format(
    high_frequency$datetime,
    "%Y-%m-01",
    tz = timezone
  ))
  daily_rows <- split(
    seq_len(nrow(high_frequency)),
    high_frequency$calendar_date
  )
  daily <- lapply(daily_rows, function(rows) {
    frame <- high_frequency[rows, , drop = FALSE]
    weights <- frame$load_tot
    supply <- frame$fit_supply_unmatched
    demand <- frame$fit_demand_unmatched
    supply[!is.finite(supply)] <- 0
    demand[!is.finite(demand)] <- 0
    data.frame(
      calendar_date = frame$calendar_date[[1]],
      observation_date = frame$observation_date[[1]],
      day_of_month = as.integer(format(frame$calendar_date[[1]], "%d")),
      hourly_observations = nrow(frame),
      supply_proxy_raw = sum(supply * weights) / sum(weights),
      demand_proxy_raw = sum(demand * weights) / sum(weights)
    )
  })
  output <- do.call(rbind, daily)
  rownames(output) <- NULL
  output[order(output$calendar_date), , drop = FALSE]
}

attach_daily_mixed_frequency_proxies <- function(country_data, path) {
  stopifnot(country_data$country == "Spain")
  daily <- construct_daily_mixed_frequency_proxies(path)
  daily <- daily[
    daily$observation_date %in% country_data$data$observation_date,
    ,
    drop = FALSE
  ]
  covered_months <- sort(unique(daily$observation_date))
  # Months before the high-frequency data (the VAR starts earlier) stay NA.
  missing_months <- setdiff(country_data$data$observation_date, covered_months)
  missing_months <- missing_months[missing_months > min(covered_months)]
  if (length(missing_months)) {
    stop(
      "Daily mixed-frequency proxies do not cover the Spanish sample: ",
      paste(format(head(missing_months, 6L), "%Y-%m"), collapse = ", ")
    )
  }

  # Remove deterministic seasonality without aggregating the daily signal.
  # Each calendar-day position is demeaned within its calendar month across
  # years, then a common PCA rotation separates supply and demand signals.
  calendar_month <- as.integer(format(daily$observation_date, "%m"))
  seasonal_cell <- interaction(
    calendar_month,
    daily$day_of_month,
    drop = TRUE
  )
  daily$supply_proxy_adjusted <- daily$supply_proxy_raw -
    ave(daily$supply_proxy_raw, seasonal_cell, FUN = mean)
  daily$demand_proxy_adjusted <- daily$demand_proxy_raw -
    ave(daily$demand_proxy_raw, seasonal_cell, FUN = mean)
  pca_input <- cbind(
    supply = daily$supply_proxy_adjusted,
    demand = daily$demand_proxy_adjusted
  )
  pca <- stats::prcomp(pca_input, center = TRUE, scale. = FALSE)
  daily$proxy_supply_mf_daily <- pca$x[, 1]
  daily$proxy_demand_mf_daily <- pca$x[, 2]
  if (stats::cor(
      daily$proxy_supply_mf_daily,
      daily$supply_proxy_adjusted
    ) < 0) {
    daily$proxy_supply_mf_daily <- -daily$proxy_supply_mf_daily
  }
  if (stats::cor(
      daily$proxy_demand_mf_daily,
      daily$demand_proxy_adjusted
    ) < 0) {
    daily$proxy_demand_mf_daily <- -daily$proxy_demand_mf_daily
  }

  maximum_days <- 31L
  supply_columns <- sprintf("proxy_supply_mf_d%02d", seq_len(maximum_days))
  demand_columns <- sprintf("proxy_demand_mf_d%02d", seq_len(maximum_days))
  for (day in seq_len(maximum_days)) {
    supply_values <- rep(NA_real_, nrow(country_data$data))
    demand_values <- rep(NA_real_, nrow(country_data$data))
    selected <- daily$day_of_month == day
    month_match <- match(
      daily$observation_date[selected],
      country_data$data$observation_date
    )
    supply_values[month_match] <- daily$proxy_supply_mf_daily[selected]
    demand_values[month_match] <- daily$proxy_demand_mf_daily[selected]
    country_data$data[[supply_columns[[day]]]] <- supply_values
    country_data$data[[demand_columns[[day]]]] <- demand_values
  }
  country_data$supply_proxies_mixed_frequency <- supply_columns
  country_data$demand_proxies_mixed_frequency <- demand_columns
  country_data$mixed_frequency_daily_proxies <- daily
  country_data$mixed_frequency_frequency <- "daily"
  country_data
}
attach_counterfactual_proxy <- function(country_data, path) {
  if (!file.exists(path)) stop("Counterfactual proxy data not found: ", path)
  proxy_data <- as.data.frame(arrow::read_parquet(path))
  proxy_data$observation_date <- as.Date(proxy_data$observation_date)
  assert_columns(
    proxy_data,
    c("observation_date", "delta_supply_incorpo"),
    "Historical counterfactual proxy"
  )
  matched <- match(
    country_data$data$observation_date,
    proxy_data$observation_date
  )
  observed <- !is.na(matched)
  if (!any(observed) ||
      any(country_data$data$observation_date[!observed] >
            min(proxy_data$observation_date))) {
    stop("Counterfactual proxy dates have gaps inside the Spanish model sample.")
  }
  country_data$data$delta_supply_incorpo <-
    proxy_data$delta_supply_incorpo[matched]
  if (any(!is.finite(country_data$data$delta_supply_incorpo[observed]))) {
    stop("The historical counterfactual proxy contains non-finite values.")
  }
  country_data$counterfactual_proxy <- "delta_supply_incorpo"
  country_data
}

attach_gas_supply_proxy <- function(country_data, path, sheet = "2025update") {
  if (!file.exists(path)) stop("Gas-supply-shock workbook not found: ", path)
  instrument <- as.data.frame(readxl::read_excel(path, sheet = sheet))
  assert_columns(instrument, c("Time", "Shocks"), "Gazzani gas-supply instrument")
  instrument <- instrument[, c("Time", "Shocks"), drop = FALSE]
  names(instrument) <- c("observation_date", "gas_supply_gazzani")
  instrument$observation_date <- as.Date(instrument$observation_date)
  instrument$gas_supply_gazzani <- as.numeric(instrument$gas_supply_gazzani)
  if (anyDuplicated(instrument$observation_date)) {
    stop("The Gazzani gas-supply instrument contains duplicate months.")
  }
  matched <- match(
    as.Date(country_data$data$observation_date), instrument$observation_date
  )
  if (anyNA(matched)) {
    missing_dates <- country_data$data$observation_date[is.na(matched)]
    stop(
      "The Gazzani gas-supply instrument does not cover the model sample: ",
      paste(format(head(missing_dates, 6L), "%Y-%m"), collapse = ", ")
    )
  }
  proxy <- instrument$gas_supply_gazzani[matched]
  if (any(!is.finite(proxy))) {
    stop("The matched Gazzani gas-supply instrument contains non-finite values.")
  }
  country_data$data$gas_supply_gazzani <- proxy
  country_data$gas_supply_proxy <- "gas_supply_gazzani"
  country_data$gas_supply_proxy_source <- paste0(basename(path), " / ", sheet)
  country_data
}
load_inputs <- function(cfg) {
  spain <- read_country_data(cfg$data$spain, "Spain")
  spain <- attach_kilian_proxies(
    spain,
    cfg$data$spain_high_frequency_proxies
  )
  spain <- attach_daily_mixed_frequency_proxies(
    spain,
    cfg$data$spain_high_frequency_proxies
  )
  spain <- attach_counterfactual_proxy(
    spain,
    cfg$data$spain_counterfactual_proxy
  )
  spain <- attach_gas_supply_proxy(
    spain,
    cfg$data$gas_supply_shocks,
    sheet = "2025update"
  )
  list(
    Spain = spain,
    France = read_country_data(cfg$data$france, "France")
  )
}

read_micro_demand_elasticity <- function(cfg) {
  path <- cfg$data$micro_elasticity
  if (!file.exists(path)) stop("Micro-elasticity file not found: ", path)
  estimates <- utils::read.csv(path, check.names = FALSE)
  if ("Lag" %in% names(estimates)) {
    # supply_demand_curve/estimate_elasticity/estimate_elas_standard_IV.R: hourly IV of log load on log price
    # instrumented by the day-ahead wind forecast (wind as a pure supply shifter, row "Supply", no lag), which
    # identifies the slope of the demand curve.
    assert_columns(estimates, c("Model", "Coef", "Lag"), "Micro-elasticity table")
    row <- estimates[estimates$Model == "Supply" & estimates$Lag == 0, , drop = FALSE]
    value <- row$Coef
  } else {
    assert_columns(estimates, c("Model", "Elasticity"), "Micro-elasticity table")
    row <- estimates[estimates$Model == "IV Demand", , drop = FALSE]
    value <- row$Elasticity
  }
  if (length(value) != 1L || !is.finite(value[[1]])) {
    stop("Expected one finite wind-instrumented demand elasticity in: ", path)
  }
  as.numeric(value[[1]])
}

attach_wedge_proxy <- function(country_data) {
  stopifnot(country_data$country == "Spain")
  assert_columns(
    country_data$data,
    "wedge_iber_exception",
    "Iberian wedge proxy"
  )
  if (any(!is.finite(country_data$data$wedge_iber_exception))) {
    stop("The Iberian wedge proxy contains non-finite values.")
  }
  country_data$wedge_proxy <- "wedge_iber_exception"
  country_data
}
