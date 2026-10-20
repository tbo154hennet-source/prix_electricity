convolution_design <- function(psi, observations, pre_shocks = 0L) {
  psi <- as.numeric(psi)
  observations <- as.integer(observations)
  pre_shocks <- as.integer(max(0L, pre_shocks))
  if (!length(psi) || abs(psi[[1]]) < 1e-12) {
    stop("The contemporaneous price IRF is zero; the target is not identifiable.")
  }

  design <- matrix(0, nrow = observations, ncol = observations + pre_shocks)
  for (tt in seq_len(observations)) {
    for (hh in 0:(length(psi) - 1L)) {
      shock_time <- tt - hh
      column <- shock_time + pre_shocks
      if (column >= 1L && column <= ncol(design)) {
        design[tt, column] <- design[tt, column] + psi[hh + 1L]
      }
    }
  }
  design
}

recover_policy_shock_ridge_ls <- function(gap, psi, ridge = 1e-3) {
  design <- convolution_design(psi, length(gap), pre_shocks = 0L)
  shocks <- solve(
    crossprod(design) + ridge * diag(ncol(design)),
    crossprod(design, as.numeric(gap))
  )
  list(
    shocks = as.numeric(shocks),
    shocks_full = as.numeric(shocks),
    pre_shocks = 0L,
    fitted_gap = as.numeric(design %*% shocks),
    design = design
  )
}

recover_policy_shocks_joint <- function(gap, psi, pre_shocks = NULL,
                                        ridge = 1e-8) {
  if (is.null(pre_shocks)) pre_shocks <- length(psi) - 1L
  design <- convolution_design(psi, length(gap), pre_shocks)
  shocks_full <- solve(
    crossprod(design) + ridge * diag(ncol(design)),
    crossprod(design, as.numeric(gap))
  )
  shocks_full <- as.numeric(shocks_full)
  current <- seq.int(pre_shocks + 1L, pre_shocks + length(gap))
  list(
    shocks = shocks_full[current],
    shocks_full = shocks_full,
    pre_shocks = pre_shocks,
    fitted_gap = as.numeric(design %*% shocks_full),
    design = design
  )
}

counterfactual_irf_matrix <- function(result) {
  horizon <- dim(result$ma)[[3]]
  variables <- result$variables
  out <- matrix(
    NA_real_,
    nrow = horizon,
    ncol = length(variables),
    dimnames = list(0:(horizon - 1L), variables)
  )
  impact <- result$theta[, 1, drop = FALSE]
  for (hh in seq_len(horizon)) {
    out[hh, ] <- drop(result$ma[, , hh] %*% impact)
  }
  out
}

propagate_price_target <- function(observed, irf, target_price,
                                   method = c("ridge", "joint"),
                                   ridge = NULL, shock_start = 1L,
                                   shock_end = nrow(observed)) {
  method <- match.arg(method)
  observed <- as.matrix(observed)
  target_price <- as.numeric(target_price)
  if (nrow(observed) != length(target_price)) {
    stop("The price target and VAR sample have different lengths.")
  }
  if (any(!is.finite(target_price))) {
    stop("The price target contains non-finite values.")
  }

  shock_start <- as.integer(shock_start)
  shock_end <- as.integer(shock_end)
  if (
    length(shock_start) != 1L || is.na(shock_start) ||
      shock_start < 1L || shock_start > nrow(observed)
  ) {
    stop("shock_start must identify one observation in the model sample.")
  }
  if (
    length(shock_end) != 1L || is.na(shock_end) ||
      shock_end < shock_start || shock_end > nrow(observed)
  ) {
    stop("shock_end must be between shock_start and the end of the model sample.")
  }
  if (method == "joint" && shock_start != 1L) {
    stop("A delayed shock_start is currently supported only for ridge recovery.")
  }

  price_gap <- observed[, 1] - target_price
  recovery <- if (method == "ridge") {
    active <- seq.int(shock_start, shock_end)
    active_recovery <- recover_policy_shock_ridge_ls(
      price_gap[active], irf[, 1],
      ridge = ridge %||% 1e-3
    )
    prefix <- numeric(shock_start - 1L)
    suffix <- numeric(nrow(observed) - shock_end)
    active_recovery$shocks <- c(prefix, active_recovery$shocks, suffix)
    active_recovery$shocks_full <- c(
      prefix, active_recovery$shocks_full, suffix
    )
    active_recovery$fitted_gap <- c(
      prefix, active_recovery$fitted_gap, suffix
    )
    active_recovery$shock_start <- shock_start
    active_recovery$shock_end <- shock_end
    active_recovery
  } else {
    joint_recovery <- recover_policy_shocks_joint(
      price_gap, irf[, 1],
      pre_shocks = nrow(irf) - 1L,
      ridge = ridge %||% 1e-8
    )
    joint_recovery$shock_start <- 1L
    joint_recovery
  }

  path <- matrix(
    0,
    nrow = nrow(observed),
    ncol = ncol(observed),
    dimnames = dimnames(observed)
  )
  for (jj in seq_len(ncol(observed))) {
    response_design <- convolution_design(
      irf[, jj], nrow(observed), recovery$pre_shocks
    )
    path[, jj] <- as.numeric(response_design %*% recovery$shocks_full)
  }
  # Shocks are zero outside the policy window, but their dynamic VAR responses
  # must be allowed to propagate after the final policy month.
  list(
    observed = observed,
    counterfactual = observed - path,
    path = path,
    price_gap = price_gap,
    fitted_gap = recovery$fitted_gap,
    shocks = recovery$shocks,
    shocks_full = recovery$shocks_full,
    pre_shocks = recovery$pre_shocks,
    shock_start = recovery$shock_start,
    shock_end = recovery$shock_end %||% nrow(observed)
  )
}

inflation_from_log_level <- function(values) {
  100 * (as.numeric(values) - lag_vector(as.numeric(values), 12L))
}

read_iberian_target <- function(result, cfg) {
  path <- cfg$data$iberian_no_subsidy
  if (!file.exists(path)) stop("Iberian target not found: ", path)
  source <- as.data.frame(arrow::read_parquet(path))
  assert_columns(
    source,
    c("year", "month", "price", "predicted_price", "pi_lower", "pi_upper"),
    "Iberian no-subsidy target"
  )
  source$Date <- as.Date(sprintf(
    "%04d-%02d-01", as.integer(source$year), as.integer(source$month)
  ))
  source <- source[order(source$Date), , drop = FALSE]

  dates <- as.Date(result$sample$observation_date)
  matched <- match(source$Date, dates)
  if (anyNA(matched)) {
    stop("Iberian target dates are not covered by the VAR sample.")
  }
  hicp <- as.numeric(result$sample$HICP)
  to_real_log_price <- function(nominal, indices) {
    real_price <- as.numeric(nominal) / hicp[indices] * 100
    log(pmax(real_price, 0) + 1)
  }

  observed <- as.numeric(result$sample$spot_price_log)
  point <- lower <- upper <- observed
  point[matched] <- to_real_log_price(source$predicted_price, matched)
  lower[matched] <- to_real_log_price(source$pi_lower, matched)
  upper[matched] <- to_real_log_price(source$pi_upper, matched)
  list(
    source = source,
    dates = dates,
    matched = matched,
    point = point,
    lower = lower,
    upper = upper
  )
}

read_wind_2030_target <- function(result, cfg) {
  path <- cfg$data$wind_2030
  if (!file.exists(path)) stop("Wind-2030 target not found: ", path)
  source <- as.data.frame(arrow::read_parquet(path))
  assert_columns(
    source,
    c("date", "mean_price", "mean_cf", "mean_cf_adj"),
    "Wind-2030 target"
  )
  source$Date <- as.Date(source$date)
  source <- source[order(source$Date), , drop = FALSE]

  dates <- as.Date(result$sample$observation_date)
  matched <- match(dates, source$Date)
  if (anyNA(matched)) {
    stop("Wind-2030 target does not cover the complete VAR sample.")
  }
  hicp <- as.numeric(result$sample$HICP)
  transform_target <- function(log_nominal_price) {
    log(exp(as.numeric(log_nominal_price)) / hicp * 100 + 1)
  }

  list(
    source = source,
    dates = dates,
    point = transform_target(source$mean_cf_adj[matched]),
    raw = transform_target(source$mean_cf[matched])
  )
}

simulate_var_path <- function(fit, initial_values, innovations) {
  coefficients <- vars::Bcoef(fit)
  p <- fit$p
  observations <- p + nrow(innovations)
  simulated <- matrix(
    NA_real_,
    nrow = observations,
    ncol = ncol(initial_values),
    dimnames = list(NULL, colnames(initial_values))
  )
  simulated[seq_len(p), ] <- initial_values[seq_len(p), , drop = FALSE]

  for (tt in seq.int(p + 1L, observations)) {
    regressors <- unlist(lapply(
      seq_len(p),
      function(lag) simulated[tt - lag, , drop = TRUE]
    ))
    regressors <- c(regressors, 1)
    simulated[tt, ] <- drop(coefficients %*% regressors) +
      innovations[tt - p, ]
  }
  simulated
}

moving_block_indices <- function(observations, block_size) {
  block_size <- min(as.integer(block_size), observations)
  starts <- sample.int(
    observations - block_size + 1L,
    ceiling(observations / block_size),
    replace = TRUE
  )
  unlist(lapply(
    starts,
    function(start) seq.int(start, length.out = block_size)
  ))[seq_len(observations)]
}

bootstrap_wind_counterfactual <- function(result, target_price, cfg) {
  draws <- max(0L, as.integer(cfg$counterfactual_bootstrap))
  variables <- result$variables
  selected <- match(c("spot_price_log", "IPI_log", "HICP_log"), variables)
  if (anyNA(selected)) stop("Macro variables are absent from the Spanish VAR.")

  observed <- as.matrix(result$sample[, variables, drop = FALSE])
  residuals <- sweep(result$residuals, 2, colMeans(result$residuals), "-")
  instrument <- as.numeric(
    result$sample[[result$proxy]][-seq_len(result$fit$p)]
  )
  instrument <- instrument - mean(instrument)
  residual_observations <- nrow(residuals)
  block_size <- as.integer(cfg$counterfactual_block_size)
  if (!is.finite(block_size) || block_size <= 0L) {
    block_size <- round(5 * residual_observations^0.25)
  }
  block_size <- max(2L, min(block_size, residual_observations))
  ratios <- matrix(
    NA_real_,
    nrow = draws,
    ncol = length(selected),
    dimnames = list(NULL, variables[selected])
  )
  observed_sd <- apply(observed[, selected, drop = FALSE], 2, stats::sd)
  accepted <- 0L
  attempted <- 0L
  max_attempts <- max(draws * 4L, draws)

  set.seed(cfg$seed + 2030L)
  while (accepted < draws && attempted < max_attempts) {
    attempted <- attempted + 1L
    candidate <- try({
      index <- moving_block_indices(residual_observations, block_size)
      innovations <- residuals[index, , drop = FALSE]
      boot_instrument <- instrument[index]
      boot_data <- simulate_var_path(
        result$fit, observed[seq_len(result$fit$p), , drop = FALSE],
        innovations
      )
      boot_fit <- vars::VAR(
        stats::ts(boot_data, frequency = 12),
        p = result$fit$p,
        type = "const"
      )
      boot_residuals <- stats::residuals(boot_fit)
      gamma <- matrix(
        drop(crossprod(boot_instrument, boot_residuals)),
        ncol = 1L
      )
      price_index <- match("spot_price_log", variables)
      if (
        !is.finite(gamma[price_index, 1]) ||
          abs(gamma[price_index, 1]) < sqrt(.Machine$double.eps)
      ) {
        stop("Weak bootstrap normalization.")
      }
      theta <- gamma / gamma[price_index, 1]
      boot_ma <- vars::Phi(boot_fit, nstep = cfg$horizon - 1L)
      boot_irf <- matrix(
        NA_real_, nrow = cfg$horizon, ncol = length(variables)
      )
      for (hh in seq_len(cfg$horizon)) {
        boot_irf[hh, ] <- drop(boot_ma[, , hh] %*% theta)
      }
      if (any(!is.finite(boot_irf)) || max(abs(boot_irf)) > 100) {
        stop("Explosive bootstrap IRF.")
      }
      # As in the source counterfactual code, bootstrap uncertainty enters
      # through the re-estimated IRF; the observed history and target stay fixed.
      propagated <- propagate_price_target(
        observed, boot_irf, target_price,
        method = "joint", ridge = 1e-8
      )
      counterfactual <- propagated$counterfactual
      if (any(!is.finite(counterfactual[, selected, drop = FALSE]))) {
        stop("Non-finite bootstrap counterfactual.")
      }
      apply(
        counterfactual[, selected, drop = FALSE],
        2, stats::sd
      ) / observed_sd
    }, silent = TRUE)

    if (!inherits(candidate, "try-error") && all(is.finite(candidate))) {
      accepted <- accepted + 1L
      ratios[accepted, ] <- candidate
    }
  }

  if (accepted < draws) {
    warning(
      "Only ", accepted, " of ", draws,
      " wind-counterfactual bootstrap draws were accepted."
    )
    ratios <- ratios[seq_len(accepted), , drop = FALSE]
  }
  list(
    ratios = ratios,
    accepted = accepted,
    attempted = attempted,
    block_size = block_size
  )
}

summarise_volatility_ratios <- function(point_counterfactual, bootstrap,
                                        variables) {
  selected_names <- c("spot_price_log", "IPI_log", "HICP_log")
  selected <- match(selected_names, variables)
  labels <- c("P_elec", "IPI", "HICP")
  observed <- point_counterfactual$observed
  counterfactual <- point_counterfactual$counterfactual
  point <- vapply(selected, function(jj) {
    stats::sd(counterfactual[, jj], na.rm = TRUE) /
      stats::sd(observed[, jj], na.rm = TRUE)
  }, numeric(1))

  if (nrow(bootstrap$ratios)) {
    quant <- function(probability) {
      apply(
        bootstrap$ratios[, selected_names, drop = FALSE],
        2, stats::quantile,
        probs = probability, na.rm = TRUE, names = FALSE
      )
    }
    median <- quant(0.50)
    lower90 <- quant(0.05)
    upper90 <- quant(0.95)
    lower68 <- quant(0.16)
    upper68 <- quant(0.84)
  } else {
    median <- lower90 <- upper90 <- lower68 <- upper68 <- point
  }

  data.frame(
    Series = labels,
    Variable = selected_names,
    Point = point,
    Median = as.numeric(median),
    Lower90 = as.numeric(lower90),
    Upper90 = as.numeric(upper90),
    Lower68 = as.numeric(lower68),
    Upper68 = as.numeric(upper68),
    Bootstrap_draws = bootstrap$accepted,
    Block_size = bootstrap$block_size,
    stringsAsFactors = FALSE
  )
}

plot_iberian_price_source <- function(result, target, cfg) {
  sample <- result$sample
  dates <- as.Date(sample$observation_date)
  hicp <- as.numeric(sample$HICP)
  spanish_nominal <- as.numeric(sample$spot_price) * hicp / 100
  gas_actual <- as.numeric(sample$spot_gas) * hicp / 100
  gas_capped <- as.numeric(sample$spot_gas_capped) * hicp / 100
  source <- target$source

  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = source,
      ggplot2::aes(
        x = Date, ymin = pi_lower, ymax = pi_upper,
        fill = "Elec ES 95% PI"
      ),
      alpha = 0.20
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = dates, y = spanish_nominal,
        colour = "Elec ES", linetype = "Observed"
      ),
      linewidth = 0.75
    ) +
    ggplot2::geom_line(
      data = source,
      ggplot2::aes(
        x = Date, y = predicted_price,
        colour = "Elec ES", linetype = "No gas subsidy"
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = dates, y = gas_actual,
        colour = "Gas TTF", linetype = "Observed"
      ),
      linewidth = 0.65
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = dates, y = gas_capped,
        colour = "Gas TTF", linetype = "Effective capped gas"
      ),
      linewidth = 0.65
    )

  france_path <- cfg$data$france_price_level
  if (file.exists(france_path)) {
    france <- as.data.frame(arrow::read_parquet(france_path))
    assert_columns(
      france, c("observation_date", "spot_price"),
      "French electricity-price series"
    )
    france$Date <- as.Date(france$observation_date)
    plot <- plot + ggplot2::geom_line(
      data = france,
      ggplot2::aes(
        x = Date, y = spot_price,
        colour = "Elec FR", linetype = "Observed"
      ),
      linewidth = 0.65
    )
  }

  plot +
    ggplot2::coord_cartesian(
      xlim = as.Date(c("2021-01-01", "2024-01-01"))
    ) +
    ggplot2::scale_colour_manual(
      values = c("Elec ES" = "#D95F02", "Gas TTF" = "#1B6CA8", "Elec FR" = "#B2182B")
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Observed" = "solid",
        "No gas subsidy" = "longdash",
        "Effective capped gas" = "dotted"
      )
    ) +
    ggplot2::scale_fill_manual(values = c("Elec ES 95% PI" = "#FDAE6B")) +
    ggplot2::labs(
      title = "Iberian exception: electricity price without the gas subsidy",
      subtitle = "Micro price equation evaluated at the uncapped TTF gas price",
      x = NULL, y = "Price (EUR/MWh)",
      colour = NULL, linetype = NULL, fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_iberian_inflation <- function(result, point, lower, upper) {
  dates <- as.Date(result$sample$observation_date)
  intervention_start <- dates[[point$shock_start]]
  intervention_end <- dates[[point$shock_end]]
  inflation_effect_end <- dates[[min(point$shock_end + 12L, length(dates))]]
  price_index <- match("spot_price_log", result$variables)
  hicp_index <- match("HICP_log", result$variables)

  observed_inflation <- inflation_from_log_level(
    point$observed[, hicp_index]
  )
  counterfactual_inflation <- inflation_from_log_level(
    point$counterfactual[, hicp_index]
  )
  bound_a <- inflation_from_log_level(lower$counterfactual[, hicp_index])
  bound_b <- inflation_from_log_level(upper$counterfactual[, hicp_index])
  lower95 <- pmin(bound_a, bound_b)
  upper95 <- pmax(bound_a, bound_b)

  # The micro source supplies a 95% prediction interval. Recover the two
  # confidence bands shown in the historical figure by normal rescaling.
  scale90 <- stats::qnorm(0.95) / stats::qnorm(0.975)
  scale68 <- stats::qnorm(0.84) / stats::qnorm(0.975)
  lower90 <- counterfactual_inflation +
    scale90 * (lower95 - counterfactual_inflation)
  upper90 <- counterfactual_inflation +
    scale90 * (upper95 - counterfactual_inflation)
  lower68 <- counterfactual_inflation +
    scale68 * (lower95 - counterfactual_inflation)
  upper68 <- counterfactual_inflation +
    scale68 * (upper95 - counterfactual_inflation)

  from_log_price <- function(log_price) {
    exp(as.numeric(log_price)) - 1
  }
  observed_price <- if ("spot_price" %in% names(result$sample)) {
    as.numeric(result$sample$spot_price)
  } else {
    from_log_price(point$observed[, price_index])
  }
  counterfactual_price <- from_log_price(
    point$counterfactual[, price_index]
  )

  inflation_multiplier <- 10
  lines <- rbind(
    data.frame(
      Date = dates, Value = observed_price,
      Type = "Observed", Series = "Electricity price"
    ),
    data.frame(
      Date = dates, Value = counterfactual_price,
      Type = "No gas subsidy", Series = "Electricity price"
    ),
    data.frame(
      Date = dates, Value = observed_inflation * inflation_multiplier,
      Type = "Observed", Series = "HICP inflation"
    ),
    data.frame(
      Date = dates,
      Value = counterfactual_inflation * inflation_multiplier,
      Type = "No gas subsidy", Series = "HICP inflation"
    )
  )
  lines <- lines[
    lines$Date >= as.Date("2021-01-01") &
      is.finite(lines$Value) &
      (
        lines$Type == "Observed" |
          (
            lines$Series == "Electricity price" &
              lines$Date >= intervention_start &
              lines$Date <= intervention_end
          ) |
          (
            lines$Series == "HICP inflation" &
              lines$Date >= intervention_start &
              lines$Date <= inflation_effect_end
          )
      ),
    ,
    drop = FALSE
  ]
  lines$Type <- factor(
    lines$Type, levels = c("Observed", "No gas subsidy")
  )
  lines$Series <- factor(
    lines$Series, levels = c("Electricity price", "HICP inflation")
  )

  ribbons <- data.frame(
    Date = dates,
    Lower90 = lower90 * inflation_multiplier,
    Upper90 = upper90 * inflation_multiplier,
    Lower68 = lower68 * inflation_multiplier,
    Upper68 = upper68 * inflation_multiplier
  )
  ribbons <- ribbons[
    ribbons$Date >= intervention_start &
      ribbons$Date <= inflation_effect_end &
      rowSums(is.finite(as.matrix(ribbons[, -1, drop = FALSE]))) == 4L,
    ,
    drop = FALSE
  ]

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = ribbons,
      ggplot2::aes(
        x = Date, ymin = Lower90, ymax = Upper90,
        fill = "HICP inflation", alpha = "90% CI"
      )
    ) +
    ggplot2::geom_ribbon(
      data = ribbons,
      ggplot2::aes(
        x = Date, ymin = Lower68, ymax = Upper68,
        fill = "HICP inflation", alpha = "68% CI"
      )
    ) +
    ggplot2::geom_line(
      data = lines,
      ggplot2::aes(
        x = Date, y = Value, colour = Series, linetype = Type
      ),
      linewidth = 0.55
    ) +
    ggplot2::geom_vline(
      xintercept = intervention_start,
      linetype = "dotted", colour = "black", linewidth = 0.6
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Observed" = "solid", "No gas subsidy" = "dashed")
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Electricity price" = "red",
        "HICP inflation" = "darkblue"
      ),
      labels = c(
        "Electricity price" = expression(P[elec]),
        "HICP inflation" = expression(pi)
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("HICP inflation" = "darkblue"),
      labels = c("HICP inflation" = expression(pi))
    ) +
    ggplot2::scale_alpha_manual(
      values = c("90% CI" = 0.15, "68% CI" = 0.35)
    ) +
    ggplot2::scale_y_continuous(
      name = expression(paste("Price (", "\u20ac", "/MWh)")),
      sec.axis = ggplot2::sec_axis(
        ~ . / inflation_multiplier,
        name = "12-month HICP inflation (%)"
      )
    ) +
    ggplot2::labs(x = "Date", colour = NULL, linetype = NULL, alpha = NULL) +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal(base_size = 14)
}
run_counterfactuals <- function(source_model, cfg) {
  irf <- counterfactual_irf_matrix(source_model)
  observed <- as.matrix(
    source_model$sample[, source_model$variables, drop = FALSE]
  )

  iberian_target <- read_iberian_target(source_model, cfg)
  iberian_point <- propagate_price_target(
    observed, irf, iberian_target$point,
    method = "ridge", ridge = 1e-3,
    shock_start = iberian_target$matched[[1]],
    shock_end = utils::tail(iberian_target$matched, 1L)
  )
  iberian_lower <- propagate_price_target(
    observed, irf, iberian_target$lower,
    method = "ridge", ridge = 1e-3,
    shock_start = iberian_target$matched[[1]],
    shock_end = utils::tail(iberian_target$matched, 1L)
  )
  iberian_upper <- propagate_price_target(
    observed, irf, iberian_target$upper,
    method = "ridge", ridge = 1e-3,
    shock_start = iberian_target$matched[[1]],
    shock_end = utils::tail(iberian_target$matched, 1L)
  )

  intervention <- range(iberian_target$source$Date)
  shock_data <- data.frame(
    Date = as.Date(source_model$sample$observation_date),
    Shock = iberian_point$shocks,
    Price_ES = (
      as.numeric(source_model$sample$spot_price) *
        as.numeric(source_model$sample$HICP) / 100
    ) / 100
  )

  shock_plot <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = intervention[[1]], xmax = intervention[[2]],
      ymin = -Inf, ymax = Inf, fill = "#FCAE91", alpha = 0.18
    ) +
    ggplot2::geom_hline(
      yintercept = 0, colour = "grey55", linewidth = 0.3
    ) +
    ggplot2::geom_line(
      data = shock_data,
      ggplot2::aes(Date, Shock, colour = "Shock"),
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = shock_data,
      ggplot2::aes(Date, Price_ES, colour = "P_ES"),
      linewidth = 0.65
    )

  france_path <- cfg$data$france_price_level
  if (file.exists(france_path)) {
    france <- as.data.frame(arrow::read_parquet(france_path))
    assert_columns(
      france, c("observation_date", "spot_price"),
      "French electricity-price series"
    )
    france$Date <- as.Date(france$observation_date)
    france$Price_FR <- as.numeric(france$spot_price) / 100
    shock_plot <- shock_plot + ggplot2::geom_line(
      data = france,
      ggplot2::aes(Date, Price_FR, colour = "P_FR"),
      linewidth = 0.65
    )
  }

  shock_plot <- shock_plot +
    ggplot2::scale_colour_manual(
      values = c(
        "Shock" = "black",
        "P_ES" = "#1F77B4",
        "P_FR" = "#4FA3D9"
      ),
      labels = c(
        "Shock" = expression(epsilon[t]^policy),
        "P_ES" = expression(P[elec]^ES),
        "P_FR" = expression(P[elec]^FR)
      )
    ) +
    ggplot2::labs(
      title = "Shocks matching the no-gas-subsidy price path",
      subtitle = "Electricity prices are expressed in hundreds of EUR/MWh",
      x = NULL,
      y = "Structural shock and electricity price / 100",
      colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14)

  save_plot(
    shock_plot, cfg, cfg$plot_names$iberian_shock,
    overleaf_dir = "policy", width = 14, height = 6,
    theme_overrides = ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  )
  save_plot(
    plot_iberian_price_source(source_model, iberian_target, cfg),
    cfg, cfg$plot_names$iberian_price,
    overleaf_dir = "policy", width = 14, height = 6
  )
  save_plot(
    plot_iberian_inflation(
      source_model, iberian_point, iberian_lower, iberian_upper
    ),
    cfg, cfg$plot_names$iberian_inflation,
    overleaf_dir = "policy", width = 14, height = 6,
    theme_overrides = ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  )

  hicp_index <- match("HICP_log", source_model$variables)
  observed_inflation <- inflation_from_log_level(
    iberian_point$observed[, hicp_index]
  )
  counterfactual_inflation <- inflation_from_log_level(
    iberian_point$counterfactual[, hicp_index]
  )
  policy_start_index <- iberian_target$matched[[1]]
  policy_end_index <- utils::tail(iberian_target$matched, 1L)
  policy_base_index <- policy_start_index - 1L
  if (policy_base_index < 1L) {
    stop("The Iberian policy window needs one pre-policy HICP observation.")
  }
  observed_hicp_log <- as.numeric(
    iberian_point$observed[, hicp_index]
  )
  counterfactual_hicp_log <- as.numeric(
    iberian_point$counterfactual[, hicp_index]
  )
  observed_cumulative_inflation <- 100 * (
    exp(
      observed_hicp_log[[policy_end_index]] -
        observed_hicp_log[[policy_base_index]]
    ) - 1
  )
  no_subsidy_cumulative_inflation <- 100 * (
    exp(
      counterfactual_hicp_log[[policy_end_index]] -
        counterfactual_hicp_log[[policy_base_index]]
    ) - 1
  )
  inflation_effect_indices <- seq.int(
    policy_start_index,
    min(policy_end_index + 12L, length(observed_inflation))
  )
  iberian_summary <- data.frame(
    Scenario = "No Iberian gas subsidy",
    Start = intervention[[1]],
    End = intervention[[2]],
    Mean_price_increase_EUR_MWh = mean(
      iberian_target$source$predicted_price - iberian_target$source$price,
      na.rm = TRUE
    ),
    Max_price_increase_EUR_MWh = max(
      iberian_target$source$predicted_price - iberian_target$source$price,
      na.rm = TRUE
    ),
    Max_abs_HICP_inflation_effect_pp = max(
      abs(
        counterfactual_inflation[inflation_effect_indices] -
          observed_inflation[inflation_effect_indices]
      ),
      na.rm = TRUE
    ),    Observed_cumulative_HICP_inflation_percent =
      observed_cumulative_inflation,
    No_subsidy_cumulative_HICP_inflation_percent =
      no_subsidy_cumulative_inflation,
    Policy_cumulative_HICP_reduction_pp =
      no_subsidy_cumulative_inflation - observed_cumulative_inflation,
    Price_target_RMSE_log = sqrt(mean(
      (iberian_point$price_gap - iberian_point$fitted_gap)^2
    )),
    Price_target_max_abs_error_log = max(abs(
      iberian_point$price_gap - iberian_point$fitted_gap
    )),
    stringsAsFactors = FALSE
  )
  save_csv(iberian_summary, cfg, "counterfactual_iberian_summary.csv")

  wind_target <- read_wind_2030_target(source_model, cfg)
  wind_point <- propagate_price_target(
    observed, irf, wind_target$point,
    method = "joint", ridge = 1e-8
  )
  wind_raw <- propagate_price_target(
    observed, irf, wind_target$raw,
    method = "joint", ridge = 1e-8
  )

  wind_plot_data <- wind_target$source
  wind_plot <- ggplot2::ggplot(
    wind_plot_data, ggplot2::aes(Date)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_price, colour = "Observed log price"),
      linewidth = 0.7, alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = mean_cf, colour = "Raw 2030 counterfactual"),
      linewidth = 0.7, alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = mean_cf_adj, colour = "Mean-adjusted 2030 counterfactual"
      ),
      linewidth = 0.8
    ) +
    ggplot2::scale_colour_manual(values = c(
      "Observed log price" = "#252525",
      "Raw 2030 counterfactual" = "#74C476",
      "Mean-adjusted 2030 counterfactual" = "#006D2C"
    )) +
    ggplot2::labs(
      title = "Spain: electricity prices with projected 2030 wind capacity",
      subtitle = paste0(
        "Source micro scenario: ",
        format(cfg$wind_capacity_2030_mw, big.mark = ","),
        " MW; adjusted path used in the VAR"
      ),
      x = NULL, y = "Average log day-ahead price", colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
  save_plot(
    wind_plot, cfg, cfg$plot_names$wind_counterfactual,
    overleaf_dir = "policy", width = 12, height = 6
  )

  bootstrap_name <- paste0(
    "counterfactual_wind2030_v3_p", cfg$p,
    "_h", cfg$horizon,
    "_b", cfg$counterfactual_bootstrap
  )
  bootstrap <- cache_model(cfg, bootstrap_name, function() {
    bootstrap_wind_counterfactual(source_model, wind_target$point, cfg)
  })
  ratios <- summarise_volatility_ratios(
    wind_point, bootstrap, source_model$variables
  )
  ratios$Wind_capacity_2030_MW <- cfg$wind_capacity_2030_mw
  ratios$Price_target_RMSE_log <- sqrt(mean(
    (wind_point$price_gap - wind_point$fitted_gap)^2
  ))
  save_csv(ratios, cfg, "counterfactual_volatility_ratios.csv")

  ratios$Series <- factor(
    ratios$Series, levels = c("P_elec", "IPI", "HICP")
  )
  macro_plot <- ggplot2::ggplot(
    ratios, ggplot2::aes(Series, Median)
  ) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dashed",
      colour = "grey45", linewidth = 0.4
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = Lower90, ymax = Upper90, colour = "90% CI"
      ),
      width = 0.13, linewidth = 0.65
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = Lower68, ymax = Upper68, colour = "68% CI"
      ),
      width = 0.06, linewidth = 1.4
    ) +
    ggplot2::geom_point(size = 2.2, colour = "#08306B") +
    ggplot2::scale_colour_manual(
      values = c("90% CI" = "#6C8EBF", "68% CI" = "#08306B")
    ) +
    ggplot2::scale_x_discrete(labels = c(
      P_elec = expression(P[elec]), IPI = "IPI", HICP = "HICP"
    )) +
    ggplot2::labs(
      title = "Macroeconomic fluctuations with projected 2030 wind capacity",
      subtitle = paste0(
        cfg$counterfactual_bootstrap,
        " paired moving-block bootstrap draws"
      ),
      x = NULL, y = "SD ratio (counterfactual / observed)",
      colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )
  save_plot(
    macro_plot, cfg, cfg$plot_names$macro_fluctuations,
    overleaf_dir = "policy", width = 12, height = 6,
    theme_overrides = ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position = "bottom"
    )
  )

  diagnostics <- data.frame(
    Scenario = c(
      "Iberian exception: no gas subsidy",
      "Wind capacity 2030: mean adjusted",
      "Wind capacity 2030: raw"
    ),
    Recovery = c("Ridge LS", "Joint with pre-shocks", "Joint with pre-shocks"),
    RMSE_log_price = c(
      sqrt(mean((iberian_point$price_gap - iberian_point$fitted_gap)^2)),
      sqrt(mean((wind_point$price_gap - wind_point$fitted_gap)^2)),
      sqrt(mean((wind_raw$price_gap - wind_raw$fitted_gap)^2))
    ),
    Max_abs_error_log_price = c(
      max(abs(iberian_point$price_gap - iberian_point$fitted_gap)),
      max(abs(wind_point$price_gap - wind_point$fitted_gap)),
      max(abs(wind_raw$price_gap - wind_raw$fitted_gap))
    ),
    stringsAsFactors = FALSE
  )
  save_csv(
    diagnostics, cfg,
    "counterfactual_reconstruction_diagnostics.csv"
  )

  invisible(list(
    iberian = iberian_point,
    iberian_lower = iberian_lower,
    iberian_upper = iberian_upper,
    wind = wind_point,
    wind_raw = wind_raw,
    volatility = ratios,
    bootstrap = bootstrap,
    diagnostics = diagnostics
  ))
}
