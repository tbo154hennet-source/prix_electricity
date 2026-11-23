fit_reduced_var <- function(data, variables, p, extra_columns = character()) {
  # Instruments are carried along but do not restrict the VAR sample: they may
  # be unobserved (NA) at its start, see proxy_instrument().
  assert_columns(data, extra_columns, "Model data")
  sample <- finite_complete(data, unique(c("observation_date", variables)))
  if (nrow(sample) <= p + length(variables) * p) {
    warning(
      "The VAR has limited residual degrees of freedom: ",
      nrow(sample), " observations for ", length(variables), " variables and ", p, " lags."
    )
  }
  x <- stats::ts(as.matrix(sample[, variables, drop = FALSE]), frequency = 12)
  fit <- vars::VAR(x, p = p, type = "const")
  list(
    fit = fit,
    sample = sample,
    variables = variables,
    residuals = stats::residuals(fit),
    sigma = stats::cov(stats::residuals(fit)),
    dates = sample$observation_date[-seq_len(p)],
    ma = vars::Phi(fit, nstep = 1L)
  )
}

ma_matrices <- function(fit, horizon) {
  vars::Phi(fit, nstep = horizon - 1L)
}

irf_from_theta <- function(ma, theta, shock_names) {
  horizon <- dim(ma)[[3]]
  variables <- rownames(theta)
  array_irf <- array(
    NA_real_,
    dim = c(horizon, length(variables), ncol(theta)),
    dimnames = list(
      Horizon = 0:(horizon - 1L),
      Variable = variables,
      Shock = shock_names
    )
  )
  for (hh in seq_len(horizon)) {
    array_irf[hh, , ] <- ma[, , hh] %*% theta
  }
  data.frame(
    Horizon = rep(0:(horizon - 1L), each = length(variables) * ncol(theta)),
    Variable = rep(rep(variables, each = ncol(theta)), times = horizon),
    Shock = rep(shock_names, times = horizon * length(variables)),
    Estimate = as.vector(aperm(array_irf, c(3, 2, 1))),
    stringsAsFactors = FALSE
  )
}

single_proxy_shocks <- function(residuals, sigma, gamma, theta) {
  sigma_a <- gamma %*%
    MASS::ginv(t(gamma) %*% MASS::ginv(sigma) %*% gamma) %*%
    t(gamma)
  phi <- MASS::ginv(theta) %*% gamma
  theta_cross <- t(theta) %*% theta
  d_matrix <- solve(theta_cross) %*% t(theta) %*% sigma_a %*%
    theta %*% solve(theta_cross)
  d_inverse <- try(solve(d_matrix), silent = TRUE)
  if (inherits(d_inverse, "try-error")) d_inverse <- MASS::ginv(d_matrix)
  left <- t(phi) %*% d_inverse
  drop(
    solve(t(left) %*% left) %*% t(left) %*%
      t(gamma) %*% MASS::ginv(sigma) %*% t(residuals)
  )
}

historical_decomposition <- function(ma, theta, shocks, variable_names, shock_names) {
  if (is.vector(shocks)) shocks <- matrix(shocks, nrow = 1L)
  observations <- ncol(shocks)
  hd <- array(
    0,
    dim = c(observations, length(variable_names), ncol(theta)),
    dimnames = list(NULL, variable_names, shock_names)
  )
  max_horizon <- dim(ma)[[3]]
  for (tt in seq_len(observations)) {
    horizons <- 0:(min(max_horizon, tt) - 1L)
    for (hh in horizons) {
      response <- ma[, , hh + 1L] %*% theta
      hd[tt, , ] <- hd[tt, , ] +
        response %*% diag(shocks[, tt - hh], nrow = ncol(theta))
    }
  }
  hd
}

fit_proxy_svar <- function(country_data, proxy, norm_var, shock_name, cfg,
                           use_inference = cfg$use_inference) {
  variables <- country_data$variables
  reduced <- fit_reduced_var(
    country_data$data, variables, cfg$p, extra_columns = proxy
  )
  residuals <- reduced$residuals
  instrument <- proxy_instrument(reduced$sample, proxy, cfg$p)$z
  gamma <- matrix(drop(crossprod(instrument, residuals)), ncol = 1L)
  rownames(gamma) <- variables
  norm_index <- match(norm_var, variables)
  if (is.na(norm_index) || abs(gamma[norm_index, 1]) < sqrt(.Machine$double.eps)) {
    stop("Proxy cannot be normalized on ", norm_var, ": ", proxy)
  }
  theta <- gamma / gamma[norm_index, 1]
  colnames(theta) <- shock_name
  ma <- ma_matrices(reduced$fit, cfg$horizon)
  shocks <- single_proxy_shocks(residuals, reduced$sigma, gamma, theta)
  hd <- historical_decomposition(
    ma, theta, shocks, variables, shock_name
  )
  point_irf <- irf_from_theta(ma, theta, shock_name)

  inference_irfs <- NULL
  if (use_inference) {
    inference_irfs <- try(
      varexternal::SVARIV(
        ydata = stats::ts(
          as.matrix(reduced$sample[, variables, drop = FALSE]),
          frequency = 12
        ),
        # Unobserved months enter as 0: Gamma and theta are unchanged and the
        # MSW variance only differs through the centering of z_t u_t.
        z = ifelse(is.finite(reduced$sample[[proxy]]), reduced$sample[[proxy]], 0),
        p = cfg$p,
        confidence = cfg$confidence,
        NWlags = cfg$nw_lags,
        norm = norm_index,
        scale = 1,
        horizons = cfg$horizon,
        instrument_name = tolower(shock_name)
      )$irfs,
      silent = TRUE
    )
    if (inherits(inference_irfs, "try-error")) {
      warning("MSW inference failed for ", country_data$country, " / ", proxy, ": ", as.character(inference_irfs))
      inference_irfs <- NULL
    }
  }

  list(
    method = "External instrument",
    country = country_data$country,
    proxy = proxy,
    norm_var = norm_var,
    shock = shock_name,
    fit = reduced$fit,
    sample = reduced$sample,
    variables = variables,
    residuals = residuals,
    sigma = reduced$sigma,
    dates = reduced$dates,
    gamma = gamma,
    theta = theta,
    shocks = matrix(shocks, nrow = 1L, dimnames = list(shock_name, NULL)),
    ma = ma,
    irf = point_irf,
    inference_irfs = inference_irfs,
    hd = hd
  )
}

mixed_frequency_bootstrap_plot_data <- function(
    point_irf, residuals, instruments, norm_index, ma, shock_name,
    replications, block_size, seed,
    observed = rep(TRUE, nrow(instruments))) {
  output <- point_irf
  output$Lower68 <- NA_real_
  output$Upper68 <- NA_real_
  output$Lower90 <- NA_real_
  output$Upper90 <- NA_real_
  replications <- as.integer(replications)
  if (!is.finite(replications) || replications < 1L) return(output)

  observations <- nrow(residuals)
  block_size <- max(1L, min(as.integer(block_size), observations))
  blocks_per_draw <- ceiling(observations / block_size)
  draws <- matrix(NA_real_, replications, nrow(point_irf))
  successful <- 0L
  set.seed(seed)
  for (replication in seq_len(replications)) {
    starts <- sample.int(observations, blocks_per_draw, replace = TRUE)
    indices <- unlist(lapply(starts, function(start) {
      ((start - 1L + seq_len(block_size) - 1L) %% observations) + 1L
    }), use.names = FALSE)
    indices <- indices[seq_len(observations)]
    boot_instruments <- instruments[indices, , drop = FALSE]
    boot_observed <- observed[indices]
    boot_instruments[boot_observed, ] <- scale(
      boot_instruments[boot_observed, , drop = FALSE],
      center = TRUE,
      scale = FALSE
    )
    boot_residuals <- residuals[indices, , drop = FALSE]
    coefficients <- MASS::ginv(crossprod(boot_instruments)) %*%
      crossprod(boot_instruments, boot_residuals)
    normalization_loadings <- coefficients[, norm_index]
    boot_proxy <- drop(boot_instruments %*% normalization_loadings)
    boot_gamma <- matrix(
      drop(crossprod(boot_proxy, boot_residuals)),
      ncol = 1L
    )
    denominator <- boot_gamma[norm_index, 1]
    if (!is.finite(denominator) ||
        abs(denominator) < sqrt(.Machine$double.eps)) {
      next
    }
    boot_theta <- boot_gamma / denominator
    rownames(boot_theta) <- colnames(residuals)
    colnames(boot_theta) <- shock_name
    boot_irf <- irf_from_theta(ma, boot_theta, shock_name)
    if (any(!is.finite(boot_irf$Estimate))) next
    successful <- successful + 1L
    draws[successful, ] <- boot_irf$Estimate
  }
  if (successful < max(50L, floor(replications / 2L))) {
    stop(
      "Too few successful mixed-frequency bootstrap draws: ",
      successful, " / ", replications
    )
  }
  draws <- draws[seq_len(successful), , drop = FALSE]
  quantiles <- apply(
    draws,
    2L,
    stats::quantile,
    probs = c(0.05, 0.16, 0.84, 0.95),
    na.rm = TRUE,
    type = 8
  )
  output$Lower90 <- quantiles[1L, ]
  output$Lower68 <- quantiles[2L, ]
  output$Upper68 <- quantiles[3L, ]
  output$Upper90 <- quantiles[4L, ]
  attr(output, "bootstrap_successful") <- successful
  output
}
fit_mixed_frequency_proxy_svar <- function(
    country_data, proxies, norm_var, shock_name, cfg,
    use_inference = cfg$use_inference) {
  variables <- country_data$variables
  proxies <- as.character(proxies)
  if (length(proxies) < 2L) {
    stop("Mixed-frequency proxy-SVAR requires at least two daily proxies.")
  }
  reduced <- fit_reduced_var(country_data$data, variables, cfg$p)
  residuals <- reduced$residuals
  source_rows <- match(
    reduced$sample$observation_date,
    country_data$data$observation_date
  )
  instruments <- as.matrix(
    country_data$data[source_rows, proxies, drop = FALSE]
  )[-seq_len(cfg$p), , drop = FALSE]
  observed_instruments <- is.finite(instruments)
  if (any(colSums(observed_instruments) == 0L)) {
    stop("At least one daily proxy has no observation in the VAR sample.")
  }
  # Months with no daily proxy at all precede the high-frequency data.
  observed_months <- rowSums(observed_instruments) > 0
  # Missing entries only denote calendar days absent from a given month.
  # Daily proxies are already demeaned by month-of-year x day-of-month cell,
  # so zero is the neutral moment contribution for structurally missing days.
  instruments[!observed_instruments] <- 0
  instruments[observed_months, ] <- scale(
    instruments[observed_months, , drop = FALSE],
    center = TRUE, scale = FALSE
  )
  norm_index <- match(norm_var, variables)
  if (is.na(norm_index)) stop("Unknown normalization variable: ", norm_var)

  # Retain each observable calendar day as a separate external-instrument
  # moment. Their joint first-stage projection is used for identification.
  gram_inverse <- MASS::ginv(crossprod(instruments))
  reduced_form_on_instruments <-
    gram_inverse %*% crossprod(instruments, residuals)
  normalization_loadings <- reduced_form_on_instruments[, norm_index]
  joint_proxy <- drop(instruments %*% normalization_loadings)
  gamma <- matrix(drop(crossprod(joint_proxy, residuals)), ncol = 1L)
  rownames(gamma) <- variables
  if (abs(gamma[norm_index, 1]) < sqrt(.Machine$double.eps)) {
    stop("Joint daily mixed-frequency proxy cannot be normalized on ", norm_var)
  }
  theta <- gamma / gamma[norm_index, 1]
  colnames(theta) <- shock_name
  ma <- ma_matrices(reduced$fit, cfg$horizon)
  shocks <- single_proxy_shocks(
    residuals,
    reduced$sigma,
    gamma,
    theta
  )
  hd <- historical_decomposition(
    ma,
    theta,
    shocks,
    variables,
    shock_name
  )
  point_irf <- irf_from_theta(ma, theta, shock_name)

  target_residual <- residuals[observed_months, norm_index]
  first_stage_instruments <- instruments[observed_months, , drop = FALSE]
  first_stage <- stats::lm(target_residual ~ first_stage_instruments)
  restricted <- stats::lm(target_residual ~ 1)
  first_stage_test <- stats::anova(restricted, first_stage)
  classical_f <- unname(first_stage_test$F[[2]])
  hac_f <- NA_real_
  if (
    requireNamespace("sandwich", quietly = TRUE) &&
      requireNamespace("lmtest", quietly = TRUE)
  ) {
    covariance <- sandwich::NeweyWest(
      first_stage,
      lag = cfg$p,
      prewhite = FALSE,
      adjust = TRUE
    )
    coefficients <- stats::coef(first_stage)[-1L]
    covariance <- covariance[-1L, -1L, drop = FALSE]
    hac_f <- drop(
      t(coefficients) %*% MASS::ginv(covariance) %*% coefficients
    ) / length(coefficients)
  }
  relevance <- data.frame(
    Country = country_data$country,
    Specification = NA_character_,
    Shock = shock_name,
    Proxy = paste(proxies, collapse = " + "),
    Normalization_equation = norm_var,
    First_stage_coefficient = NA_real_,
    First_stage_SE = NA_real_,
    F_statistic = classical_f,
    HAC_F_statistic = hac_f,
    Proxy_residual_correlation = stats::cor(
      joint_proxy[observed_months],
      target_residual,
      use = "complete.obs"
    ),
    Weak_below_10 = ifelse(
      is.finite(hac_f), hac_f < 10, classical_f < 10
    ),
    stringsAsFactors = FALSE
  )
  first_stage_weights <- data.frame(
    Shock = shock_name,
    Day = seq_along(proxies),
    Proxy = proxies,
    Normalization_loading = normalization_loadings,
    Observed_months = colSums(observed_instruments),
    stringsAsFactors = FALSE
  )

  bootstrap_replications <- if (use_inference) {
    cfg$mixed_frequency_bootstrap
  } else {
    0L
  }
  plot_irfs <- mixed_frequency_bootstrap_plot_data(
    point_irf = point_irf,
    residuals = residuals,
    instruments = instruments,
    observed = observed_months,
    norm_index = norm_index,
    ma = ma,
    shock_name = shock_name,
    replications = bootstrap_replications,
    block_size = cfg$p,
    seed = cfg$seed + ifelse(shock_name == "Supply", 101L, 202L)
  )
  bootstrap_successful <- attr(plot_irfs, "bootstrap_successful")

  list(
    method = "Daily mixed-frequency external instruments",
    instrument_frequency = "daily",
    country = country_data$country,
    proxy = paste(proxies, collapse = " + "),
    proxies = proxies,
    norm_var = norm_var,
    shock = shock_name,
    fit = reduced$fit,
    sample = reduced$sample,
    variables = variables,
    residuals = residuals,
    sigma = reduced$sigma,
    dates = reduced$dates,
    gamma = gamma,
    theta = theta,
    combined_proxy = joint_proxy,
    shocks = matrix(
      shocks,
      nrow = 1L,
      dimnames = list(shock_name, NULL)
    ),
    ma = ma,
    irf = point_irf,
    plot_irfs = plot_irfs,
    inference_irfs = NULL,
    bootstrap_replications = bootstrap_replications,
    bootstrap_successful = bootstrap_successful,
    hd = hd,
    relevance = relevance,
    first_stage_weights = first_stage_weights
  )
}
fit_cholesky <- function(country_data, cfg) {
  variables <- country_data$variables
  if (variables[[1]] != "spot_price_log" || variables[[2]] != "load_tot_log") {
    stop("Cholesky robustness requires the ordering price, quantity, ...")
  }
  reduced <- fit_reduced_var(country_data$data, variables, cfg$p)
  impact <- t(chol(reduced$sigma))
  theta <- cbind(
    Supply = impact[, 1] / impact[1, 1],
    Demand = impact[, 2] / impact[2, 2]
  )
  rownames(theta) <- variables
  ma <- ma_matrices(reduced$fit, cfg$horizon)
  list(
    method = "Cholesky: price, quantity",
    country = country_data$country,
    variables = variables,
    fit = reduced$fit,
    sample = reduced$sample,
    dates = reduced$dates,
    theta = theta,
    ma = ma,
    irf = irf_from_theta(ma, theta, c("Supply", "Demand"))
  )
}

fit_mertens_ravn <- function(country_data, demand_proxy, supply_proxy,
                             demand_elasticity, cfg) {
  variables <- country_data$variables
  reduced <- fit_reduced_var(
    country_data$data,
    variables,
    cfg$p,
    extra_columns = c(supply_proxy, demand_proxy)
  )
  residuals <- reduced$residuals
  sigma <- reduced$sigma
  instruments <- proxy_instrument(
    reduced$sample, c(supply_proxy, demand_proxy), cfg$p
  )$z
  gamma <- t(crossprod(instruments, residuals))
  n <- length(variables)
  selector <- rbind(diag(2), matrix(0, n - 2L, 2L))

  # The supply shock traces the demand curve on impact. With variables ordered
  # price then quantity, theta_q / theta_p equals the micro demand elasticity.
  theta_supply <- gamma %*% MASS::ginv(t(selector) %*% gamma) %*%
    c(1, demand_elasticity)
  restriction_demand <- selector
  restriction_demand[, 1] <- MASS::ginv(sigma) %*% theta_supply
  theta_demand <- gamma %*%
    MASS::ginv(t(restriction_demand) %*% gamma) %*% c(0, 1)
  theta <- cbind(Supply = theta_supply, Demand = theta_demand)
  rownames(theta) <- variables

  sigma_a <- gamma %*%
    MASS::ginv(t(gamma) %*% MASS::ginv(sigma) %*% gamma) %*%
    t(gamma)
  theta_cross <- t(theta) %*% theta
  d_matrix <- MASS::ginv(theta_cross) %*% t(theta) %*% sigma_a %*%
    theta %*% MASS::ginv(theta_cross)
  phi <- matrix(c(1, 0, -theta[1, 2], 1), nrow = 2) %*%
    t(selector) %*% gamma
  left <- t(phi) %*% MASS::ginv(d_matrix)
  shocks <- MASS::ginv(t(left) %*% left) %*% t(left) %*%
    t(gamma) %*% MASS::ginv(sigma) %*% t(residuals)
  rownames(shocks) <- c("Supply", "Demand")

  ma <- ma_matrices(reduced$fit, cfg$horizon)
  hd <- historical_decomposition(
    ma, theta, shocks, variables, c("Supply", "Demand")
  )
  list(
    method = "Mertens-Ravn",
    country = country_data$country,
    demand_proxy = demand_proxy,
    supply_proxy = supply_proxy,
    demand_elasticity = demand_elasticity,
    elasticity_restriction = "impact quantity response / price response for supply shock",
    variables = variables,
    fit = reduced$fit,
    sample = reduced$sample,
    dates = reduced$dates,
    theta = theta,
    shocks = shocks,
    ma = ma,
    irf = irf_from_theta(ma, theta, c("Supply", "Demand")),
    hd = hd
  )
}
