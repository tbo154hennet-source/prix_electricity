bvar_lag_matrix <- function(sample, variables, p) {
  observations <- nrow(sample)
  lags <- lapply(seq_len(p), function(lag) {
    as.matrix(sample[(p + 1L - lag):(observations - lag), variables, drop = FALSE])
  })
  cbind(const = 1, do.call(cbind, lags))
}

bvar_ma_matrices <- function(lag_coefficients, n, p, horizon) {
  # lag_coefficients is (n p) x n, ordered lag 1 block first, one column per
  # equation, so A_l = t(rows of block l).
  a <- lapply(seq_len(p), function(lag) {
    t(lag_coefficients[(lag - 1L) * n + seq_len(n), , drop = FALSE])
  })
  ma <- array(0, dim = c(n, n, horizon + 1L))
  ma[, , 1] <- diag(n)
  for (hh in seq_len(horizon)) {
    for (lag in seq_len(min(hh, p))) {
      ma[, , hh + 1L] <- ma[, , hh + 1L] + ma[, , hh + 1L - lag] %*% a[[lag]]
    }
  }
  ma
}

bvar_is_explosive <- function(lag_coefficients, n, p) {
  companion <- matrix(0, n * p, n * p)
  companion[seq_len(n), ] <- t(lag_coefficients)
  if (p > 1L) {
    companion[(n + 1L):(n * p), seq_len(n * (p - 1L))] <- diag(n * (p - 1L))
  }
  max(Mod(eigen(companion, only.values = TRUE)$values)) >= 1
}

# Proxy SVAR with a diffuse normal-inverse-Wishart prior
# p(B, Sigma) ~ |Sigma|^{-(m + 1) / 2}. The proxy is appended as an extra
# equation with the same regressors, so that the conjugate posterior holds for
# the joint system and gamma = Cov(u, e_z) is drawn together with B and Sigma.
fit_proxy_bvar <- function(country_data, proxy, norm_var, shock_name, cfg,
                           draws = cfg$bvar$draws,
                           horizon = cfg$bvar$horizon) {
  set.seed(cfg$seed)
  variables <- country_data$variables
  n <- length(variables)
  p <- cfg$p
  reduced <- fit_reduced_var(
    country_data$data, variables, p, extra_columns = proxy
  )
  sample <- reduced$sample
  x <- bvar_lag_matrix(sample, variables, p)
  y <- as.matrix(sample[-seq_len(p), c(variables, proxy), drop = FALSE])
  m <- ncol(y)

  xtx_inv <- solve(crossprod(x))
  b_hat <- xtx_inv %*% crossprod(x, y)
  s_hat <- crossprod(y - x %*% b_hat)
  dof <- nrow(y) - ncol(x)
  if (dof <= m + 1L) {
    stop("Not enough degrees of freedom for the BVAR posterior: ", dof)
  }
  norm_index <- match(norm_var, variables)
  chol_xtx_inv <- t(chol(xtx_inv))
  s_hat_inv <- solve(s_hat)

  responses <- array(
    NA_real_, dim = c(horizon + 1L, n, draws),
    dimnames = list(NULL, variables, NULL)
  )
  explosive <- logical(draws)
  for (dd in seq_len(draws)) {
    sigma <- solve(stats::rWishart(1L, dof, s_hat_inv)[, , 1])
    b <- b_hat + chol_xtx_inv %*%
      matrix(stats::rnorm(ncol(x) * m), ncol(x), m) %*% chol(sigma)
    gamma <- sigma[seq_len(n), m]
    theta <- gamma / gamma[norm_index]
    lag_coefficients <- b[-1L, seq_len(n), drop = FALSE]
    ma <- bvar_ma_matrices(lag_coefficients, n, p, horizon)
    for (hh in seq_len(horizon + 1L)) {
      responses[hh, , dd] <- ma[, , hh] %*% theta
    }
    explosive[dd] <- bvar_is_explosive(lag_coefficients, n, p)
  }
  if (cfg$bvar$stable_only) {
    if (all(explosive)) stop("All BVAR draws are explosive for ", proxy)
    responses <- responses[, , !explosive, drop = FALSE]
  }
  message(
    "BVAR ", country_data$country, " / ", proxy, ": ", draws,
    " draws, explosive share ", round(mean(explosive), 3),
    if (cfg$bvar$stable_only) " (dropped)" else " (kept)"
  )

  band <- function(probability) {
    apply(responses, c(1, 2), stats::quantile, probs = probability, names = FALSE)
  }
  frame <- function(matrix_value) as.vector(matrix_value)
  plot_irfs <- data.frame(
    Variable = rep(variables, each = horizon + 1L),
    Horizon = rep(0:horizon, times = n),
    Estimate = frame(band(0.5)),
    Lower68 = frame(band(0.16)),
    Upper68 = frame(band(0.84)),
    Lower90 = frame(band(0.05)),
    Upper90 = frame(band(0.95)),
    Shock = shock_name,
    stringsAsFactors = FALSE
  )

  list(
    method = "Bayesian proxy SVAR (diffuse NIW prior)",
    country = country_data$country,
    proxy = proxy,
    norm_var = norm_var,
    shock = shock_name,
    variables = variables,
    draws = draws,
    explosive_share = mean(explosive),
    dates = reduced$dates,
    plot_irfs = plot_irfs
  )
}

truncate_plot_irfs <- function(result, horizon) {
  data <- prepare_iv_plot_data(result)
  list(shock = result$shock, plot_irfs = data[data$Horizon <= horizon, , drop = FALSE])
}
