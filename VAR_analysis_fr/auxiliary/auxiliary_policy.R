
recover_policy_shocks_joint <- function(d, psi, K = NULL, ridge = 1e-4) {
  d   <- as.numeric(d)
  psi <- as.numeric(psi)
  T <- length(d)
  H <- length(psi)
  
  if (is.null(K)) K <- H - 1L
  K <- as.integer(max(0, K))
  
  # Build design matrix X so that d = X %*% eps_full + error
  # eps_full indexes times s = -K+1 ... T
  # for each t=1..T: d[t] = sum_{h=0..H-1} psi[h+1] * eps[t-h]
  # eps index for time (t-h) is: (t-h) - (-K+1) + 1 = (t-h) + K
  p <- T + K
  X <- matrix(0, nrow = T, ncol = p)
  
  for (t in 1:T) {
    for (h in 0:(H-1)) {
      s <- t - h
      j <- s + K  # because s=-K+1 -> j=1, s=0 -> j=K, s=T -> j=T+K
      if (j >= 1 && j <= p) {
        X[t, j] <- X[t, j] + psi[h + 1]
      }
    }
  }
  
  # Ridge solution: (X'X + ridge I) eps = X'd
  XtX <- crossprod(X)
  Xtd <- crossprod(X, d)
  eps_full <- solve(XtX + ridge * diag(p), Xtd)
  eps_full <- as.numeric(eps_full)
  
  pre  <- eps_full[1:K]          # eps_{-K+1}..eps_0
  eps  <- eps_full[(K+1):(K+T)]  # eps_1..eps_T
  
  list(eps = eps, pre = pre, eps_full = eps_full, X = X)
}

# ----------------------------
# Helper: bootstrap sd-ratio + quantiles
# boot_mat: apply(..., 2, ...) assumes each column = one bootstrap draw (a time series)
# obs_vec: observed series (same scale as the bootstrap series)
# ----------------------------
boot_sd_ratio_summary <- function(boot_mat, obs_vec,
                                  probs_90 = c(0.05, 0.95),
                                  probs_68 = c(0.16, 0.84)) {
  obs_vec <- as.numeric(obs_vec)
  sd_obs  <- sqrt(var(obs_vec, na.rm = TRUE))
  
  sd_ratio <- apply(
    boot_mat,
    2,
    function(x) sqrt(var(x, na.rm = TRUE)) / sd_obs
  )
  
  list(
    sd_obs = sd_obs,
    sd_ratio = sd_ratio,
    q90 = quantile(sd_ratio, probs = probs_90, na.rm = TRUE),
    q68 = quantile(sd_ratio, probs = probs_68, na.rm = TRUE),
    median = quantile(sd_ratio, probs = 0.5, na.rm = TRUE)
  )
}
