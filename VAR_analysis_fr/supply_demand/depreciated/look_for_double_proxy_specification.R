# elas_grid <- c(0, -0.1, -0.2, -0.3, -0.4)
# elas_grid_supply <- c(0, 0.1, 0.3,  0.5, 1, 2)
elas_grid_demand <- seq(from = -1, to = 0, by = 0.1)
elas_grid_supply <- seq(from = -1, to = 5, by = 0.5)

elas_value = -0.2
run_bootstrap_for_elas <- function(elas_value) {
  
  # Estimation initiale
  theta_0 <- first_var_estimation_bis(
    y, res, p, proxies, irhor,
    controls = NULL,
    elas = elas_value
  )$theta_0

  lambda_0_log <- get_lambda_0(C, theta_0)
  # Bootstrap
  bootIRFs <- gamma_bootstrap_mbb_corrected(
    x = x,
    nboot = nboot,
    BlockSize = BlockSize,
    theta_0 = theta_0,
    proxies = proxies,
    controls = NULL,
    elas = elas_value,
    start_ts = c(2015,1),
    end_ts   = c(2023,12),
    irhor = irhor
  )
  
  # Confidence bands
  list(
    elas     = elas_value,
    bootIRFs = bootIRFs,
    lambda_0 = lambda_0_log,
    IRF_90   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_90),
    IRF_68   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_68),
    IRF_50   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_50)[1,,,]
  )
}

results_by_elas <- lapply(elas_grid_demand, run_bootstrap_for_elas)
names(results_by_elas) <- paste0( elas_grid_demand)

###### Imppose Supply elas instead of Demand elas
results_by_elas_bis <- lapply(elas_grid_supply, run_bootstrap_for_elas_bis)
names(results_by_elas_bis) <- paste0(elas_grid_supply)


