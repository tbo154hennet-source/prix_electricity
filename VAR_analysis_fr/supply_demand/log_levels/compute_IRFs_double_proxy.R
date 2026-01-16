
IV_data_log_esp[, c("delta_s_orth", "delta_d_orth","delta_supply", "delta_supply_incorpo", "delta_demand")] %>% 
  cor()


controls = NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_demand, p, type = "none")
} else {
  vars::VAR(x_demand, p, type = "none", exogen = controls)
}



coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

theta_0_supply
theta_0_supply_control
theta_0_supply_wedge

proxies <- ts(IV_data_log_esp[, c("delta_supply_incorpo", "delta_demand" )], frequency=12)
k=2
l=2
theta_0_double <- first_var_estimation_bis( x= x_demand, res, p, proxies, irhor, controls = NULL, elas = -0.02)$theta_0



AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon

# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)
lambda_0_log_double <- get_lambda_0(C, theta_0_double)
nboot = 1000

compute_quantile_bands_dim3 <- function(boot_array, probs) {
  # boot_array: horizon x nvar x nsim x (optionnel: nshock)
  # quantiles calculées sur la 3ème dimension (nsim)
  
  dims <- dim(boot_array)
  if (length(dims) == 3) {
    # 3D array: horizon x nvar x nsim
    apply(boot_array, c(1,2), function(x) quantile(x, probs = probs, na.rm = TRUE))
  } else if (length(dims) == 4) {
    # 4D array: horizon x nvar x nsim x nshock
    horizon <- dims[1]; nvar <- dims[2]; nsim <- dims[3]; nshock <- dims[4]
    result <- array(NA, dim = c(length(probs), horizon, nvar, nshock))
    for (s in 1:nshock) {
      result[,,,s] <- apply(boot_array[,,,s], c(1,2), function(x) quantile(x, probs = probs, na.rm = TRUE))
    }
    return(result) # dims: quantile x horizon x nvar x nshock
  } else {
    stop("boot_array must be 3D or 4D")
  }
}


bootIRFs_double <-gamma_bootstrap_mbb_corrected(x = x_demand, nboot,  theta_0 = theta_0_double ,
                                                proxies = proxies, controls=NULL, target_elas,
                                                start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)

###################### double with original shocks


theta_0_wind
theta_0_wind_control

proxies <- ts(IV_data_log_esp[, c("delta_s_tot", "delta_d_tot")], frequency=12)
k=2
l=2
theta_0_double_original <- first_var_estimation_bis(x_supply, res, p, proxies, irhor, controls = NULL, elas = -0.02)$theta_0
#first_var_estimation_double(x_demand, res, p, proxies, irhor, controls = NULL, elas = -0.3)$theta_0
dim(res)
dim(proxies)
AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
hori <- 20 # Forecast horizon
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)
lambda_0_log_double_original <- get_lambda_0(C, theta_0_double_original)
nboot = 1000

compute_quantile_bands_dim3 <- function(boot_array, probs) {
  # boot_array: horizon x nvar x nsim x (optionnel: nshock)
  # quantiles calculées sur la 3ème dimension (nsim)
  
  dims <- dim(boot_array)
  if (length(dims) == 3) {
    # 3D array: horizon x nvar x nsim
    apply(boot_array, c(1,2), function(x) quantile(x, probs = probs, na.rm = TRUE))
  } else if (length(dims) == 4) {
    # 4D array: horizon x nvar x nsim x nshock
    horizon <- dims[1]; nvar <- dims[2]; nsim <- dims[3]; nshock <- dims[4]
    result <- array(NA, dim = c(length(probs), horizon, nvar, nshock))
    for (s in 1:nshock) {
      result[,,,s] <- apply(boot_array[,,,s], c(1,2), function(x) quantile(x, probs = probs, na.rm = TRUE))
    }
    return(result) # dims: quantile x horizon x nvar x nshock
  } else {
    stop("boot_array must be 3D or 4D")
  }
}


bootIRFs_double_original <- gamma_bootstrap_mbb_corrected_final(x = x_supply, nboot,  theta_0 = theta_0_double_original ,
                                                proxies = proxies, controls=NULL, elas =0,
                                                start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)



