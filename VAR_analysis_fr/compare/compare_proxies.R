
#compare supply and demand proxy entering

confidence = c(0.68, 0.9)
NWlags= 0
norm        = 1 # Variable used for normalization
scale       = 1

IV_data_log_esp %>% 
  dplyr::select(delta_d_tot, delta_d_orth, delta_supply,
                delta_d_tot,delta_d_orth,delta_demand
  ) %>% 
  cor()


######## DEMAND

p = 12
irhor = 20

y =x_demand[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_demand, p, type = "const")
} else {
  vars::VAR(x_demand, p, type = "const", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)


proxies <- ts(IV_data_log_esp[, c("delta_demand")[[1]]], frequency=12)
theta_0_demand <- first_var_estimation_bis(x =x_demand, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

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
lambda_0_log_demand <- get_lambda_0(C, theta_0_demand)


proxies <- ts(IV_data_log_esp[, c("delta_d_tot")[[1]]], frequency=12)
theta_0_d_tot <- first_var_estimation_bis(x =x_demand, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0
lambda_0_log_d_tot <- get_lambda_0(C, theta_0_d_tot)

proxies <- ts(IV_data_log_esp[, c("delta_d_orth")[[1]]], frequency=12)
theta_0_d_orth <- first_var_estimation_bis(x =x_demand, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0
lambda_0_log_d_orth <- get_lambda_0(C, theta_0_d_orth)



controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "const")
} else {
  vars::VAR(x_supply, p, type = "const", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)

proxies <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply <- get_lambda_0(C, theta_0_supply)

proxies <- ts(IV_data_log_esp[, c("delta_s_tot")[[1]]], frequency=12)
theta_0_s_tot <- first_var_estimation_bis(x =x_supply, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0
lambda_0_log_s_tot <- get_lambda_0(C, theta_0_s_tot)

proxies <- ts(IV_data_log_esp[, c("delta_s_orth")[[1]]], frequency=12)
theta_0_s_orth <- first_var_estimation_bis(x =x_supply, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0
lambda_0_log_s_orth <- get_lambda_0(C, theta_0_s_orth)




# Step 1: Put into a list with names
# list_irfs <- list(lambda_0_log_d_tot, lambda_0_log_demand, lambda_0_log_d_orth)
list_irfs_demand <- list(lambda_0_log_d_tot[,,1],
                         lambda_0_log_demand[,,1], lambda_0_log_d_orth[,,1])
list_irfs_supply <- list(lambda_0_log_s_tot[,,1],
                         lambda_0_log_supply[,,1], lambda_0_log_s_orth[,,1])

names(list_irfs_demand ) <- c("Raw" ,   "PCA" ,     "Residualized")
names(list_irfs_supply ) <- c("Raw" ,   "PCA" ,     "Residualized")




