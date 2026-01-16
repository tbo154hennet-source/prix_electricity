
IV_data_log_esp$wedge_iber_exception <- IV_data_log_esp$spot_gas - IV_data_log_esp$spot_gas_capped
IV_data_log_esp<- IV_data_log_esp %>% 
  mutate(wedge_iber_exception= ifelse(wedge_iber_exception > 10, wedge_iber_exception, 0 ))



#######



#pour normaliser par rapport à la demand

p = 12
irhor = 20

y =vars_demand[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


controls <- ts(IV_data_log_esp[, c("wedge_iber_exception")[[1]]], frequency=12)
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_demand, p, type = "none")
} else {
  vars::VAR(x_demand, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)


proxies <- ts(IV_data_log_esp[, c("delta_demand")[[1]]], frequency=12)
theta_0_demand_control <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

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
lambda_0_log_demand_control <- get_lambda_0(C, theta_0_demand_control)
# nboot = 1000
# bootIRFs_demand_control <-gamma_bootstrap_mbb_corrected(x_demand_control, nboot, theta_0 = theta_0_demand_control , proxies = proxies, controls=NULL, elas = 0,
#                                                         start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# 

##### supply shock

controls  <- ts(IV_data_log_esp[, c("wedge_iber_exception")[[1]]], frequency=12)
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "none")
} else {
  vars::VAR(x_supply, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

proxies <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply_control <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_control <- get_lambda_0(C, theta_0_supply_control)
# nboot = 1000
# bootIRFs_supply_control <-gamma_bootstrap_mbb_corrected(x_demand_control, nboot, theta_0 = theta_0_supply_control , 
#                                                         proxies = proxies, controls = NULL, elas = 0,
#                                                         start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# # 

proxies <- ts(IV_data_log_esp[, c("wind_forecast_log")[[1]]], frequency=12)
theta_0_wind_control <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls, elas = 0)$theta_0


IV_data_log_esp %>% 
  select(delta_demand, delta_supply, wind_forecast_log,
         delta_d_orth, delta_s_orth) %>% 
  cor(use = "complete.obs")

