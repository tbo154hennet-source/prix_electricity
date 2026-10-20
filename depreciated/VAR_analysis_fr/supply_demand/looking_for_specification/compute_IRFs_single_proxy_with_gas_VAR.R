
headers_var_gen_first_mp_gas <- c(headers_var_gen_first_mp, "spot_gas_capped_log")
headers_var_spot_first_mp_gas <- c(headers_var_spot_first_mp, "spot_gas_capped_log")


#pour normaliser par rapport à la demand
vars_demand<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp_gas)], frequency=12)

p = 12
irhor = 20
x_demand =ts(vars_demand , start=c(2015,1), end=c(2025,9), frequency=12)
y =vars_demand[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


controls <- NULL
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
theta_0_demand_gas_bis <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

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
lambda_0_log_demand_gas_bis <- get_lambda_0(C, theta_0_demand_gas_bis)
# nboot = 1000
bootIRFs_demand_gas_bis <-gamma_bootstrap_mbb_corrected(x_demand, nboot, theta_0 = theta_0_demand , proxies = proxies, controls=NULL, elas = 0,
                                                    start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


##### supply shock

proxies <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply_gas_bis <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_gas_bis <- get_lambda_0(C, theta_0_supply_gas_bis)
# nboot = 1000
bootIRFs_supply_gas_bis <-gamma_bootstrap_mbb_corrected(x_demand, nboot, theta_0 = theta_0_supply_gas_bis , 
                                                    proxies = proxies, controls = NULL, elas = 0,
                                                    start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# 

proxies <- ts(IV_data_log_esp[, c("wind_forecast_log")[[1]]], frequency=12)
theta_0_wind_gas_bis <- first_var_estimation_bis(y_demand, res, p, proxies, irhor, controls = controls, elas = 0)$theta_0




# y: data.frame/matrix of VAR variables
# z: instrument vector
# W: controls matrix/data.frame (include lags/deterministics as desired)


# then feed Y_tilde and z_tilde into your SVARIV pipeline

VAR_supply_pc_gas_bis <-SVARIV(ydata = x_demand, z = IV_data_log_esp$delta_supply,p,
                           confidence,NWlags,
                           norm,scale,irhor,instrument_name="supply")

# vars_supply<- ts(IV_data_log_esp[, c(headers_var_spot_first_mp_gas)], frequency=12)
# 
# p = 12
# irhor = 20
# x_supply =ts(vars_supply , start=c(2015,1), end=c(2025,9), frequency=12)
# 
# 
# VAR_supply_pc_gas_ter <-SVARIV(ydata = x_supply, z = IV_data_log_esp$delta_supply,p,
#                                confidence,NWlags,
#                                norm,scale,irhor,instrument_name="supply")

