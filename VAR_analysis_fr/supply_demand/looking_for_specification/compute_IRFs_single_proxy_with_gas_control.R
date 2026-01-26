
#pour normaliser par rapport à la demand
vars_demand<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp)], frequency=12)

p = 12
irhor = 20
x_demand =ts(vars_demand , start=c(2015,1), end=c(2025,9), frequency=12)
y_demand =vars_demand[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


controls <- ts(IV_data_log_esp[, c("spot_gas_capped")], frequency=12)
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
theta_0_demand_gas <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

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
lambda_0_log_demand_gas <- get_lambda_0(C, theta_0_demand_gas)
# nboot = 1000
bootIRFs_demand_gas <-gamma_bootstrap_mbb_corrected(x_demand, nboot, theta_0 = theta_0_demand , proxies = proxies, controls=controls, elas = 0,
                                                start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


##### supply shock

proxies <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply_gas <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_gas <- get_lambda_0(C, theta_0_supply_gas)
# nboot = 1000
bootIRFs_supply_gas <-gamma_bootstrap_mbb_corrected(x_demand, nboot, theta_0 = theta_0_supply_gas , 
                                                    proxies = proxies, controls = NULL, elas = 0,
                                               start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# 

proxies <- ts(IV_data_log_esp[, c("wind_forecast_log")[[1]]], frequency=12)
theta_0_wind <- first_var_estimation_bis(y_demand, res, p, proxies, irhor, controls = controls, elas = 0)$theta_0




# y: data.frame/matrix of VAR variables
# z: instrument vector
# W: controls matrix/data.frame (include lags/deterministics as desired)

residize <- function(v, W) resid(lm(v ~ W))

x_demand_tilde <- as.matrix(as.data.frame(lapply(x_demand, residize, W = controls)))
z_tilde <- residize(IV_data_log_esp$delta_supply, W = controls)

# then feed Y_tilde and z_tilde into your SVARIV pipeline

VAR_supply_pc_gas <-SVARIV(ydata = x_demand_tilde, z = z_tilde,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")




