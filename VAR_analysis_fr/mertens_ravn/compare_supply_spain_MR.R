
save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log_elas_sd_2026_pc.parquet')


headers_var_spot_first_mp_gas <- c(headers_var_spot_first_mp_all,
                                   "spot_gas_log")

headers_var_gen_first_mp_gas <- c(headers_var_gen_first_mp_all,
                                  "spot_gas_log")

# headers_var_gen_first_mp_all_gas <- c(headers_var_gen_first_mp_all, "spot_gas_log")
# headers_var_spot_first_mp_all_gas <- c(headers_var_spot_first_mp_all, "spot_gas_log")
# 


vars_supply <- ts(IV_data_log_esp[, c(headers_var_spot_first_mp_gas)], frequency=12)
vars_demand <- ts(IV_data_log_esp[, c(headers_var_gen_first_mp_gas)], frequency=12)

p = 12
irhor = 20
#variables dans VAR
n <- ncol(vars_supply)
T = nrow(vars_supply) - p
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1
controls = NULL

x_supply =ts(vars_supply, start=c(2015,1), end=c(2025,9), frequency=12)
x_demand =ts(vars_demand, start=c(2015,1), end=c(2025,9), frequency=12)



VAR_supply_pc <-SVARIV(ydata = x_supply, z = IV_data_log_esp$delta_supply,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")


names_supply <- c("Pelec"   ,    "Load"   , "Generation",
                  "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names_demand <- c(   "Load"   , "Pelec"   , "Generation",
                     "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names<- c("Pelec"   ,    "Load"   , "Generation",
          "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 

# VAR_wind <-SVARIV(ydata = x_supply, z = IV_data_log_esp$wind_forecast_log,p,
#                   confidence,NWlags,
#                   norm,scale,irhor,instrument_name="supply")



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

nboot = 1000
bootIRFs_supply <- gamma_bootstrap_mbb_corrected(x_supply, nboot, theta_0 = theta_0_supply ,
                                                                 proxies = proxies, controls = NULL, elas = 0,
                                                                 start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)

# bootIRFs_supply_final <-  gamma_bootstrap_mbb_corrected_final(x= x_supply, nboot, theta_0 = theta_0_supply ,
#                               proxies = proxies, controls = NULL, elas = 0,
#                               start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)



proxies <- ts(IV_data_log_esp[, c("temperature_discomfort", "wind_forecast_log" )], frequency=12)
k=2
l=2
theta_0_double <- first_var_estimation_double_bis( x= x_demand, res, p, proxies, irhor, controls = NULL, elas = theta_0_supply[2])$theta_0


vl <- if (is.null(controls)) {
  vars::VAR(x_demand, p, type = "const")
} else {
  vars::VAR(x_demand, p, type = "const", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
C  <- MARep(t(AL), p, hori)

lambda_0_log_double <- get_lambda_0(C, theta_0_double)



irf_cholesky <- irf(vl, impulse = colnames(x_demand), response = colnames(x_demand),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

lambda_0_log_double_supply <- lambda_0_log_double[,c(c(2,1),3:n),2]
irf_cholesky_supply <- irf_cholesky$irf$spot_price_log[,c(c(2,1),3:n)]/irf_cholesky$irf$spot_price_log[1,2]

lambda_0_log_supply
lambda_0_log_double_supply 
irf_cholesky_supply



