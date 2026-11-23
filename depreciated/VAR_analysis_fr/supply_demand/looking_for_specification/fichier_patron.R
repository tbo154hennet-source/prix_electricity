library(vars)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log_elas_sd.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)

# 


#   
headers_proxies_supply = c(
  "delta_s_orth"#,
  #"delta_d_tot"
)
headers_proxies_demand = c(
  #"delta_s_tot"#,
  "delta_d_orth"
)
# headers_proxies_supply_demand = c(
#   "delta_s_tot",#,
#   "delta_d_tot"
# )
# headers_proxies_supply <- c("delta_s_orth")
# headers_proxies_demand <- c("delta_d_orth")

headers_var_spot_first <- c("spot_price_log",
                            "gen_tot_log",
                            "IPI_log",
                            "HICP_log")

headers_var_spot_first_mp = c(
  "spot_price_log",
  "gen_tot_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

headers_var_gen_first <- c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  "HICP_log")

headers_var_gen_first_mp = c(
  
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)


# result_esp_sd_price <- run_iv_analysis(my_IV_data = IV_data_log_esp, 
#                                        headers_var_spot_first, c("delta_s_tot"), p =4, 
#                                        dependent = "spot_price_log", k = 1)
# result_esp_sd_price$fstat
# print(summary(result_esp_sd_price$m_summary))
# result_esp_sd_price$result_qty$gmin_generalized
# result_esp_sd_price$result_IPI$gmin_generalized
# result_esp_sd_price$result_HICP$gmin_generalized



vars<- ts(IV_data_log_esp[, c(headers_var_spot_first_mp)], frequency=12)

p = 12
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


class(proxies)

proxies <- ts(IV_data_log_esp[, c(headers_proxies_supply)[[1]]], frequency=12)
proxies <- ts(IV_data_log_esp$delta_s_orth, frequency=12)
proxies <- ts(IV_data_log_esp$wind_forecast, frequency=12)
proxies[is.na(proxies)] <- 0

VAR_supply <-SVARIV(ydata = x, z = IV_data_log_esp$delta_s_orth,p,
                    confidence,NWlags,
                    norm,scale,irhor,instrument_name="supply")
VAR_demand <-SVARIV(ydata = x, z = IV_data_log_esp$delta_d_orth,p,
                    confidence,NWlags,
                    norm,scale,irhor,instrument_name="supply")

VAR_wind <- SVARIV(ydata = x, z = IV_data_log_esp$wind_forecast_log,p,
                   confidence,NWlags,
                   norm,scale,irhor,instrument_name="supply")

VAR_cold <-SVARIV(ydata = x, z = IV_data_log_esp$cold_excess,p,
                    confidence,NWlags,
                    norm,scale,irhor,instrument_name="supply")

VAR_temp <-SVARIV(ydata = x, z = IV_data_log_esp$cold_excess + IV_data_log_esp$heat_excess ,p,
                  confidence,NWlags,
                  norm,scale,irhor,instrument_name="supply")

# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x, p, type = "none")
} else {
  vars::VAR(x, p, type = "none", exogen = controls)
}

#k  = ncol(proxies)
#n = ncol(y)

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

proxies <- ts(IV_data_log_esp[, c(headers_proxies_supply)[[1]]], frequency=12)
theta_0 <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = NULL, elas = 0)$theta_0
#theta_0_supply avec ortho shock -0.48

proxies_bis <- ts(IV_data_log_esp[, c("delta_s_tot")[[1]]], frequency=12)
theta_0_bis <- first_var_estimation_bis(y, res, p, proxies_bis, irhor, controls = NULL, elas = 0)$theta_0
#theta_0_supply avec shock -0.06

proxies_ter <- ts(IV_data_log_esp[, c("delta_s_orth_bis")[[1]]], frequency=12)
theta_0_ter <- first_var_estimation_bis(y, res, p, proxies_ter, irhor, controls = NULL, elas = 0)$theta_0
#theta_0_supply avec shock -0.26


AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)
lambda_0_log_supply <- get_lambda_0(C, theta_0)
nboot = 1000
bootIRFs <-gamma_bootstrap_mbb_corrected(x, nboot, vars, BlockSize, theta_0 = theta_0 , proxies = proxies, controls=NULL, elas = 0,
                                         start_ts = c(2015,1), end_ts =c(2023,12), irhor = irhor)
# 90% confidence bands (5%-95%)
IRF_90 <- compute_quantile_bands(bootIRFs, quantile_probs_90)
IRF_68 <- compute_quantile_bands(bootIRFs, quantile_probs_68)
IRF_50 <- compute_quantile_bands(bootIRFs, quantile_probs_50)[1,,]


######## Demand

proxies <- ts(IV_data_log_esp[, c("cold_excess", "heat_excess")[[1]]], frequency=12)
theta_0 <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = NULL, elas = 0)$theta_0

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
lambda_0_log <- get_lambda_0(C, theta_0)
nboot = 1000
bootIRFs <-gamma_bootstrap_mbb_corrected(x, nboot, vars, BlockSize, theta_0 = theta_0 , proxies = proxies, controls=NULL, elas = 0,
                                         start_ts = c(2015,1), end_ts =c(2023,12), irhor = irhor)
# 90% confidence bands (5%-95%)
IRF_90 <- compute_quantile_bands(bootIRFs, quantile_probs_90)
IRF_68 <- compute_quantile_bands(bootIRFs, quantile_probs_68)
IRF_50 <- compute_quantile_bands(bootIRFs, quantile_probs_50)[1,,]




IV_data_log_esp[, c("delta_s_tot", "delta_s_orth", "delta_d_tot", "delta_d_orth")] %>% 
  cor()

proxies <- ts(IV_data_log_esp[, c("delta_s_tot", "delta_d_tot")], frequency=12)

k=2
l=2
theta_0 <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = NULL, elas = -0.3)$theta_0



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
lambda_0_log <- get_lambda_0(C, theta_0)
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


bootIRFs <-gamma_bootstrap_mbb_corrected(x, nboot,  BlockSize, theta_0 = theta_0 ,
                                         proxies = proxies, controls=NULL, elas = -0.3,
                                         start_ts = c(2015,1), end_ts =c(2023,12), irhor = irhor)
dim(bootIRFs)
# 90% confidence bands (5%-95%)
IRF_90 <- compute_quantile_bands_dim3(bootIRFs, quantile_probs_90)
IRF_68 <- compute_quantile_bands_dim3(bootIRFs, quantile_probs_68)
IRF_50 <- compute_quantile_bands_dim3(bootIRFs, quantile_probs_50)[1,,,]
dim(IRF_90)

