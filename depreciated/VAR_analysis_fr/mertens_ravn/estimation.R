library(vars)
library(varexternal)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")
source("bootstrap_ter.R")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log_elas_sd_2026.parquet')


IV_data_log_fr$observation_date <- as.Date(IV_data_log_fr$observation_date)

ggplot(data = IV_data_log_fr, aes(x= observation_date))+
  geom_line(aes(y = spot_price, color = "Price"))+
  geom_line(aes(y = gen_forecast/100, color = "Generation"))+
  geom_line(aes(y = load_tot/100, color = "Load"))+
  geom_line(aes(y = Fore_Wind/10, color = "Wind"))

headers_var_spot_first_mp_all = c(
  "spot_price_log",
  "load_tot_log",
  "gen_forecast_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

headers_var_gen_first_mp_all = c(
  
  "load_tot_log",
  "spot_price_log",
  "gen_forecast_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

headers_var_spot_first_mp_gas <- c(headers_var_spot_first_mp_all,
                                   "spot_gas_log")

headers_var_gen_first_mp_gas <- c(headers_var_gen_first_mp_all,
                                  "spot_gas_log")

# headers_var_gen_first_mp_all_gas <- c(headers_var_gen_first_mp_all, "spot_gas_log")
# headers_var_spot_first_mp_all_gas <- c(headers_var_spot_first_mp_all, "spot_gas_log")
# 


vars_supply_fr <- ts(IV_data_log_fr[, c(headers_var_spot_first_mp_gas)], frequency=12)
vars_demand_fr <- ts(IV_data_log_fr[, c(headers_var_gen_first_mp_gas)], frequency=12)

p = 12
irhor = 20
#variables dans VAR
n <- ncol(vars_supply_fr)
T = nrow(vars_supply_fr) - p
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1
controls = NULL

x_supply_fr =ts(vars_supply_fr, start=c(2015,1), end=c(2025,9), frequency=12)
x_demand_fr =ts(vars_demand_fr, start=c(2015,1), end=c(2025,9), frequency=12)


names_supply <- c("Pelec"   ,    "Load"   , "Generation",
                  "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names_demand <- c(   "Load"   , "Pelec"   , "Generation",
                     "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names<- c("Pelec"   ,    "Load"   , "Generation",
                  "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 

VAR_wind_fr <-SVARIV(ydata = x_supply_fr, z = IV_data_log_fr$Fore_Wind_log,p,
                  confidence,NWlags,
                  norm,scale,irhor,instrument_name="supply")


IV_data_log_fr %>% 
  dplyr::select(delta_demand_fr, delta_supply_fr, wind_forecast_log) %>% 
  cor(use = "complete.obs")
#######
unique(VAR_wind_fr$irfs$type)
colnames(VAR_wind_fr$irfs)

theta_0_wind_fr <- VAR_wind_fr$irfs %>% 
  filter(confi_type == "msw" & horizon==1 & type == "point" & confi_level == 0.68)

#pour normaliser par rapport à la demand

controls = NULL
# Fit the VAR model
vl_fr <- if (is.null(controls)) {
  vars::VAR(x_demand_fr, p, type = "const")
} else {
  vars::VAR(x_demand_fr, p, type = "const", exogen = controls)
}



coef = coef(vl_fr)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res_fr = residuals(vl_fr)



proxies <- ts(IV_data_log_fr[, c("Fore_Wind_log", "temperature_discomfort" )], frequency=12)
k=2
l=2
theta_0_double_fr <- first_var_estimation_bis( x= x_demand_fr, res_fr, p, proxies, irhor, controls = NULL, elas = theta_0_wind_fr$value[2])$theta_0


AL <- do.call(cbind, lapply(coef(vl_fr), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl_fr))
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon

# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)
lambda_0_log_double_fr <- get_lambda_0(C, theta_0_double_fr)

theta_0_double_fr
lambda_0_log_double_fr


irf_cholesky_fr <- irf(vl_fr, impulse = colnames(x_demand_fr), response = colnames(x_demand_fr),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

lambda_0_log_double_fr_supply <- lambda_0_log_double_fr[,c(c(2,1),3:n),2]
lambda_0_log_double_fr_demand <- lambda_0_log_double_fr[,,1]
irf_cholesky_supply_fr <- irf_cholesky_fr$irf$spot_price_log[,c(c(2,1),3:n)]/irf_cholesky_fr$irf$spot_price_log[1,2]
irf_cholesky_demand_fr <- irf_cholesky_fr$irf$load_tot_log/irf_cholesky_fr$irf$load_tot_log[1,1]

