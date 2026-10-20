library(vars)
library(varexternal)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")
source("bootstrap_ter.R")

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log_elas_sd_2026.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)


headers_var_spot_first_mp = c(
  "spot_price_log",
  "load_tot_log",
  "gen_tot_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

headers_var_gen_first_mp = c(
  
  "load_tot_log",
  "spot_price_log",
  "gen_tot_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

# headers_var_gen_first_mp_gas <- c(headers_var_gen_first_mp, "spot_gas_log")
# headers_var_spot_first_mp_gas <- c(headers_var_spot_first_mp, "spot_gas_log")
# 


vars_supply <- ts(IV_data_log_esp[, c(headers_var_spot_first_mp)], frequency=12)
vars_demand <- ts(IV_data_log_esp[, c(headers_var_gen_first_mp)], frequency=12)

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


IV_data_log_esp %>% 
  dplyr::select(delta_s_tot, delta_d_tot,
                delta_s_orth, delta_d_orth
  ) %>% 
  cor()

X <- scale(cbind(s = IV_data_log_esp$delta_s_tot , d = IV_data_log_esp$delta_d_tot), center = TRUE, scale = FALSE)

pca <- prcomp(X)

# Valeurs propres (variances) des composantes principales
eigenvalues <- pca$sdev^2


# Loadings
loadings <- pca$rotation

# Eigenvalues
eigenvalues <- pca$sdev^2

# Variance totale (somme des variances des variables)
total_var <- sum(apply(X, 2, stats::var))

# Part de variance expliquée par variable et par composante
var_by_var_pc <- sweep(loadings^2, 2, eigenvalues, `*`) / total_var

var_by_var_pc

# Variance expliquée par chaque composante
variance_explained <- eigenvalues / sum(eigenvalues)
variance_explained


# Reconstruction
IV_data_log_esp$delta_supply <- pca$x[, 1]
IV_data_log_esp$delta_demand <- pca$x[, 2]


####### With MSW
confidence = c(0.68, 0.9)
NWlags= 0
norm        = 1 # Variable used for normalization
scale       = 1

dim(x_demand)
length(z)
VAR_supply_pc_ng <-SVARIV(ydata = x_supply, z = IV_data_log_esp$delta_supply,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")


VAR_demand_pc_ng <-SVARIV(ydata = x_demand, z = IV_data_log_esp$delta_demand,p,
                       confidence,NWlags,
                       norm,scale,horizons = irhor,instrument_name="demand")

names_supply <- c("Pelec"   ,    "Qelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names_demand <- c(   "Qelec"   , "Pelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 

VAR_wind_ng <-SVARIV(ydata = x_demand, z = IV_data_log_esp$wind_forecast_log,p,
                  confidence,NWlags,
                  norm,scale,irhor,instrument_name="supply")


IV_data_log_esp %>% 
  dplyr::select(delta_demand, delta_supply, wind_forecast_log) %>% 
  cor(use = "complete.obs")
#######



#pour normaliser par rapport à la demand
vars_demand<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp)], frequency=12)

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
  vars::VAR(x_demand, p, type = "const")
} else {
  vars::VAR(x_demand, p, type = "const", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)


proxies <- ts(IV_data_log_esp[, c("delta_demand")[[1]]], frequency=12)
theta_0_demand_ng <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

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
lambda_0_log_demand_ng <- get_lambda_0(C, theta_0 = theta_0_demand_ng)
# nboot = 1000
# bootIRFs_demand <-gamma_bootstrap_mbb_corrected(x_demand, nboot, theta_0 = theta_0_demand , proxies = proxies, controls=NULL, elas = 0,
#                                                         start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# 

##### supply shock

proxies <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply_ng <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_ng <- get_lambda_0(C, theta_0_supply_ng)
# nboot = 1000
# bootIRFs_supply <-gamma_bootstrap_mbb_corrected(x_demand, nboot, theta_0 = theta_0_supply , 
#                                                         proxies = proxies, controls = NULL, elas = 0,
#                                                         start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# # 

proxies <- ts(IV_data_log_esp[, c("wind_forecast_log")[[1]]], frequency=12)
theta_0_wind_ng <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls, elas = 0)$theta_0





