library(vars)
library(varexternal)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")
source("bootstrap_ter.R")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log_elas_sd_2026_2026.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)

IV_data_log_esp$wedge_iber_exception <- IV_data_log_esp$spot_gas - IV_data_log_esp$spot_gas_capped
IV_data_log_esp<- IV_data_log_esp %>% 
  mutate(wedge_iber_exception= ifelse(wedge_iber_exception > 10, wedge_iber_exception, 0 ))


ggplot(data = IV_data_log_esp)+
  geom_line(aes(x= observation_date, y=spot_gas, color = "Gas price" ))+
  geom_line(aes(x= observation_date, y=spot_gas_capped, color = "Gas price" ))+
  geom_line(aes(x= observation_date, y=wedge_iber_exception, color = "Wedge" ))

ggplot(data = IV_data_log_esp)+
  geom_line(aes(x= observation_date, y=spot_price_log, color = "Elec price" ))+
  geom_line(aes(x= observation_date, y=spot_price_log_resid, color = "Elec price" ))+
  geom_line(aes(x= observation_date, y=wedge_iber_exception/10, color = "Wedge" ))


headers_var_spot_first_mp = c(
  "spot_price_log_resid",
  "gen_tot_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

headers_var_gen_first_mp = c(
  
  "gen_tot_log",
  "spot_price_log_resid",
  "IPI_log",
  "HICP_log",
  "ffr"
)

headers_var_gen_first_mp_gas <- c(headers_var_gen_first_mp, "spot_gas_log")
headers_var_spot_first_mp_gas <- c(headers_var_spot_first_mp, "spot_gas_log")

#IV_data_log_esp$spot_price_log_resid <- lm(spot_price_log ~ wedge_iber_exception, data = IV_data_log_esp)$resid
m<-lm(spot_price~ wedge_iber_exception, data = IV_data_log_esp)
summary(m)

IV_data_log_esp$spot_price_log_resid <- log(m$resid + m$coefficients[["(Intercept)"]] )
IV_data_log_esp$spot_price_log_resid - IV_data_log_esp$spot_price_log

vars_supply_resid <- ts(IV_data_log_esp[, c(headers_var_spot_first_mp_gas)], frequency=12)
vars_demand_resid <- ts(IV_data_log_esp[, c(headers_var_gen_first_mp_gas)], frequency=12)

p = 12
irhor = 20
#variables dans VAR
n <- ncol(vars_supply_resid)
T = nrow(vars_supply_resid) - p
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1
controls = NULL

x_supply_resid =ts(vars_supply_resid, start=c(2015,1), end=c(2025,9), frequency=12)
x_demand_resid =ts(vars_demand_resid, start=c(2015,1), end=c(2025,9), frequency=12)


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
IV_data_log_esp$delta_supply_resid <- pca$x[, 1]
IV_data_log_esp$delta_demand_resid <- pca$x[, 2]


####### With MSW
confidence = c(0.68, 0.9)
NWlags= 0
norm        = 1 # Variable used for normalization
scale       = 1

VAR_supply_resid_pc <-SVARIV(ydata = x_demand_resid, z = IV_data_log_esp$delta_supply_resid,p,
                          confidence,NWlags,
                          norm,scale,irhor,instrument_name="supply")


VAR_demand_resid_pc <-SVARIV(ydata = x_demand_resid, z = IV_data_log_esp$delta_demand_resid,p,
                          confidence,NWlags,
                          norm,scale,irhor,instrument_name="demand")

names_supply_resid <- c("Pelec"   ,    "Qelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names_demand_resid <- c(   "Qelec"   , "Pelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 

VAR_wind <-SVARIV(ydata = x_demand_resid, z = IV_data_log_esp$wind_forecast_log,p,
                  confidence,NWlags,
                  norm,scale,irhor,instrument_name="supply")


IV_data_log_esp %>% 
  select(delta_demand_resid, delta_supply_resid, wind_forecast_log) %>% 
  cor(use = "complete.obs")
#######



#pour normaliser par rapport à la demand
vars_demand_resid<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp_gas)], frequency=12)

p = 12
irhor = 20
x_demand_resid =ts(vars_demand_resid , start=c(2015,1), end=c(2025,9), frequency=12)
y =vars_demand_resid[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_demand_resid, p, type = "none")
} else {
  vars::VAR(x_demand_resid, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)


proxies <- ts(IV_data_log_esp[, c("delta_demand_resid")[[1]]], frequency=12)
theta_0_demand_resid <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

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
lambda_0_log_demand_resid <- get_lambda_0(C, theta_0_demand_resid)
# nboot = 1000
bootIRFs_demand_resid <-gamma_bootstrap_mbb_corrected(x_demand_resid, nboot, theta_0 = theta_0_demand_resid , proxies = proxies, controls=NULL, elas = 0,
                                                   start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


##### supply shock

proxies <- ts(IV_data_log_esp[, c("delta_supply_resid")[[1]]], frequency=12)
theta_0_supply_resid <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_resid <- get_lambda_0(C, theta_0_supply_resid)
# nboot = 1000
bootIRFs_supply_resid <-gamma_bootstrap_mbb_corrected(x_demand_resid, nboot, theta_0 = theta_0_supply_resid , 
                                                   proxies = proxies, controls = NULL, elas = 0,
                                                   start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)
# 

proxies <- ts(IV_data_log_esp[, c("wind_forecast_log")[[1]]], frequency=12)
theta_0_wind <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls, elas = 0)$theta_0





