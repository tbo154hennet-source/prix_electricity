library(vars)
library(varexternal)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")
source("bootstrap_ter.R")

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp_diff <-  read_parquet('IV_data_esp_log_diff_elas_sd_2026_2026.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp_diff)
IV_data_log_esp_diff$observation_date <- as.Date(IV_data_log_esp_diff$observation_date)

IV_data_log_esp_diff <- IV_data_log_esp_diff %>%
  mutate(
    ffr_diff = ffr - lag(as.numeric(ffr)),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff)
  ) 


headers_var_spot_first_mp_diff = paste0(headers_var_spot_first_mp, "_diff")

headers_var_gen_first_mp_diff =  paste0(headers_var_gen_first_mp, "_diff")

# headers_var_gen_first_mp_diff_gas <- c(headers_var_gen_first_mp_diff, "spot_gas_log")
# headers_var_spot_first_mp_diff_gas <- c(headers_var_spot_first_mp_diff, "spot_gas_log")
# 


vars_supply <- ts(IV_data_log_esp_diff[, c(headers_var_spot_first_mp_diff)], frequency=12)
vars_demand <- ts(IV_data_log_esp_diff[, c(headers_var_gen_first_mp_diff)], frequency=12)

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


IV_data_log_esp_diff %>% 
  dplyr::select(delta_s_tot, delta_d_tot,
                delta_s_orth, delta_d_orth
  ) %>% 
  cor()

X <- scale(cbind(s = IV_data_log_esp_diff$delta_s_tot , d = IV_data_log_esp_diff$delta_d_tot), center = TRUE, scale = FALSE)

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
IV_data_log_esp_diff$delta_supply <- pca$x[, 1]
IV_data_log_esp_diff$delta_demand <- pca$x[, 2]

IV_data_log_esp_diff <- IV_data_log_esp_diff %>% 
  mutate(
    delta_supply_diff = (delta_supply - lag(as.numeric(delta_supply)))/1000,
    delta_demand_diff = (delta_demand - lag(as.numeric(delta_demand)))/1000,
    delta_s_orth_diff = (delta_s_orth - lag(as.numeric(delta_s_orth)))/1000,
    delta_d_orth_diff = (delta_d_orth - lag(as.numeric(delta_d_orth)))/1000,
    delta_supply_diff = if_else(is.na(delta_supply_diff), mean(delta_supply_diff, na.rm = TRUE), delta_supply_diff),
    delta_demand_diff = if_else(is.na(delta_demand_diff),  mean(delta_demand_diff, na.rm = TRUE), delta_demand_diff),
    delta_s_orth_diff = if_else(is.na(delta_s_orth_diff),  mean(delta_s_orth_diff, na.rm = TRUE), delta_s_orth_diff),
    delta_d_orth_diff = if_else(is.na(delta_d_orth_diff),  mean(delta_d_orth_diff, na.rm = TRUE), delta_d_orth_diff)
  )

#pour normaliser par rapport à la demand
vars_demand<- ts(IV_data_log_esp_diff[, c(headers_var_gen_first_mp_diff)], frequency=12)

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


proxies <- ts(IV_data_log_esp_diff[, c("delta_demand")[[1]]], frequency=12)
theta_0_demand <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls , elas = 0)$theta_0

proxies_diff <- ts(IV_data_log_esp_diff[, c("delta_demand_diff")[[1]]], frequency=12)
theta_0_demand_diff <- first_var_estimation_bis(y, res, p, proxies_diff, irhor, controls = controls , elas = 0)$theta_0


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
lambda_0_log_demand<- apply(lambda_0_log_demand , c(2, 3), function(x) cumsum(x))
lambda_0_log_demand

lambda_0_log_supply_diff

# nboot = 1000

##### supply shock

proxies <- ts(IV_data_log_esp_diff[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

proxies_diff <- ts(IV_data_log_esp_diff[, c("delta_supply_diff")[[1]]], frequency=12)
theta_0_supply_diff <- first_var_estimation_bis(y, res, p, proxies_diff, irhor, controls =controls, elas = 0)$theta_0


lambda_0_log_supply <- get_lambda_0(C, theta_0_supply)
lambda_0_log_supply<- apply(lambda_0_log_supply , c(2, 3), function(x) cumsum(x))
lambda_0_log_supply

lambda_0_log_supply_diff <- get_lambda_0(C, theta_0_supply_diff)
lambda_0_log_supply<- apply(lambda_0_log_supply_diff , c(2, 3), function(x) cumsum(x))
lambda_0_log_supply_diff

proxies <- ts(IV_data_log_esp_diff[, c("wind_forecast_log_diff")[[1]]], frequency=12)
theta_0_wind <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = controls, elas = 0)$theta_0



VAR_supply_pc <-SVARIV(ydata = x_demand, z = IV_data_log_esp_diff$delta_supply,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")

VAR_demand_pc <-SVARIV(ydata = x_demand, z =IV_data_log_esp_diff$delta_demand,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="demand")


VAR_supply_pc_diff <-SVARIV(ydata = x_demand, z = IV_data_log_esp_diff$delta_supply_diff,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")

VAR_demand_pc_diff <-SVARIV(ydata = x_demand, z = IV_data_log_esp_diff$delta_demand_diff,p,
                            confidence,NWlags,
                            norm,scale,irhor,instrument_name="supply")




VAR_supply_pc_alternative <-SVARIV(ydata = x_supply, z = IV_data_log_esp_diff$delta_supply,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")


VAR_supply_pc_diff_alternative <-SVARIV(ydata =  x_supply, z = IV_data_log_esp_diff$delta_supply_diff,p,
                            confidence,NWlags,
                            norm,scale,irhor,instrument_name="supply")

