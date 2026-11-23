
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(data.table)
library(ggplot2)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_diff_fr <-  read_parquet('IV_data_fr_diff.parquet')

# correct for the Iberian exception

IV_data_diff_fr$observation_date <- as.Date(IV_data_diff_fr$observation_date)
IV_data_diff_fr <- IV_data_diff_fr %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  )
IV_data_diff_fr <- IV_data_diff_fr %>% 
  mutate(
    diff_cbf = cbf_fr - lag(as.numeric(cbf_fr)),
    ffr_diff = ffr - lag(as.numeric(ffr)),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff)
  )

headers_var_gen_first = c("gen_tot_diff",
                          "avg_spot_price_diff",
                          "IPI_diff",
                          # "ECB_rate_diff",
                          # "E_Index_diff",
                          # "Exp_pi_diff",
                          # "UnE_diff" ,
                          "HICP_diff"
)

headers_var_gen_first_mp = c("gen_tot_diff",
                          "avg_spot_price_diff",
                          "IPI_diff",
                          "HICP_diff",
                          "ffr_diff"
)


headers_proxies = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "renewable_day_ahead_forecast_fr_diff",
  "diff_cbf"
)

proxies <- ts(IV_data_diff_fr[, c(headers_proxies)], frequency=12)
proxies[is.na(proxies)] <- 0



vars<- ts(IV_data_diff_fr[, c(headers_var_gen_first)], frequency=12)

p = 4     
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- ncol(proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)

matrix_12 <- rbind( diag(k) , zero_matrix)
headers_controls_fr <- c("Spot_gas_diff")
controls = ts(IV_data_diff_fr[, headers_controls_fr], frequency = 12)


estimate_diff <- first_var_estimation(x,y,p, proxies, irhor, controls =  controls)
theta_0_diff <- estimate_diff$theta_0
gamma_diff <- estimate_diff$gamma
phi_diff <- estimate_diff$phi
var_coef_diff <- estimate_diff$coef

AL <- do.call(cbind, lapply(var_coef_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_diff)
p <- 4  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_diff <- get_lambda_0(C, theta_0_diff)
apply(lambda_0_diff, c(2, 3), function(x) cumsum(x))






