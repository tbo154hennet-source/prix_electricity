library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff_elas.parquet')

# correct for the Iberian exception
colnames(IV_data_log_diff_esp)
IV_data_log_diff_esp$observation_date <- as.Date(IV_data_log_diff_esp$observation_date)
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply,
    temp_excess_instrument = temp_excess_supply
  )
IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    ffr_diff = ffr - lag(as.numeric(ffr))
  )%>%
  fill(ffr_diff, diff_cbf, .direction = "up")

#Start date	June 15, 2022
iberian_cap <- data.frame(
  Month = c("2022-06", "2022-07", "2022-08", "2022-09", "2022-10", "2022-11",
            "2022-12", "2023-01", "2023-02", "2023-03",
            "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09",
            "2023-10", "2023-11", "2023-12"),
  cap = c(40, 40, 40, 40, 40, 40, 40,
          45, 50, 55, 56.1, 57.2, 58.3,
          59.4, 60.6, 61.7, 62.8, 63.9,	65)
) %>%
  mutate(date = as.Date(paste0(Month, "-01")),
         year = as.numeric(year(date)),
         month = month(date))

#on veut construire un instrument supplémentaire qui est la différence
#entre le prix du gas et le prix du MWh de gas et le prix payé subventionné
# et le prix qui était payé à la période précédente

IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  left_join(iberian_cap, by = c("year", "month")) %>%
  mutate(cap = ifelse(observation_date == as.Date("2022-06-01"), (spot_gas + cap)/2, cap),
         cap_gap_diff = ifelse(observation_date == as.Date("2022-06-01"),  cap - spot_gas , 0),
         cap_gap_diff = ifelse(observation_date == as.Date("2024-01-01"), spot_gas - lag(cap), cap_gap_diff),
         cap_gap_diff = ifelse(observation_date > as.Date("2022-06-01") & observation_date < as.Date("2024-01-01"),
                               cap - lag(cap), cap_gap_diff)
  )
IV_data_log_diff_esp %>% 
  filter(observation_date >= as.Date("2022-05-01") & observation_date <= as.Date("2022-07-01")  ) %>%
  dplyr::select(spot_gas, cap, cap_gap_diff, avg_spot_price, spot_price)



headers_proxies_esp = c(
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  "temp_excess_instrument",
  "diff_cbf",
  "cap_gap_diff",
  "renewable_day_ahead_forecast_esp_log_diff"
)
headers_var_gen_first = c("gen_tot_log_diff",
                          "spot_price_log_diff",
                          "IPI_log_diff",
                          # "ECB_rate_log_diff",
                          # "E_Index_log_diff",
                          # "Exp_pi_log_diff",
                          # "UnE_log_diff" ,
                          "HICP_log_diff"
)


headers_var_gen_first_mp = c("gen_tot_log_diff",
                          "spot_price_log_diff",
                          "IPI_log_diff",
                          "HICP_log_diff",
                          "ffr_diff"
)



proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_esp)], frequency=12)
proxies[is.na(proxies)] <- 0

vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)

p = 12
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

#controls = ts(IV_data_log_diff_esp[, headers_controls_esp], frequency = 12)
#nbre de variables dans la VAR
headers_controls = c("avg_spot_price_log_diff")

library(vars)

# Tester jusqu'à 12 retards
lag_selection <- VARselect(x, lag.max = 24, type = "const")

# Afficher le tableau des critères
print(lag_selection$selection)


lag_selection_bis <- VARselect(x, lag.max = 18, type = "const")
print(lag_selection_bis$selection)

lag_selection_ter <- VARselect(x, lag.max = 12, type = "const")
print(lag_selection_ter$selection)

estimate_log_diff <- first_var_estimation(x,y,p, proxies, irhor, elas = -0.16)
theta_0_log_diff <- estimate_log_diff$theta_0
gamma_log_diff <- estimate_log_diff$gamma
phi_log_diff <- estimate_log_diff$phi
var_coef_log_diff <- estimate_log_diff$coef

AL <- do.call(cbind, lapply(var_coef_log_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log_diff)
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_log_diff <- get_lambda_0(C, theta_0_log_diff)
cumulative_response <- apply(lambda_0_log_diff , c(2, 3), function(x) cumsum(x))

#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
print(passthrough_esp)

