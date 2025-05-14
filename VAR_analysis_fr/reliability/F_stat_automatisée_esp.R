
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/reliability'
setwd(source_path)
source("reliability_auxiliary.R")

# source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr'
# setwd(source_path)
# source("auxiliary_control.R")

library(MASS)
library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
library(purrr)
#my_IV_data <- IV_data_log_esp

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')


headers_proxies = c(
  # "T_avg_log",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  # "wind_forecast_log",
  # "solar_forecast_log",
  #"cbf_es_log",
  #"renewable_forecast_log",
  #"renewable_forecast_hors_esp_weighted_log",
  #"PC1_supply",
  #"log_diff_renewable"
  # "NTC_ESP_FR_log",
  # "NTC_FR_ESP_log"
  #"nuclear_unavailabilities_log"
  # "thermal_unavailabilities_log",
  #"predicted_unavailabilities_log"
)



headers_var = c("spot_price_log",
                "gen_tot_log",
                "IPI_log",
                # "ECB_rate_log",
                # "E_Index_log",
                # "Exp_pi_log",
                # "UnE_log" ,
                "NE_Index_log"
)

headers_proxies = c(
  "PC1_supply",
  "log_diff_renewable"#, "log_diff_pred_unavail"
)


result_price_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                          dependent = "spot_price_log", k = 1)
# Access outputs
print(result_price_esp$fstat)
print(summary(result_price_esp$m_summary))

headers_proxies = c(
  "temp_excess_instrument"
)
result_gen_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                          dependent = "gen_tot_log", k = 1)
# Access outputs
print(result_gen_esp$fstat)
print(summary(result_gen_esp$m_summary))



headers_proxies = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "PC1_supply",
  "log_diff_renewable"#,"log_diff_pred_unavail"
  
)

colnames(IV_data_log_esp)

result_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                                dependent = "", k = 2)
# Access outputs
print(result_esp$reliability_measure)
print(result_esp$cragg_donald_stat)
print(result_esp$fstat_price)
print(summary(result_esp$m_price_summary))
print(result_esp$fstat_gen)
print(summary(result_esp$m_demand_summary))



