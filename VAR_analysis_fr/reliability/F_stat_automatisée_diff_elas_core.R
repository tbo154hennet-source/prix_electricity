
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/reliability'
setwd(source_path)
source("reliability_auxiliary_elas.R")

# source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr'
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
library(gtable)
library(gt)
#my_IV_data <- IV_data_log_diff_esp

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
  ) 

IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    ffr_diff = ffr - lag(as.numeric(ffr)),
    delta_s_renewable_day_ahead_forecast_diff = (delta_s_renewable_day_ahead_forecast - lag(as.numeric(delta_s_renewable_day_ahead_forecast)))/1000,
    delta_d_renewable_day_ahead_forecast_diff = (delta_d_renewable_day_ahead_forecast - lag(as.numeric(delta_d_renewable_day_ahead_forecast)))/1000,
    delta_s_cold_excess_diff = (delta_s_cold_excess - lag(as.numeric(delta_s_cold_excess)))/1000,
    delta_d_cold_excess_diff = (delta_d_cold_excess  - lag(as.numeric(delta_d_cold_excess )))/1000,
    delta_s_heat_excess_diff = (delta_s_heat_excess - lag(as.numeric(delta_s_heat_excess)))/1000,
    delta_d_heat_excess_diff = (delta_d_heat_excess - lag(as.numeric(delta_d_heat_excess)))/1000,
    delta_s_Published.Predicted.Unavailable.Capacity_diff = (delta_s_Published.Predicted.Unavailable.Capacity - lag(as.numeric(delta_s_Published.Predicted.Unavailable.Capacity)))/1000,
    delta_d_Published.Predicted.Unavailable.Capacity_diff = (delta_d_Published.Predicted.Unavailable.Capacity - lag(as.numeric(delta_d_Published.Predicted.Unavailable.Capacity)))/1000,
    delta_s_cbf_es_diff = (delta_s_cbf_es - lag(as.numeric(delta_s_cbf_es)))/1000,
    delta_d_cbf_es_diff = (delta_d_cbf_es - lag(as.numeric(delta_d_cbf_es)))/1000,
    diff_cbf = if_else(is.na(diff_cbf), 0, diff_cbf),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff),
    
    delta_s_renewable_day_ahead_forecast_diff = if_else(is.na(delta_s_renewable_day_ahead_forecast_diff), 0, delta_s_renewable_day_ahead_forecast_diff),
    delta_d_renewable_day_ahead_forecast_diff = if_else(is.na(delta_d_renewable_day_ahead_forecast_diff), 0, delta_d_renewable_day_ahead_forecast_diff),
    delta_s_cold_excess_diff = if_else(is.na(delta_s_cold_excess_diff), 0, delta_s_cold_excess_diff),
    delta_d_cold_excess_diff = if_else(is.na(delta_d_cold_excess_diff), 0, delta_d_cold_excess_diff),
    delta_s_heat_excess_diff = if_else(is.na(delta_s_heat_excess_diff), 0, delta_s_heat_excess_diff),
    delta_d_heat_excess_diff = if_else(is.na(delta_d_heat_excess_diff), 0, delta_d_heat_excess_diff),
    delta_s_Published.Predicted.Unavailable.Capacity_diff = if_else(is.na(delta_s_Published.Predicted.Unavailable.Capacity_diff), 0, delta_s_Published.Predicted.Unavailable.Capacity_diff),
    delta_d_Published.Predicted.Unavailable.Capacity_diff = if_else(is.na(delta_d_Published.Predicted.Unavailable.Capacity_diff), 0, delta_d_Published.Predicted.Unavailable.Capacity_diff),
    delta_s_cbf_es_diff = if_else(is.na(delta_s_cbf_es_diff), 0, delta_s_cbf_es_diff),
    delta_d_cbf_es_diff = if_else(is.na(delta_d_cbf_es_diff), 0, delta_d_cbf_es_diff),
    
    
    delta_s_total_diff = delta_s_renewable_day_ahead_forecast_diff  + delta_s_cold_excess_diff+
                            delta_s_heat_excess_diff + delta_s_Published.Predicted.Unavailable.Capacity_diff,
    delta_d_total_diff = delta_d_renewable_day_ahead_forecast_diff  + delta_d_cold_excess_diff+
      delta_d_heat_excess_diff + delta_d_Published.Predicted.Unavailable.Capacity_diff,
    delta_s_total_diff = if_else(is.na(delta_s_total_diff), 0, delta_s_total_diff),
    delta_d_total_diff = if_else(is.na(delta_d_total_diff), 0, delta_d_total_diff),
  )%>%
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply,
    temp_excess_instrument = temp_excess_supply
  )

#pour la baisse du 06-01 on encode par un -1

# View(IV_data_log_diff_esp %>% 
#        dplyr::select(PC1_supply, observation_date, spot_price_log_diff))

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
  mutate(
         cap = ifelse(observation_date == as.Date("2022-06-01"), (spot_gas + cap)/2, cap),
         cap = ifelse(observation_date == as.Date("2022-05-01"), spot_gas, cap),
         cap_gap_diff = ifelse(observation_date == as.Date("2022-06-01"),  cap - spot_gas , 0),
         cap_gap_diff = ifelse(observation_date == as.Date("2024-01-01"), spot_gas - lag(cap), cap_gap_diff),
         cap_gap_diff = ifelse(observation_date > as.Date("2022-06-01") & observation_date < as.Date("2024-01-01"),
                               cap - lag(cap), cap_gap_diff)
  )



  
headers_var = c( "gen_tot_log_diff",
                "spot_price_log_diff",
                "IPI_log_diff",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                #"HICP_log_diff",
                "NE_Index_log_diff"
)

colnames(IV_data_log_diff_esp)

headers_proxies_esp = c(
  "temp_excess_instrument",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  #"PC1_supply",
  #"diff_predicted_unavailabilities",
  #"diff_cbf",
  #"cap_gap_diff",
  "renewable_day_ahead_forecast_esp_log_diff"
)
headers_proxies_fr = c(
  "cold_excess_instrument",
  #"heat_excess_instrument",
  #"diff_predicted_unavailabilities",
  #"diff_cbf",
  #"PC1_supply",
  "renewable_day_ahead_forecast_fr_log_diff"
)

#dim(my_IV_data)
#IV_data_log_diff_esp$predicted_unavailabilities_log_diff
#headers_proxies = headers_proxies_esp

result_esp <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, headers_var, headers_proxies_esp, p = 12,
                                dependent = "", k = 2)
# Access outputs
print(result_esp$reliability_measure)
print(result_esp$cragg_donald_stat)
print(result_esp$fstat_price)
print(result_esp$fstat_gen)
print(summary(result_esp$m_price_summary))
print(summary(result_esp$m_demand_summary))


headers_proxies_supply <- c("delta_s_total_diff")
headers_proxies_demand <- c("delta_d_total_diff")
headers_proxies_supply_demand = c(
  "delta_s_total_diff",
  "delta_d_total_diff"
)

result_esp_sd <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, headers_var, 
                                 headers_proxies_supply_demand, p = 12,
                              dependent = "", k = 2)
# Access outputs
print(result_esp_sd$reliability_measure)
print(result_esp_sd$cragg_donald_stat)
print(result_esp_sd$fstat_price)
print(result_esp_sd$fstat_gen)
print(summary(result_esp_sd$m_price_summary))
print(summary(result_esp_sd$m_demand_summary))


result_esp_sd$result_NE_INDEX$gmin_generalized
result_esp_sd$result_IPI$gmin_generalized



headers_var_spot_first <- c("spot_price_log_diff",
                              "gen_tot_log_diff",
                              "IPI_log_diff",
                              "NE_Index_log_diff")

# result_esp_sd_price_supply <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
#                                        headers_var_spot_first, headers_proxies = headers_proxies_supply, p =12, 
#                                        dependent = "spot_price_log_diff", k = 1)
# result_esp_sd_price_supply$fstat
# print(summary(result_esp_sd_price_supply$m_summary))
# result_esp_sd_price_supply$result_gen_tot_log_diff$gmin_generalized
# result_esp_sd_price_supply$result_IPI$gmin_generalized
# result_esp_sd_price_supply$result_NE_INDEX$gmin_generalized
# 
# result_esp_sd_price_supply$result_gen_tot_log_diff$stock_yogo_test_statistic
# result_esp_sd_price_supply$result_IPI$stock_yogo_test_statistic
# result_esp_sd_price_supply$result_NE_INDEX$stock_yogo_test_statistic



result_esp_sd_price_supply <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                       headers_var_spot_first, headers_proxies = headers_proxies_supply, p =12, 
                                       dependent = "spot_price_log_diff", k = 1, cov_type = "NW")
result_esp_sd_price_supply$fstat
print(summary(result_esp_sd_price_supply$m_summary))
result_esp_sd_price_supply$result_gen_tot_log_diff$gmin_generalized
result_esp_sd_price_supply$result_IPI$gmin_generalized
result_esp_sd_price_supply$result_NE_Index$gmin_generalized

result_esp_sd_price_supply$result_gen_tot_log_diff$stock_yogo_test_statistic
result_esp_sd_price_supply$result_IPI$stock_yogo_test_statistic
result_esp_sd_price_supply$result_NE_Index$stock_yogo_test_statistic




headers_var_gen_first <- headers_var
result_esp_sd_gen_supply <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                       headers_var_gen_first, headers_proxies = headers_proxies_supply, p =12, 
                                       dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")
result_esp_sd_gen_supply$fstat
print(summary(result_esp_sd_gen_supply$m_summary))
result_esp_sd_gen_supply$result_spot_price_log_diff$gmin_generalized
result_esp_sd_gen_supply$result_IPI$gmin_generalized
result_esp_sd_gen_supply$result_NE_Index$gmin_generalized

result_esp_sd_gen_supply$result_spot_price_log_diff$stock_yogo_test_statistic
result_esp_sd_gen_supply$result_IPI$stock_yogo_test_statistic
result_esp_sd_gen_supply$result_NE_Index$stock_yogo_test_statistic





result_esp_sd_qty <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp,
                                     headers_var, headers_proxies_demand, p = 12,
                                       dependent = "gen_tot_log_diff", k = 1)
result_esp_sd_qty$result_qty$gmin_generalized
result_esp_sd_qty$result_IPI$gmin_generalized
result_esp_sd_qty$result_NE_Index_log_diff$gmin_generalized
result_esp_sd_qty$fstat
print(summary(result_esp_sd_qty$m_summary))



result_esp_sd_price_wind <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                              headers_var_spot_first, headers_proxies = c("wind_forecast_log_diff"), p =12, 
                                              dependent = "spot_price_log_diff", k = 1, cov_type = "NW")
result_esp_sd_price_wind$fstat
print(summary(result_esp_sd_price_wind$m_summary))
result_esp_sd_price_wind$result_gen_tot_log_diff$gmin_generalized
result_esp_sd_price_wind$result_IPI$gmin_generalized
result_esp_sd_price_wind$result_NE_Index$gmin_generalized

result_esp_sd_price_wind$result_gen_tot_log_diff$stock_yogo_test_statistic
result_esp_sd_price_wind$result_IPI$stock_yogo_test_statistic
result_esp_sd_price_wind$result_NE_Index$stock_yogo_test_statistic



result_esp_sd_price_supply_pre <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp %>% 
                                                filter(observation_date <= as.Date("2022-01-01")), 
                                                start_time = c(2015, 1),
                                                end_time = c(2022, 1),
                                              headers_var_spot_first, headers_proxies = headers_proxies_supply, p =12, 
                                              dependent = "spot_price_log_diff", k = 1, cov_type = "NW")
result_esp_sd_price_supply_pre$fstat
print(summary(result_esp_sd_price_supply_pre$m_summary))
result_esp_sd_price_supply_pre$result_gen_tot_log_diff$gmin_generalized
result_esp_sd_price_supply_pre$result_IPI$gmin_generalized
result_esp_sd_price_supply_pre$result_NE_Index$gmin_generalized


# result_esp_sd_qty <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp,
#                                      headers_var, headers_proxies_supply, p = 12,
#                                      dependent = "gen_tot_log_diff", k = 1)
# result_esp_sd_qty$result_qty$gmin_generalized
# result_esp_sd_qty$result_IPI$gmin_generalized
# result_esp_sd_qty$result_NE_INDEX$gmin_generalized
# result_esp_sd_qty$fstat
# print(summary(result_esp_sd_qty$m_summary))

colnames(IV_data_log_diff_esp)


ggplot(data = IV_data_log_diff_esp)+
  geom_line(aes(x = observation_date, y = delta_s_total_diff, color = "Supply Instrument")) +
  geom_line(aes(x = observation_date, y = delta_d_total_diff, color = "Demand Instrument")) +
  labs(title = "Supply and Demand Instruments Over Time",
       x = "Observation Date", y = "Instrument Value") 




# selected_ins <- c("delta_d_cold_excess" ,"delta_d_heat_excess" ,                            
# "delta_d_cbf_es" ,"delta_s_renewable_day_ahead_forecast")            
#   
# #select cols according to intuition main movement expected ex renewable supply cold_heat demand
# result_esp_sd_select <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, headers_var_spot_first, selected_ins, p =12,
#                                      dependent = "", k = 2)
# # Access outputs
# print(result_esp_sd_select$reliability_measure)
# print(result_esp_sd_select$cragg_donald_stat)
# print(result_esp_sd_select$fstat_price)
# print(summary(result_esp_sd_select$m_price_summary))
# print(result_esp_sd_select$fstat_gen)
# print(summary(result_esp_sd_select$m_demand_summary))




dim(IV_data_log_diff_esp)

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_fr <-  read_parquet('IV_data_fr_log_diff_elas.parquet')


IV_data_log_diff_fr <- IV_data_log_diff_fr %>% 
  mutate(
    diff_cbf = cbf_fr - lag(as.numeric(cbf_fr)),
    ffr_diff = ffr - lag(as.numeric(ffr)),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff),
    delta_s_renewable_day_ahead_forecast_diff = (delta_s_renewable_day_ahead_forecast - lag(as.numeric(delta_s_renewable_day_ahead_forecast)))/1000,
    delta_d_renewable_day_ahead_forecast_diff = (delta_d_renewable_day_ahead_forecast - lag(as.numeric(delta_d_renewable_day_ahead_forecast)))/1000,
    delta_s_cold_excess_instrument_diff = (delta_s_cold_excess_instrument - lag(as.numeric(delta_s_cold_excess_instrument)))/1000,
    delta_d_cold_excess_instrument_diff = (delta_d_cold_excess_instrument  - lag(as.numeric(delta_d_cold_excess_instrument )))/1000,
    delta_s_heat_excess_instrument_diff = (delta_s_heat_excess_instrument - lag(as.numeric(delta_s_heat_excess_instrument)))/1000,
    delta_d_heat_excess_instrument_diff = (delta_d_heat_excess_instrument - lag(as.numeric(delta_d_heat_excess_instrument)))/1000,
    delta_s_Published.Predicted.Unavailable.Capacity_diff = (delta_s_Published.Predicted.Unavailable.Capacity - lag(as.numeric(delta_s_Published.Predicted.Unavailable.Capacity)))/1000,
    delta_d_Published.Predicted.Unavailable.Capacity_diff = (delta_d_Published.Predicted.Unavailable.Capacity - lag(as.numeric(delta_d_Published.Predicted.Unavailable.Capacity)))/1000,
    delta_s_cbf_fr_diff = (delta_s_cbf_fr - lag(as.numeric(delta_s_cbf_fr)))/1000,
    delta_d_cbf_fr_diff = (delta_d_cbf_fr - lag(as.numeric(delta_d_cbf_fr)))/1000,
    diff_cbf = if_else(is.na(diff_cbf), 0, diff_cbf),
    delta_s_renewable_day_ahead_forecast_diff = if_else(is.na(delta_s_renewable_day_ahead_forecast_diff), 0, delta_s_renewable_day_ahead_forecast_diff),
    delta_d_renewable_day_ahead_forecast_diff = if_else(is.na(delta_d_renewable_day_ahead_forecast_diff), 0, delta_d_renewable_day_ahead_forecast_diff),
    delta_s_cold_excess_instrument_diff = if_else(is.na(delta_s_cold_excess_instrument_diff), 0, delta_s_cold_excess_instrument_diff),
    delta_d_cold_excess_instrument_diff = if_else(is.na(delta_d_cold_excess_instrument_diff), 0, delta_d_cold_excess_instrument_diff),
    delta_s_heat_excess_instrument_diff = if_else(is.na(delta_s_heat_excess_instrument_diff), 0, delta_s_heat_excess_instrument_diff),
    delta_d_heat_excess_instrument_diff = if_else(is.na(delta_d_heat_excess_instrument_diff), 0, delta_d_heat_excess_instrument_diff),
    delta_s_Published.Predicted.Unavailable.Capacity_diff = if_else(is.na(delta_s_Published.Predicted.Unavailable.Capacity_diff), 0, delta_s_Published.Predicted.Unavailable.Capacity_diff),
    delta_d_Published.Predicted.Unavailable.Capacity_diff = if_else(is.na(delta_d_Published.Predicted.Unavailable.Capacity_diff), 0, delta_d_Published.Predicted.Unavailable.Capacity_diff),
    delta_s_cbf_fr_diff = if_else(is.na(delta_s_cbf_fr_diff), 0, delta_s_cbf_fr_diff),
    delta_d_cbf_fr_diff = if_else(is.na(delta_d_cbf_fr_diff), 0, delta_d_cbf_fr_diff),
    delta_s_total_diff = delta_s_renewable_day_ahead_forecast_diff  + delta_s_cold_excess_instrument_diff+
      delta_s_heat_excess_instrument_diff + delta_s_Published.Predicted.Unavailable.Capacity_diff,
    delta_d_total_diff = delta_d_renewable_day_ahead_forecast_diff  + delta_d_cold_excess_instrument_diff+
      delta_d_heat_excess_instrument_diff + delta_d_Published.Predicted.Unavailable.Capacity_diff,
    delta_s_total_diff = if_else(is.na(delta_s_total_diff), 0, delta_s_total_diff),
    delta_d_total_diff = if_else(is.na(delta_d_total_diff), 0, delta_d_total_diff)
  )

result_fr <- run_iv_analysis(my_IV_data =  IV_data_log_diff_fr, headers_var, headers_proxies = headers_proxies_fr,
                              p =12, dependent = "", k = 2)

dim(IV_data_log_diff_fr)

# Access outputs
print(result_fr$reliability_measure)
print(result_fr$cragg_donald_stat)
print(result_fr$fstat_price)
print(summary(result_fr$m_price_summary))
print(result_fr$fstat_gen)
print(summary(result_fr$m_demand_summary))


result_fr_sd_price <- run_iv_analysis(my_IV_data = IV_data_log_diff_fr, 
                                       headers_var_spot_first, headers_proxies_supply, p =12, 
                                       dependent = "spot_price_log_diff", k = 1)
result_fr_sd_price$fstat
print(summary(result_fr_sd_price$m_summary))
result_fr_sd_price$result_qty$gmin_generalized
result_fr_sd_price$result_IPI$gmin_generalized
result_fr_sd_price$result_NE_INDEX$gmin_generalized

result_fr_sd_price$result_qty$stock_yogo_test_statistic
result_fr_sd_price$result_NE_INDEX$stock_yogo_test_statistic
result_fr_sd_price$result_IPI$stock_yogo_test_statistic





#headers_controls <- c("avg_spot_price_log_diff")

headers_var_gen_first_mp <- c("gen_tot_log_diff",
                              "spot_price_log_diff",
                              "IPI_log_diff",
                              "HICP_log_diff",
                              "ffr_diff")
#adding gas control (and mp policy)




result_esp_mp_gas <- run_iv_analysis(IV_data_log_diff_esp, headers_var_gen_first_mp, headers_proxies_esp, p = 12,
                              dependent = "", k = 2, headers_controls = headers_controls)
# Access outputs
print( result_esp_mp_gas$reliability_measure)
print( result_esp_mp_gas$cragg_donald_stat)
print( result_esp_mp_gas$fstat_price)
print(summary( result_esp_mp_gas$m_price_summary))
print( result_esp_mp_gas$fstat_gen)
print(summary( result_esp_mp_gas$m_demand_summary))

result_esp_mp_gas$result_NE_INDEX$gmin_generalized
result_esp_mp_gas$result_IPI$gmin_generalized
result_esp_mp_gas$result_FFR_DIFF$gmin_generalized

result_esp_mp_gas$result_NE_INDEX$stock_yogo_test_statistic
result_esp_mp_gas$result_IPI$stock_yogo_test_statistic
result_esp_mp_gas$result_FFR_DIFF$stock_yogo_test_statistic


result_fr_mp_gas <- run_iv_analysis(IV_data_log_diff_fr, headers_var_gen_first_mp, headers_proxies_fr, p =4,
                             dependent = "", k = 2, headers_controls = headers_controls)
# Access outputs
print(result_fr_mp_gas$reliability_measure)
print(result_fr_mp_gas$cragg_donald_stat)
print(result_fr_mp_gas$fstat_price)
print(summary(result_fr_mp_gas$m_price_summary))
print(result_fr_mp_gas$fstat_gen)
print(summary(result_fr_mp_gas$m_demand_summary))



library(gt)

# Créer le tableau avec un champ Groupe pour la séparation visuelle
summary_table <- tibble::tibble(
  Groupe = c("Baseline", "Baseline", "With gas and monetary policy", "With gas and monetary policy"),
  Pays = c("France", "Spain", "France", "Spain"),
  `F-statistic-Price` = c(
    round(result_fr$fstat_price, 2),
    round(result_esp$fstat_price, 2),
    round(result_fr_mp_gas$fstat_price, 2),
    round(result_esp_mp_gas$fstat_price, 2)
  ),
  `F-statistic-Qty` = c(
    round(result_fr$fstat_gen, 2),
    round(result_esp$fstat_gen, 2),
    round(result_fr_mp_gas$fstat_gen, 2),
    round(result_esp_mp_gas$fstat_gen, 2)
  ),
  `Cragg-Donald` = c(
    round(result_fr$cragg_donald_stat, 2),
    round(result_esp$cragg_donald_stat, 2),
    round(result_fr_mp_gas$cragg_donald_stat, 2),
    round(result_esp_mp_gas$cragg_donald_stat, 2)
  ),
  `Mertens and Ravn (2013)` = c(
    paste(round(result_fr$reliability_measure[1:2], 2), collapse = ", "),
    paste(round(result_esp$reliability_measure[1:2], 2), collapse = ", "),
    paste(round(result_fr_mp_gas$reliability_measure[1:2], 2), collapse = ", "),
    paste(round(result_esp_mp_gas$reliability_measure[1:2], 2), collapse = ", ")
  )
)

# Générer le tableau avec regroupement vertical
iv_table <- summary_table %>%
  gt(groupname_col = "Groupe") %>%
  tab_header(title = md("**IV Relevance**")) %>%
  cols_label(
    `F-statistic-Price` = "F-statistic-Price",
    `F-statistic-Qty` = "F-statistic-Qty",
    `Cragg-Donald` = "Cragg-Donald",
    `Mertens and Ravn (2013)` = "Mertens and Ravn (2013)"
  ) %>%
  fmt_missing(columns = everything(), missing_text = "-") %>%
  tab_options(row_group.as_column = FALSE)

iv_table
# Sauvegarder en HTML
gtsave(iv_table, filename = "iv_relevance_table_log_diff.html")




