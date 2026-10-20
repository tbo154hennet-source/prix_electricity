
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



headers_var_spot_first = c("spot_price_log_diff",
                "gen_tot_log_diff",
                "IPI_log_diff",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                "HICP_log_diff"
)

headers_var_gen_first = c("spot_price_log_diff",
                           "gen_tot_log_diff",
                           "IPI_log_diff",
                           # "ECB_rate_log_diff",
                           # "E_Index_log_diff",
                           # "Exp_pi_log_diff",
                           # "UnE_log_diff" ,
                           "HICP_log_diff"
)

headers_var_spot_first_mp <- c(headers_var_spot_first,
                                          "ffr_diff")
headers_var_gen_first_mp <- c(headers_var_gen_first,
                                          "ffr_diff")

colnames(IV_data_log_diff_esp)



headers_proxies_supply <- c("delta_s_total_diff")

#IV_data_log_diff_esp$predicted_unavailabilities_log_diff

result_esp <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                              headers_var_gen_first, headers_proxies = headers_proxies_supply, p =12, 
                                              dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")
result_esp$fstat
print(summary(result_esp$m_summary))
# result_fr$result_gen_tot_log_diff$gmin_generalized
# result_fr$result_IPI$gmin_generalized
result_esp$result_HICP$gmin_generalized



#headers_controls <- c("avg_spot_price_log_diff")


#adding gas control (and mp policy)



result_esp_mp <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                              headers_var_gen_first_mp, headers_proxies = headers_proxies_supply, p =12, 
                              dependent = "gen_tot_log_diff", k = 1, cov_type = "NW") #, headers_controls = headers_controls)
# Access outputs
result_esp_mp$fstat
print(summary(result_esp_mp$m_summary))
# result_fr$result_gen_tot_log_diff$gmin_generalized
# result_fr$result_IPI$gmin_generalized
result_esp_mp$result_HICP$gmin_generalized





save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_fr <-  read_parquet('IV_data_fr_log_diff_elas.parquet')

IV_data_log_diff_fr$observation_date <- as.Date(IV_data_log_diff_fr$observation_date)


IV_data_log_diff_fr <- IV_data_log_diff_fr %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    ffr_diff = ffr - lag(as.numeric(ffr)),
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
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff),
    
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
    delta_d_total_diff = if_else(is.na(delta_d_total_diff), 0, delta_d_total_diff),
  )

result_fr <- run_iv_analysis(my_IV_data = IV_data_log_diff_fr, 
                              headers_var_spot_first, headers_proxies = headers_proxies_supply, p =12, 
                              dependent = "spot_price_log_diff", k = 1, cov_type = "NW")
result_fr$fstat
print(summary(result_fr$m_summary))
# result_fr_sd_price_supply$result_gen_tot_log_diff$gmin_generalized
# result_fr_sd_price_supply$result_IPI$gmin_generalized
result_fr$result_HICP$gmin_generalized



headers_controls <- c("avg_spot_price_log_diff")

headers_var_spot_first_mp <- c("spot_price_log_diff",
                                          "gen_tot_log_diff",
                                          "IPI_log_diff",
                                          "HICP_log_diff",
                                          "ffr_diff")
#adding gas control (and mp policy)



result_fr_mp <- run_iv_analysis(my_IV_data = IV_data_log_diff_fr, 
                                    headers_var_spot_first_mp, headers_proxies = headers_proxies_supply, p =12, 
                                     dependent = "spot_price_log_diff", k = 1, cov_type = "NW") #, headers_controls = headers_controls)
# Access outputs
result_fr_mp$fstat
print(summary(result_fr_mp$m_summary))
# result_fr_sd_price_supply$result_gen_tot_log_diff$gmin_generalized
# result_fr_sd_price_supply$result_IPI$gmin_generalized
result_fr_mp$result_HICP$gmin_generalized





library(gt)

# Fill in your actual results
summary_table <- tibble::tibble(
  Specification = c(
    "ESP", 
    "ESP (with MP)",
    "FR",
    "FR (with MP)"
  ),
  `F-stat (Price)` = c(
    round(result_esp$fstat, 2),
    round(result_esp_mp$fstat, 2),
    round(result_fr$fstat, 2),
    round(result_fr_mp$fstat, 2)
  ),
  `LW (HICP)` = c(
    round(result_esp$result_HICP$gmin_generalized, 2),
    round(result_esp_mp$result_HICP$gmin_generalized, 2),
    round(result_fr$result_HICP$gmin_generalized, 2),
    round(result_fr_mp$result_HICP$gmin_generalized, 2)
  ),
  `LW (IPI)` = c(
    round(result_esp$result_IPI$gmin_generalized, 2),
    round(result_esp_mp$result_IPI$gmin_generalized, 2),
    round(result_fr$result_IPI$gmin_generalized, 2),
    round(result_fr_mp$result_IPI$gmin_generalized, 2)
  )
)

# Generate the table
gt_table <- summary_table %>%
  gt() %>%
  tab_header(title = md("**IV Relevance Diagnostics**")) %>%
  fmt_missing(columns = everything(), missing_text = "-") %>%
  tab_options(row_group.as_column = FALSE)%>%
  tab_source_note(md("*LW = Lewis and Mertens (2024)*"))

# Save if needed

save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table ,
  filename = "iv_relevance_compare.tex"
)

# Sauvegarder en HTML



# 
# result_esp_gen <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
#                               headers_var_gen_first, headers_proxies = headers_proxies_supply, p =12, 
#                               dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")
# 
# 
# 
# headers_var_gen_first_spot_first_mp <- c("gen_tot_log_diff",
#                                          "spot_price_log_diff",
#                                           "IPI_log_diff",
#                                           "HICP_log_diff",
#                                           "ffr_diff")
# #adding gas control (and mp policy)
# 
# 
# 
# result_esp_mp_gen <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
#                                      headers_var_gen_first_mp, headers_proxies = headers_proxies_supply, p =12, 
#                                      dependent = "gen_tot_log_diff", k = 1, cov_type = "NW") #, headers_controls = headers_controls)
# 
# 
# 
# result_fr_gen <- run_iv_analysis(my_IV_data = IV_data_log_diff_fr, 
#                              headers_var_gen_first, headers_proxies = headers_proxies_supply, p =12, 
#                              dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")
# 
# 
# result_fr_mp_gen <- run_iv_analysis(my_IV_data = IV_data_log_diff_fr, 
#                                     headers_var_gen_first_mp, headers_proxies = headers_proxies_supply, p =12, 
#                                     dependent = "gen_tot_log_diff", k = 1, cov_type = "NW") #, headers_controls = headers_controls)
# 
# 
# 
# 
# # Fill in your actual results
# summary_table_complete <- tibble::tibble(
#   Specification = c(
#     "ESP", 
#     "ESP (gas + MP)",
#     "FR",
#     "FR (gas + MP)"
#   ),
#   `F-stat Price` = c(
#     round(result_esp$fstat, 2),
#     round(result_esp_mp$fstat, 2),
#     round(result_fr$fstat, 2),
#     round(result_fr_mp$fstat, 2)
#   ),
#   `LW Price (HICP)` = c(
#     round(result_esp$result_HICP$gmin_generalized, 2),
#     round(result_esp_mp$result_HICP$gmin_generalized, 2),
#     round(result_fr$result_HICP$gmin_generalized, 2),
#     round(result_fr_mp$result_HICP$gmin_generalized, 2)
#   ),
#   `F-stat Qty` = c(
#     round(result_esp_gen$fstat, 2),
#     round(result_esp_mp_gen$fstat, 2),
#     round(result_fr_gen$fstat, 2),
#     round(result_fr_mp_gen$fstat, 2)
#   ),
#   `LW Qty (HICP)` = c(
#     round(result_esp_gen$result_HICP$gmin_generalized, 2),
#     round(result_esp_mp_gen$result_HICP$gmin_generalized, 2),
#     round(result_fr_gen$result_HICP$gmin_generalized, 2),
#     round(result_fr_mp_gen$result_HICP$gmin_generalized, 2)
#   )
# )
# 
# # Generate the table
# gt_table <- summary_table_complete %>%
#   gt() %>%
#   tab_header(title = md("**IV Relevance Diagnostics**")) %>%
#   fmt_missing(columns = everything(), missing_text = "-") %>%
#   tab_options(row_group.as_column = FALSE)%>%
#   tab_source_note(md("*LW = Lewis and Mertens (2024)*"))
# 
# # Save if needed
# 
# save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
# setwd(save_latex_path)
# gtsave(
#   gt_table ,
#   filename = "iv_relevance_compare_complete.tex"
# )


