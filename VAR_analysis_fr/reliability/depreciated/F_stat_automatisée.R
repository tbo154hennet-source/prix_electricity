
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/reliability'
setwd(source_path)
source("reliability_auxiliary.R")

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
#my_IV_data <- IV_data_log_esp

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)
IV_data_log_esp <- IV_data_log_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply
  )

#pour la baisse du 06-01 on encode par un -1

# View(IV_data_log_esp %>% 
#        dplyr::select(PC1_supply, observation_date, spot_price_log))

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
  #"predicted_unavailabilities_log",
  #"log_diff_wind",
  #"log_diff_solar",
  #log_diff_cloud_cover
)



headers_var = c("spot_price_log",
                "gen_tot_log",
                "IPI_log",
                # "ECB_rate_log",
                # "E_Index_log",
                # "Exp_pi_log",
                # "UnE_log" ,
                "HICP_log"
)




# headers_proxies = c(
#   "temp_excess_supply",
#   #"log_diff_pred_unavail",
#   #"PC1_supply",
#   "log_diff_renewable",
#   "diff_cbf"
# )

headers_proxies = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  #"log_diff_pred_unavail",
  #"PC1_supply",
  "log_diff_renewable",
  "diff_cbf"
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


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log.parquet')



result_fr <- run_iv_analysis(IV_data_log_fr, headers_var, headers_proxies, p = 4,
                              dependent = "", k = 2)
# Access outputs
print(result_fr$reliability_measure)
print(result_fr$cragg_donald_stat)
print(result_fr$fstat_price)
print(summary(result_fr$m_price_summary))
print(result_fr$fstat_gen)
print(summary(result_fr$m_demand_summary))



#adding gas control (and mp policy)




result_esp_mp_gas <- run_iv_analysis(IV_data_log_esp, headers_var_gen_first_mp, headers_proxies, p = 4,
                              dependent = "", k = 2, headers_controls = headers_controls_esp)
# Access outputs
print( result_esp_mp_gas$reliability_measure)
print( result_esp_mp_gas$cragg_donald_stat)
print( result_esp_mp_gas$fstat_price)
print(summary( result_esp_mp_gas$m_price_summary))
print( result_esp_mp_gas$fstat_gen)
print(summary( result_esp_mp_gas$m_demand_summary))





result_fr_mp_gas <- run_iv_analysis(IV_data_log_fr, headers_var_gen_first_mp, headers_proxies, p = 4,
                             dependent = "", k = 2, headers_controls = headers_controls_fr)
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
  Pays = c("France", "Espagne", "France", "Espagne"),
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

# Sauvegarder en HTML
gtsave(iv_table, filename = "iv_relevance_table.html")




