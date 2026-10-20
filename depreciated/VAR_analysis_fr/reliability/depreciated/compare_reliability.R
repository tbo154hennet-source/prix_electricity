source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/reliability'
setwd(source_path)
source("reliability_auxiliary.R")

source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control.R")

library(MASS)
library(gt)



# most complete specification FE + controls for the whole period
#for title don't forget .png
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
  # "T_avg_log",
  "cold_excess_instrument",
  #"heat_excess_instrument",
  # "wind_forecast_log",
  # "solar_forecast_log",
  #"diff_cbf",
  #"log_diff_ex_renewable",
  #"renewable_forecast_log",
  #"renewable_forecast_hors_esp_weighted_log",
  "PC1_supply",
  "log_diff_renewable"
  # "NTC_ESP_FR_log",
  # "NTC_FR_ESP_log"
  #"nuclear_unavailabilities_log"
  # "thermal_unavailabilities_log",
  #"predicted_unavailabilities_log"
)
getwd()
save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log.parquet')


save_reliable_path <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/reliability"
setwd(save_reliable_path)
get_table(IV_data_log_esp, headers_var, headers_proxies ,p ,  "reliability_table_levels_es_with_ex_renewable.png")


save_reliable_path <- "Z:/prix_electricite_gitlab/VAR_analysis/VAR_output_fr/reliability"
setwd(save_reliable_path)
get_table(IV_data_log_fr, headers_var, headers_proxies ,p ,  "reliability_table_levels_fr.png")


demand_instruments <- c("cold_excess_instrument","heat_excess_instrument")
supply_instruments = c("PC1_supply","log_diff_renewable")


get_table_compare(headers_var,IV_data_log_esp, IV_data_log_fr,
                  demand_instruments, supply_instruments,
                  save_reliable_path, "test")

demand_instruments <- c("cold_excess_instrument","heat_excess_instrument")
supply_instruments = c("PC1_supply","log_diff_renewable", "diff_cbf")


get_table_compare(headers_var,IV_data_log_esp, IV_data_log_fr,
                  demand_instruments, supply_instruments,
                  save_reliable_path, "test_cbf")


library(gt)
  
# Rassembler les données dans un tableau unique
summary_table <- tibble::tibble(
  Pays = c("France", "France", "France", "Espagne", "Espagne", "Espagne"),
  Modèle = c("Prix (k=1)", "Demande (k=1)", "Joint (k=2)",
             "Prix (k=1)", "Demande (k=1)", "Joint (k=2)"),
  `F-statistic` = c(round(result_price_fr$fstat,2), round(result_gen_fr$fstat,2), 
                    paste(round(result_fr$fstat_price,2), round(result_fr$fstat_gen,2)),
                          round(result_price_esp$fstat,2), round(result_gen_esp$fstat,2), 
                    paste(round(result_esp$fstat_price,2), round(result_esp$fstat_gen,2)) ),
  `Cragg-Donald` = c(NA, NA, round(result_fr$cragg_donald_stat,2),
                     NA, NA, round(result_esp$cragg_donald_stat,2)),
  `Mertens and Ravn (2013)` = c(NA, NA, paste(round(result_fr$reliability_measure[1:2], 2), collapse = ", "),
                                NA, NA, paste(round(result_esp$reliability_measure[1:2], 2), collapse = ", ")),

)

# Générer le tableau avec séparation France/Espagne
iv_table <- summary_table %>%
  gt(groupname_col = "Pays") %>%
  tab_header(
    title = md("**IV Relevance**")
  ) %>%
  cols_label(
    Modèle = "Modèle",
    `F-statistic` = "F-stat",
    `Cragg-Donald` = "Cragg-Donald",
    `Mertens and Ravn (2013)` = "Mertens and Ravn (2013)"
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  tab_options(row_group.as_column = TRUE) 

getwd()
# Sauvegarder en PNG
gtsave(iv_table, filename = "iv_relevance_table.html")

  
  
