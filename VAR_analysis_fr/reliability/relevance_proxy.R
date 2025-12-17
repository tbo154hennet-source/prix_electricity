
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
#my_IV_data <- IV_data_log_esp

library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log_elas_sd.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)


#dim(my_IV_data)
#IV_data_log_esp$predicted_unavailabilities_log
#headers_proxies = headers_proxies_esp
headers_proxies_esp <- c("delta_s_tot", "delta_d_tot")
headers_proxies_supply_orth = c(
  "delta_s_orth"
)
headers_proxies_demand_orth = c(
  "delta_d_orth"
)

result_esp <- run_iv_analysis(my_IV_data = IV_data_log_esp, 
                              headers_var_spot_first, 
                              headers_proxies_esp, p = 12,
                                dependent = "", k = 2)

result_esp_pc <- run_iv_analysis(my_IV_data = IV_data_log_esp,
                                 headers_var_spot_first, 
                                 c("delta_supply", "delta_demand"),
                                 p = 12, dependent = "", k = 2)

summary(result_esp_pc$m_demand_summary)
summary(result_esp_pc$m_price_summary)

result_esp_sd_price_supply <- run_iv_analysis(my_IV_data = IV_data_log_esp, 
                                                   headers_var_spot_first, headers_proxies = "delta_s_tot", p =12, 
                                                   dependent = "spot_price_log", k = 1, cov_type = "NW")


result_esp_sd_price_supply_orth <- run_iv_analysis(my_IV_data = IV_data_log_esp, 
                                              headers_var_spot_first, headers_proxies = "delta_s_orth", p =12, 
                                              dependent = "spot_price_log", k = 1, cov_type = "NW")


result_esp_sd_price_supply_pc <- run_iv_analysis(my_IV_data = IV_data_log_esp, 
                                       headers_var_spot_first, headers_proxies = "delta_supply", p =12, 
                                       dependent = "spot_price_log", k = 1, cov_type = "NW")

result_esp_sd_gen_demand <- run_iv_analysis(my_IV_data = IV_data_log_esp,
                                                 headers_var_gen_first , "delta_d_tot", p = 12,
                                                 dependent = "gen_tot_log", k = 1, cov_type = "NW")

result_esp_sd_gen_demand_orth <- run_iv_analysis(my_IV_data = IV_data_log_esp,
                                     headers_var_gen_first , "delta_d_orth", p = 12,
                                       dependent = "gen_tot_log", k = 1, cov_type = "NW")

result_esp_sd_gen_demand_pc <- run_iv_analysis(my_IV_data = IV_data_log_esp,
                                            headers_var_gen_first , "delta_demand", p = 12,
                                            dependent = "gen_tot_log", k = 1, cov_type = "NW")


result_esp_sd_price_wind <- run_iv_analysis(my_IV_data = IV_data_log_esp, 
                                            headers_var_spot_first, headers_proxies = c("wind_forecast_log"), p =12, 
                                              dependent = "spot_price_log", k = 1, cov_type = "NW")




extract_stats <- function(result, label, k = 1) {
  if (k == 1) {
    tibble::tibble(
      Model = label,
      Fstat = result$fstat,
      Gmin_IPI = result$result_IPI$gmin_generalized,
      Gmin_HICP = result$result_HICP$gmin_generalized
    )
  } else if (k == 2) {
    tibble::tibble(
      Model = label,
      Fstat_price = result$fstat_price,
      Fstat_gen   = result$fstat_gen,
      Gmin_IPI = result$result_IPI$gmin_generalized,
      Gmin_HICP = result$result_HICP$gmin_generalized
    )
  } else {
    stop("k must be either 1 or 2")
  }
}

# --- petite fonction pour extraire les stats cl?s ---
# extract_stats <- function(result, label) {
#   tibble::tibble(
#     Model = label,
#     Fstat = result$fstat,
#     Gmin_IPI = result$result_IPI$gmin_generalized,
#     Gmin_HICP = result$result_HICP$gmin_generalized
#   )
# }
# results_list_ortho <- list(
#   price_supply      = result_esp_sd_price_supply,
#   gen_supply        = result_esp_sd_gen_supply,
#   qty_demand        = result_esp_sd_qty,
#   price_wind        = result_esp_sd_price_wind,
#   price_supply_pre  = result_esp_sd_price_supply_pre
# )

extract_stats_bis <- function(result, label) {
  if (!is.null(result$fstat)) {
    # k = 1 case
    tibble::tibble(
      Model = label,
      Fstat = result$fstat,
      Gmin_IPI = result$result_IPI$gmin_generalized,
      Gmin_HICP = result$result_HICP$gmin_generalized
    )
  } else if (!is.null(result$fstat_price) && !is.null(result$fstat_gen)) {
    # k = 2 case
    tibble::tibble(
      Model = label,
      Fstat_price = result$fstat_price,
      Fstat_gen   = result$fstat_gen,
      Gmin_IPI = result$result_IPI$gmin_generalized,
      Gmin_HICP = result$result_HICP$gmin_generalized
    )
  } else {
    stop("Could not detect whether k=1 or k=2: check the result structure")
  }
}

results_list_ortho <- list(
  price_supply      = result_esp_sd_price_supply,  # k = 1
  gen_demand     = result_esp_sd_gen_demand,  # k = 1
  price_supply_pc       = result_esp_sd_price_supply_pc,    # k = 1
  gen_demand_pc        = result_esp_sd_gen_demand_pc,           # k = 1
  price_wind        = result_esp_sd_price_wind,    # k = 1
  initial = result_esp                # k = 2
)

stats_table <- purrr::imap_dfr(results_list_ortho, extract_stats_bis)
stats_table <- stats_table %>% 
  mutate(Fstat = ifelse(is.na(Fstat),paste(round(Fstat_gen,1), " , ",round(Fstat_price,1)) ,round(Fstat,1)))


model_labels <- c(

  "gen_supply"        = "Supply",
  "gen_demand"        = "Demand",
  "gen_wind"        = "Wind",
  "initial"  = "Exogenous",
  "supply_demand"     = "Supply & Demand"
)

spain_relevance <- stats_table %>%
  #filter(Model != "price_supply" & Model != "price_demand") %>%
  #mutate(Model = dplyr::recode(Model, !!!model_labels)) %>%
  dplyr::select(-Fstat_price, -Fstat_gen) %>%
  gt() %>%
  fmt_number(
    columns = c(Fstat, Gmin_IPI, Gmin_HICP),
    decimals = 2
  ) %>%
  cols_label(
    Fstat     = "F-stat",
    Gmin_IPI  = "LW (IPI)",
    Gmin_HICP = "LW (HICP)"
  )%>% 
  tab_header(
    title = "Key IV Diagnostics (Spain)"
  )%>%
  tab_source_note(md("*LW = Lewis and Mertens (2024)*"))
spain_relevance


# Save if needed

save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  spain_relevance ,
  filename = "iv_relevance_spain_tot.tex"
)



ggplot(data = IV_data_log_esp)+
  geom_line(aes(x = observation_date, y = delta_s_tot, color = "Supply Instrument", linetype ="Baseline")) +
  geom_line(aes(x = observation_date, y = delta_d_tot, color = "Demand Instrument", linetype ="Baseline")) +
  geom_line(aes(x = observation_date, y = delta_supply, color = "Supply Instrument", linetype ="PCA")) +
  geom_line(aes(x = observation_date, y = delta_demand, color = "Demand Instrument", linetype ="PCA")) +
  labs(title = "Supply and Demand Instruments Over Time",
       x = "Observation Date", y = "Instrument Value") 








save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log_elas.parquet')


IV_data_log_fr <- IV_data_log_fr %>% 
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

result_fr <- run_iv_analysis(my_IV_data =  IV_data_log_fr, headers_var_gen_first , headers_proxies = headers_proxies_fr,
                              p =12, dependent = "", k = 2)

dim(IV_data_log_fr)

# Access outputs

result_fr_sd_price <- run_iv_analysis(my_IV_data = IV_data_log_fr, 
                                       headers_var_spot_first, headers_proxies_fr, p =12, 
                                       dependent = "spot_price_log", k = 1)

result_fr$result_HICP_log$gmin_generalized
result_fr_sd_price$result_HICP_log$gmin_generalized
#headers_controls <- c("avg_spot_price_log")

headers_var_gen_first_mp <- c("gen_tot_log",
                              "spot_price_log",
                              "IPI_log",
                              "HICP_log",
                              "ffr_diff")


result_esp_mp_gas <- run_iv_analysis(IV_data_log_esp, headers_var_gen_first_mp, headers_proxies_esp, p = 12,
                              dependent = "", k = 2)
# Access outputs

result_fr_mp_gas <- run_iv_analysis(IV_data_log_fr, headers_var_gen_first_mp, headers_proxies_fr, p =12,
                             dependent = "", k = 2)
# Access outputs


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
  # `Cragg-Donald` = c(
  #   round(result_fr$cragg_donald_stat, 2),
  #   round(result_esp$cragg_donald_stat, 2),
  #   round(result_fr_mp_gas$cragg_donald_stat, 2),
  #   round(result_esp_mp_gas$cragg_donald_stat, 2)
  # ),
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
    #`Cragg-Donald` = "Cragg-Donald",
    `Mertens and Ravn (2013)` = "Mertens and Ravn (2013)"
  ) %>%
  fmt_missing(columns = everything(), missing_text = "-") %>%
  tab_options(row_group.as_column = FALSE)



# save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
# setwd(save_latex_path)
# gtsave(
#   spain_relevance,
#   filename = "relevance_spain_macro.tex"
# )


save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  iv_table,
  filename = "relevance_international_macro.tex"
)



