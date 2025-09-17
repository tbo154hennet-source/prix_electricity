
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
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff_elas_exo.parquet')

# correct for the Iberian exception
colnames(IV_data_log_diff_esp)
IV_data_log_diff_esp$observation_date <- as.Date(IV_data_log_diff_esp$observation_date)
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply),
    ffr_diff = ffr - lag(as.numeric(ffr)),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff)
  ) 

IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    #ffr_diff = ffr - lag(as.numeric(ffr)),
    delta_s_total_diff = delta_s_total - lag(as.numeric(delta_s_total))/1000,
    delta_d_total_diff = delta_d_total - lag(as.numeric(delta_d_total))/1000,
    delta_s_orth_diff = delta_s_orth - lag(as.numeric(delta_s_orth))/1000,
    delta_d_orth_diff = delta_d_orth - lag(as.numeric(delta_d_orth))/1000,
    delta_s_total_diff = if_else(is.na(delta_s_total_diff), mean(delta_s_total_diff, na.rm = TRUE), delta_s_total_diff),
    delta_d_total_diff = if_else(is.na(delta_d_total_diff),  mean(delta_d_total_diff, na.rm = TRUE), delta_d_total_diff),
    delta_s_orth_diff = if_else(is.na(delta_s_orth_diff),  mean(delta_s_orth_diff, na.rm = TRUE), delta_s_orth_diff),
    delta_d_orth_diff = if_else(is.na(delta_d_orth_diff),  mean(delta_d_orth_diff, na.rm = TRUE), delta_d_total_diff),
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



  
headers_var_gen_first = c( "gen_tot_log_diff",
                "spot_price_log_diff",
                "IPI_log_diff",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                "HICP_log_diff"
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

result_esp <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, headers_var_gen_first , headers_proxies_esp, p = 12,
                                dependent = "", k = 2)
# Access outputs
print(result_esp$reliability_measure)
print(result_esp$cragg_donald_stat)
print(result_esp$fstat_price)
print(result_esp$fstat_gen)
print(summary(result_esp$m_price_summary))
print(summary(result_esp$m_demand_summary))

result_esp$result_HICP$gmin_generalized
result_esp$result_IPI$gmin_generalized


headers_proxies_supply <- c("delta_s_total_diff")
headers_proxies_demand <- c("delta_d_total_diff")
headers_proxies_supply_demand = c(
  "delta_s_total_diff",
  "delta_d_total_diff"
)

# headers_proxies_supply <- c("delta_s_orth_diff")
# headers_proxies_demand <- c("delta_d_orth_diff")
# headers_proxies_supply_demand = c(
#   "delta_s_orth_diff",
#   "delta_d_orth_diff"
# )


result_esp_sd <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, headers_var_gen_first , headers_proxies_supply_demand, p = 12,
                              dependent = "", k = 2)
# Access outputs
print(result_esp_sd$reliability_measure)
print(result_esp_sd$cragg_donald_stat)
print(result_esp_sd$fstat_price)
print(result_esp_sd$fstat_gen)
print(summary(result_esp_sd$m_price_summary))
print(summary(result_esp_sd$m_demand_summary))


result_esp_sd$result_HICP$gmin_generalized
result_esp_sd$result_IPI$gmin_generalized



headers_var_spot_first <- c("spot_price_log_diff",
                              "gen_tot_log_diff",
                              "IPI_log_diff",
                              "HICP_log_diff")


result_esp_sd_price_supply <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                       headers_var_spot_first, headers_proxies = headers_proxies_supply, p =12, 
                                       dependent = "spot_price_log_diff", k = 1, cov_type = "NW")


result_esp_sd_gen_supply <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                       headers_var_gen_first, headers_proxies = headers_proxies_supply, p =12, 
                                       dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")


result_esp_sd_gen_demand <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp,
                                     headers_var_gen_first , headers_proxies_demand, p = 12,
                                       dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")


result_esp_sd_price_demand <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp,
                                     headers_var_gen_first , headers_proxies_demand, p = 12,
                                     dependent = "spot_price_log_diff", k = 1, cov_type = "NW")


result_esp_sd_gen_wind <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
                                            headers_var_gen_first, headers_proxies = c("wind_forecast_log_diff"), p =12, 
                                              dependent = "gen_tot_log_diff", k = 1, cov_type = "NW")




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

# --- petite fonction pour extraire les stats cl�s ---
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
  price_demand      = result_esp_sd_price_demand,  # k = 1
  gen_supply        = result_esp_sd_gen_supply,    # k = 1
  gen_demand        = result_esp_sd_gen_demand,           # k = 1
  gen_wind        = result_esp_sd_gen_wind,    # k = 1
  initial = result_esp,
  supply_demand     = result_esp_sd                # k = 2
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
  filter(Model != "price_supply" & Model != "price_demand") %>%
  mutate(Model = dplyr::recode(Model, !!!model_labels)) %>%
  dplyr::select(-Fstat_price, -Fstat_gen) %>%
  gt() %>%
  fmt_number(
    columns = c(Fstat, Gmin_IPI, Gmin_HICP),
    decimals = 2
  ) %>%
  tab_header(
    title = "Key IV Diagnostics (Spain)"
  )
spain_relevance

ggplot(data = IV_data_log_diff_esp)+
  geom_line(aes(x = observation_date, y = delta_s_total_diff, color = "Supply Instrument", linetype ="Baseline")) +
  geom_line(aes(x = observation_date, y = delta_d_total_diff, color = "Demand Instrument", linetype ="Baseline")) +
  geom_line(aes(x = observation_date, y = delta_s_orth_diff, color = "Supply Instrument", linetype ="Ortho")) +
  geom_line(aes(x = observation_date, y = delta_d_orth_diff, color = "Demand Instrument", linetype ="Ortho")) +
  labs(title = "Supply and Demand Instruments Over Time",
       x = "Observation Date", y = "Instrument Value") 











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

result_fr <- run_iv_analysis(my_IV_data =  IV_data_log_diff_fr, headers_var_gen_first , headers_proxies = headers_proxies_fr,
                              p =12, dependent = "", k = 2)

dim(IV_data_log_diff_fr)

# Access outputs

result_fr_sd_price <- run_iv_analysis(my_IV_data = IV_data_log_diff_fr, 
                                       headers_var_spot_first, headers_proxies_fr, p =12, 
                                       dependent = "spot_price_log_diff", k = 1)

result_fr$result_HICP_LOG_DIFF$gmin_generalized
result_fr_sd_price$result_HICP_log_diff$gmin_generalized
#headers_controls <- c("avg_spot_price_log_diff")

headers_var_gen_first_mp <- c("gen_tot_log_diff",
                              "spot_price_log_diff",
                              "IPI_log_diff",
                              "HICP_log_diff",
                              "ffr_diff")


result_esp_mp_gas <- run_iv_analysis(IV_data_log_diff_esp, headers_var_gen_first_mp, headers_proxies_esp, p = 12,
                              dependent = "", k = 2)
# Access outputs

result_fr_mp_gas <- run_iv_analysis(IV_data_log_diff_fr, headers_var_gen_first_mp, headers_proxies_fr, p =12,
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



save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  spain_relevance,
  filename = "relevance_spain_macro.tex"
)


save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  iv_table,
  filename = "relevance_international_macro.tex"
)



