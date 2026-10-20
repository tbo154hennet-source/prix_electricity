
#compare with IRFs from the pre-period
#until the beginnning of the bouclier tarifaire


colnames(IV_data_log_diff_esp)

# View(IV_data_log_diff_esp %>% 
#        dplyr::select(year, month, observation_date))
# class(IV_data_log_diff_esp$observation_date)


##############################################
#                 ESPAGNE
##############################################

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

#---------------------------
# ESP - Simple - pré-crise
#---------------------------
gen_benchmark_esp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 1)
)

saveRDS(gen_benchmark_esp_pre_crisis, "gen_benchmark_esp_pre_crisis_supply_sd.rds")

# ESP - Simple - avec crise
gen_benchmark_esp_simple <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12)
)

saveRDS(gen_benchmark_esp_simple, "gen_benchmark_esp_supply_with_crisis_sd.rds")

#---------------------------
# ESP - MP - pré-crise
#---------------------------
gen_benchmark_esp_mp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_mp,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 1)
)

saveRDS(gen_benchmark_esp_mp_pre_crisis, "gen_benchmark_esp_mp_pre_crisis_supply_sd.rds")

# ESP - MP - avec crise
gen_benchmark_esp_mp_simple <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_mp,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12)
)

saveRDS(gen_benchmark_esp_mp_simple, "gen_benchmark_esp_mp_supply_with_crisis_sd.rds")


##############################################
#                 FRANCE
##############################################



headers_var_spot_first_core <- c("spot_price_log_diff", "gen_tot_log_diff",    "IPI_log_diff"  ,     
                                 "NE_Index_log_diff")

headers_var_spot_first_mp_core <- c(headers_var_spot_first_core, "ffr_diff")

headers_var_gen_first_core <- c("gen_tot_log_diff","spot_price_log_diff",  "IPI_log_diff", "NE_Index_log_diff")
headers_var_gen_first_mp_core <- c(headers_var_gen_first_core, "ffr_diff")


save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

#---------------------------
# ESP - Simple - pré-crise
#---------------------------
gen_benchmark_esp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_core,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 1)
)

saveRDS(gen_benchmark_esp_pre_crisis, "gen_benchmark_esp_pre_crisis_supply_core_sd.rds")

# ESP - Simple - avec crise
gen_benchmark_esp_simple <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_core,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12)
)

saveRDS(gen_benchmark_esp_simple, "gen_benchmark_esp_supply_with_crisis_core_sd.rds")

#---------------------------
# ESP - MP - pré-crise
#---------------------------
gen_benchmark_esp_mp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_mp_core,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 1)
)

saveRDS(gen_benchmark_esp_mp_pre_crisis, "gen_benchmark_esp_mp_pre_crisis_supply_core_sd.rds")

# ESP - MP - avec crise
gen_benchmark_esp_mp_simple <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_mp_core,
  headers_proxies = headers_proxies_supply,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12)
)

saveRDS(gen_benchmark_esp_mp_simple, "gen_benchmark_esp_mp_supply_with_crisis_core_sd.rds")













