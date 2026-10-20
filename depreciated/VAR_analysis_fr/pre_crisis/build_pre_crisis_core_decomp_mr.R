
headers_var_spot_first_decomp <- c("spot_price_log_diff", "gen_tot_log_diff", 
                                    "IPI_log_diff"  ,     "E_Index_log_diff",
                                 "NE_Index_log_diff")

headers_var_spot_first_mp_decomp <- c(headers_var_spot_first_decomp, "ffr_diff")

headers_var_gen_first_decomp <- c("gen_tot_log_diff","spot_price_log_diff",
                                  "IPI_log_diff", "E_Index_log_diff", "NE_Index_log_diff")
headers_var_gen_first_mp_decomp <- c(headers_var_gen_first_decomp, "ffr_diff")



### ESPAGNE - DECOMP ###
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

n <- length(headers_var_spot_first_decomp)
k <- 1
l <- length(headers_proxies_supply)

# Pré-crise
vars <- ts(IV_data_log_diff_esp %>%
             filter(observation_date <= as.Date("2022-01-01")) %>%
             dplyr::select(headers_var_spot_first_decomp), frequency = 12)

gen_benchmark_esp_decomp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2022, 1)
)

# Complet
vars <- ts(IV_data_log_diff_esp %>%
             dplyr::select(headers_var_spot_first_decomp), frequency = 12)

gen_benchmark_esp_decomp <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12)
)

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_decomp, "gen_benchmark_esp_decomp_supply.rds")
saveRDS(gen_benchmark_esp_decomp_pre_crisis, "gen_benchmark_esp_decomp_pre_crisis_supply.rds")


### ESPAGNE - MP ###
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

n <- length(headers_var_spot_first_mp_decomp)

# Pré-crise
vars <- ts(IV_data_log_diff_esp %>%
             filter(observation_date <= as.Date("2022-01-01")) %>%
             dplyr::select(headers_var_spot_first_mp_decomp), frequency = 12)

gen_benchmark_esp_decomp_mp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_mp_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2022, 1)
)

# Complet
vars <- ts(IV_data_log_diff_esp %>%
             dplyr::select(headers_var_spot_first_mp_decomp), frequency = 12)

gen_benchmark_esp_decomp_mp <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_mp_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12)
)

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_decomp_mp, "gen_benchmark_esp_decomp_mp_supply.rds")
saveRDS(gen_benchmark_esp_decomp_mp_pre_crisis, "gen_benchmark_esp_decomp_mp_pre_crisis_supply.rds")


### FRANCE - DECOMP ###
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

n <- length(headers_var_spot_first_decomp)

# Pré-crise
vars <- ts(IV_data_log_diff_fr %>%
             filter(observation_date <= as.Date("2022-01-01")) %>%
             dplyr::select(headers_var_spot_first_decomp), frequency = 12)

gen_benchmark_fr_decomp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_fr %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2022, 1)
)

# Complet
vars <- ts(IV_data_log_diff_fr %>%
             dplyr::select(headers_var_spot_first_decomp), frequency = 12)

gen_benchmark_fr_decomp <- run_VAR(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_spot_first_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12)
)

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_fr, "gen_benchmark_fr_decomp_supply.rds")
saveRDS(gen_benchmark_fr_pre_crisis, "gen_benchmark_fr_decomp_pre_crisis_supply.rds")


### FRANCE - MP ###
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

n <- length(headers_var_spot_first_mp_decomp)

# Pré-crise
vars <- ts(IV_data_log_diff_fr %>%
             filter(observation_date <= as.Date("2022-01-01")) %>%
             dplyr::select(headers_var_spot_first_mp_decomp), frequency = 12)

gen_benchmark_fr_mp_decomp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_fr %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_spot_first_mp_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2022, 1)
)

# Complet
vars <- ts(IV_data_log_diff_fr %>%
             dplyr::select(headers_var_spot_first_mp_decomp), frequency = 12)

gen_benchmark_fr_mp_decomp <- run_VAR(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_spot_first_mp_decomp,
  headers_proxies = headers_proxies_supply,
  p = 12,
  irhor = 20,
  nboot = 1000,
  block_size = 15,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12)
)

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_fr_mp_decomp, "gen_benchmark_fr_decomp_mp_supply.rds")
saveRDS(gen_benchmark_fr_mp_decomp_pre_crisis, "gen_benchmark_fr_decomp_mp_pre_crisis_supply.rds")
