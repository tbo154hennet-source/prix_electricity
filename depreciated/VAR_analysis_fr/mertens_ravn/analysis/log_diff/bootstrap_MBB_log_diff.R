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
    heat_excess_instrument = heat_excess_supply
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
  "cold_excess_instrument",
  "heat_excess_instrument",
  #"PC1_supply",
  "renewable_day_ahead_forecast_esp_log_diff",
  #"diff_predicted_unavailabilities",
  "diff_cbf",
  "cap_gap_diff"
)
headers_proxies_fr = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  #"PC1_supply",
  "renewable_day_ahead_forecast_fr_log_diff",
  #"diff_predicted_unavailabilities",
  "diff_cbf"
)

headers_var_gen_first = c(
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  # "ECB_rate_log_diff",
  # "E_Index_log_diff",
  # "Exp_pi_log_diff",
  # "UnE_log_diff" ,
  "HICP_log_diff"
)

headers_var_gen_first_decomp = c(
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "E_Index_log_diff",
  "NE_Index_log_diff"
)


headers_var_spot_first = c(
  "spot_price_log_diff",
  "gen_tot_log_diff",
  "IPI_log_diff",
  # "ECB_rate_log_diff_log_diff",
  # "E_Index_log_diff_log_diff",
  # "Exp_pi_log_diff_log_diff",
  # "UnE_log_diff_log_diff" ,
  "HICP_log_diff"
)

headers_var_gen_first_mp <- c(
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff",
  "ffr_diff"
)

#attention
#avg spot price et last_spot_price sont les prix du gas


quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_prob_50 <- c(0.5, 0.5)

#names(sample_matrices)[[lambda_name]]
get_strong_instruments <- function(sample_matrices, quantile_probs){
  # Calcul des quantiles pour lambda_01 et lambda_02
  strong_instr_array <- array(NA, dim = c(2, irhor, n, 2))
  dimnames(strong_instr_array) <- list(
    c("lower", "upper"), NULL, NULL, c("lambda_01", "lambda_02")
  )
  for (lambda_idx in 1:2) {
    lambda_name <- paste0("ncvg_lambda_0", lambda_idx)
    quantiles <- apply(sample_matrices[[lambda_name]],
                       c(1, 2), function(x) quantile(x, probs = quantile_probs))
    
    strong_instr_array[,,,lambda_idx] <- array(quantiles, dim = c(irhor, n, 2))
  }
  
  strong_instr_array <- aperm(strong_instr_array, perm = c(2, 3, 1, 4))
  return(strong_instr_array)
}
run_VAR_bootstrap_alternative <- function(df,
                                          headers_var,
                                          headers_proxies,
                                          p = 12,
                                          irhor = 20,
                                          nboot = 100,
                                          block_size = 15,
                                          start_ts = c(2015, 1),
                                          end_ts = c(2023, 12),
                                          headers_controls = NULL) {
  # Crée les objets VAR et proxies
  proxies <- ts(df[, headers_proxies], frequency = 12)
  proxies[is.na(proxies)] <- 0
  
  vars <- ts(df[, headers_var], frequency = 12)
  
  x <- ts(vars, start = start_ts, end = end_ts, frequency = 12)
  y <- vars[-1:-p, ]
  T <- nrow(y)
  n <- ncol(y)
  k <- 2
  l <- ncol(proxies)
  controls <- if (is.null(headers_controls)) {
    NULL
  } else {
    ts(df[, headers_controls], frequency = 12)
  }
  # controls =NULL
  
  estimate_diff <- first_var_estimation(x, y, p, proxies, irhor, controls, elas = -0.16)
  theta_0_diff <- estimate_diff$theta_0
  gamma_diff <- estimate_diff$gamma
  phi_diff <- estimate_diff$phi
  var_coef_diff <- estimate_diff$coef
  
  AL <- do.call(cbind, lapply(var_coef_diff, function(df) df[, 1]))
  names(AL) <- names(var_coef_diff)
  
  C <- MARep(t(AL), p, irhor)
  lambda_0_diff <- get_lambda_0(C, theta_0_diff, irhor =irhor)
  
  gamma_estimate_diff_MBB <- gamma_bootstrap_mbb_alternative(
    x =x,
    nboot = nboot,
    vars = vars,
    BlockSize = block_size,
    theta_0 = theta_0_diff,
    proxies = proxies,
    controls = controls
  )
  
  list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                       "ncvg_lambda_01", "ncvg_lambda_02")
  sample_matrices <- lapply(list_of_samples, function(name) {
    gamma_estimate_diff_MBB[[paste0("boot_", name)]]
  })
  
  names(sample_matrices) <- list_of_samples
  
  # Appliquer cumsum sur l'axe 1 (temps), pour chaque variable et simulation
  sample_matrices$ncvg_lambda_01 <- apply(sample_matrices$ncvg_lambda_01, c(2, 3), function(x) cumsum(x))
  sample_matrices$ncvg_lambda_02 <- apply(sample_matrices$ncvg_lambda_02, c(2, 3), function(x) cumsum(x))
  
  #attention on veut prendre les quantiles sur les IRFs cumulées
  #pour ne pas accumuler l'erreur
  
  lambda_0_diff <- apply(lambda_0_diff, c(2, 3), function(x) cumsum(x))
  
  
  bandwith_90 <- get_strong_instruments(sample_matrices, quantile_probs_90)
  bandwith_68 <- get_strong_instruments(sample_matrices, quantile_probs_68)
  median_50 <- get_strong_instruments(sample_matrices, quantile_prob_50)
  
  return(list(
    headers_var = headers_var,
    headers_proxies = headers_proxies,
    theta_0 = theta_0_diff,
    lambda_0 = lambda_0_diff,
    sample_matrices = sample_matrices,
    bandwith_90 = bandwith_90,
    bandwith_68 = bandwith_68,
    median_50 = median_50
  ))
}



#nbre de variables dans la VAR
headers_controls = c("avg_spot_price_log_diff")


save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

#nbre de variables dans la VAR
n <- length(headers_var_gen_first)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies_esp)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_esp,
                                       headers_var = headers_var_gen_first,
                                       headers_proxies = headers_proxies_esp,
                                       p = 12,
                                       irhor = 20,
                                       nboot = 1000,
                                       block_size = 15,
                                       start_ts = c(2015, 1),
                                       end_ts = c(2023, 12)
                                       )



save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/benchmark'
setwd(save_rds_path)

saveRDS(gen_benchmark_esp, "gen_benchmark_esp.rds")


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_fr <-  read_parquet('IV_data_fr_log_diff_elas.parquet')
colnames(IV_data_log_diff_fr)

IV_data_log_diff_fr <- IV_data_log_diff_fr %>% 
  mutate(
    diff_cbf = cbf_fr - lag(as.numeric(cbf_fr)),
    ffr_diff = ffr - lag(as.numeric(ffr))
  )%>%
  fill(ffr_diff, diff_cbf, .direction = "up")

#same for france
vars<- ts(IV_data_log_diff_fr[, c(headers_var_gen_first)], frequency=12)
l <- length(headers_proxies_fr)

gen_benchmark_fr <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_fr,
                                   headers_var = headers_var_gen_first,
                                   headers_proxies = headers_proxies_fr,
                                   p = 12,
                                   irhor = 20,
                                   nboot = 1000,
                                   block_size = 15,
                                   start_ts = c(2015, 1),
                                   end_ts = c(2023, 12)
                                   #,headers_controls = headers_controls_fr
                                   )

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/benchmark'
setwd(save_rds_path)
saveRDS(gen_benchmark_fr, "gen_benchmark_fr.rds")


gen_benchmark_esp$theta_0











