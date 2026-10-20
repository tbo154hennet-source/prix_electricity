library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")


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


headers_proxies = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  #"log_diff_pred_unavail",
  #"PC1_supply",
  "log_diff_renewable",
  "diff_cbf"
)
headers_var_gen_first = c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  # "ECB_rate_log",
  # "E_Index_log",
  # "Exp_pi_log",
  # "UnE_log" ,
  "HICP_log"
)

headers_var_gen_first_mp = c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  # "E_Index_log",
  # "Exp_pi_log",
  # "UnE_log" ,
  "HICP_log",
  "ffr"
)

headers_var_spot_first = c(
  "spot_price_log",
  "gen_tot_log",
  "IPI_log",
  # "ECB_rate_log_diff",
  # "E_Index_log_diff",
  # "Exp_pi_log_diff",
  # "UnE_log_diff" ,
  "HICP_log"
)

headers_proxies


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

run_VAR_bootstrap <- function(df,
                              headers_var,
                              headers_proxies,
                              p = 4,
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
  
  estimate_log <- first_var_estimation(x, y, p, proxies, irhor, controls)
  theta_0_log <- estimate_log$theta_0
  gamma_log <- estimate_log$gamma
  phi_log <- estimate_log$phi
  var_coef_log <- estimate_log$coef
  
  AL <- do.call(cbind, lapply(var_coef_log, function(df) df[, 1]))
  names(AL) <- names(var_coef_log)
  
  C <- MARep(t(AL), p, irhor)
  lambda_0_log <- get_lambda_0(C, theta_0_log, irhor =irhor)
  
  gamma_estimate_log_MBB <- gamma_bootstrap_mbb_original(
    nboot = nboot,
    vars = vars,
    BlockSize = block_size,
    theta_0 = theta_0_log,
    proxies = proxies,
    controls = controls
  )
  
  list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                       "ncvg_lambda_01", "ncvg_lambda_02")
  sample_matrices <- lapply(list_of_samples, function(name) {
    gamma_estimate_log_MBB[[paste0("boot_", name)]]
  })
  names(sample_matrices) <- list_of_samples
  
  bandwith_90 <- get_strong_instruments(sample_matrices, quantile_probs_90)
  bandwith_68 <- get_strong_instruments(sample_matrices, quantile_probs_68)
  median_50 <- get_strong_instruments(sample_matrices, quantile_prob_50)
  
  return(list(
    headers_var = headers_var,
    headers_proxies = headers_proxies,
    theta_0 = theta_0_log,
    lambda_0 = lambda_0_log,
    sample_matrices = sample_matrices,
    bandwith_90 = bandwith_90,
    bandwith_68 = bandwith_68,
    median_50 = median_50
  ))
}


#nbre de variables dans la VAR
headers_controls_fr = c("Spot_gas_log")
headers_controls_esp = c("spot_gas_capped_log")


save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

#nbre de variables dans la VAR
n <- length(headers_var_gen_first)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_log_esp[, c(headers_var_gen_first)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap(df = IV_data_log_esp,
                                       headers_var = headers_var_gen_first,
                                       headers_proxies = headers_proxies,
                                       p = 4,
                                       irhor = 20,
                                       nboot = 1000,
                                       block_size = 15,
                                       start_ts = c(2015, 1),
                                       end_ts = c(2023, 12))



save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

saveRDS(gen_benchmark_esp, "gen_benchmark_esp.rds")

headers_var_gen_first_mp <- c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)
headers_proxies <-  c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "log_diff_renewable",
  "diff_cbf"
)




save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log.parquet')
colnames(IV_data_log_fr)



#same for france
vars<- ts(IV_data_log_fr[, c(headers_var_gen_first_mp)], frequency=12)

gen_benchmark_fr <- run_VAR_bootstrap(df = IV_data_log_fr,
                                   headers_var = headers_var_gen_first,
                                   headers_proxies = headers_proxies,
                                   p = 4,
                                   irhor = 20,
                                   nboot = 1000,
                                   block_size = 15,
                                   start_ts = c(2015, 1),
                                   end_ts = c(2023, 12)
                                   #,headers_controls = headers_controls_fr
                                   )

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)
saveRDS(gen_benchmark_fr, "gen_benchmark_fr.rds")














