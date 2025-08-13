library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_diff_esp <-  read_parquet('IV_data_esp_diff.parquet')

# correct for the Iberian exception
colnames(IV_data_diff_esp)
IV_data_diff_esp$observation_date <- as.Date(IV_data_diff_esp$observation_date)
IV_data_diff_esp <- IV_data_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply
  )
IV_data_diff_esp <- IV_data_diff_esp %>% 
  mutate(
    diff_cbf = cbf_es - lag(as.numeric(cbf_es)),
    ffr_diff = ffr - lag(as.numeric(ffr)),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff)
  )


headers_proxies = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "renewable_day_ahead_forecast_esp_diff",
  "diff_cbf"
)
headers_var_gen_first = c(
  "gen_tot_diff",
  "spot_price_diff",
  "IPI_diff",
  # "ECB_rate_diff",
  # "E_Index_diff",
  # "Exp_pi_diff",
  # "UnE_diff" ,
  "HICP_diff"
)

headers_var_gen_first_decomp = c(
  "gen_tot_diff",
  "spot_price_diff",
  "IPI_diff",
  "E_Index_diff",
  "NE_Index_diff"
)


headers_var_spot_first = c(
  "spot_price_diff",
  "gen_tot_diff",
  "IPI_diff",
  # "ECB_rate_diff_diff",
  # "E_Index_diff_diff",
  # "Exp_pi_diff_diff",
  # "UnE_diff_diff" ,
  "HICP_diff"
)

headers_var_gen_first_mp <- c(
  "gen_tot_diff",
  "spot_price_diff",
  "IPI_diff",
  "HICP_diff",
  "ffr_diff"
)



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
  
  estimate_diff <- first_var_estimation(x, y, p, proxies, irhor, controls)
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
headers_controls_fr = c("Spot_gas_diff")
headers_controls_esp = c("spot_gas_capped_diff")


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
vars<- ts(IV_data_diff_esp[, c(headers_var_gen_first)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap_alternative(df = IV_data_diff_esp,
                                       headers_var = headers_var_gen_first,
                                       headers_proxies = headers_proxies,
                                       p = 4,
                                       irhor = 20,
                                       nboot = 1000,
                                       block_size = 4,
                                       start_ts = c(2015, 1),
                                       end_ts = c(2023, 12)
                                       )



save_rds_path_bench <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/benchmark'
setwd(save_rds_path_bench)

saveRDS(gen_benchmark_esp, "gen_benchmark_esp.rds")


#controling for spot gas price on the period 



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


gen_benchmark_esp_gas <- run_VAR_bootstrap_alternative(df = IV_data_diff_esp,
                                           headers_var = headers_var_gen_first,
                                           headers_proxies = headers_proxies,
                                           p = 4,
                                           irhor = 20,
                                           nboot = 1000,
                                           block_size = 4,
                                           start_ts = c(2015, 1),
                                           end_ts = c(2023, 12),
                                           headers_controls = headers_controls_esp)

setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_gas, "gen_benchmark_esp_gas.rds")


#change to different ordering
#nbre de variables dans la VAR
n <- length(headers_var_spot_first)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_diff_esp[, c(headers_var_spot_first)], frequency=12)
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]

gen_benchmark_esp_spot_first <- run_VAR_bootstrap_alternative(df = IV_data_diff_esp,
                                                              headers_var = headers_var_spot_first,
                                                              headers_proxies = headers_proxies,
                                                              p = 4,
                                                              irhor = 20,
                                                              nboot = 1000,
                                                              block_size = 4,
                                                              start_ts = c(2015, 1),
                                                              end_ts = c(2023, 12))

setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_spot_first, "gen_benchmark_esp_spot_first.rds")

#with mp

headers_var_gen_first_mp <- c(
  "gen_tot_diff",
  "spot_price_diff",
  "IPI_diff",
  "HICP_diff",
  "ffr_diff"
)


#nbre de variables dans la VAR
n <- length(headers_var_gen_first_mp)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)

gen_benchmark_esp_gas_mp <- run_VAR_bootstrap_alternative(df = IV_data_diff_esp,
                                                       headers_var = headers_var_gen_first_mp,
                                                       headers_proxies = headers_proxies,
                                                       p = 4,
                                                       irhor = 20,
                                                       nboot = 1000,
                                                       block_size = 4,
                                                       start_ts = c(2015, 1),
                                                       end_ts = c(2023, 12),
                                                       headers_controls = headers_controls_esp)

setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_gas_mp, "gen_benchmark_esp_gas_mp.rds")







