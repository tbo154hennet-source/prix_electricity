
quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_prob_50 <- c(0.5, 0.5)
#names(sample_matrices)[[lambda_name]]
get_strong_instruments_1var <- function(sample_matrices, quantile_probs){
  # Calcul des quantiles pour lambda_01 et lambda_02
  strong_instr_array <- array(NA, dim = c(2, irhor, n))
  dimnames(strong_instr_array) <- list(
    c("lower", "upper"), NULL, NULL
  )

 
  quantiles <- apply(sample_matrices[["ncvg_lambda_01"]],
                     c(1, 2), function(x) quantile(x, probs = quantile_probs))
  
  strong_instr_array[,,] <- array(quantiles, dim = c(irhor, n, 2))
  
  strong_instr_array <- aperm(strong_instr_array, perm = c(2, 3, 1))
  
  return(strong_instr_array)
}
run_VAR_bootstrap_alternative_1var <- function(df,
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
  proxies <- ts(df[, headers_proxies], start = start_ts, end = end_ts, frequency = 12)
  proxies[is.na(proxies)] <- 0
  
  vars <- ts(df[, headers_var], frequency = 12)
  
  x <- ts(vars, start = start_ts, end = end_ts, frequency = 12)
  y <- vars[-1:-p, ]
  T <- nrow(y)
  n <- ncol(y)
  k <- 1
  l <- length(headers_proxies)
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
  
  gamma_estimate_diff_MBB <- gamma_bootstrap_mbb_alternative_bis(
    x =x,
    nboot = nboot,
    vars = vars,
    BlockSize = block_size,
    theta_0 = theta_0_diff,
    proxies = proxies,
    controls = controls,
    start_ts =  start_ts,
    end_ts = end_ts
  )
  
  list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                       "ncvg_lambda_01")
  sample_matrices <- lapply(list_of_samples, function(name) {
    gamma_estimate_diff_MBB[[paste0("boot_", name)]]
  })
  
  names(sample_matrices) <- list_of_samples
  
  # Appliquer cumsum sur l'axe 1 (temps), pour chaque variable et simulation
  sample_matrices$ncvg_lambda_01 <- apply(sample_matrices$ncvg_lambda_01, c(2, 3), function(x) cumsum(x))

  #attention on veut prendre les quantiles sur les IRFs cumulées
  #pour ne pas accumuler l'erreur
  
  lambda_0_diff <- apply(lambda_0_diff, c(2, 3), function(x) cumsum(x))
  
  
  bandwith_90 <- get_strong_instruments_1var(sample_matrices, quantile_probs_90)
  bandwith_68 <- get_strong_instruments_1var(sample_matrices, quantile_probs_68)
  median_50 <- get_strong_instruments_1var(sample_matrices, quantile_prob_50)
  
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
n <- length(headers_var_spot_first)
#nbre de variables instrumentees
k <-1
#nbre d'instruments
l <- length(headers_proxies_supply)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
#matrix_12 <- rbind( diag(k) , zero_matrix)
vars <- ts(IV_data_log_diff_esp[, c(headers_var_spot_first)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                   headers_var = headers_var_spot_first,
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

saveRDS(gen_benchmark_esp, "gen_benchmark_esp_supply.rds")




#nbre de variables dans la VAR
n <- length(headers_var_spot_first_mp)

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp)], frequency=12)


gen_benchmark_esp_mp <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                          headers_var = headers_var_spot_first_mp ,
                                                          headers_proxies = headers_proxies_supply,
                                                          p =12,
                                                          irhor = 20,
                                                          nboot = 1000,
                                                          block_size = 15,
                                                          start_ts = c(2015, 1),
                                                          end_ts = c(2023, 12))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_mp, "gen_benchmark_esp_mp_supply.rds")



#nbre de variables dans la VAR

n <- length(headers_var_spot_first)

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first)], frequency=12)


gen_benchmark_esp_pre_crisis <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                           headers_var = headers_var_spot_first ,
                                                           headers_proxies = headers_proxies_supply,
                                                           p =12,
                                                           irhor = 20,
                                                           nboot = 1000,
                                                           block_size = 15,
                                                           start_ts = c(2015, 1),
                                                           end_ts = c(2022, 5))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_pre_crisis, "gen_benchmark_esp_pre_crisis_supply_test.rds")




#Transmission of demand shocks


#nbre de variables dans la VAR
n <- length(headers_var_gen_first)
#nbre de variables instrumentees
k <-1
#nbre d'instruments
l <- length(headers_proxies_demand)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
#matrix_12 <- rbind( diag(k) , zero_matrix)
vars <- ts(IV_data_log_diff_esp[, c(headers_var_gen_first)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap_alternative_1var (df = IV_data_log_diff_esp,
                                                         headers_var = headers_var_gen_first,
                                                         headers_proxies = headers_proxies_demand,
                                                         p = 12,
                                                         irhor = 20,
                                                         nboot = 1000,
                                                         block_size = 15,
                                                         start_ts = c(2015, 1),
                                                         end_ts = c(2023, 12)
)



save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

saveRDS(gen_benchmark_esp, "gen_benchmark_esp_demand.rds")




#nbre de variables dans la VAR
n <- length(headers_var_gen_first_mp)

vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)


gen_benchmark_esp_mp <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                               headers_var = headers_var_gen_first_mp ,
                                                               headers_proxies = headers_proxies_demand,
                                                               p =12,
                                                               irhor = 20,
                                                               nboot = 1000,
                                                               block_size = 15,
                                                               start_ts = c(2015, 1),
                                                               end_ts = c(2023, 12))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_mp, "gen_benchmark_esp_mp_demand.rds")



n <- length(headers_var_gen_first)

vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first)], frequency=12)


gen_benchmark_esp_pre_crisis <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                                   headers_var = headers_var_gen_first  ,
                                                                   headers_proxies = headers_proxies_demand,
                                                                   p =12,
                                                                   irhor = 20,
                                                                   nboot = 1000,
                                                                   block_size = 15,
                                                                   start_ts = c(2015, 1),
                                                                   end_ts = c(2022, 5))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_pre_crisis, "gen_benchmark_esp_pre_crisis_demand_test.rds")





#for core inflation
#change the headers

colnames(IV_data_log_diff_esp)
headers_var_spot_first_core <- c("spot_price_log_diff", "gen_tot_log_diff",    "IPI_log_diff"  ,     
                            "NE_Index_log_diff")

headers_var_spot_first_mp_core <- c(headers_var_spot_first_core, "ffr_diff")

headers_var_gen_first_core <- c("gen_tot_log_diff","spot_price_log_diff",  "IPI_log_diff", "NE_Index_log_diff")
headers_var_gen_first_mp_core <- c(headers_var_gen_first_core, "ffr_diff")



save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

#nbre de variables dans la VAR
n <- length(headers_var_spot_first_core)
#nbre de variables instrumentees
k <-1
#nbre d'instruments
l <- length(headers_proxies_supply)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
#matrix_12 <- rbind( diag(k) , zero_matrix)
vars <- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_core)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                        headers_var = headers_var_spot_first_core,
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

saveRDS(gen_benchmark_esp, "gen_benchmark_esp_supply_core.rds")




#nbre de variables dans la VAR
n <- length(headers_var_spot_first_mp_core)

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp_core)], frequency=12)


gen_benchmark_esp_mp_core <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                                headers_var = headers_var_spot_first_mp_core ,
                                                                headers_proxies = headers_proxies_supply,
                                                                p =12,
                                                                irhor = 20,
                                                                nboot = 1000,
                                                                block_size = 15,
                                                                start_ts = c(2015, 1),
                                                                end_ts = c(2023, 12))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_mp_core, "gen_benchmark_esp_mp_core_supply.rds")







#Transmission of demand shocks


#nbre de variables dans la VAR
n <- length(headers_var_gen_first_core)
#nbre de variables instrumentees
k <-1
#nbre d'instruments
l <- length(headers_proxies_demand)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
#matrix_12 <- rbind( diag(k) , zero_matrix)
vars <- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_core)], frequency=12)

gen_benchmark_esp <- run_VAR_bootstrap_alternative_1var (df = IV_data_log_diff_esp,
                                                         headers_var = headers_var_gen_first_core,
                                                         headers_proxies = headers_proxies_demand,
                                                         p = 12,
                                                         irhor = 20,
                                                         nboot = 1000,
                                                         block_size = 15,
                                                         start_ts = c(2015, 1),
                                                         end_ts = c(2023, 12)
)



save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

saveRDS(gen_benchmark_esp, "gen_benchmark_esp_demand_core.rds")




#nbre de variables dans la VAR
n <- length(headers_var_gen_first_mp_core)

vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp_core)], frequency=12)


gen_benchmark_esp_mp_core <- run_VAR_bootstrap_alternative_1var(df = IV_data_log_diff_esp,
                                                                headers_var = headers_var_gen_first_mp_core ,
                                                                headers_proxies = headers_proxies_demand,
                                                                p =12,
                                                                irhor = 20,
                                                                nboot = 1000,
                                                                block_size = 15,
                                                                start_ts = c(2015, 1),
                                                                end_ts = c(2023, 12))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_mp_core, "gen_benchmark_esp_mp_core_demand.rds")




