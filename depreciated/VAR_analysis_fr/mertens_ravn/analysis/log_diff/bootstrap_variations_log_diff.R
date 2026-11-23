headers_var_gen_first_mp <- c(
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff",
  "ffr_diff"
)

headers_var_gen_first_gas <- c(
  "gen_tot_log_diff",
  "Spot_gas_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff"
)

#change to different ordering
#nbre de variables dans la VAR
n <- length(headers_var_spot_first)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies_esp)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first)], frequency=12)
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]

gen_benchmark_esp_spot_first <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_esp,
                                                  headers_var = headers_var_spot_first,
                                                  headers_proxies = headers_proxies_esp,
                                                  p =12,
                                                  irhor = 20,
                                                  nboot = 1000,
                                                  block_size = 15,
                                                  start_ts = c(2015, 1),
                                                  end_ts = c(2023, 12))
save_rds_path_bench <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/benchmark'
setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_spot_first, "gen_benchmark_esp_spot_first.rds")



#nbre de variables dans la VAR
n <- length(headers_var_gen_first_mp)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies_esp)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)


gen_benchmark_esp_gas_mp <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_esp,
                                              headers_var = headers_var_gen_first_mp ,
                                              headers_proxies = headers_proxies_esp,
                                              p =12,
                                              irhor = 20,
                                              nboot = 1000,
                                              block_size = 15,
                                              start_ts = c(2015, 1),
                                              end_ts = c(2023, 12),
                                              headers_controls = headers_controls)

save_rds_path_bench <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/benchmark'
setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_gas_mp, "gen_benchmark_esp_gas_mp.rds")


#controling for spot gas price on the period 



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


gen_benchmark_esp_gas <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_esp,
                                           headers_var = headers_var_gen_first,
                                           headers_proxies = headers_proxies_esp,
                                           p =12,
                                           irhor = 20,
                                           nboot = 1000,
                                           block_size = 15,
                                           start_ts = c(2015, 1),
                                           end_ts = c(2023, 12),
                                           headers_controls = headers_controls)

setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_gas, "gen_benchmark_esp_gas.rds")

getwd()
#adding variantes with monetary policy
#more variables


#nbre de variables dans la VAR
n <- length(headers_var_gen_first_mp)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies_esp)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]

gen_benchmark_esp_mp <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_esp,
                                          headers_var = headers_var_gen_first_mp ,
                                          headers_proxies = headers_proxies_esp,
                                          p =12,
                                          irhor = 20,
                                          nboot = 1000,
                                          block_size = 15,
                                          start_ts = c(2015, 1),
                                          end_ts = c(2023, 12))

setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_mp, "gen_benchmark_esp_mp.rds")










#for france

#nbre de variables dans la VAR
n <- length(headers_var_gen_first_mp)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies_fr)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)
vars<- ts(IV_data_log_diff_fr[, c(headers_var_gen_first_mp)], frequency=12)


gen_benchmark_fr_gas_mp <- run_VAR_bootstrap_alternative(df = IV_data_log_diff_fr,
                                                          headers_var = headers_var_gen_first_mp ,
                                                          headers_proxies = headers_proxies_fr,
                                                          p =12,
                                                          irhor = 20,
                                                          nboot = 1000,
                                                          block_size = 15,
                                                          start_ts = c(2015, 1),
                                                          end_ts = c(2023, 12),
                                                          headers_controls = headers_controls_fr)


saveRDS(gen_benchmark_fr_gas_mp, "gen_benchmark_fr_gas_mp.rds")





#Instrumenting for gas


#with gas instrument
headers_proxies_gas <- c(headers_proxies_esp, "PC1_supply")
#nbre de variables dans la VAR
n <- length(headers_var_gen_first_gas)
#nbre de variables instrumentees
k <-3
#nbre d'instruments
l <- length(headers_proxies_gas)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = k)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)


gen_benchmark_esp_gas_ins <- run_VAR_bootstrap_alternative_3(df = IV_data_log_diff_esp,
                                                             headers_var = headers_var_gen_first_gas,
                                                             headers_proxies = headers_proxies_gas,
                                                             p =12,
                                                             irhor = 20,
                                                             nboot = 1000,
                                                             block_size = 15,
                                                             start_ts = c(2015, 1),
                                                             end_ts = c(2023, 12))

setwd(save_rds_path_bench)
saveRDS(gen_benchmark_esp_gas_ins, "gen_benchmark_esp_gas_ins.rds")
#with gas instrument

headers_proxies_gas <- c(headers_proxies_fr, "PC1_supply")
#nbre de variables dans la VAR
n <- length(headers_var_gen_first_gas)
#nbre de variables instrumentees
k <-3
#nbre d'instruments
l <- length(headers_proxies_gas)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = k)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)

gen_benchmark_fr_gas_ins <- run_VAR_bootstrap_alternative_3(df = IV_data_log_diff_fr,
                                                         headers_var = headers_var_gen_first_gas ,
                                                         headers_proxies = headers_proxies_gas,
                                                         p =12,
                                                         irhor = 20,
                                                         nboot = 1000,
                                                         block_size = 15,
                                                         start_ts = c(2015, 1),
                                                         end_ts = c(2023, 12))



