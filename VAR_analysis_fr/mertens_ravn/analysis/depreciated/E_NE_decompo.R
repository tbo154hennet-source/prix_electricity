
save_var_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data <-read_parquet( 'IV_data_esp.parquet')



ggplot(data = IV_data)+
  geom_line(aes(x = observation_date, y = E_Index, color = '$E_Index$')) +
  geom_line(aes(x = observation_date, y = NE_Index, color = '$NE_Index$')) +
  geom_line(aes(x = observation_date, y = HICP, color = '$HICP$')) +
  geom_line(aes(x = observation_date, y = CPElec, color = '$P_{elec}$')) +
  geom_line(aes(x = observation_date, y = spot_price, color = 'price')) +
  theme_minimal()

ggplot(data = IV_data_log_esp)+
  geom_line(aes(x = observation_date, y = E_Index_log, color = '$E_Index$')) +
  geom_line(aes(x = observation_date, y = NE_Index_log, color = '$NE_Index$')) +
  geom_line(aes(x = observation_date, y = HICP_log, color = '$HICP$')) +
  geom_line(aes(x = observation_date, y = CPElec_log, color = '$P_{elec}$')) +
  #geom_line(aes(x = observation_date, y = spot_price_log, color = 'price')) +
  theme_minimal()

headers_var_gen_first_decomp = c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  # "ECB_rate_log",
  "E_Index_log",
  # "Exp_pi_log",
  # "UnE_log" ,
  "NE_Index_log",
  "HICP_log",
  "ffr"
)


save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)

#nbre de variables dans la VAR
n <- length(headers_var_gen_first_decomp)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- length(headers_proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
matrix_12 <- rbind( diag(k) , zero_matrix)

vars<- ts(IV_data_log_esp[, c(headers_var_gen_first_decomp)], frequency=12)


headers_proxies <-  c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "log_diff_renewable",
  "diff_cbf"
)


gen_benchmark_esp_decomp <- run_VAR_bootstrap(df = IV_data_log_esp,
                                              headers_var = headers_var_gen_first_decomp,
                                              headers_proxies = headers_proxies,
                                              p = 4,
                                              irhor = 20,
                                              nboot = 1000,
                                              block_size = 15,
                                              start_ts = c(2015, 1),
                                              end_ts = c(2023, 12),
                                              headers_controls = headers_controls_esp)
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)
saveRDS(gen_benchmark_esp_decomp, "gen_benchmark_esp_decomp.rds")

  
  

