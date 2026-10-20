


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

headers_var_gen_first_mp <- c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  "HICP_log",
  "ffr"
)

run_chol_var <- function(df,
                        headers_var,
                        p = 4,
                        irhor = 20,
                        start_ts = c(2015, 1),
                        end_ts = c(2023, 12),
                        ortho = TRUE,
                        boot = TRUE,
                        headers_controls = NULL) {
  # Subset variables and demean
  vars <- df[, headers_var]
  if(is.null(headers_controls)) {
    controls <- NULL
  } else {
    controls <- df[, c( headers_controls)]
  }
  vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x, na.rm = TRUE)))
  
  # Convert to time series object
  x <- ts(vars_demeaned, start = start_ts, end = end_ts, frequency = 12)
  
  # Estimate VAR
  vl <- vars::VAR(x, p, type = "none")
  
  # Compute Cholesky IRF
  irf_cholesky <- vars::irf(vl,
                            impulse = colnames(x),
                            response = colnames(x),
                            n.ahead = irhor - 1,
                            ortho = ortho,
                            boot = boot,
                            exogen = controls)
  
  return(
    irf_cholesky = irf_cholesky
  )
}


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log.parquet')


irf_cholesky_fr <- run_chol_var(
  df = IV_data_log_fr,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
getwd()
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)
saveRDS(irf_cholesky_fr, file = 'irf_cholesky_fr.rds')


irf_cholesky_fr_gas <- run_chol_var(
  df = IV_data_log_fr,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls_fr
)
setwd(save_rds_path)
saveRDS(irf_cholesky_fr_gas, file = 'irf_cholesky_esp_gas.rds')



#ESP


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')


irf_cholesky_esp <- run_chol_var(
  df = IV_data_log_esp,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)
saveRDS(irf_cholesky_esp, file = 'irf_cholesky_esp.rds')



irf_cholesky_esp_mp <- run_chol_var(
  df = IV_data_log_esp,
  headers_var = headers_var_gen_first_mp,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
getwd()
save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
setwd(save_rds_path)
saveRDS(irf_cholesky_esp_mp, file = 'irf_cholesky_esp_mp.rds')



irf_cholesky_esp_gas <- run_chol_var(
  df = IV_data_log_esp,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls_esp
)
setwd(save_rds_path)
saveRDS(irf_cholesky_esp_gas, file = 'irf_cholesky_esp_gas.rds')


irf_cholesky_esp_mp_gas <- run_chol_var(
  df = IV_data_log_esp,
  headers_var = headers_var_gen_first_mp,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls_esp
)
setwd(save_rds_path)
saveRDS(irf_cholesky_esp_mp_gas, file = 'irf_cholesky_esp_mp_gas.rds')



irf_cholesky_esp_spot_first <- run_chol_var(
  df = IV_data_log_esp,
  headers_var = headers_var_spot_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
setwd(save_rds_path)
saveRDS(irf_cholesky_esp_spot_first, file = 'irf_cholesky_esp_spot_first.rds')





irf_cholesky_esp_decomp <- run_chol_var(
  df = IV_data_log_esp,
  headers_var = headers_var_gen_first_decomp,
  p = 4,
  irhor = 40,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
setwd(save_rds_path)
saveRDS(irf_cholesky_esp_decomp, file = 'irf_cholesky_esp_decomp.rds')




