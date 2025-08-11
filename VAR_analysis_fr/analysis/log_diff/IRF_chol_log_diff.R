


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
headers_var_spot_first = c(
  "spot_price_log_diff",
  "gen_tot_log_diff",
  "IPI_log_diff",
  # "ECB_rate_log_diff_log_diff",
  # "E_Index_log_diff_log_diff",
  # "Exp_pi_log_diff_log_diff",
  # "UnE_log_diff_log_diff" ,
  "HICP_log_diff",
  "ffr_diff"
)

headers_var_gen_first_mp <- c(
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff",
  "ffr_diff"
)

run_chol_var_log_diff <- function(df,
                        headers_var,
                        p=12,
                        irhor = 20,
                        start_ts = c(2015, 1),
                        end_ts = c(2023, 12),
                        ortho = TRUE,
                        boot = TRUE,
                        headers_controls = NULL) {
  # Subset variables and demean
  vars <- df[, headers_var]
  vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x, na.rm = TRUE)))
  
  # Convert to time series object
  x <- ts(vars_demeaned, start = start_ts, end = end_ts, frequency = 12)
  
  if(is.null(headers_controls)) {
    controls <- NULL
    # Estimate VAR
    vl <- vars::VAR(x, p)
    # Compute Cholesky IRF
    irf_cholesky <- vars::irf(vl,
                              impulse = colnames(x),
                              response = colnames(x),
                              n.ahead = irhor - 1,
                              ortho = ortho,
                              boot = boot)
  } else {
    controls <- df[, c( headers_controls)]
    # Estimate VAR
    vl <- vars::VAR(x, p, exogen = controls)
    # Compute Cholesky IRF
    irf_cholesky <- vars::irf(vl,
                              impulse = colnames(x),
                              response = colnames(x),
                              n.ahead = irhor - 1,
                              ortho = ortho,
                              boot = boot,
                              exogen = controls)
  }
  # Cumulative IRFs example :
  #cum_effects <- apply(irf_cholesky$irf$spot_price_log_diff, 2, cumsum)
  
  # Apply cumsum columnwise for each shock-response matrix
  cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))
  
  return(
    cum_irfs_all
  )
}




irf_cholesky_fr <- run_chol_var_log_diff(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_gen_first,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)

getwd()
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_fr, file = 'irf_cholesky_fr_log_diff.rds')

irf_cholesky_fr <- run_chol_var_log_diff(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_gen_first_mp,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls
)

getwd()
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_fr, file = 'irf_cholesky_fr_mp_gas_log_diff.rds')

irf_cholesky_fr_spot_first <- run_chol_var_log_diff(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_spot_first,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_fr_spot_first, file = 'irf_cholesky_fr_spot_first_log_diff.rds')



irf_cholesky_fr_spot_first_mp_gas <- run_chol_var_log_diff(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_spot_first_mp,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_fr_spot_first_mp_gas, file = 'irf_cholesky_fr_spot_first_mp_gas_log_diff.rds')







#ESP




irf_cholesky_esp <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp, file = 'irf_cholesky_esp_log_diff.rds')



irf_cholesky_esp_mp <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first_mp,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
getwd()
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_mp, file = 'irf_cholesky_esp_mp_log_diff.rds')



irf_cholesky_esp_mp_spot <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_mp,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
getwd()
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_mp_spot, file = 'irf_cholesky_esp_spot_first_mp_log_diff.rds')



irf_cholesky_esp_gas <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_gas, file = 'irf_cholesky_esp_gas_log_diff.rds')


irf_cholesky_esp_mp_gas <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first_mp,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls
)
getwd()
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_mp_gas, file = 'irf_cholesky_esp_mp_gas_log_diff.rds')



irf_cholesky_esp_spot_first <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_spot_first, file = 'irf_cholesky_esp_spot_first_log_diff.rds')



irf_cholesky_esp_spot_first_mp_gas <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_spot_first_mp,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_spot_first_mp_gas, file = 'irf_cholesky_esp_spot_first_mp_gas_log_diff.rds')





#gas_instrumented_alternative
headers_var_gen_first_gas <- c(
  "Spot_gas_log_diff",
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff"
)

irf_cholesky_esp <- run_chol_var_log_diff(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first_gas,
  p=12,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp, file = 'irf_cholesky_esp_log_diff_gas_ins.rds')



