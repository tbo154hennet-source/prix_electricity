


headers_var_gen_first = c(
                "gen_tot_diff",
                "avg_spot_price_diff",
                "IPI_diff",
                # "ECB_rate_diff",
                # "E_Index_diff",
                # "Exp_pi_diff",
                # "UnE_diff" ,
                "HICP_diff"
)
headers_var_spot_first = c(
  "avg_spot_price_diff",
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
  "avg_spot_price_diff",
  "IPI_diff",
  "HICP_diff",
  "ffr"
)

run_chol_var_diff <- function(df,
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
  #cum_effects <- apply(irf_cholesky$irf$avg_spot_price_diff, 2, cumsum)
  
  # Apply cumsum columnwise for each shock-response matrix
  cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))
  
  return(
    cum_irfs_all
  )
}


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_diff_fr <-  read_parquet('IV_data_fr_diff.parquet')


irf_cholesky_fr <- run_chol_var_diff(
  df = IV_data_diff_fr,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)

getwd()
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_fr, file = 'irf_cholesky_fr.rds')


irf_cholesky_fr_gas_mp <- run_chol_var_diff(
  df = IV_data_diff_fr,
  headers_var = headers_var_gen_first_mp,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls_fr
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_fr_gas_mp, file = 'irf_cholesky_fr_gas_mp_diff.rds')



#ESP


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_diff_esp <-  read_parquet('IV_data_esp_diff.parquet')


irf_cholesky_esp <- run_chol_var_diff(
  df = IV_data_diff_esp,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp, file = 'irf_cholesky_esp_diff.rds')



irf_cholesky_esp_mp <- run_chol_var_diff(
  df = IV_data_diff_esp,
  headers_var = headers_var_gen_first_mp,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
getwd()
save_rds_path_irf <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/irf'
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_mp, file = 'irf_cholesky_esp_mp_diff.rds')



irf_cholesky_esp_gas <- run_chol_var_diff(
  df = IV_data_diff_esp,
  headers_var = headers_var_gen_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls_esp
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_gas, file = 'irf_cholesky_esp_gas_diff.rds')


irf_cholesky_esp_mp_gas <- run_chol_var_diff(
  df = IV_data_diff_esp,
  headers_var = headers_var_gen_first_mp,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE,
  headers_controls = headers_controls_esp
)
getwd()
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_mp_gas, file = 'irf_cholesky_esp_mp_gas_diff.rds')



irf_cholesky_esp_spot_first <- run_chol_var_diff(
  df = IV_data_diff_esp,
  headers_var = headers_var_spot_first,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_spot_first, file = 'irf_cholesky_esp_spot_first_diff.rds')





irf_cholesky_esp_decomp <- run_chol_var_diff(
  df = IV_data_diff_esp,
  headers_var = headers_var_gen_first_decomp,
  p = 4,
  irhor = 20,
  start_ts = c(2015, 1),
  end_ts = c(2023, 12),
  ortho = TRUE,
  boot = TRUE
)
setwd(save_rds_path_irf)
saveRDS(irf_cholesky_esp_decomp, file = 'irf_cholesky_esp_decomp_diff.rds')



setwd(save_rds_path_irf)
saveRDS(irf_cholesky, file = 'irf_cholesky_esp_diff_not_index.rds')

