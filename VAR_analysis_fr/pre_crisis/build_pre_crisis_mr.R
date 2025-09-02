
#compare with IRFs from the pre-period
#until the beginnning of the bouclier tarifaire


colnames(IV_data_log_diff_esp)

# View(IV_data_log_diff_esp %>% 
#        dplyr::select(year, month, observation_date))
# class(IV_data_log_diff_esp$observation_date)
elas_esp = -0.3
elas_fr = -0.3
run_VAR <- function(df,
                    headers_var,
                    headers_proxies,
                    p = 12,
                    irhor = 20,
                    nboot = 100,
                    block_size = 15,
                    start_ts = c(2015, 1),
                    end_ts = c(2023, 12),
                    headers_controls = NULL,
                    elas = -0.16 ) {
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
  
  estimate_diff <- first_var_estimation(x,y,p, proxies, irhor, elas =  elas)
  theta_0_diff <- estimate_diff$theta_0
  gamma_diff <- estimate_diff$gamma
  phi_diff <- estimate_diff$phi
  var_coef_diff <- estimate_diff$coef
  
  AL <- do.call(cbind, lapply(var_coef_diff, function(df) df[, 1]))
  names(AL) <- names(var_coef_diff)
  
  C <- MARep(t(AL), p, irhor)
  lambda_0_diff <- get_lambda_0(C, theta_0_diff, irhor =irhor)
  lambda_0_diff<- apply(lambda_0_diff , c(2, 3), function(x) cumsum(x))
  
  return(list(
    headers_var = headers_var,
    headers_proxies = headers_proxies,
    theta_0 = theta_0_diff,
    lambda_0 = lambda_0_diff
  ))
}



##############################################
#                 ESPAGNE
##############################################

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

#---------------------------
# ESP - Simple - pré-crise
#---------------------------
gen_benchmark_esp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-05-01")),
  headers_var = headers_var_gen_first,
  headers_proxies = headers_proxies_esp,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 5), elas = elas_esp
)

saveRDS(gen_benchmark_esp_pre_crisis, "gen_benchmark_esp_pre_crisis_supply.rds")

# ESP - Simple - avec crise
gen_benchmark_esp_simple <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first,
  headers_proxies = headers_proxies_esp,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12), elas = elas_esp
)

saveRDS(gen_benchmark_esp_simple, "gen_benchmark_esp_supply_with_crisis.rds")

#---------------------------
# ESP - MP - pré-crise
#---------------------------
gen_benchmark_esp_mp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_esp %>% filter(observation_date <= as.Date("2022-05-01")),
  headers_var = headers_var_gen_first_mp,
  headers_proxies = headers_proxies_esp,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 5), elas = elas_esp
)

saveRDS(gen_benchmark_esp_mp_pre_crisis, "gen_benchmark_esp_mp_pre_crisis_supply.rds")


# ESP - MP - avec crise
gen_benchmark_esp_mp_simple <- run_VAR(
  df = IV_data_log_diff_esp,
  headers_var = headers_var_gen_first_mp,
  headers_proxies = headers_proxies_esp,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12), elas = elas_esp
)

saveRDS(gen_benchmark_esp_mp_simple, "gen_benchmark_esp_mp_supply_with_crisis.rds")

##############################################
#                 FRANCE
##############################################

# FR - Simple - pré-crise
gen_benchmark_fr_pre_crisis <- run_VAR(
  df = IV_data_log_diff_fr %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_gen_first,
  headers_proxies = headers_proxies_fr,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 1), elas = elas_fr
)

saveRDS(gen_benchmark_fr_pre_crisis, "gen_benchmark_fr_pre_crisis_supply.rds")

# FR - Simple - avec crise
gen_benchmark_fr_simple <- run_VAR(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_gen_first,
  headers_proxies = headers_proxies_fr,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12), elas = elas_fr
)

saveRDS(gen_benchmark_fr_simple, "gen_benchmark_fr_supply_with_crisis.rds")

# FR - MP - pré-crise
gen_benchmark_fr_mp_pre_crisis <- run_VAR(
  df = IV_data_log_diff_fr %>% filter(observation_date <= as.Date("2022-01-01")),
  headers_var = headers_var_gen_first_mp,
  headers_proxies = headers_proxies_fr,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2022, 1), elas = elas_fr
)

saveRDS(gen_benchmark_fr_mp_pre_crisis, "gen_benchmark_fr_mp_pre_crisis_supply.rds")

# FR - MP - avec crise
gen_benchmark_fr_mp_simple <- run_VAR(
  df = IV_data_log_diff_fr,
  headers_var = headers_var_gen_first_mp,
  headers_proxies = headers_proxies_fr,
  p = 12, irhor = 20, nboot = 1000, block_size = 15,
  start_ts = c(2015, 1), end_ts = c(2023, 12), elas = elas_fr
)

saveRDS(gen_benchmark_fr_mp_simple, "gen_benchmark_fr_mp_supply_with_crisis.rds")





#compare pass-throughs

#one year cumulative pass-through
passthrough_esp_mp <- gen_benchmark_esp_mp_simple$lambda_0[12, 4, 1] # 0.0055844890
passthrough_esp_mp_pre_crisis <- gen_benchmark_esp_mp_pre_crisis$lambda_0[12, 4, 1]


passthrough_fr_mp <- gen_benchmark_fr_mp_simple$lambda_0[12, 4, 1] # 0.0055844890
passthrough_fr_mp_pre_crisis <- gen_benchmark_fr_mp_pre_crisis$lambda_0[12, 4, 1] # 0.0055844890

print(passthrough_esp_mp)
print(passthrough_esp_mp_pre_crisis)
print(passthrough_fr_mp)
print(passthrough_fr_mp_pre_crisis)




passthrough_esp <- gen_benchmark_esp_simple$lambda_0[12, 4, 1] # 0.0055844890
passthrough_esp_pre_crisis <- gen_benchmark_esp_pre_crisis$lambda_0[12, 4, 1]


passthrough_fr <- gen_benchmark_fr_simple$lambda_0[12, 4, 1]
passthrough_fr_pre_crisis <- gen_benchmark_fr_pre_crisis$lambda_0[12, 4, 1]


print(passthrough_esp)
print(passthrough_esp_pre_crisis)
print(passthrough_fr)
print(passthrough_fr_pre_crisis)















