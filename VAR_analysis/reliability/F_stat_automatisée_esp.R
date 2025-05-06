
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/reliability'
setwd(source_path)
source("reliability_auxiliary.R")

source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("auxiliary_control.R")

library(MASS)

#my_IV_data <- IV_data_log_esp

run_iv_analysis <- function(my_IV_data, headers_var, headers_proxies, 
                            p = 4, irhor = 20, k = 2, dependent = "spot_price_log") {
  
  vars<- ts(my_IV_data[, c(headers_var)], frequency=12)
  proxies <- ts(my_IV_data[, c(headers_proxies)], frequency=12)
  
  x <- ts(vars, start = c(2015, 1), end = c(2023, 12), frequency = 12)
  y <- vars[-1:-p, ]
  T <- nrow(y)
  n <- ncol(y)
  l <- ncol(proxies)
  
  # VAR estimation
  vl <- vars::VAR(x, p, type = "none")
  coef <- coef(vl)
  bet <- do.call(cbind, lapply(1:n, function(i) coef[[i]][, 1]))
  res <- residuals(vl)
  Sigma_eta <- cov(res)
  
  # Instruments window
  Z <- window(proxies, start = start(proxies) + c(0, p))
  PZ <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  if (k==1){
    
    # OLS regression with residuals
    
    data <- data.frame(cbind(res[, dependent], Z))
    res_var <- paste0("res_",dependent)
    colnames(data) <- c(res_var, headers_proxies)
    # Create formula
    formula_str <- paste(res_var, "~", paste(headers_proxies, collapse = "+"))
    model <- lm(as.formula(formula_str), data = data)
    fstat <- as.numeric(summary(model)$fstatistic[1])
    
    return(list(
      fstat = fstat,
      m_summary = summary(model)
    ))
    
  }else{
    X <- res[, 1:k]
    Y <- res[, (k+1):n]
    
    # TSLS estimates
    b21ib11_TSLS <- ginv(t(X) %*% PZ %*% X) %*% t(X) %*% PZ %*% Y
    b21ib11 <- t(b21ib11_TSLS)
    Sig11 <- Sigma_eta[(1:k), (1:k)]
    Sig21 <- Sigma_eta[(k+1):n, 1:k]
    Sig22 <- Sigma_eta[(k+1):n, (k+1):n]
    
    ZZp <- b21ib11 %*% Sig11 %*% t(b21ib11) - (Sig21 %*% t(b21ib11) + b21ib11 %*% t(Sig21)) + Sig22
    b12b12p <- t(Sig21 - b21ib11 %*% Sig11) %*% ginv(ZZp) %*% (Sig21 - b21ib11 %*% Sig11)
    b11b11p <- Sig11 - b12b12p
    
    res_TSLS <- Y - X %*% b21ib11_TSLS
    
    # Reliability: Mertens-Ravn
    proxies_postlag <- proxies[-1:-p, ]
    mbis <- ginv(proxies_postlag)
    Sigmm <- mbis %*% t(mbis) / T
    sig_mu1 <- mbis %*% res[, 1:k] / T
    PhiPhip <- sig_mu1 %*% solve(b11b11p) %*% t(sig_mu1)
    RM <- solve(Sigmm) %*% PhiPhip
    RMeigs <- sort(eigen(RM)$values, decreasing = TRUE)
    reliability_measure <- Re(RMeigs)
    
    # Cragg-Donald statistic
    ZtZ <- t(Z) %*% Z
    ZtX <- t(Z) %*% X
    X1tX1 <- t(X) %*% X
    matrix_C <- solve(ZtZ) %*% ZtX %*% solve(X1tX1) %*% t(ZtX)
    lambda_min <- min(Re(eigen(matrix_C)$values[1:k]))
    cragg_donald_stat <- nrow(Z) * lambda_min
    
    
    data <- data.frame(cbind(res[,1:k], Z))
    colnames(data) <- c( paste0('res_',headers_var[1:2]) ,headers_proxies)
    
    formula_price <- as.formula(paste('res_spot_price_log', "~",
                                      paste(headers_proxies, collapse = "+")))
    # Construct the formula
    formula_demand <- as.formula(paste('res_gen_tot_log', "~",
                                       paste(headers_proxies, collapse = "+")))
    
    m_price <- lm( formula_price, data = data)
    # the R^2 in the regression 
    fstat_price <- as.numeric(summary(m_price)$fstat[1])# Get all the statistical coefficients
    
    
    m_demand <- lm(formula_demand, data = data)
    fstat_gen <-as.numeric(summary(m_demand)$fstat[1])
    
    return(list(
      reliability_measure = reliability_measure,
      cragg_donald_stat = cragg_donald_stat,
      fstat_price = fstat_price,
      m_price_summary = summary(m_price),
      fstat_gen = fstat_gen,
      m_demand_summary = summary(m_demand)
    ))
    
    
  }
}


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_fr <-  read_parquet('IV_data_fr_log.parquet')


headers_proxies = c(
  # "T_avg_log",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  # "wind_forecast_log",
  # "solar_forecast_log",
  #"cbf_es_log",
  #"renewable_forecast_log",
  #"renewable_forecast_hors_esp_weighted_log",
  #"PC1_supply",
  #"log_diff_renewable"
  # "NTC_ESP_FR_log",
  # "NTC_FR_ESP_log"
  #"nuclear_unavailabilities_log"
  # "thermal_unavailabilities_log",
  #"predicted_unavailabilities_log"
)



headers_var = c("spot_price_log",
                "gen_tot_log",
                "IPI_log",
                # "ECB_rate_log",
                # "E_Index_log",
                # "Exp_pi_log",
                # "UnE_log" ,
                "NE_Index_log"
)

headers_proxies = c(
  "PC1_supply",
  "log_diff_renewable", "diff_cbf"
)
result_price_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                          dependent = "spot_price_log", k = 1)
# Access outputs
print(result_price_esp$fstat)
print(result_price_esp$m_summary)

headers_proxies = c(
  "temp_excess_instrument"
)
result_gen_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                          dependent = "gen_tot_log", k = 1)
# Access outputs
print(result_gen_esp$fstat)
print(result_gen_esp$m_summary)



headers_proxies = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "PC1_supply",
  "log_diff_renewable"
)

colnames(IV_data_log_esp)

result_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                                dependent = "", k = 2)
# Access outputs
print(result_esp$reliability_measure)
print(result_esp$cragg_donald_stat)
print(result_esp$fstat_price)
print(result_esp$m_price_summary)
print(result_esp$fstat_gen)
print(result_esp$m_demand_summary)



