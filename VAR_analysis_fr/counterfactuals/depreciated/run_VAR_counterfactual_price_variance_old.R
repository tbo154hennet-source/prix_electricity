
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_irf_plot.R")

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <- read_parquet( 'IV_data_esp_log_elas_sd_2026_pc.parquet')

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data <- read_parquet('IV_data_esp_elas_sd_2026.parquet')%>%
  mutate(across(contains("spot"), ~ . / HICP*100 )) %>% 
  #pour l'instant IPI Esp va jusqu'à 2025-10-01  
  filter(observation_date <= as.Date("2025-09-01") ) 


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
#pred_post_df <- read_parquet( "prediction_gas_price_counterfactual_2022.parquet")
price_var_counterfactual <- read_parquet("price_variance_counterfactual_esp_ter.parquet")

colnames(price_var_counterfactual)

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

counterfactual_wind_evolution <- price_var_counterfactual %>% 
  rename(counterfactual_spot_price_log = mean_cf_adj ) %>% 
  arrange(year, month)%>% 
  filter(year !=2025 | month <=9)

counterfactual_wind_evolution <- counterfactual_wind_evolution %>%
  mutate(
    across(
      .cols = -c(year, month, date),
      .fns  = ~ log(exp(.x) / IV_data$HICP * 100 + 1)
    )
  )
colnames(counterfactual_wind_evolution)
# all.equal(
#   ts(IV_data_log_esp$spot_price_log, start=start_date, frequency=frequency),
#   ts(counterfactual_wind_evolution$price_mean, start=start_date, frequency=frequency),
#   check.attributes = FALSE
# )





IV_data_log_esp_counterfactual <- left_join(IV_data_log_esp, counterfactual_wind_evolution %>% 
                                          mutate(year = as.numeric(year),
                                                 month = as.numeric(month)) %>% 
                                            dplyr::select(counterfactual_spot_price_log, year, month ),
                                        by = c("year", "month")
                                        )%>%
mutate( #counterfactual_spot_price = counterfactual_spot_price,
#         #   se_pred_month = se_pred_month,
#           counterfactual_spot_price_log = ifelse(is.na(counterfactual_spot_price), spot_price_log, log(counterfactual_spot_price)),
#           counterfactual_spot_price = ifelse(is.na(counterfactual_spot_price), spot_price, counterfactual_spot_price),
#         #   se_pred_month = ifelse(is.na(counterfactual_spot_price), 0, se_pred_month),
#           #counterfactual_spot_price_log = log(counterfactual_spot_price),
#           #j'interpr?te la diff?rence comme un choc pur de policy
          policy_shock = counterfactual_spot_price_log - spot_price_log
          )


#IV_data_log_esp_counterfactual$counterfactual_spot_price_log
T_controls <- nrow(x_demand)   # ou nrow(res_demand)

controls <- matrix(1, nrow = T_controls, ncol = 1)
colnames(controls) <- "const"

x_gas =ts(IV_data_log_esp %>% filter(observation_date <= as.Date("2022-05-15")), start=c(2015,1), end=c(2022,5), frequency=12)
y_gas =x_gas[-1:-p, ]
T_gas = nrow(y_gas)

vars_supply <- ts(IV_data_log_esp[, c(headers_var_spot_first_mp_gas)], frequency=12)
p = 12
irhor = 20
#variables dans VAR
n <- ncol(vars_supply)
T = nrow(vars_supply) - p
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


x_supply =ts(vars_supply, start=c(2015,1), end=c(2025,9), frequency=12)

#controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "none")
} else {
  vars::VAR(x_supply, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


# Fit the VAR model

proxies <- ts(IV_data_log_esp[, c("delta_supply_incorpo")[[1]]], frequency=12)
theta_0_supply_wind <- first_var_estimation_bis(x_supply, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_wind <- get_lambda_0(C, theta_0_supply_wind)

# confidence = c(0.68, 0.9)
# NWlags= 0
# norm        = 1 # Variable used for normalization
# scale       = 1
# VAR_wind <-SVARIV(ydata = x_supply, z = IV_data_log_esp$delta_supply_incorpo,p,
#                   confidence,NWlags,
#                   norm,scale,irhor,instrument_name="supply")

# #don't do bootstrap but instead take bands from Anderson Rubin confidence test

# irf_data_cum <- VAR_wind$irfs %>%
#     filter(confi_type == "msw" )#| confi_type == "msw_cum")
  
# lower_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
# upper_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))
# lower_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
# upper_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))
# main_IV_irf <- irf_data_cum %>% 
#     filter(confi_level == 0.68 & type == "point") %>%
#     dplyr::select(variable, horizon, value) %>% 
#     pivot_wider(
#       names_from = variable,
#       values_from = value
#     ) %>% 
#     dplyr::select(-horizon)
# dim(main_IV_irf)

# d: effet observé (T×1) = observed - counterfactual, sur la variable cible
# psi: IRF (H×1) de cette variable au choc policy, horizon 0..H-1
# Renvoie eps_policy (T×1)


IRF <- lambda_0_log_supply_wind
if (length(dim(IRF)) == 3) IRF <- IRF[,,1]   # H x n
IRF <- as.matrix(IRF)

psi <- as.numeric(IRF[, 1])                  # longueur H, h=0..H-1


recover_policy_shock_exact <- function(d, psi) {
  d <- as.numeric(d)
  psi <- as.numeric(psi)
  T <- length(d)
  H <- length(psi)
  
  if (abs(psi[1]) < 1e-12) stop("psi[1] (impact h=0) est nul -> non identifiable.")
  
  eps <- numeric(T)
  for (t in 1:T) {
    hmax <- min(H - 1, t - 1)
    rhs <- d[t]
    if (hmax >= 1) {
      rhs <- rhs - sum(psi[2:(hmax+1)] * eps[(t-1):(t-hmax)])
    }
    eps[t] <- rhs / psi[1]
  }
  eps
}

d_price <- IV_data_log_esp_counterfactual$spot_price_log - IV_data_log_esp_counterfactual$counterfactual_spot_price_log
# d_price[1:p] <- rep(0,12)
eps_policy <- recover_policy_shock_exact(d_price, psi)
IV_data_log_esp_counterfactual$eps_policy <- eps_policy

IV_data_log_esp_counterfactual %>% 
dplyr::select(eps_policy, wind_forecast_log, wind_speed_100m_log) %>%
mutate(eps_policy = as.numeric(eps_policy)) %>%
  cor(use = "complete.obs")


ggplot(data = IV_data_log_esp_counterfactual, aes(x=observation_date))+
  geom_line( aes( y = eps_policy , color = "Policy"))+
  #geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y = eps_policy_hat_lambda, color = "Policy"))+
  geom_line( aes( y = wind_forecast_log - mean(wind_forecast_log), color = "Wind"))

# reconstruit d_hat via convolution
d_hat <- as.numeric(stats::filter(eps_policy, filter = psi,
                                  method = "convolution", sides = 1))
d_hat[is.na(d_hat)] <- 0

max(abs(d_price - d_hat), na.rm = TRUE)
H <- length(psi)
keep <- (H:length(d_price))
# fit quality only after burn-in
max(abs(d_price[keep] - d_hat[keep]), na.rm = TRUE)


Tp <- length(eps_policy)
H  <- nrow(IRF)
n  <- ncol(IRF)

path_policy <- matrix(0, nrow = Tp, ncol = n)
colnames(path_policy) <- colnames(IRF)

for (j in 1:n) {
  tmp <- stats::filter(eps_policy, filter = IRF[, j],
                       method = "convolution", sides = 1)
  path_policy[, j] <- replace(as.numeric(tmp), is.na(tmp), 0)
}

# contrefactuel "sans policy shock" pour toutes les variables
counterfactual_prediction  <- x_supply - path_policy

ggplot()+
  geom_line(data = IV_data_log_esp, aes(x= observation_date, y= spot_price_log, color = "Original"))+
  geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y= counterfactual_spot_price_log, color = "Target"))+
  geom_line( aes(x= IV_data_log_esp$observation_date, y= counterfactual_prediction[,1], color = "Counterfactual"))
# 



# ---- Extraire série originale (colonne 4) ----
price_obs <- x_supply[, 1]
IPI_obs <- x_supply[, 4]
IPI_obs  <- IV_data_log_esp$IPI_log
sd_obs = sqrt(var(IPI_obs	, na.rm = TRUE))

price_pred <- counterfactual_prediction[, 1]
IPI_pred  <- counterfactual_prediction[, 4]
sd_pred =sqrt(var(IPI_pred	, na.rm = TRUE))
sd_ratio = sd_pred/sd_obs
sd_ratio

ggplot()+
  geom_line(data = IV_data_log_esp, aes(x= observation_date, y= IPI_obs, color = "Original"))+
  geom_line( aes(x= IV_data_log_esp$observation_date, y= IPI_pred, color = "Counterfactual"))


# View(IV_data_esp_sim %>% 
#        dplyr::select(counterfactual_spot_price, counterfactual_spot_price_sim, spot_price, policy_shock_sim, year, month))
#a chaque fois il faut que je tire une trajectoire de policy shock
nboot = 1000

bootIRFs_supply_counterfactual <- gamma_bootstrap_mbb_corrected_counterfactual(x= x_supply, nboot, theta_0 = theta_0_supply_wind ,
                                                        proxies = proxies, controls = NULL, elas = 0,
                                                        start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


bootIRFs_supply <- bootIRFs_supply_counterfactual$bootIRFs
boots_conterfactuals_pred <-  bootIRFs_supply_counterfactual$counterfactual_prediction_boot
dim(bootIRFs_supply )


# dernière valeur observée en niveau avant la prévision
boot_counterfactual_IPI <- array(NA, dim = c((T+p), nboot))
boot_counterfactual_spot <- array(NA, dim = c((T+p), nboot))
#boot_counterfactual_level_IPI <- array(NA, dim = c((T+p), nboot))

recover_policy_shock_ridge_ls <- function(d, psi, lambda = 1e-3) {
  d <- as.numeric(d); psi <- as.numeric(psi)
  T <- length(d); H <- length(psi)

  A <- matrix(0, nrow = T, ncol = T)
  for (t in 1:T) {
    for (h in 0:min(H-1, t-1)) {
      A[t, t-h] <- psi[h+1]
    }
  }
  eps_hat <- solve(t(A) %*% A + lambda * diag(T), t(A) %*% d)
  as.numeric(eps_hat)
}


for (i in 1:nboot) {
    # Matrice T x 4 remplie de 0
    #IRF_sim <- as.matrix(draws[,,i])
    # IRF_sim[1, 1] <- 1
	IRF_sim <- as.matrix(bootIRFs_supply[,,i])
    psi_sim <- as.numeric(IRF_sim[, 1])                  # longueur H, h=0..H-1
    
    #eps_policy_sim <- recover_policy_shock_exact(d_price, psi_sim)
    eps_policy_sim <- recover_policy_shock_ridge_ls(d_price, psi_sim, lambda = 3e-1)


    path_policy_sim <- matrix(0, nrow = Tp, ncol = n)
    for (j in 1:n) {
      tmp <- stats::filter(eps_policy_sim, filter = IRF_sim[, j],
                           method = "convolution", sides = 1)
      path_policy_sim[, j] <- replace(as.numeric(tmp), is.na(tmp), 0)
    }
    
    counterfactual_prediction_sim  <- x_supply - path_policy_sim
    
    
    
    boot_counterfactual_IPI[,i] <- counterfactual_prediction_sim[, 4]
    boot_counterfactual_spot[,i] <- counterfactual_prediction_sim[, 1]
}


setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(boot_counterfactual_IPI, "boot_counterfactual_IPI_wind.csv")
write.csv(boot_counterfactual_spot, "boot_counterfactual_spot_wind.csv")




#etudie si la concession de ne pas faire une reocrering exact du choc
#permet ou non de reconstituer le price target


# dates must have length 129
dates <- IV_data_log_esp$observation_date

# pointwise bands (row-wise quantiles across bootstrap draws)
q90_lo <- apply(boot_counterfactual_spot, 1, quantile, probs = 0.05, na.rm = TRUE)
q90_hi <- apply(boot_counterfactual_spot, 1, quantile, probs = 0.95, na.rm = TRUE)

q68_lo <- apply(boot_counterfactual_spot, 1, quantile, probs = 0.16, na.rm = TRUE)
q68_hi <- apply(boot_counterfactual_spot, 1, quantile, probs = 0.84, na.rm = TRUE)

# central line (median is robust; you can use mean instead)
cf_med <- apply(boot_counterfactual_spot, 1, median, na.rm = TRUE)

band_df <- data.frame(
  observation_date = dates,
  q90_lo = q90_lo, q90_hi = q90_hi,
  q68_lo = q68_lo, q68_hi = q68_hi,
  cf_med = cf_med
)

ggplot() +
  # 90% band (wider, lighter)
  geom_ribbon(
    data = band_df,
    aes(x = observation_date, ymin = q90_lo, ymax = q90_hi),
    alpha = 0.2
  ) +
  # 68% band (narrower, darker)
  geom_ribbon(
    data = band_df,
    aes(x = observation_date, ymin = q68_lo, ymax = q68_hi),
    alpha = 0.5
  ) +
  # original series
  geom_line(
    data = IV_data_log_esp_counterfactual,
    aes(x = observation_date, y = counterfactual_spot_price_log, color = "Original"),
    linewidth = 0.3
  ) +
#   # counterfactual central path (median of bootstrap)
#   geom_line(
#     data = band_df,
#     aes(x = observation_date, y = cf_med, color = "Counterfactual (median)"),
#     linewidth = 0.8
#   ) +
  scale_color_manual(values = c("Original" = "Blue")) +
  labs(color = NULL, x = NULL, y = NULL) +
  theme_minimal()




#discard non-converging draws
#print fraction non converging

quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_probs_50 <- c(0.5, 0.5)

boot_counterfactual_IPI_var <- apply(
  boot_counterfactual_IPI,
  2,
  function(x) sqrt(var(x, na.rm = TRUE))/sd_obs
)
length(boot_counterfactual_IPI_var[boot_counterfactual_IPI_var > 10])

IPI_std_90 <- quantile(boot_counterfactual_IPI_var,
                       probs = c(0.05, 0.95),
                       na.rm = TRUE)
IPI_std_68 <- quantile(boot_counterfactual_IPI_var,
                       probs = c(0.16, 0.84),
                       na.rm = TRUE)

IPI_std_90
IPI_std_68
sd_ratio
