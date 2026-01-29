


source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_policy.R")
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





IV_data_log_esp_counterfactual <- left_join(IV_data_log_esp, counterfactual_wind_evolution %>% 
                                              mutate(year = as.numeric(year),
                                                     month = as.numeric(month)) %>% 
                                              dplyr::select(counterfactual_spot_price_log, year, month ),
                                            by = c("year", "month")
)%>%
  mutate( 
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

IRF <- lambda_0_log_supply_wind
if (length(dim(IRF)) == 3) IRF <- IRF[,,1]   # H x n
IRF <- as.matrix(IRF)

psi <- as.numeric(IRF[, 1])                  # longueur H, h=0..H-1



d_price <- IV_data_log_esp_counterfactual$spot_price_log -
  IV_data_log_esp_counterfactual$counterfactual_spot_price_log

out <- recover_policy_shocks_joint(d_price, psi, K = length(psi)-1, ridge = 1e-8)

eps_policy <- out$eps
pre_shocks <- out$pre

IV_data_log_esp_counterfactual$eps_policy <- eps_policy

IV_data_log_esp_counterfactual %>% 
  dplyr::select(eps_policy, wind_forecast_log, wind_speed_100m_log) %>%
  mutate(eps_policy = as.numeric(eps_policy)) %>%
  cor(use = "complete.obs")

ggplot(data = IV_data_log_esp_counterfactual, aes(x=observation_date))+
  # geom_line( aes( y = pre_shocks , color = "Policy"))+
  geom_line( aes( y = eps_policy , color = "Policy"))+
  #geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y = eps_policy_hat_lambda, color = "Policy"))+
  geom_line( aes( y = wind_forecast_log - mean(wind_forecast_log), color = "Wind"))


# predicted d
d_hat <- as.numeric(out$X %*% out$eps_full)

max(abs(d_price - d_hat), na.rm = TRUE)


Tp <- length(eps_policy)
H  <- nrow(IRF)
n  <- ncol(IRF)

# K pre-shocks: eps0, eps_-1, ... (length K)
K <- length(pre_shocks)

# Build padded shock series so that:
# index 1..K corresponds to times 0, -1, -2, ... (reverse time order in index)
# We need time order increasing: eps_{-K+1},...,eps_0, eps_1,...eps_T
# If your pre_shocks are stored as (eps0, eps_-1, ...), reverse them:
eps_pre_time <- rev(pre_shocks)                 # now eps_{-K+1} ... eps_0
eps_pad <- c(eps_pre_time, as.numeric(eps_policy))  # length K+Tp

path_policy <- matrix(0, nrow = Tp, ncol = n)
colnames(path_policy) <- colnames(IRF)

for (j in 1:n) {
  # Convolve on the padded series
  tmp_pad <- stats::filter(eps_pad, filter = IRF[, j],
                           method = "convolution", sides = 1)
  tmp_pad <- replace(as.numeric(tmp_pad), is.na(tmp_pad), 0)
  
  # Drop the first K entries to align with sample t=1..Tp
  path_policy[, j] <- tmp_pad[(K + 1):(K + Tp)]
}

# Counterfactual "without policy shock"
counterfactual_prediction <- x_supply - path_policy

# Plot check
library(ggplot2)
ggplot() +
  geom_line(data = IV_data_log_esp,
            aes(x = observation_date, y = spot_price_log, color = "Original")) +
  geom_line(data = IV_data_log_esp_counterfactual,
            aes(x = observation_date, y = counterfactual_spot_price_log, color = "Target")) +
  geom_line(aes(x = IV_data_log_esp$observation_date,
                y = counterfactual_prediction[, 1], color = "Counterfactual"))



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


nboot = 1000

bootIRFs_supply_counterfactual <- gamma_bootstrap_mbb_corrected_counterfactual(x= x_supply, nboot, theta_0 = theta_0_supply_wind ,
                                                                               proxies = proxies, controls = NULL, elas = 0,
                                                                               start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


bootIRFs_supply <- bootIRFs_supply_counterfactual$bootIRFs
boots_conterfactuals_pred <-  bootIRFs_supply_counterfactual$counterfactual_prediction_boot
dim(bootIRFs_supply )


# dernière valeur observée en niveau avant la prévision
boot_counterfactual_IPI <- array(NA, dim = c(Tp, nboot))
boot_counterfactual_spot <- array(NA, dim = c(Tp, nboot))
boot_counterfactual_HICP <- array(NA, dim = c(Tp, nboot))
#boot_counterfactual_level_IPI <- array(NA, dim = c((T+p), nboot))


for (i in 1:nboot) {
  # Matrice T x 4 remplie de 0
  #IRF_sim <- as.matrix(draws[,,i])
  # IRF_sim[1, 1] <- 1
  IRF_sim <- as.matrix(bootIRFs_supply[,,i])
  psi_sim <- as.numeric(IRF_sim[, 1])                  # longueur H, h=0..H-1
  
  #eps_policy_sim <- recover_policy_shock_exact(d_price, psi_sim)
  # eps_policy_sim <- recover_policy_shock_ridge_ls(d_price, psi_sim, lambda = 3e-1)
  
  out_sim <- recover_policy_shocks_joint(d_price, psi_sim, K = length(psi_sim)-1, ridge = 1e-8)
  # 
  eps_policy_sim <- out_sim$eps
  pre_shocks_sim <- out_sim$pre
  # 
  # IV_data_log_esp_counterfactual$eps_policy <- eps_policy
  
  # If your pre_shocks are stored as (eps0, eps_-1, ...), reverse them:
  eps_pre_time_sim <- rev(pre_shocks_sim)                 # now eps_{-K+1} ... eps_0
  eps_pad_sim <- c(eps_pre_time_sim, as.numeric(eps_policy_sim))  # length K+Tp
  
  path_policy_sim <- matrix(0, nrow = Tp, ncol = n)
  colnames(path_policy_sim) <- colnames(IRF)
  
  for (j in 1:n) {
    # Convolve on the padded series
    tmp_pad_sim <- stats::filter(eps_pad_sim, filter = IRF_sim[, j],
                             method = "convolution", sides = 1)
    tmp_pad_sim <- replace(as.numeric(tmp_pad_sim), is.na(tmp_pad_sim), 0)
    
    # Drop the first K entries to align with sample t=1..Tp
    path_policy_sim[, j] <- tmp_pad_sim[(K + 1):(K + Tp)]
  }
  counterfactual_prediction_sim  <- x_supply - path_policy_sim
  
  # dim(boot_counterfactual_IPI)
  # length(counterfactual_prediction_sim[, 4])
  boot_counterfactual_IPI[,i] <- counterfactual_prediction_sim[, 4]
  boot_counterfactual_HICP[,i] <- counterfactual_prediction_sim[, 5]
  boot_counterfactual_spot[,i] <- counterfactual_prediction_sim[, 1]
}


setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(boot_counterfactual_IPI, "boot_counterfactual_IPI_wind_with_preshocks.csv")
write.csv(boot_counterfactual_spot, "boot_counterfactual_spot_wind_with_preshocks.csv")
write.csv(boot_counterfactual_HICP, "boot_counterfactual_HICP_wind_with_preshocks.csv")



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



quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_probs_50 <- c(0.5, 0.5)


# ----------------------------
# Observed series (MUST match scale of boot series)
# ----------------------------
spot_obs <- as.numeric(x_supply[, 1])   # or IV_data_log_esp$spot_price_log if boots are log
IPI_obs  <- as.numeric(x_supply[, 4])   # or IV_data_log_esp$IPI_log
HICP_obs <- as.numeric(x_supply[, 5])   # or IV_data_log_esp$HICP_log

# ----------------------------
# Compute summaries
# ----------------------------
res_spot <- boot_sd_ratio_summary(boot_counterfactual_spot, spot_obs)
res_IPI  <- boot_sd_ratio_summary(boot_counterfactual_IPI,  IPI_obs)
res_HICP <- boot_sd_ratio_summary(boot_counterfactual_HICP, HICP_obs)

# ----------------------------
# Table for plotting (ratio + 90% CI)
# ----------------------------

library(grid)   # for unit()

plot_tbl <- tibble(
  Series = c("P_elec", "IPI", "HICP"),
  sd_ratio_median = c(res_spot$median, res_IPI$median, res_HICP$median),
  sd_ratio_p05    = c(res_spot$q90[[1]], res_IPI$q90[[1]], res_HICP$q90[[1]]),
  sd_ratio_p95    = c(res_spot$q90[[2]], res_IPI$q90[[2]], res_HICP$q90[[2]]),
  sd_ratio_p16    = c(res_spot$q68[[1]], res_IPI$q68[[1]], res_HICP$q68[[1]]),
  sd_ratio_p84    = c(res_spot$q68[[2]], res_IPI$q68[[2]], res_HICP$q68[[2]])
)

macro_fluct_plot <- ggplot(plot_tbl, aes(x = Series, y = sd_ratio_median)) +
  
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.4, alpha = 0.6) +
  
  geom_errorbar(aes(ymin = sd_ratio_p05, ymax = sd_ratio_p95, color = "90% CI"),
                width = 0.12, linewidth = 0.6) +
  
  geom_errorbar(aes(ymin = sd_ratio_p16, ymax = sd_ratio_p84, color = "68% CI"),
                width = 0.20, linewidth = 0.6) +
  
  geom_point(size = 2.5, color = "black") +
  
  scale_color_manual(
    name = NULL,
    values = c("90% CI" = "#6C8EBF", "68% CI" = "darkblue")
  ) +
  
  # ✅ plotmath x labels
  scale_x_discrete(labels = c(
    P_elec = expression(P[elec]),
    IPI    = "IPI",
    HICP   = "HICP"
  )) +
  
  labs(
    y = "SD ratio (counterfactual / observed)",
    x = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    legend.key.size = unit(1.5, "cm"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 12)
  )

macro_fluct_plot





save_plot_path <- 'Z:/Overleaf_elec/images/macro'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("macro_fluct.png", plot = macro_fluct_plot, width = 12, height = 6, dpi = 300)
