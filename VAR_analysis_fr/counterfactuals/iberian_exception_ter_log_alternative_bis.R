
library(MASS)
library(slider)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
pred_post_df <- read_parquet( "prediction_gas_price_counterfactual_monthly.parquet")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}


counterfactual_gas_evolution <- pred_post_df %>% 
  rename(counterfactual_spot_price = predicted_price) %>% 
  arrange(year, month)

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <- read_parquet( 'IV_data_esp_log_elas_sd_2026_pc.parquet')
save_var_path <- 'Z:/prix_electricite_gitlab/Data_Structuring/output'
setwd(save_var_path)
IV_data_fr <-  read_parquet('IV_data_fr_elas.parquet')



IV_data_log_esp_counterfactual <- left_join(IV_data_log_esp, counterfactual_gas_evolution %>% 
                                          mutate(year = as.numeric(year),
                                                 month = as.numeric(month)) %>% 
                                            dplyr::select(counterfactual_spot_price, se_pred_month, year, month, se_pred_month ),
                                        by = c("year", "month")
                                        )%>%
  mutate( counterfactual_spot_price = counterfactual_spot_price / HICP*100,
          se_pred_month = se_pred_month / HICP*100,
          counterfactual_spot_price_log = ifelse(is.na(counterfactual_spot_price), spot_price_log, log(counterfactual_spot_price)),
          counterfactual_spot_price = ifelse(is.na(counterfactual_spot_price), spot_price, counterfactual_spot_price),
          se_pred_month = ifelse(is.na(counterfactual_spot_price), 0, se_pred_month),
          #counterfactual_spot_price_log = log(counterfactual_spot_price),
          #j'interpr?te la diff?rence comme un choc pur de policy
          policy_shock = counterfactual_spot_price_log - spot_price_log
          )


# View(IV_data_log_esp_counterfactual %>% 
#   filter(year >=2022) %>% 
#   dplyr::select(year, month, spot_price, counterfactual_spot_price ))
# 
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
theta_0_supply_wedge <- first_var_estimation_bis(x_supply, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_wedge <- get_lambda_0(C, theta_0_supply_wedge)


# d: effet observé (T×1) = observed - counterfactual, sur la variable cible
# psi: IRF (H×1) de cette variable au choc policy, horizon 0..H-1
# Renvoie eps_policy (T×1)


IRF <- lambda_0_log_supply_wedge
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
eps_policy <- recover_policy_shock_exact(d_price, psi)
IV_data_log_esp_counterfactual$eps_policy <- eps_policy

ggplot()+
  geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y = eps_policy, color = "Policy"))+
  #geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y = eps_policy_hat_lambda, color = "Policy"))+
  geom_line( aes(x= IV_data_fr$observation_date, y = IV_data_fr$spot_price/100, color = "FR"))

# reconstruit d_hat via convolution
d_hat <- as.numeric(stats::filter(eps_policy, filter = psi,
                                  method = "convolution", sides = 1))
d_hat[is.na(d_hat)] <- 0

max(abs(d_price - d_hat), na.rm = TRUE)

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
hicp_obs <- x_supply[, 5]
HICP_obs  <- IV_data_log_esp$HICP_log
HICP_diff_obs<- c(NA, diff(HICP_obs))
inflation_obs <- slide_dbl(HICP_diff_obs, sum, .before = 11, .complete = TRUE)

price_pred <- counterfactual_prediction[, 1]
HICP_pred  <- counterfactual_prediction[, 5]
HICP_diff_pred<- c(NA, diff(HICP_pred))
inflation_pred<- slide_dbl(HICP_diff_pred, sum, .before = 11, .complete = TRUE)
inflation_pred[(T_gas+p):(T_gas+h+p)]
inflation_obs[(T_gas+p):(T_gas+h+p)]

ggplot()+
  geom_line(data = IV_data_log_esp, aes(x= observation_date, y= inflation_obs, color = "Original"))+
  geom_line( aes(x= IV_data_log_esp$observation_date, y= inflation_pred, color = "Counterfactual"))




# View(IV_data_esp_sim %>% 
#        dplyr::select(counterfactual_spot_price, counterfactual_spot_price_sim, spot_price, policy_shock_sim, year, month))
#a chaque fois il faut que je tire une trajectoire de policy shock
nboot = 2000

bootIRFs_supply_counterfactual <- gamma_bootstrap_mbb_corrected_counterfactual(x= x_supply, nboot, theta_0 = theta_0_supply_wedge ,
                                                        proxies = proxies, controls = NULL, elas = 0,
                                                        start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


bootIRFs_supply <- bootIRFs_supply_counterfactual$bootIRFs
boots_conterfactuals_pred <-  bootIRFs_supply_counterfactual$counterfactual_prediction_boot
dim(bootIRFs_supply )

# Sous-array: 129 x 100 (time x bootstrap)
boot_var5 <- boots_conterfactuals_pred[, 5, ]
dim(boot_var5)
# [1] 129 100
quantiles_var5 <- t(apply(boot_var5, 1, function(x) {
  c(
    p05 = quantile(x, 0.05, na.rm = TRUE),
    p50 = quantile(x, 0.50, na.rm = TRUE),
    p95 = quantile(x, 0.95, na.rm = TRUE)
  )
}))


dim(boots_conterfactuals_pred)

# dernière valeur observée en niveau avant la prévision
boot_counterfactual_HICP <- array(NA, dim = c((T+p), nboot))
boot_counterfactual_spot <- array(NA, dim = c((T+p), nboot))
#boot_counterfactual_level_HICP <- array(NA, dim = c((T+p), nboot))

for (i in 1:nboot) {
    # Matrice T x 4 remplie de 0

    
    IRF_sim <- as.matrix(bootIRFs_supply[,,i])
    
    psi_sim <- as.numeric(IRF_sim[, 1])                  # longueur H, h=0..H-1
    
    eps_policy_sim <- recover_policy_shock_exact(d_price, psi_sim)
    
    path_policy_sim <- matrix(0, nrow = Tp, ncol = n)
    for (j in 1:n) {
      tmp <- stats::filter(eps_policy_sim, filter = IRF_sim[, j],
                           method = "convolution", sides = 1)
      path_policy_sim[, j] <- replace(as.numeric(tmp), is.na(tmp), 0)
    }
    
    counterfactual_prediction_sim  <- x_supply - path_policy_sim
    
    
    
    boot_counterfactual_HICP[,i] <- counterfactual_prediction_sim[, 5]
    boot_counterfactual_spot[,i] <- counterfactual_prediction_sim[, 1]
}


setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(boot_counterfactual_HICP, "boot_counterfactual_HICP_bis.csv")
write.csv(boot_counterfactual_spot, "boot_counterfactual_spot_bis.csv")


quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_probs_50 <- c(0.5, 0.5)

boot_counterfactual_HICP_diff <- apply(
  boot_counterfactual_HICP,
  2,
  function(x) c(NA, diff(x))
)
boot_counterfactual_HICP_inflation_12 <- apply(
  boot_counterfactual_HICP_diff ,
  2,
  function(x) slide_dbl(x, sum, .before = 11, .complete = TRUE)
)



quantiles_90_inflation <- apply(boot_counterfactual_HICP_inflation_12[(p+1):(T+p),],
                      c(1), function(x) quantile(x, probs = quantile_probs_90))

quantiles_68_inflation<- apply(boot_counterfactual_HICP_inflation_12[(p+1):(T+p),],
                      c(1), function(x) quantile(x, probs = quantile_probs_68))

quantiles_50_inflation<- apply(boot_counterfactual_HICP_inflation_12[(p+1):(T+p),],
                               c(1), function(x) quantile(x, probs = quantile_probs_50))[1,]

inflation_pred[(T_gas+p):(T_gas+h+p)]
inflation_obs[(T_gas+p):(T_gas+h+p)]
quantiles_90_inflation[,(T_gas):(T_gas+h)]

inflation_lower_90 <- t(quantiles_90_inflation   )[,1]            # fcst - 1 sigma
inflation_upper_90 <- t(quantiles_90_inflation   )[,2]               # fcst + 1 sigma
inflation_lower_68 <- t(quantiles_68_inflation   )[,1]             # fcst - 1 sigma
inflation_upper_68 <- t(quantiles_68_inflation   )[,2]


df_inflation <- data.frame(
  date = c(IV_data_log_esp$observation_date),
  #value = inflation_pred,
  value = inflation_pred,
  type = "Forecast",
  lower_68 = c(rep(NA, p),inflation_lower_68),
  upper_68 =  c(rep(NA, p),inflation_upper_68),
  lower_90 = c(rep(NA, p),inflation_lower_90),
  upper_90 =  c(rep(NA, p),inflation_upper_90),
 
  grandeur = "inflation"
) 




quantiles_90_spot <- apply(exp(boot_counterfactual_spot[(p+1):(T+p),]),
                                c(1), function(x) quantile(x, probs = quantile_probs_90))

quantiles_68_spot<- apply(exp(boot_counterfactual_spot[(p+1):(T+p),]),
                               c(1), function(x) quantile(x, probs = quantile_probs_68))

quantiles_50_spot <- apply(exp(boot_counterfactual_spot[(p+1):(T+p),]),
                               c(1), function(x) quantile(x, probs = quantile_probs_50))[1,]


quantiles_50_spot[(T_gas-2):(T_gas+h)]
IV_data_log_esp$spot_price[(T_gas-2+p):(T_gas+h+p)]


df_price <- data.frame(
  date = c(IV_data_log_esp$observation_date),
  value = c(exp(price_pred)),
  type = "Forecast",
  #type = c(rep("Observed", T+p-1), rep("Forecast", h+1)),
  lower_68 =  c(rep(NA, p),t(quantiles_68_spot  )[,1]) ,
  upper_68 =  c(rep(NA, p),t(quantiles_68_spot  )[,2]) ,
  lower_90 = c(rep(NA, p),t(quantiles_90_spot  )[,1] ),
  upper_90 =  c(rep(NA, p),t(quantiles_90_spot  )[,2]) ,
  grandeur = "spot_price"
) 

#   mutate(lower =ifelse(date <= as.Date("2022-06-01"), NA, lower),
#          upper =ifelse(date <= as.Date("2022-06-01"), NA, upper)
#          )
# ---- Combiner les deux séries ----
df_plot_all <- rbind(df_price, df_inflation)

# ---- Construire dataframe Observed pour spot_price ----
df_obs_price <- data.frame(
  date = IV_data_log_esp$observation_date,
  value = IV_data_log_esp$spot_price,
  type = "Observed",
  grandeur = "spot_price",
  lower_68 = NA,
  upper_68 = NA,
  lower_90 = NA,
  upper_90 = NA
)





# ---- Construire dataframe Observed pour HICP ----
df_obs_HICP <- data.frame(
  date = IV_data_log_esp$observation_date,
  value = inflation_obs,
  type = "Observed",
  grandeur = "inflation",
  lower_68 = NA,
  upper_68 = NA,
  lower_90 = NA,
  upper_90 = NA
)


# ---- Combiner Observed avec Forecast déjà calculé (df_plot_all) ----
df_plot_all <- rbind(df_plot_all, df_obs_price, df_obs_HICP)



# Créer une colonne transformée pour HICP
df_plot_all_scaled <- df_plot_all %>%
  mutate(
    value_plot = ifelse(grandeur == "inflation", value * 1000, value),
    #median_plot = ifelse(grandeur == "inflation", median * 1000, median),
    lower_68_plot = ifelse(grandeur == "inflation", lower_68 * 1000, lower_68),
    upper_68_plot = ifelse(grandeur == "inflation", upper_68 * 1000, upper_68),
    lower_90_plot = ifelse(grandeur == "inflation", lower_90 * 1000, lower_90),
    upper_90_plot = ifelse(grandeur == "inflation", upper_90 * 1000, upper_90)
  )

# 
# 
View(df_plot_all_scaled  %>%
       filter(grandeur == "inflation" & type == "Forecast"))
# View(df_plot_all_scaled  %>%
#        filter(grandeur == "spot_price" & type == "Observed"))

# ---- Plot avec axe secondaire ----
counterfactual <- ggplot() +
  # Spot_price
  # geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "spot_price", type == "Forecast"),
  #             aes(x = date, ymin = lower_90_plot, ymax = upper_90_plot,
  #                 fill = "spot_price 90% CI"), alpha = 0.2) +
  geom_line(data = df_plot_all_scaled %>% filter(grandeur == "spot_price"),
            aes(x = date, y = value_plot, linetype = type, color = grandeur),
            size = 0.5) +
  
  # HICP (multiplié par 100)
  geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "inflation", type == "Forecast"),
              aes(x = date, ymin = lower_68_plot, ymax = upper_68_plot,
                  fill = "inflation 68% CI"), alpha = 0.2) +
  # geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "inflation", type == "Forecast"),
  #             aes(x = date, ymin = lower_90_plot, ymax = upper_90_plot,
  #                 fill = "inflation 90% CI"), alpha = 0.2) +
  geom_line(data = df_plot_all_scaled %>% filter(grandeur == "inflation"),
            aes(x = date, y = value_plot, linetype = type, color = grandeur),
            size = 0.5) +
  # Vertical line for T_gas
  geom_vline(xintercept = as.Date("2022-06-01"), 
             linetype = "dotted", color = "black", linewidth = 0.6) +
  theme_minimal() +
  labs(
    title = "",
    x = "Date",
    y = expression(paste("Price (", "\u20AC","/Mwh)"))
  ) +
  scale_linetype_manual(values = c("Observed" = "solid", "Forecast" = "dashed")) +
  scale_color_manual(values = c("spot_price" = "red", "inflation" = "blue")) +
  scale_fill_manual(
    values = c(
      "spot_price 90% CI" = "red",
      "inflation 68% CI"  = "blue"
    ),
    labels = c(
      "spot_price 90% CI" = expression(P[elec]~"90% CI"),
      "inflation 68% CI"  = expression(pi~"68% CI")
    )
  )+
  # Axe secondaire pour HICP
  scale_y_continuous(
    sec.axis = sec_axis(~./100, name = "")
  )+
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )

counterfactual


save_plot_path <- 'Z:/Overleaf_elec/images/policy'
setwd(save_plot_path)
ggsave("iberian_exception_plot_inflation_counterfactual.png", plot = counterfactual, width = 14 , height = 6, dpi = 300)





boot_summary <- t(apply(boot_counterfactual_HICP[(T_gas+p):(T_gas+h+p),], 1, function(x) {
  c(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    p05    = quantile(x, 0.05, na.rm = TRUE),
    p95    = quantile(x, 0.95, na.rm = TRUE)
  )
}))

boot_summary <- as.data.frame(boot_summary)
boot_summary$Time <- seq_len(nrow(boot_summary))  # ou une vraie date

boot_summary



boot_counterfactual_HICP_diff_iberic_exception <- apply(
  boot_counterfactual_HICP,
  2,
  function(x) x[T_gas+p+h]- x[T_gas+p])
quantile(boot_counterfactual_HICP_diff_iberic_exception, 0.05, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception, 0.95, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception, 0.16, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception, 0.84, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception, 0.50, na.rm = TRUE)

HICP_pred[T_gas+p+h]- HICP_pred[T_gas+p]

delta_log <- HICP_pred[T_gas + p + h] - HICP_pred[T_gas + p]
delta_counterfactual <- HICP_obs[T_gas + p + h] - HICP_obs[T_gas + p]

infl_cum_pct <- 100 * (exp(delta_log -delta_counterfactual) - 1)
#points d'infaltion évités sur la période
infl_cum_pct


boot_counterfactual_HICP_diff_iberic_exception_inf_points <- apply(
  boot_counterfactual_HICP,
  2,
  function(x) 100 * (exp(x[T_gas+p+h]- x[T_gas+p] - (HICP_obs[T_gas + p + h] - HICP_obs[T_gas + p])) - 1)
)
quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.05, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.95, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.16, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.84, na.rm = TRUE)
quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.50, na.rm = TRUE)

# 
# quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.16, na.rm = TRUE)
# 16%
# 1.971137
# > quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.84, na.rm = TRUE)
# 84%
# 3.762433
# > quantile(boot_counterfactual_HICP_diff_iberic_exception_inf_points, 0.50, na.rm = TRUE)
# 50%
# 3.108533

df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2022-12-01")) %>% 
  dplyr::select(date, value, lower_68, upper_68)
diff(df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2022-12-01")) %>% 
       dplyr::select(date, value, lower_68, upper_68) %>% 
       pull(value))

df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2023-06-01")) %>% 
  dplyr::select(date, value, lower_68, upper_68)
diff(df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2023-06-01")) %>% 
       dplyr::select(date, value, lower_68, upper_68) %>% 
       pull(value))




