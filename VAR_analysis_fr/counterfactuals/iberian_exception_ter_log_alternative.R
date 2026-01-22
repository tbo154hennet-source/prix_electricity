
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

controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "none")
} else {
  vars::VAR(x_supply, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)



proxies <- ts(IV_data_log_esp[, c("delta_supply_incorpo")[[1]]], frequency=12)
theta_0_supply_wedge <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply_wedge <- get_lambda_0(C, theta_0_supply_wedge)




# d: effet observé (T×1) = observed - counterfactual, sur la variable cible
# psi: IRF (H×1) de cette variable au choc policy, horizon 0..H-1
# Renvoie eps_policy (T×1)

recover_policy_shock <- function(d, psi) {
  d <- as.numeric(d)
  psi <- as.numeric(psi)
  T <- length(d)
  H <- length(psi)
  
  # Matrice Toeplitz triangulaire: d_t = sum_{h=0}^{min(H-1,t-1)} psi[h+1] * eps[t-h]
  K <- matrix(0, nrow = T, ncol = T)
  for (t in 1:T) {
    hmax <- min(H - 1, t - 1)
    for (h in 0:hmax) {
      K[t, t - h] <- psi[h + 1]
    }
  }
  
  # Solve (least squares in case of noise / non-invertibility)
  eps_hat <- qr.solve(K, d)
  eps_hat
}


# Example: target = electricity price gap
d_price <-IV_data_log_esp_counterfactual$spot_price_log - IV_data_log_esp_counterfactual$counterfactual_spot_price_log 

# psi: IRF of price to policy shock (from your identified supply shock channel)
psi <- lambda_0_log_supply_wedge  # length H, starting at h=0

eps_policy_hat <- recover_policy_shock(d_price, psi)
IV_data_log_esp_counterfactual$eps_policy_hat <- eps_policy_hat
ggplot()+
  geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y = eps_policy_hat, color = "Policy"))+
  geom_line( aes(x= IV_data_fr$observation_date, y = IV_data_fr$spot_price/100, color = "FR"))



# Contribution du choc policy aux innovations VAR (T-p x n)
u_policy <- eps_policy_hat[-(1:p)] %o% theta_0_supply_wedge


# Créer une matrice de zéros
# eps_mat <- matrix(0, nrow = T, ncol = n)
# # Mettre le choc uniquement dans la 1ère colonne
# eps_mat[, 1] <- as.numeric(eps_policy_hat[-(1:p)])
# Résidus contrefactuels
counterfactual_residuals <- res - u_policy



counterfactual_prediction <- iterate_var_ter(x= x_supply, initial_obs = vars_supply[1:p,], 
                                             errors =counterfactual_residuals , var_model = vl)
dim(counterfactual_prediction)
colnames(counterfactual_prediction)

ggplot()+
  geom_line(data = IV_data_log_esp, aes(x= observation_date, y= spot_price_log, color = "Original"))+
  geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y= counterfactual_spot_price_log, color = "Target"))+
  geom_line( aes(x= IV_data_log_esp$observation_date, y= counterfactual_prediction$spot_price_log, color = "Counterfactual"))
# 
ggplot()+
  geom_line(data = IV_data_log_esp, aes(x= observation_date, y= exp(spot_price_log), color = "Original"))+
  geom_line(data = IV_data_log_esp_counterfactual, aes(x= observation_date, y= counterfactual_spot_price, color = "Target"))+
  geom_line( aes(x= IV_data_log_esp$observation_date, y= exp(counterfactual_prediction$spot_price_log), color = "Counterfactual"))


HICP_pred  <- counterfactual_prediction[, 5]
HICP_diff_pred<- c(NA, diff(HICP_pred))
inflation_pred<- slide_dbl(HICP_diff_pred, sum, .before = 11, .complete = TRUE)
inflation_pred[(T_gas+p):(T_gas+h+p)]
inflation_obs[(T_gas+p):(T_gas+h+p)]


ggplot()+
  geom_line(data = IV_data_log_esp, aes(x= observation_date, y= inflation_obs, color = "Original"))+
  geom_line( aes(x= IV_data_log_esp$observation_date, y= inflation_pred, color = "Counterfactual"))




HICP_pred  <- counterfactual_prediction[, 5]
HICP_diff_pred<- c(NA, diff(HICP_pred))
inflation_pred<- slide_dbl(HICP_diff_pred, sum, .before = 11, .complete = TRUE)
inflation_pred[(T_gas+p):(T_gas+h+p)]
inflation_obs[(T_gas+p):(T_gas+h+p)]


# View(IV_data_esp_sim %>% 
#        dplyr::select(counterfactual_spot_price, counterfactual_spot_price_sim, spot_price, policy_shock_sim, year, month))
#a chaque fois il faut que je tire une trajectoire de policy shock
nboot = 100

bootIRFs_supply_counterfactual <- gamma_bootstrap_mbb_corrected_counterfactual(x= x_supply, nboot, theta_0 = theta_0_supply_wedge ,
                                                        proxies = proxies, controls = NULL, elas = 0,
                                                        start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


bootIRFs_supply <- bootIRFs_supply_counterfactual$bootIRFs
bootss_supply <-  bootIRFs_supply_counterfactual$boot_structural_shocks
bootres_supply <-  bootIRFs_supply_counterfactual$bootres_s


# dernière valeur observée en niveau avant la prévision
boot_counterfactual_HICP <- array(NA, dim = c((T+p), nboot))
boot_counterfactual_spot <- array(NA, dim = c((T+p), nboot))
#boot_counterfactual_level_HICP <- array(NA, dim = c((T+p), nboot))

for (i in 1:nboot) {
    theta_0_i <- bootIRFs_supply[,,i]
    # Matrice T x 4 remplie de 0
    
    IV_data_esp_sim <- IV_data_log_esp_counterfactual %>%
      mutate(
        counterfactual_spot_price_sim = rnorm(n = n(), mean = counterfactual_spot_price, sd = se_pred_month),
        counterfactual_spot_price_log_sim = log(counterfactual_spot_price_sim),
        counterfactual_spot_price_log_sim = ifelse(is.na(counterfactual_spot_price_log_sim), spot_price_log, counterfactual_spot_price_log_sim)
      )
    
    # Example: target = electricity price gap
    d_price_sim <-IV_data_esp_sim$spot_price_log - IV_data_esp_sim$counterfactual_spot_price_log_sim 
    

    eps_policy_hat_sim <- recover_policy_shock(d_price_sim, lambda_0_log_supply_wedge)
    u_policy_sim <- eps_policy_hat_sim[-(1:p)] %o% theta_0_i[1,]
    counterfactual_residuals_sim <- res - u_policy_sim
    counterfactual_prediction_sim <- iterate_var_ter(x= x_supply, initial_obs = vars_supply[1:p,], 
                                                 errors =counterfactual_residuals_sim , var_model = vl)
    
    boot_counterfactual_HICP[,i] <- counterfactual_prediction_sim[, 5]
    boot_counterfactual_spot[,i] <- counterfactual_prediction_sim[, 1]
}


setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(boot_counterfactual_HICP, "boot_counterfactual_HICP.csv")
write.csv(boot_counterfactual_spot, "boot_counterfactual_spot.csv")




length(counterfactual_prediction[, 1])

HICP_pred  <- counterfactual_prediction[, 5]
HICP_diff_pred<- c(NA, diff(HICP_pred))
inflation_pred<- slide_dbl(HICP_diff_pred, sum, .before = 11, .complete = TRUE)
inflation_pred[(T_gas+p):(T_gas+h+p)]

h <- 7

boot_counterfactual_HICP_inflation_12[,2][(T_gas+p):(T_gas+h+p)]
quantiles_50_inflation[(T_gas+p):(T_gas+h+p)]

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
quantiles_50_inflation[(T_gas):(T_gas+h)]


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


# ---- Extraire série originale (colonne 4) ----
price_obs <- x_supply[, 1]
hicp_obs <- x_supply[, 5]

quantiles_90_spot <- apply(exp(boot_counterfactual_spot[(p+1):(T+p),]),
                                c(1), function(x) quantile(x, probs = quantile_probs_90))

quantiles_68_spot<- apply(exp(boot_counterfactual_spot[(p+1):(T+p),]),
                               c(1), function(x) quantile(x, probs = quantile_probs_68))

quantiles_50_spot <- apply(exp(boot_counterfactual_spot[(p+1):(T+p),]),
                               c(1), function(x) quantile(x, probs = quantile_probs_50))[1,]

quantiles_50_spot[(T_gas):(T_gas+h)]
IV_data_log_esp$spot_price[(T_gas+p):(T_gas+h+p)]


df_price <- data.frame(
  date = c(IV_data_log_esp$observation_date),
  value = c(rep(NA, p),price_pred),
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


HICP_obs  <- IV_data_log_esp$HICP_log
HICP_diff_obs<- c(NA, diff(HICP_obs))
inflation_obs <- slide_dbl(HICP_diff_obs, sum, .before = 11, .complete = TRUE)



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


# View(df_plot_all %>% 
#   filter(grandeur == "spot_price" & type == "Forecast"))
View(df_plot_all_scaled  %>%
       filter(grandeur == "spot_price" & type == "Forecast"))
View(df_plot_all_scaled  %>%
       filter(grandeur == "spot_price" & type == "Observed"))

# ---- Plot avec axe secondaire ----
counterfactual <- ggplot() +
  # Spot_price
  geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "spot_price", type == "Forecast"),
              aes(x = date, ymin = lower_68_plot, ymax = upper_68_plot,
                  fill = "spot_price 68% CI"), alpha = 0.2) +
  geom_line(data = df_plot_all_scaled %>% filter(grandeur == "spot_price"),
            aes(x = date, y = value_plot, linetype = type, color = grandeur),
            size = 0.5) +
  
  # HICP (multiplié par 100)
  geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "inflation", type == "Forecast"),
              aes(x = date, ymin = lower_68_plot, ymax = upper_68_plot,
                  fill = "inflation 68% CI"), alpha = 0.2) +
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
      "spot_price 68% CI" = "red",
      "inflation 68% CI"  = "blue"
    ),
    labels = c(
      "spot_price 68% CI" = expression(P[elec]~"68% CI"),
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




#De combien a réduit l'inflation sur 2022


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


#3,8 points d'inflation sur l'année 2022
# 2,9 sur la période d'iberian exception même si pas binding vers la fin



