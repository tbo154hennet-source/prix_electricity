
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





# Matrice T x 4 remplie de 0
dim(eps_policy_hat)
class(eps_policy_hat)


counterfactual_residuals <- res - contrib[-(1:p),] 

counterfactual_prediction <- iterate_var_ter(x= x_supply, initial_obs = vars_supply[1:p,], 
                                    errors =counterfactual_residuals , var_model = vl)


HICP_pred  <- counterfactual_prediction[, 5]
HICP_diff_pred<- c(NA, diff(HICP_pred))
inflation_pred<- slide_dbl(HICP_diff_pred, sum, .before = 11, .complete = TRUE)
inflation_pred[(T_gas+p):(T_gas+h+p)]
inflation_obs[(T_gas+p):(T_gas+h+p)]


counterfactual_prediction[, 1]
# View(IV_data_esp_sim %>% 
#        dplyr::select(counterfactual_spot_price, counterfactual_spot_price_sim, spot_price, policy_shock_sim, year, month))
#a chaque fois il faut que je tire une trajectoire de policy shock
nboot = 100
policy_shock_samples <- vector("list", nboot)
#get confidence interval by using bootstrap

bet = do.call(cbind, lapply(1:n, function(x) coef(vl)[[x]][,1]))
coef_hat <- as.vector(bet)
# Covariance des coefficients
coef_sample <- mvrnorm(n = nboot, mu = coef_hat, Sigma = vcov(vl))  # tirage

counterfactual_scenario <- vector("list", nboot)

bootIRFs_supply_counterfactual <- gamma_bootstrap_mbb_corrected_counterfactual(x= x_supply, nboot, theta_0 = theta_0_supply_wedge ,
                                                        proxies = proxies, controls = NULL, elas = 0,
                                                        start_ts = c(2015,1), end_ts =c(2025,9), irhor = irhor)


bootIRFs_supply <- bootIRFs_supply_counterfactual$bootIRFs
bootss_supply <-  bootIRFs_supply_counterfactual$boot_structural_shocks



# dernière valeur observée en niveau avant la prévision
boot_counterfactual_HICP <- array(NA, dim = c((T+p), nboot))
#boot_counterfactual_level_HICP <- array(NA, dim = c((T+p), nboot))

for (i in 1:nboot) {
    theta_0_i <- bootIRFs_supply[,,i]
    # Matrice T x 4 remplie de 0
    
    policy_res_i <- matrix(eps_policy_hat, nrow = T, ncol = n) * 
      matrix(theta_0_i, nrow = T, ncol = n, byrow = TRUE)
    
    counterfactual_residuals_i <- res + policy_res_i
    
    counterfactual_prediction_i <- iterate_var_ter(x= x_supply, initial_obs = vars_supply[1:p,], 
                                                 errors =counterfactual_residuals_i , var_model = vl)
    
    boot_counterfactual_HICP[,i] <- counterfactual_prediction_i[, 5]
    # policy_res_sim <- matrix(policy_shock_sim[-(1:p)], nrow = T, ncol = n) * 
    #   matrix(theta_0_i, nrow = T, ncol = n, byrow = TRUE)
    # 
    # counterfactual_residuals_i <- res + policy_res_sim
    # 
    # # counterfactual_prediction_i <- iterate_var_ter(x= x, initial_obs = vars[1:p,], 
    # #                                              errors = counterfactual_residuals_i , var_model =  vl_sim)
    # counterfactual_prediction_i <- iterate_var_ter(x= x_supply, initial_obs = vars[1:p,], 
    #                                                errors = counterfactual_residuals_i , var_model =  vl)
    # 
    # counterfactual_scenario[i] <- counterfactual_prediction_i
    # 
    # HICP_pred_i  <- counterfactual_prediction_i[, 5]
    # boot_counterfactual_HICP[,i] <- HICP_pred_i
}


setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(boot_counterfactual_HICP, "boot_counterfactual_HICP.csv")


boot_counterfactual_HICP <- read.csv( "boot_counterfactual_HICP.csv")




# ---- Extraire série originale (colonne 4) ----
price_obs <- vars[, 1]
hicp_obs <- vars[, 4]

# ---- Extraire prévisions et IC (colonne 4) ----
price_pred  <- counterfactual_prediction[, 1]
#price_pred  <- IV_data_log_esp_counterfactual$counterfactual_spot_price
sigma <-IV_data_log_esp_counterfactual$se_pred_month
price_lower_90 <-price_pred - 1.6*sigma               # fcst - 1 sigma
price_upper_90 <- price_pred + 1.6*sigma               # fcst + 1 sigma

price_lower_68 <-price_pred - sigma               # fcst - 1 sigma
price_upper_68 <- price_pred + sigma               # fcst + 1 sigma



df_price <- data.frame(
  date = c(IV_data_log_esp$observation_date),
  value = price_pred,
  type = "Forecast",
  #type = c(rep("Observed", T+p-1), rep("Forecast", h+1)),
  lower_68 = price_lower_68,
  upper_68 =  price_upper_68,
  lower_90 = price_lower_90,
  upper_90 =  price_upper_90,
  grandeur = "spot_price"
) 





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


#
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
    value_plot = ifelse(grandeur == "HICP", value * 1000, value),
    lower_68_plot = ifelse(grandeur == "HICP", lower_68 * 1000, lower_68),
    upper_68_plot = ifelse(grandeur == "HICP", upper_68 * 1000, upper_68),
    lower_90_plot = ifelse(grandeur == "HICP", lower_90 * 1000, lower_90),
    upper_90_plot = ifelse(grandeur == "HICP", upper_90 * 1000, upper_90),
    value_plot = ifelse(grandeur == "inflation", value * 1000, value),
    #median_plot = ifelse(grandeur == "inflation", median * 1000, median),
    lower_68_plot = ifelse(grandeur == "inflation", lower_68 * 1000, lower_68),
    upper_68_plot = ifelse(grandeur == "inflation", upper_68 * 1000, upper_68),
    lower_90_plot = ifelse(grandeur == "inflation", lower_90 * 1000, lower_90),
    upper_90_plot = ifelse(grandeur == "inflation", upper_90 * 1000, upper_90)
  )





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
      "inflation 68% CI" = "blue")
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
0.08986621 - 0.05288838

df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2023-06-01")) %>% 
  dplyr::select(date, value, lower_68, upper_68)
0.04255027 - 0.01589382
