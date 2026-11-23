
library(MASS)
library(slider)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
pred_post_df <- read_parquet( "prediction_gas_price_counterfactual_monthly.parquet")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}


counterfactual_gas_evolution <- pred_post_df %>% 
  rename(counterfactual_spot_price = predicted_price) %>% 
  arrange(year, month)


IV_data_esp_log_diff_counterfactual <- left_join(IV_data_log_diff_esp, counterfactual_gas_evolution %>% 
                                          mutate(year = as.numeric(year),
                                                 month = as.numeric(month)) %>% 
                                            dplyr::select(counterfactual_spot_price, se_pred_month, year, month, se_pred_month ),
                                        by = c("year", "month")
                                        )%>%
  mutate( counterfactual_spot_price = counterfactual_spot_price / HICP*100,
          se_pred_month = se_pred_month / HICP*100,
          counterfactual_spot_price = ifelse(observation_date <= as.Date("2022-05-01"), spot_price, counterfactual_spot_price),
          se_pred_month = ifelse(observation_date <= as.Date("2022-05-01"), 0, se_pred_month),
          #remplace pour la pr?-p?riode non-pr?dite
          #counterfactual_spot_price = ifelse(observation_date <= as.Date("2022-05-01"), spot_price , counterfactual_spot_price),
          counterfactual_spot_price_log_diff = log(counterfactual_spot_price + 1) - log(lag(counterfactual_spot_price) + 1),
          #j'interpr?te la diff?rence comme un choc pur de policy
          policy_shock = counterfactual_spot_price_log_diff - spot_price_log_diff
          )

IV_data_esp_log_diff_counterfactual$policy_shock

View(IV_data_esp_log_diff_counterfactual %>% 
  filter(year >=2022) %>% 
  dplyr::select(year, month, spot_price, counterfactual_spot_price ))



proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_supply)], frequency=12)
proxies[is.na(proxies)] <- 0

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp)], frequency=12)

p = 12
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
n = ncol(y)
#Fit the VAR model
vl <- vars::VAR(x, p, type = "none")

#k  = ncol(proxies)
#n = ncol(y)
coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
list.files()
gen_benchmark_esp_supply_mp <- readRDS(  "gen_benchmark_esp_mp_supply_save.rds")

names(gen_benchmark_esp_supply_mp$sample_matrices)


theta_0_log_diff <-  gen_benchmark_esp_supply_mp$theta_0

#rejouer en propageant les r?sidus auxquels on ajoute le structural shock elec supply
policy_shock <- IV_data_esp_log_diff_counterfactual$policy_shock
policy_shock[is.na(policy_shock)] <- 0
# Matrice T x 4 remplie de 0

policy_res <- matrix(policy_shock[-(1:p)], nrow = T, ncol = n) * 
  matrix(theta_0_log_diff, nrow = T, ncol = n, byrow = TRUE)

counterfactual_residuals <- res + policy_res  

counterfactual_prediction <- iterate_var_ter(x= x, initial_obs = vars[1:p,], 
                                    errors =counterfactual_residuals , var_model = vl)



set.seed(123)  # pour reproductibilit?
IV_data_esp_log_diff_counterfactual$counterfactual_spot_price
# 
# View(IV_data_esp_sim %>% 
#        dplyr::select(counterfactual_spot_price, counterfactual_spot_price_sim, spot_price, policy_shock_sim, year, month))
#a chaque fois il faut que je tire une trajectoire de policy shock
boot_size = 1000
policy_shock_samples <- vector("list", boot_size)
#get confidence interval by using bootstrap

bet = do.call(cbind, lapply(1:n, function(x) coef(vl)[[x]][,1]))
coef_hat <- as.vector(bet)
# Covariance des coefficients
coef_sample <- mvrnorm(n = boot_size, mu = coef_hat, Sigma = vcov(vl))  # tirage

counterfactual_scenario <- vector("list", boot_size)


vars_gas <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_var_spot_first_mp)),
  frequency = 12
)
x_gas =ts(vars_gas, start=c(2015,1), end=c(2022,5), frequency=12)
y_gas =vars_gas[-1:-p, ]
T_gas = nrow(y_gas)
# dernière valeur observée en niveau avant la prévision
last_obs_HICP <- tail(IV_data_log_diff_esp$HICP[1:(T_gas+p)], 1)
first_HICP <- IV_data_log_diff_esp$HICP[1]
boot_counterfactual_HICP <- array(NA, dim = c((T+p), boot_size))
boot_counterfactual_level_HICP <- array(NA, dim = c((T+p), boot_size))
for (i in 1:boot_size) {
    theta_0_i <- gen_benchmark_esp_supply_mp$sample_matrices$ncvg_theta_0[,,i]
    
    # Tirage counterfactual spot prices pour ce bootstrap
    IV_data_esp_sim <- IV_data_esp_log_diff_counterfactual %>%
      mutate(
        counterfactual_spot_price_sim = rnorm(n = n(), mean = counterfactual_spot_price, sd = se_pred_month),
        counterfactual_spot_price_sim_log_diff = log(counterfactual_spot_price_sim + 1) - log(lag(counterfactual_spot_price_sim) + 1),
        policy_shock_sim = counterfactual_spot_price_sim_log_diff - spot_price_log_diff
      )
    
    
    # Coefficients estim?s
    #coef_hat <- coef(vl)  # liste : une entr?e par ?quation, chaque entr?e = dataframe des coeffs

    
    # coef_sim <- coef_sample[i,]
    # # Reconstruire la liste
    # coef_new <- rebuild_coef_list(vl, coef_sim)
    # 
    # # Copier l'objet VAR
    # vl_sim <- vl
    # 
    # # Remplacer les coefficients
    # for(eq in names(coef_new)) {
    #   vl_sim$varresult[[eq]]$coefficients <- coef_new[[eq]]$Estimate
    # }
    
    # Stocker le r?sultat
    #rejouer en propageant les r?sidus auxquels on ajoute le structural shock elec supply

    policy_shock_sim <- IV_data_esp_sim$policy_shock_sim
    policy_shock_sim[is.na(policy_shock_sim)] <- 0
    policy_shock_samples[[i]] <- policy_shock_sim
    
    
    policy_res_sim <- matrix(policy_shock_sim[-(1:p)], nrow = T, ncol = n) * 
      matrix(theta_0_i, nrow = T, ncol = n, byrow = TRUE)
    
    counterfactual_residuals_i <- res + policy_res_sim
    
    # counterfactual_prediction_i <- iterate_var_ter(x= x, initial_obs = vars[1:p,], 
    #                                              errors = counterfactual_residuals_i , var_model =  vl_sim)
    counterfactual_prediction_i <- iterate_var_ter(x= x, initial_obs = vars[1:p,], 
                                                   errors = counterfactual_residuals_i , var_model =  vl)
    
    counterfactual_scenario[i] <- counterfactual_prediction_i
    
    HICP_pred_i  <- counterfactual_prediction_i[, 4]
    # Reconstituer les niveaux prédits
    HICP_pred_level_i <- first_HICP  * exp(cumsum(HICP_pred_i))
    boot_counterfactual_level_HICP[,i] <- HICP_pred_level_i
    boot_counterfactual_HICP[,i] <- HICP_pred_i
}

getwd()

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(boot_counterfactual_level_HICP, "boot_counterfactual_level_HICP.csv")
write.csv(boot_counterfactual_HICP, "boot_counterfactual_HICP.csv")

boot_counterfactual_level_HICP <- read.csv( "boot_counterfactual_level_HICP.csv")
boot_counterfactual_HICP <- read.csv( "boot_counterfactual_HICP.csv")


class(counterfactual_scenario_HICP)


View(IV_data_esp_sim %>% 
       dplyr::select(year, month, spot_price, counterfactual_spot_price, counterfactual_spot_price_sim))

# ---- Extraire série originale (colonne 4) ----
price_obs <- vars[, 1]
hicp_obs <- vars[, 4]

# ---- Extraire prévisions et IC (colonne 4) ----
price_pred  <- counterfactual_prediction[, 1]
price_pred  <- IV_data_esp_log_diff_counterfactual$counterfactual_spot_price
sigma <-IV_data_esp_log_diff_counterfactual$se_pred_month
price_lower_90 <-price_pred - 1.6*sigma               # fcst - 1 sigma
price_upper_90 <- price_pred + 1.6*sigma               # fcst + 1 sigma

price_lower_68 <-price_pred - sigma               # fcst - 1 sigma
price_upper_68 <- price_pred + sigma               # fcst + 1 sigma



df_price <- data.frame(
  date = c(IV_data_log_diff_esp$observation_date),
  value = price_pred,
  type = "Forecast",
  #type = c(rep("Observed", T+p-1), rep("Forecast", h+1)),
  lower_68 = price_lower_68,
  upper_68 =  price_upper_68,
  lower_90 = price_lower_90,
  upper_90 =  price_upper_90,
  grandeur = "spot_price"
) 



HICP_pred  <- counterfactual_prediction[, 4]
inflation_pred <- slide_dbl(HICP_pred , sum, .before = 11, .complete = TRUE)
h <- 7
HICP_pred[(T_gas+1+p):(T_gas+h+p)]

boot_counterfactual_HICP_cum <- apply(boot_counterfactual_HICP,
                      c(2), function(x) slide_dbl(x, sum, .before = 11, .complete = TRUE))

quantiles_90_cum <- apply(boot_counterfactual_HICP_cum[(p+1):(T+p),],
                      c(1), function(x) quantile(x, probs = quantile_probs_90))
quantiles_68_cum <- apply(boot_counterfactual_HICP_cum[(p+1):(T+p),],
                      c(1), function(x) quantile(x, probs = quantile_probs_68))

inflation_lower_90 <- t(quantiles_90_cum   )[,1]            # fcst - 1 sigma
inflation_upper_90 <- t(quantiles_90_cum   )[,2]               # fcst + 1 sigma
inflation_lower_68 <- t(quantiles_68_cum   )[,1]             # fcst - 1 sigma
inflation_upper_68 <- t(quantiles_68_cum   )[,2]


df_inflation <- data.frame(
  date = c(IV_data_log_diff_esp$observation_date),
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
  date = IV_data_log_diff_esp$observation_date,
  value = IV_data_log_diff_esp$spot_price,
  type = "Observed",
  grandeur = "spot_price",
  lower_68 = NA,
  upper_68 = NA,
  lower_90 = NA,
  upper_90 = NA
)

inflation_12m = slide_dbl(IV_data_log_diff_esp$HICP_log_diff, sum, .before = 11, .complete = TRUE)
# ---- Construire dataframe Observed pour HICP ----
df_obs_HICP <- data.frame(
  date = IV_data_log_diff_esp$observation_date,
  value = inflation_12m,
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


save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/policy'
setwd(save_plot_path)
ggsave("iberian_exception_plot_inflation_counterfactual.png", plot = counterfactual, width = 14 , height = 6, dpi = 300)




#De combien a réduit l'inflation sur 2022

df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2022-12-01")) %>% 
  dplyr::select(date, value, lower_68, upper_68)
0.08986621 - 0.05288838

df_plot_all%>% filter(grandeur == "inflation" & date == as.Date("2023-06-01")) %>% 
  dplyr::select(date, value, lower_68, upper_68)
0.04255027 - 0.01589382
