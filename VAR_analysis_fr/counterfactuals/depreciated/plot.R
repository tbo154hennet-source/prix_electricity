
# ---- Construire dataframe tidy pour HICP ----
df_HICP <- data.frame(
  date = c(IV_data_log_diff_esp$observation_date[1:(T+p)],
           IV_data_log_diff_esp$observation_date[(T+p+1):(T+p+h)]),
  value = c(IV_data_log_diff_esp$HICP_log_diff[1:(T+p)], HICP_pred),
  type = c(rep("Observed", T+p-1), rep("Forecast", h+1)),
  lower = c(IV_data_log_diff_esp$HICP_log_diff[1:(T+p)], HICP_lower),
  upper = c(IV_data_log_diff_esp$HICP_log_diff[1:(T+p)], HICP_upper),
  grandeur = "HICP"
)  %>%
  mutate(
    value = slide_dbl(value, sum, .before = 11, .complete = TRUE),
    lower = slide_dbl(lower, sum, .before = 11, .complete = TRUE),
    upper = slide_dbl(upper, sum, .before = 11, .complete = TRUE)
  ) 
# %>% 
#   mutate(lower =ifelse(date <= as.Date("2022-06-01"), NA, lower),
#          upper =ifelse(date <= as.Date("2022-06-01"), NA, upper)
#          )

# ---- Combiner les deux séries ----
df_plot_all <- rbind(df_price, df_HICP)


# ---- Construire dataframe Observed pour spot_price ----
df_obs_price <- data.frame(
  date = IV_data_log_diff_esp$observation_date,
  value = IV_data_log_diff_esp$spot_price,
  type = "Observed",
  grandeur = "spot_price",
  lower = NA,
  upper = NA
)

inflation_12m = slide_dbl(IV_data_log_diff_esp$HICP_log_diff, sum, .before = 11, .complete = TRUE)
# ---- Construire dataframe Observed pour HICP ----
df_obs_HICP <- data.frame(
  date = IV_data_log_diff_esp$observation_date,
  value = inflation_12m,
  type = "Observed",
  grandeur = "HICP",
  lower = NA,
  upper = NA
)



# ---- Combiner Observed avec Forecast déjà calculé (df_plot_all) ----
df_plot_all <- rbind(df_plot_all, df_obs_price, df_obs_HICP)



# Créer une colonne transformée pour HICP
df_plot_all_scaled <- df_plot_all %>%
  mutate(
    value_plot = ifelse(grandeur == "HICP", value * 1000, value),
    lower_plot = ifelse(grandeur == "HICP", lower * 1000, lower),
    upper_plot = ifelse(grandeur == "HICP", upper * 1000, upper)
  )





# ---- Plot avec axe secondaire ----
counterfactual <- ggplot() +
  # Spot_price
  geom_line(data = df_plot_all_scaled %>% filter(grandeur == "spot_price"),
            aes(x = date, y = value_plot, linetype = type, color = grandeur),
            size = 0.5) +
  geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "spot_price", type == "Forecast"),
              aes(x = date, ymin = lower_plot, ymax = upper_plot),
              fill = "red", alpha = 0.2) +
  
  # HICP (multiplié par 100)
  geom_line(data = df_plot_all_scaled %>% filter(grandeur == "HICP"),
            aes(x = date, y = value_plot, linetype = type, color = grandeur),
            size = 0.5) +
  geom_ribbon(data = df_plot_all_scaled %>% filter(grandeur == "HICP", type == "Forecast"),
              aes(x = date, ymin = lower_plot, ymax = upper_plot),
              fill = "blue", alpha = 0.2) +
  # Vertical line for T_gas
  geom_vline(xintercept = as.Date("2022-06-01"), 
             linetype = "dotted", color = "black", linewidth = 0.6) +
  theme_minimal() +
  labs(
    title = "Iberian Exception Counterfactual",
    x = "Time", y = "Spot Price"
  ) +
  scale_linetype_manual(values = c("Observed" = "solid", "Forecast" = "dashed")) +
  scale_color_manual(values = c("spot_price" = "red", "HICP" = "blue")) +
  
  # Axe secondaire pour HICP
  scale_y_continuous(
    sec.axis = sec_axis(~./1000, name = "Inflation")
  )

counterfactual


#De combien a réduit l'inflation sur 2022

df_plot_all_scaled %>% filter(grandeur == "HICP" & date == as.Date("2022-12-01"))




# chaque �l�ment est une matrice de taille T x 5

quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_prob_50 <- c(0.5, 0.5)
#names(sample_matrices)[[lambda_name]]

quantiles_90 <- apply(boot_counterfactual_HICP,
                      c(1), function(x) quantile(x, probs = quantile_probs_90))
quantiles_68 <- apply(boot_counterfactual_HICP,
                      c(1), function(x) quantile(x, probs = quantile_probs_68))

names(quantiles_68)



#inflation prediction for 2022
View(as.data.frame(HICP_pred))
View(as.data.frame(IV_data_esp_log_diff_counterfactual$HICP_log_diff))










counterfactual <- ggplot() +
geom_line(data = df_plot_all_scaled %>% filter(grandeur == "inflation"),
          aes(x = date, y = value_plot, linetype = type, color = grandeur),
          size = 0.5) 


