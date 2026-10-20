


library(dplyr)
library(fixest)

# 1) get all variables used in stage 1
vars1 <- all.vars(
  as.formula("asinh(price) ~ log(wind_speed_100m_supply) + wind_onshore_capacity +
              Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity +
              Published.Unavailable.Nuclear.Capacity + Unpublished.Unavailable.Nuclear.Capacity +
              Fore_Solar + nuclear + thermal_tot + hydro_tot + spot_gas +
              asinh(gen_forecast_net) + asinh(load_tot) +
              temperature_demand + precipitation_demand + carbon_price +
              as.factor(year) + as.factor(weekday) + as.factor(creneau)")
)

# 2) keep only rows with no NA in stage-1 variables
df_cc <- price_indispo_merge_esp %>%
  filter(complete.cases(across(all_of(vars1))))

# 3) stage 1
mp <- feols(
  asinh(price) ~ log(wind_speed_100m_supply) * log(wind_onshore_capacity) +
    Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity +
    Published.Unavailable.Nuclear.Capacity + Unpublished.Unavailable.Nuclear.Capacity +
    Fore_Solar + nuclear + thermal_tot + hydro_tot + spot_gas +
    asinh(gen_forecast_net) + asinh(load_tot) +
    temperature_demand + precipitation_demand + carbon_price +
    factor(year) + factor(weekday) + factor(creneau),
  data = df_cc
)

# 4) stage 2 target: log(resid^2) (add small epsilon to avoid log(0))
df_cc$resid_sq <- resid(mp)^2
eps <- 1e-12
df_cc$resid_sq[1:5]
var_reg <- feols(
  log(resid_sq + 1e-12) ~ log(wind_onshore_capacity) * log(wind_speed_100m_supply),
  data = df_cc
)
etable(var_reg)
#Conditional on wind speed and other controls, 
#higher installed wind capacity is associated with higher price volatility.
#A one-unit (MWh) increase in wind capacity 
#increases the conditional variance of electricity prices by approximately 0.03%

df_cc$wind_onshore_capacity[1:5]
## keep baseline untouched

df_cc <- df_cc %>%
  mutate(
    wind_cap2 = 62000
  )
logv_base <- predict(var_reg, newdata = df_cc)  # log Var(eps_t) baseline
df2 <- df_cc %>% mutate(wind_onshore_capacity = wind_cap2)
logv_cf   <- predict(var_reg, newdata = df2) # log Var(eps_t) under doubled wind
logv_base[1:5]
logv_cf[1:5]

var_ratio <- exp(logv_cf - logv_base)   # Var_cf / Var_base
sd_ratio  <- sqrt(var_ratio)            # SD_cf / SD_base
summary(sd_ratio)

#at the daily level, increasing wind capacity from 31 GW to 62 GW
#increases the standard deviation of price residuals by a crazy amount * 15 on average



#on agrège pour avoir des contrefactuels crédibles

colnames(all_variance_data)
colnames(carbon_price_data)
colnames(wind_monthly)

monthly_mean_data <- left_join(all_variance_data,   
 	carbon_price_data %>% 
	mutate(month = month(time), year = year(time)) %>%
	group_by(month, year) %>%
	summarise(carbon_price = mean(carbon_price, na.rm = TRUE)),
  by= c('month', 'year'))
monthly_mean_data <- left_join(monthly_mean_data,
 wind_monthly %>%
 rename(wind_capacity = Wind) %>%
	mutate(month = month(date), year = year(date)),
  by= c('month', 'year'))

colnames(monthly_mean_data)
formula <- as.formula("log(spot_price_mean) ~ log(wind_speed_100m_supply_mean * wind_capacity) +
    Fore_Solar_mean + spot_gas_mean +
    log(gen_forecast_mean) + log(load_tot_mean) +
    temperature_demand_mean  + carbon_price")
	
mp <- feols(
  formula,
  data = monthly_mean_data
)
etable(mp)
# 4) stage 2 target: log(resid^2) (add small epsilon to avoid log(0))
monthly_mean_data$resid_sq <- resid(mp)^2
eps <- 1e-12
monthly_mean_data$resid_sq[1:5]
var_reg <- feols(
  log(resid_sq + 1e-12) ~
  log(wind_capacity * wind_speed_100m_supply_mean),
  data = monthly_mean_data
)
etable(var_reg)


monthly_mean_data <- monthly_mean_data %>%
  mutate(
    wind_cap2 = 62000
  )

logv_base <- predict(var_reg, newdata = monthly_mean_data)  # log Var(eps_t) baseline
df2 <- monthly_mean_data %>% mutate(wind_capacity = wind_cap2)
logv_cf   <- predict(var_reg, newdata = df2) # log Var(eps_t) under doubled wind
logv_base[1:5]
logv_cf[1:5]

var_ratio <- exp(logv_cf - logv_base)   # Var_cf / Var_base
sd_ratio  <- sqrt(var_ratio)            # SD_cf / SD_base
summary(sd_ratio)

e_hat <- resid(mp)             # baseline residuals
e_cf  <- e_hat * sqrt(var_ratio)  # counterfactual residuals with higher variance
mu_hat <- predict(mp, newdata = monthly_mean_data)      # fitted mean in asinh(price)
y_cf_asinh <- mu_hat + e_cf


mu_cf <- predict(mp, newdata = df2)
y_cf_asinh_mean <- mu_cf + e_hat

monthly_mean_data$price_log_counterfactual_only_var <- y_cf_asinh
monthly_mean_data$price_log_counterfactual_var_mean <- y_cf_asinh_mean


y_cf_asinh[1:5]
y_cf_asinh_mean[1:5]
monthly_mean_data$price_mean <- log(monthly_mean_data$spot_price_mean)

wind_counterfactuals <- ggplot(monthly_mean_data, aes(x = date)) +
  geom_line(aes(y = price_mean,     color = "Observed price"), alpha = 0.7) +
  #geom_line(aes(y = price_fit_mean, color = "Fitted price"), alpha = 0.7) +
#   geom_line(aes(y = price_counterfactual_mean_adj,
  geom_line(aes(y = price_log_counterfactual_only_var ,
   color = "Variance Counterfactual"), alpha = 0.7) +
   geom_line(aes(y = price_log_counterfactual_var_mean ,
   color = "Level Counterfactual"), alpha = 0.7) +
#   geom_line(aes(y = price_sd,     color = "Observed price SD"), alpha = 0.7) +
#   geom_line(aes(y = price_counterfactual_ov_sd , color = "Counterfactual OV SD"), alpha = 0.7) +
#   geom_line(aes(y = price_counterfactual_mv_sd , color = "Counterfactual MV SD"), alpha = 0.7) +
  labs(
    title = "",
    x = "Date",
    y = "Price (EUR/MWh)",
    color = ""
  ) +
  theme_minimal()+
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )

wind_counterfactuals

save_plot_path <- 'Z:/Overleaf_elec/images/macro'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("wind_counterfactuals.png", plot = wind_counterfactuals  , width = 12, height = 6, dpi = 300)



var(monthly_mean_data$price_log_counterfactual_only_var)
var(monthly_mean_data$price_log_counterfactual_var_mean)
var(monthly_mean_data$price_mean)


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(monthly_mean_data  %>%
dplyr::select(year, month, price_log_counterfactual_only_var,
price_mean, price_log_counterfactual_var_mean) ,
			  "price_variance_counterfactual_esp_ter.parquet")





