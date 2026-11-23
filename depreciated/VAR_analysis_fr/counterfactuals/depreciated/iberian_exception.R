


#on suppose que le coeficient est sous-évalué pour la France
#qu'il doit contribuer plus mais a été atténué ou déplacé par la politique


#au contraire on considère que le choc de prix de l'élec est sous-évalué
# parce que décorrélé du choc de gas en Espagne





#small linear model to predict price of electricity from price of gas
# within the VAR model 
library(slider)

library(vars)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff_elas.parquet')

# correct for the Iberian exception
colnames(IV_data_log_diff_esp)
IV_data_log_diff_esp$observation_date <- as.Date(IV_data_log_diff_esp$observation_date)
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply,
    temp_excess_instrument = temp_excess_supply
  )

IV_data_log_diff_esp <-  IV_data_log_diff_esp %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    ffr_diff = ffr - lag(as.numeric(ffr)),
    delta_s_renewable_day_ahead_forecast_diff = (delta_s_renewable_day_ahead_forecast - lag(as.numeric(delta_s_renewable_day_ahead_forecast)))/1000,
    delta_d_renewable_day_ahead_forecast_diff = (delta_d_renewable_day_ahead_forecast - lag(as.numeric(delta_d_renewable_day_ahead_forecast)))/1000,
    delta_s_cold_excess_diff = (delta_s_cold_excess - lag(as.numeric(delta_s_cold_excess)))/1000,
    delta_d_cold_excess_diff = (delta_d_cold_excess  - lag(as.numeric(delta_d_cold_excess )))/1000,
    delta_s_heat_excess_diff = (delta_s_heat_excess - lag(as.numeric(delta_s_heat_excess)))/1000,
    delta_d_heat_excess_diff = (delta_d_heat_excess - lag(as.numeric(delta_d_heat_excess)))/1000,
    delta_s_Published.Predicted.Unavailable.Capacity_diff = (delta_s_Published.Predicted.Unavailable.Capacity - lag(as.numeric(delta_s_Published.Predicted.Unavailable.Capacity)))/1000,
    delta_d_Published.Predicted.Unavailable.Capacity_diff = (delta_d_Published.Predicted.Unavailable.Capacity - lag(as.numeric(delta_d_Published.Predicted.Unavailable.Capacity)))/1000,
    delta_s_cbf_es_diff = (delta_s_cbf_es - lag(as.numeric(delta_s_cbf_es)))/1000,
    delta_d_cbf_es_diff = (delta_d_cbf_es - lag(as.numeric(delta_d_cbf_es)))/1000,
    diff_cbf = if_else(is.na(diff_cbf), 0, diff_cbf),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff),
    
    delta_s_renewable_day_ahead_forecast_diff = if_else(is.na(delta_s_renewable_day_ahead_forecast_diff), 0, delta_s_renewable_day_ahead_forecast_diff),
    delta_d_renewable_day_ahead_forecast_diff = if_else(is.na(delta_d_renewable_day_ahead_forecast_diff), 0, delta_d_renewable_day_ahead_forecast_diff),
    delta_s_cold_excess_diff = if_else(is.na(delta_s_cold_excess_diff), 0, delta_s_cold_excess_diff),
    delta_d_cold_excess_diff = if_else(is.na(delta_d_cold_excess_diff), 0, delta_d_cold_excess_diff),
    delta_s_heat_excess_diff = if_else(is.na(delta_s_heat_excess_diff), 0, delta_s_heat_excess_diff),
    delta_d_heat_excess_diff = if_else(is.na(delta_d_heat_excess_diff), 0, delta_d_heat_excess_diff),
    delta_s_Published.Predicted.Unavailable.Capacity_diff = if_else(is.na(delta_s_Published.Predicted.Unavailable.Capacity_diff), 0, delta_s_Published.Predicted.Unavailable.Capacity_diff),
    delta_d_Published.Predicted.Unavailable.Capacity_diff = if_else(is.na(delta_d_Published.Predicted.Unavailable.Capacity_diff), 0, delta_d_Published.Predicted.Unavailable.Capacity_diff),
    delta_s_cbf_es_diff = if_else(is.na(delta_s_cbf_es_diff), 0, delta_s_cbf_es_diff),
    delta_d_cbf_es_diff = if_else(is.na(delta_d_cbf_es_diff), 0, delta_d_cbf_es_diff),
    
    
    delta_s_total_diff = delta_s_renewable_day_ahead_forecast_diff  + delta_s_cold_excess_diff+
      delta_s_heat_excess_diff + delta_s_Published.Predicted.Unavailable.Capacity_diff,
    delta_d_total_diff = delta_d_renewable_day_ahead_forecast_diff  + delta_d_cold_excess_diff+
      delta_d_heat_excess_diff + delta_d_Published.Predicted.Unavailable.Capacity_diff,
    delta_s_total_diff = if_else(is.na(delta_s_total_diff), 0, delta_s_total_diff),
    delta_d_total_diff = if_else(is.na(delta_d_total_diff), 0, delta_d_total_diff)
  )
#Start date	June 15, 2022
iberian_cap <- data.frame(
  Month = c("2022-06", "2022-07", "2022-08", "2022-09", "2022-10", "2022-11",
            "2022-12", "2023-01", "2023-02", "2023-03",
            "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09",
            "2023-10", "2023-11", "2023-12"),
  cap = c(40, 40, 40, 40, 40, 40, 40,
          45, 50, 55, 56.1, 57.2, 58.3,
          59.4, 60.6, 61.7, 62.8, 63.9,	65)
) %>%
  mutate(date = as.Date(paste0(Month, "-01")),
         year = as.numeric(year(date)),
         month = month(date))

#on veut construire un instrument supplémentaire qui est la différence
#entre le prix du gas et le prix du MWh de gas et le prix payé subventionné
# et le prix qui était payé à la période précédente

IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  left_join(iberian_cap, by = c("year", "month")) %>%
  mutate(cap = ifelse(observation_date == as.Date("2022-06-01"), (spot_gas + cap)/2, cap),
         cap_gap_diff = ifelse(observation_date == as.Date("2022-06-01"),  cap - spot_gas , 0),
         cap_gap_diff = ifelse(observation_date == as.Date("2024-01-01"), spot_gas - lag(cap), cap_gap_diff),
         cap_gap_diff = ifelse(observation_date > as.Date("2022-06-01") & observation_date < as.Date("2024-01-01"),
                               cap - lag(cap), cap_gap_diff)
  )
IV_data_log_diff_esp %>% 
  filter(observation_date >= as.Date("2022-05-01") & observation_date <= as.Date("2022-07-01")  ) %>%
  dplyr::select(spot_gas, cap, cap_gap_diff, avg_spot_price, spot_price)



headers_proxies_esp = c(
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  "temp_excess_instrument",
  "diff_cbf",
  "cap_gap_diff",
  "renewable_day_ahead_forecast_esp_log_diff"
)
headers_var_gen_first = c("gen_tot_log_diff",
                          "spot_price_log_diff",
                          "IPI_log_diff",
                          # "ECB_rate_log_diff",
                          # "E_Index_log_diff",
                          # "Exp_pi_log_diff",
                          # "UnE_log_diff" ,
                          "HICP_log_diff"
)


headers_var_gen_first_mp = c("gen_tot_log_diff",
                             "spot_price_log_diff",
                             "IPI_log_diff",
                             "HICP_log_diff",
                             "ffr_diff"
)


headers_proxies_supply <- c("delta_s_total_diff")
# proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_esp)], frequency=12)
# proxies[is.na(proxies)] <- 0

proxies_gas <-  ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_proxies_supply)),
  frequency = 12
)
proxies_gas[is.na(proxies_gas)] <- 0

vars_gas <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_var_spot_first)),
  frequency = 12
)


headers_proxies_supply = c(
  "delta_s_total_diff"#,
  #"delta_d_total_diff"
)
headers_proxies_supply_demand = c(
  "delta_s_total_diff",#,
  "delta_d_total_diff"
)
headers_var_spot_first <- c("spot_price_log_diff",
                            "gen_tot_log_diff",
                            "IPI_log_diff",
                            "HICP_log_diff")

headers_var_spot_first_mp = c(
  headers_var_spot_first,
  "ffr_diff"
)

proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_supply)], frequency=12)
proxies[is.na(proxies)] <- 0

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first)], frequency=12)

p = 12
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)







x =ts(vars_gas, start=c(2015,1), end=c(2022,5), frequency=12)
y =vars_gas[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <-1

headers_controls = c("avg_spot_price_log_diff")

controls_gas <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_controls)),
  frequency = 12
)

# Fit the VAR model
vl <- if (is.null(controls_gas)) {
  vars::VAR(x, p, type = "none")
} else {
  vars::VAR(x, p, type = "none", exogen = controls_gas)
}

#k  = ncol(proxies)
#n = ncol(y)

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

T = nrow(y)
Sigma_eta <- cov(res) 

#suivre identification simplifiee de olea and al
gamma <- t( t(proxies_gas[c(-1:-p),]) %*% res)
Sigma_a <- gamma %*%  ginv(t(gamma) %*% ginv(Sigma_eta) %*% gamma) %*% t(gamma)
theta_0 <-  Sigma_a[1,]/ Sigma_a[1,1]

phi <-  ginv(theta_0)%*% gamma





controls_futures <-  ts(
  IV_data_log_diff_esp %>%
    filter(observation_date >= as.Date("2022-06-01") & year <= 2022) %>%
    dplyr::select(all_of(headers_controls)),
  frequency = 12
)

h = 7
# prédiction à 12 périodes le temps où est active 
pred <- predict(vl, n.ahead = h, dumvar = controls_futures)  # les exogènes futurs







# ---- Extraire série originale (colonne 4) ----
price_obs <- vars[, 1]
hicp_obs <- vars[, 4]

# ---- Extraire prévisions et IC (colonne 4) ----

fcst <- predict(vl, n.ahead = h, ci = 0.68, dumvar = controls_futures)$fcst[[1]]

price_pred  <- fcst[, "fcst"]

# Calcul manuel des bornes pour 68%
sigma <- (fcst[, "upper"] - fcst[, "fcst"])   # borne supérieure = fcst + z*sigma (ici z=1.96)
sigma <- sigma / 1.96                         # retrouver sigma

price_lower <-price_pred - sigma               # fcst - 1 sigma
price_upper <- price_pred + sigma               # fcst + 1 sigma
# Prévision pour colonne 4
fcst <- predict(vl, n.ahead = h, ci = 0.68, dumvar = controls_futures)$fcst[[4]]

HICP_pred  <- fcst[, "fcst"]

# Calcul manuel des bornes pour 68%
sigma <- (fcst[, "upper"] - fcst[, "fcst"])   # borne supérieure = fcst + z*sigma (ici z=1.96)
sigma <- sigma / 1.96                         # retrouver sigma

HICP_lower <- HICP_pred - sigma               # fcst - 1 sigma
HICP_upper <- HICP_pred + sigma               # fcst + 1 sigma


y =vars_gas[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)

h<- length(price_pred)


#en niveau

# dernière valeur observée en niveau avant la prévision
last_obs_price <- tail(IV_data_log_diff_esp$spot_price[1:(T+p)], 1)

# Reconstituer les niveaux prédits
price_pred_level <- last_obs_price * exp(cumsum(price_pred))
price_lower_level <- last_obs_price * exp(cumsum(price_lower))
price_upper_level <- last_obs_price * exp(cumsum(price_upper))

# ---- Construire dataframe tidy pour spot_price ----
df_price <- data.frame(
  date = c(IV_data_log_diff_esp$observation_date[1:(T+p)],
           IV_data_log_diff_esp$observation_date[(T+p+1):(T+p+h)]),
  value = c(IV_data_log_diff_esp$spot_price[1:(T+p)], price_pred_level),
  type = c(rep("Observed", T+p-1), rep("Forecast", h+1)),
  lower = c(rep(NA, T+p), price_lower_level),
  upper = c(rep(NA, T+p), price_upper_level),
  grandeur = "spot_price"
)

# dernière valeur observée en niveau avant la prévision

# dernière valeur observée en niveau avant la prévision
last_obs_HICP <- tail(IV_data_log_diff_esp$HICP[1:(T+p)], 1)

# Reconstituer les niveaux prédits
HICP_pred_level <- last_obs_HICP * exp(cumsum(price_pred))
HICP_lower_level <- last_obs_HICP * exp(cumsum(price_lower))
HICP_upper_level <- last_obs_HICP * exp(cumsum(price_upper))


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

#Au lieu d'une inflation à 9,6 sur l'année 2022 en moyenne
#permet de baisser à une inflation à 5,2 en moyenne

# là il pense que le gas baisse alors que c'est juste le gas pour l'électricité
#pas un bon counterfactual




