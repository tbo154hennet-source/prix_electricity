

var_path <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output"
setwd(var_path)

VAR_series <- read_parquet("VAR_series_esp_2026.parquet")

head(VAR_series %>% 
       filter(observation_date >= as.Date("2015-01-01")))

output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")

colnames(price_indispo_merge)

# Orthogonaliser delta_s_total par rapport ? delta_d_total
price_indispo_merge$fit_supply_unmatched_orth <- resid(
  lm(fit_supply_unmatched ~ fit_demand_unmatched,
     data = price_indispo_merge,
     na.action = na.exclude)
)

# Orthogonaliser delta_d_total par rapport ? delta_s_total
price_indispo_merge$fit_demand_unmatched_orth <- resid(
  lm(fit_demand_unmatched ~ fit_supply_unmatched , data = price_indispo_merge,
     na.action = na.exclude)
)


#adjust for iberian exception gas cap

iberian_cap <- data.frame(
  Month = c("2022-06", "2022-07", "2022-08", "2022-09", "2022-10", "2022-11",
            "2022-12", "2023-01", "2023-02", "2023-03",
            "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09", 
            "2023-10", "2023-11", "2023-12"),
  Cap_EUR_MWh = c(40, 40, 40, 40, 40, 40,
                  45, 50, 55, 60,
                  55, 56.1, 57.2, 58.3, 59.4, 60.5,
                  61.6, 62.7, 63.8)
) %>% 
  mutate(date = as.Date(paste0(Month, "-01")),
        year = as.numeric(year(date)),
         month = as.numeric(month(date)))


price_indispo_merge <- price_indispo_merge %>% 
  mutate(
    year  = as.numeric(as.character(year)),
    month = as.numeric(as.character(month))
  ) %>%
  left_join(iberian_cap, by = c("year", "month")) %>%
  fill(spot_gas, .direction = "up")%>% # Backfill (bfill) 
  fill(spot_gas, .direction = "down") %>%    # Backfill (bfill)%>%
  mutate(spot_gas_capped = ifelse(spot_gas < Cap_EUR_MWh | is.na(Cap_EUR_MWh), spot_gas, Cap_EUR_MWh)) 

# View(price_indispo_merge %>% 
#        select(datetime, spot_gas, spot_gas_capped))
weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

colnames(price_indispo_merge)

price_indispo_merge %>% 
  select(gen_tot,gen_forecast ,gen_forecast_conso,
         load_forecast, load_tot, gen_forecast_net
         ) %>% 
  cor(use = "complete.obs")

monthly_energy_data <- price_indispo_merge %>%
  arrange(datetime) %>% 
  mutate(
    Nuclear.Unavailabilities =
      #coalesce(Planned.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Published.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Uneffective.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Unpublished.Unavailable.Nuclear.Capacity, 0),
    
    Thermal.Unavailabilities =
      #coalesce(Planned.Unavailable.Capacity, 0) +
      coalesce(Published.Unavailable.Capacity, 0) +
      coalesce(Uneffective.Unavailable.Capacity, 0) +
      coalesce(Unpublished.Unavailable.Capacity, 0) -
      coalesce(Nuclear.Unavailabilities, 0),
    
    Predicted.Unavailabilities =
      #coalesce(Planned.Predicted.Unavailable.Capacity, 0) +
      coalesce(Published.Predicted.Unavailable.Capacity, 0) +
      coalesce(Uneffective.Predicted.Unavailable.Capacity, 0) +
      coalesce(Unpublished.Predicted.Unavailable.Capacity, 0)
  )%>% 
  group_by(month, year) %>%
  summarise(
    spot_price = weighted_mean(price, load_tot),
    renewable_forecast = weighted_mean(renewable_day_ahead_forecast, load_tot),
    wind_forecast = weighted_mean(Fore_Wind, load_tot),
    solar_forecast = weighted_mean(Fore_Solar, load_tot),
    nuclear_unavailabilities = weighted_mean(Nuclear.Unavailabilities, load_tot),
    thermal_unavailabilities = weighted_mean(Thermal.Unavailabilities, load_tot),
    predicted_unavailabilities = weighted_mean(Predicted.Unavailabilities, load_tot),
    # NTC_ESP_FR = mean(NTC_ESP_FR, na.rm = TRUE),
    # NTC_FR_ESP = mean(NTC_FR_ESP, na.rm = TRUE),
    gen_forecast = weighted_mean(gen_forecast, load_tot),
    gen_forecast_conso = weighted_mean(gen_forecast_conso, load_tot),
    load_forecast = weighted_mean(load_forecast, load_tot),
    gen_forecast_net = weighted_mean(gen_forecast_net, load_tot),
    gen_tot = weighted_mean(gen_tot, load_tot),
    spot_gas_capped = mean(spot_gas_capped, na.rm = TRUE),
    spot_gas= mean(spot_gas, na.rm = TRUE),
    delta_s_orth= weighted_mean(fit_supply_unmatched_orth, load_tot),
    delta_d_orth= weighted_mean(fit_demand_unmatched_orth, load_tot),
    delta_s_tot= weighted_mean(fit_supply_unmatched, load_tot),
    delta_d_tot= weighted_mean(fit_demand_unmatched, load_tot),
    load_tot = mean(load_forecast, na.rm = TRUE),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()
# View(monthly_energy_data %>%
#        dplyr::select(datetime, spot_gas, spot_gas_capped))
# 

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/cross_border")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

price_merge_CBF <- read_parquet("price_merge_CBF_max.parquet")
colnames(price_merge_CBF)
monthly_crossborder_data <- price_merge_CBF %>% 
  filter(gen_tot_fr > 0 & gen_tot_esp > 0) %>%
  group_by(month, year) %>%
  summarise(
    renewable_day_ahead_forecast_ex_es = weighted_mean(renewable_day_ahead_forecast_pt +
                                                         renewable_day_ahead_forecast_fr , 
                                                       gen_tot_esp),
    cbf_es = weighted_mean(CBF_ES_pred, gen_tot_esp),
    
    renewable_day_ahead_forecast_pt = weighted_mean(renewable_day_ahead_forecast_pt, gen_tot_esp),
    renewable_day_ahead_forecast_fr = weighted_mean(renewable_day_ahead_forecast_fr, 
                                                    gen_tot_esp),
    
    
    renewable_day_ahead_forecast_ex_fr = weighted_mean(renewable_day_ahead_forecast_esp+
                                                         renewable_day_ahead_forecast_it_nord+
                                                         renewable_day_ahead_forecast_de+
                                                         renewable_day_ahead_forecast_be+
                                                         renewable_day_ahead_forecast_ch+
                                                         renewable_day_ahead_forecast_gb, 
                                                       gen_tot_fr),
    cbf_fr = weighted_mean(CBF_FR_pred, gen_tot_fr),
    renewable_day_ahead_forecast_esp = weighted_mean(renewable_day_ahead_forecast_esp, gen_tot_fr),
    renewable_day_ahead_forecast_it_nord = weighted_mean(renewable_day_ahead_forecast_it_nord, gen_tot_fr),
    renewable_day_ahead_forecast_de = weighted_mean(renewable_day_ahead_forecast_de, gen_tot_fr),
    renewable_day_ahead_forecast_be = weighted_mean(renewable_day_ahead_forecast_be, gen_tot_fr),
    renewable_day_ahead_forecast_ch = weighted_mean(renewable_day_ahead_forecast_ch, gen_tot_fr),
    renewable_day_ahead_forecast_gb = weighted_mean(renewable_day_ahead_forecast_gb, gen_tot_fr),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()

#monthly_crossborder_data$renewable_forecast_hors_esp


# setwd('Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output')
# gas_fundamental_data <- read_parquet("gas_fundamental_data.parquet")


#design partie temperature

colnames(price_indispo_merge)

monthly_weather_data <- price_indispo_merge %>%
  arrange(datetime) %>% 
  group_by(year, month) %>%
  summarise(
    temperature = mean(temperature_sth, na.rm = TRUE),
    cold_excess = mean(cold_excess_demand, na.rm = TRUE),
    heat_excess = mean(heat_excess_demand, na.rm = TRUE),
    temperature_discomfort = mean(temperature_discomfort_demand, na.rm = TRUE),
    temp_excess = mean(cold_excess_demand + heat_excess_demand, na.rm = TRUE),
    temp_spatial_mismatch_supply= mean(temp_spatial_mismatch_supply, na.rm = TRUE),
    precipitation = mean(precipitation_sth, na.rm = TRUE),
    relative_humidity = mean(relative_humidity_2m_sth, na.rm = TRUE),
    wind_speed_100m = mean(wind_speed_100m_supply, na.rm = TRUE),
    cloud_cover = mean(cloud_cover_supply, na.rm = TRUE),
    global_tilted_irradiance = mean(global_tilted_irradiance_supply, na.rm = TRUE),
    datetime = first(datetime),
    .groups = "drop"
  )

### at the macro level use for instrument cooling degree hours and heating degree hours
### and use deseasonnalize

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
tempall_data_data <- read_parquet( "tempall_data_data.parquet")

colnames(tempall_data_data)

T_base <- 18

tempall_data_data <- tempall_data_data %>%
  mutate(
    across(
      c("temperature_demand"),
      list(
        HDD = ~pmax(0,. - T_base),
        CDD = ~pmax(0, T_base - .)
      ),
      .names = "{.fn}_{sub('^temperature_', '', .col)}"
    )
  )

HCDD_cols <- colnames(tempall_data_data)[grep("^HDD_|^CDD_", colnames(tempall_data_data))]


temp_monthly <- tempall_data_data %>%
  mutate(
    month = month(date),
    year = year(date)
  ) %>%
  group_by(year, month) %>%
  summarise(
    across(all_of(HCDD_cols), ~ mean(.x, na.rm = TRUE)),  # fix here
    date = first(date),
    .groups = "drop"
  )



colnames(VAR_series)
colnames(monthly_energy_data)
#colnames(temperature_instrument)
colnames(monthly_crossborder_data)
colnames(monthly_weather_data)
colnames(temp_monthly)


IV_data <- merge(VAR_series, monthly_energy_data, by= c('month', 'year'))
# IV_data <- merge(IV_data, monthly_crossborder_data %>%
#                    dplyr::select(contains("es"), month, year),
#                  by= c('month', 'year'))
# IV_data <- merge(IV_data, gas_fundamental_data,
#                  by= c('month', 'year'))
IV_data <- merge(IV_data, monthly_weather_data %>% 
                   dplyr::select(-datetime),
                 by= c('month', 'year'))
IV_data <- merge(IV_data, temp_monthly %>% 
                   dplyr::select(-date),
                 by= c('month', 'year'))%>%
  arrange(datetime)




save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
write_parquet(IV_data, 'IV_data_esp_elas_sd_2026.parquet')
dim(IV_data)
colnames(IV_data)

ggplot(price_indispo_merge, aes(x = datetime)) +
  geom_line(aes(y = price , color = "Price")) +
  geom_line(aes(y = gen_tot, color = "Generation")) +
  geom_line(aes(y = load_tot, color = "Load")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  theme_minimal()

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y = spot_price , color = "Price")) +
  geom_line(aes(y = gen_tot/100, color = "Generation")) +
  geom_line(aes(y = load_tot/100, color = "Load")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  theme_minimal()


library(ggplot2)
plot_data <- monthly_crossborder_data %>%
  dplyr::select(datetime, FR = renewable_day_ahead_forecast_fr, 
         PT = renewable_day_ahead_forecast_pt) %>%
  pivot_longer(cols = -datetime, names_to = "zone", values_to = "forecast") 

ggplot(data = plot_data) +
  geom_area( aes(x = datetime, y = forecast, fill = zone),position = "stack", alpha = 0.6) +
  geom_line(data = monthly_crossborder_data, 
            aes(x= datetime, y = renewable_day_ahead_forecast_ex_es ,linetype = "dashed")) +
  labs(title = "Contributions des prévisions renouvelables FR et PT",
       y = "Prévision (MW)", x = "Date", fill = "Zone") +
  theme_minimal()

plot_data <- monthly_crossborder_data %>%
  dplyr::select(datetime, ES = renewable_day_ahead_forecast_fr, 
         CH = renewable_day_ahead_forecast_ch,
         DE = renewable_day_ahead_forecast_de,
         BE = renewable_day_ahead_forecast_be,
         IT = renewable_day_ahead_forecast_it_nord,
         GB = renewable_day_ahead_forecast_gb
         
         ) %>%
  pivot_longer(cols = -datetime, names_to = "zone", values_to = "forecast") 

ggplot(data = plot_data) +
  geom_area( aes(x = datetime, y = forecast, fill = zone),position = "stack", alpha = 0.6) +
  geom_line(data = monthly_crossborder_data, 
            aes(x= datetime, y = renewable_day_ahead_forecast_ex_fr ,linetype = "dashed")) +
  labs(title = "Contributions des prévisions renouvelables",
       y = "Prévision (MW)", x = "Date", fill = "Zone") +
  theme_minimal()

colnames(IV_data)

plot_data_elec <- IV_data %>%
  dplyr::select(datetime, 
         wind_forecast, solar_forecast
         
  )  %>%
  pivot_longer(cols = -datetime, names_to = "energy", values_to = "forecast") 


renewable_energy_decomposition <- ggplot(data = plot_data_elec) +
  geom_area( aes(x = datetime, y = forecast, fill = energy),position = "stack", alpha = 0.6) +
  #geom_line(data = IV_data, aes(x= datetime, y = renewable_forecast ,linetype = "dashed")) +
  geom_line(data = IV_data, aes(x= datetime, y = wind_speed_100m *100, color = "Wind speed")) +
  geom_line(data = IV_data, aes(x= datetime, y = global_tilted_irradiance*10 , color = "Solar radiance")) +
  labs(title = "Renewable energy decomposition",
       y = "Prévision (MW)", x = "Date", fill = "Zone") +
  theme_minimal()

save_plot_path <- 'Z:/Overleaf_elec/images/data_description'
setwd(save_plot_path)

# Assume your ggplot object is called 'p'
ggsave("renewable_energy_decomposition.png", plot = renewable_energy_decomposition , width = 12, height = 6, dpi = 300)





#compare the two tmeperature measures

IV_data %>%
  select(cold_excess, heat_excess,
         HDD_demand, CDD_demand) %>% 
  cor()


