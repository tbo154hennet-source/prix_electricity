library(ggplot2)

renewable_factors <- c("renewable_day_ahead_forecast", "unpredicted_renewables", 
                     "intraday_update_renewables", "renewable_day_ahead_forecast_ex_es")

international_exchanges_factors <- c("NTC_ES_FR" ,"NTC_FR_ES",                                 
                             "NTC_ES_PT", "NTC_PT_ES"
                             )
temperature_factors <- c(
  "temperature_supply", "temperature_mismatch","temperature_demand", 
  "cold_excess_supply", "heat_excess_supply",
  "cold_excess_demand", "heat_excess_demand",
  "cold_excess_mismatch", "heat_excess_mismatch"
  
)

other_weather_factors <- c("precipitation_supply", "relative_humidity_supply",
                           "precipitation_demand","precipitation_mismatch",
                           "relative_humidity_demand","relative_humidity_mismatch")
indispo_factors <- c("Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
                     "Unpublished.Predicted.Unavailable.Capacity")

sum_up_factors <- c(
  "Published.Predicted.Unavailable.Capacity","renewable_day_ahead_forecast",
  "cold_excess_supply", "heat_excess_supply",
  "cold_excess_mismatch", "heat_excess_mismatch"
)

setwd("Z:/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
coef_supply_unmatched <- readRDS("reg_shift_result_supply_unmatched.rds")
coef_supply_unmatched_data <- rbindlist(coef_supply_unmatched)

coef_supply_matched <- readRDS("reg_shift_result_supply_matched.rds")
coef_supply_matched_data <- rbindlist(coef_supply_matched)


coef_demand_unmatched <- readRDS("reg_shift_result_demand_unmatched.rds")
coef_demand_unmatched_data <- rbindlist(coef_demand_unmatched)


coef_demand_matched <- readRDS("reg_shift_result_demand_matched.rds")
coef_demand_matched_data <- rbindlist(coef_demand_matched)

cum_grid <- seq(0, 60000, length.out = 1000)
price_grid <- seq(-200, 1000, length.out = 1001)
names(coef_supply_matched) <- round(price_grid,0)

# cum_grid <- seq(0, 60000,length.out = 1000)
# names(coef_supply_matched) <- round(cum_grid,0)
unique(coef_supply_matched_data$variable)
plot_shift <- function(coef_data, variables, type){
  plot_data <- coef_data %>% 
    filter(variable %in% variables) %>% 
    filter(cum_energy_test >= 5000)
  my_plot <- ggplot(data = plot_data)+
    geom_line( aes(x = cum_energy_test, y = estimate, color = variable)) +
    geom_ribbon(aes(x = cum_energy_test, 
                    ymin = estimate - 1.96*std_error, ymax = estimate + 1.96*std_error ,
                    fill = variable), alpha = 0.2) +
    labs(
      title = paste0("Shift in ",type ," Curve"),
      x = "Cumulative Energy (MWh)",
      y = "Price (€/GWh)",
      color = "Variable",
      fill = "Variable"
    ) +
    theme_minimal()
  
  # if (type == "Supply") {
  #   my_plot <- my_plot + 
  #     coord_flip()
  # }
  
  return(my_plot)
}


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp_NTC.parquet")

plot_shift(coef_supply_matched_data, sum_up_factors, "Supply" )
plot_shift(coef_supply_matched_data, temperature_factors, "Supply" )
plot_shift(coef_supply_matched_data, c("cold_excess_supply", "heat_excess_supply"), "Supply" )
plot_shift(coef_supply_matched_data, other_weather_factors, "Supply" )
plot_shift(coef_supply_matched_data, renewable_factors, "Supply" )
plot_shift(coef_supply_matched_data, indispo_factors, "Supply" )
plot_shift(coef_supply_matched_data, international_exchanges_factors, "Supply" )
plot_shift(coef_supply_matched_data, "spot_gas", "Supply")

plot_shift(coef_demand_matched_data, sum_up_factors, "Demand")
plot_shift(coef_demand_matched_data, temperature_factors, "Demand")
plot_shift(coef_demand_matched_data, renewable_factors, "Demand")
plot_shift(coef_demand_matched_data, indispo_factors, "Demand")
plot_shift(coef_demand_matched_data, international_exchanges_factors, "Demand")
plot_shift(coef_demand_matched_data, "spot_gas", "Demand")

coef_data <- coef_demand_matched_data
plot_data <- coef_data %>% 
  filter(variable %in% c("cold_excess_demand", "heat_excess_demand")) %>% 
  filter(cum_energy_test >= 5000)

# Définir un facteur de mise à l'échelle pour que l'histogramme rentre dans l'échelle principale
scale_factor <- max(plot_data$estimate, na.rm = TRUE) / max(hist(price_indispo_merge_esp$gen_tot, plot = FALSE)$counts)

ggplot(data = plot_data) +
  # Courbe principale avec bande de confiance
  geom_line(aes(x = cum_energy_test, y = estimate, color = variable)) +
  geom_ribbon(
    aes(
      x = cum_energy_test, 
      ymin = estimate - 1.96 * std_error, 
      ymax = estimate + 1.96 * std_error,
      fill = variable
    ), 
    alpha = 0.2
  ) +
  # Histogramme mis à l'échelle
  geom_histogram(
    data = price_indispo_merge_esp,
    mapping = aes(x = load_tot, y = ..count.. * scale_factor),
    bins = 60,
    color = "black",
    fill = NA
  ) +
  coord_cartesian(xlim = c(0, 60000))+
  # Axe secondaire
  scale_y_continuous(
    name = "Price (€/GWh)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Fréquence de gen_tot")
  ) +
  labs(
    title = paste0("Shift in Demand Curve"),
    x = "Cumulative Energy (MWh)",
    color = "Variable",
    fill = "Variable"
  ) +
  theme_minimal()






coef_data <- coef_supply_matched_data
plot_data <- coef_data %>% 
  filter(variable %in% c("cold_excess_supply", "heat_excess_supply",
                         "cold_excess_mismatch", "heat_excess_mismatch")) %>% 
  filter(cum_energy_test >= 5000)

# Définir un facteur de mise à l'échelle pour que l'histogramme rentre dans l'échelle principale
scale_factor <- max(plot_data$estimate, na.rm = TRUE) / max(hist(price_indispo_merge_esp$load_tot, plot = FALSE)$counts)

ggplot(data = plot_data) +
  # Courbe principale avec bande de confiance
  geom_line(aes(x = cum_energy_test, y = estimate, color = variable)) +
  geom_ribbon(
    aes(
      x = cum_energy_test, 
      ymin = estimate - 1.96 * std_error, 
      ymax = estimate + 1.96 * std_error,
      fill = variable
    ), 
    alpha = 0.2
  ) +
  # Histogramme mis à l'échelle
  geom_histogram(
    data = price_indispo_merge_esp,
    mapping = aes(x = load_tot, y = ..count.. * scale_factor),
    bins = 60,
    color = "black",
    fill = NA
  )+ 
  coord_cartesian(xlim = c(0, 60000))+
  # Axe secondaire
  scale_y_continuous(
    name = "Price (€/GWh)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Fréquence de gen_tot")
  ) +
  labs(
    title = paste0("Shift in Supply Curve"),
    x = "Cumulative Energy (MWh)",
    color = "Variable",
    fill = "Variable"
  ) +
  theme_minimal()
