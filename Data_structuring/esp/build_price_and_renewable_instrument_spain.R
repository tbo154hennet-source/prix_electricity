

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs"
setwd(save_path_data)
price_indispo_merge <- read_parquet("price_indispo_merge_esp.parquet")
  

  # Fonction pour calculer la moyenne pondérée
weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}



monthly_data <- price_indispo_merge %>%
  mutate(Nuclear.Unavailabilities =
           Planned.Unavailable.Nuclear.Capacity+ Published.Unavailable.Nuclear.Capacity+
           Uneffective.Unavailable.Nuclear.Capacity+Unpublished.Unavailable.Nuclear.Capacity,
         Thermal.Unavailabilities = Planned.Unavailable.Capacity + Published.Unavailable.Capacity+
           Uneffective.Unavailable.Capacity+Unpublished.Unavailable.Capacity - Nuclear.Unavailabilities,
         Predicted.Unavailabilities = Planned.Predicted.Unavailable.Capacity + Published.Predicted.Unavailable.Capacity+
           Uneffective.Predicted.Unavailable.Capacity+Unpublished.Predicted.Unavailable.Capacity
           ) %>% 
  group_by(month, year) %>%
  summarise(
    spot_price = weighted_mean(price, gen_tot),
    renewable_forecast = weighted.mean(day_ahead_forecast, w = gen_tot, na.rm = TRUE),
    nuclear_unavailabilities = weighted.mean(Nuclear.Unavailabilities, w = gen_tot, na.rm = TRUE),
    thermal_unavailabilities = weighted.mean(Thermal.Unavailabilities, w = gen_tot, na.rm = TRUE),
    predicted_unavailabilities = weighted.mean(Predicted.Unavailabilities, w = gen_tot, na.rm = TRUE),
    gen_tot = mean(gen_tot, na.rm = TRUE)
  ) %>%
  ungroup()



