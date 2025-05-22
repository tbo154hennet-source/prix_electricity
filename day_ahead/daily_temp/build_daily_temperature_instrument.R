#est que prendre hour ou day change quelque chose
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Temperature_instrument/esp'
setwd(save_path_esp)
mean_temp_instrument <- read_excel( 'temperature_instrument.xlsx')


cold_excess_test <- temperature_openmeteo_data_test %>% 
  group_by(month, year) %>% 
  summarise(
            # cold_excess_supply_hour = mean(cold_excess_supply_hour, na.rm = TRUE),
            # cold_excess_demand_hour = mean(cold_excess_demand_hour, na.rm = TRUE),
            # cold_excess_supply_day = mean(cold_excess_supply_day, na.rm = TRUE),
            # cold_excess_demand_day = mean(cold_excess_demand_day, na.rm = TRUE),
            cold_excess_supply = mean(cold_excess_supply, na.rm = TRUE),
            cold_excess_demand = mean(cold_excess_demand, na.rm = TRUE),
            cold_excess_nuclear = mean(cold_excess_nuclear, na.rm = TRUE),
            date = first(date))
# Convert to data.tables



# Merge by Date

cold_excess_data <- merge(cold_excess_test, mean_temp_instrument %>% 
                            mutate(Date = as.POSIXct(Date)), 
                   by.x = "date", by.y = "Date")



cold_excess_data %>% 
  dplyr::select(
    # cold_excess_demand_hour,
    #              cold_excess_demand_day,
                cold_excess_supply,
                cold_excess_demand,
                cold_excess_nuclear,
                cold_excess_instrument
                ) %>% 
  cor()

ggplot(cold_excess_test) +
  #geom_line(aes(y =IV_data$cold_excess_demand , color = "Original")) +
  geom_line(aes(x = date, y = cold_excess_demand_day, color = "Demand Day")) +
  geom_line(aes(x = date, y = cold_excess_demand_hour, color = "Demand Hour")) +
  #geom_line(aes(x = date, y = cold_excess_supply_day, color = "Supply Day")) +
  #geom_line(aes(x = date, y = cold_excess_supply_hour, color = "Supply Hour")) +
  geom_line(data = mean_temp_instrument, aes(x= as.POSIXct(Date) , y = cold_excess_instrument, color = "Old")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  theme_minimal()




#regarder si mon nouveau cold_excess_demand cohérent avec l'ancien




class( IV_data$datetime)
colnames(mean_temp_instrument)
class(mean_temp_instrument$Date)
ggplot() +
  #geom_line(aes(y =IV_data$cold_excess_demand , color = "Original")) +
  geom_line(aes(x = as.Date(IV_data$datetime), y = seasonally_adjusted_df$cold_excess_demand, color = "Detrended")) +
  geom_line(data = mean_temp_instrument, aes(x= Date, y = cold_excess_instrument, color = "Old")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue",
                                "Detrended" = "red",
                                "Old" = "green")) +
  theme_minimal()

library(data.table)

# Convert to data.tables
sa_dt <- data.table(Date = as.Date(IV_data$datetime),
                    seasonally_adjusted = seasonally_adjusted_df$cold_excess_demand)

old_dt <- data.table(Date = mean_temp_instrument$Date,
                     old_instrument = mean_temp_instrument$cold_excess_instrument)

# Merge by Date
merged_dt <- merge(sa_dt, old_dt, by = "Date", all = FALSE)

correlation_result <- cor(merged_dt$seasonally_adjusted, merged_dt$old_instrument, use = "complete.obs")
print(correlation_result)

#cor(merged_dt$seasonally_adjusted, merged_dt$old_instrument, use = "complete.obs", method = "spearman")

