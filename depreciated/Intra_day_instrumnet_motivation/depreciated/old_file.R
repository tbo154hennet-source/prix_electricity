

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)
indispo_day_ahead <- read_parquet("indispo_day_ahead_hourly_bis.parquet")

colnames(indispo_day_ahead)
colnames(plant_indispos)

plant_indispos <- plant_indispos %>% 
  mutate(Date = as.Date(paste0("01-", month, "-", year), format = "%d-%m-%Y"))

monthly_indispo_day_ahead$Date

monthly_indispo_day_ahead <- indispo_day_ahead %>%
  mutate(Year = year(indispo_day), Month = month(indispo_day)) %>%
  group_by(Year, Month) %>%
  summarise(
            Tot_Planned_Unavailability = mean(Tot_Planned_Unavailability),
            Tot_Unplanned_Unavailability = mean(Tot_Unplanned_Unavailability),
            Date = as.Date(first(indispo_day)))


# unplanned
ggplot() +
  geom_line(data= monthly_indispo_day_ahead, 
            aes(x=Date, y=Tot_Unplanned_Unavailability, color = 'New')) +
  geom_line(data= plant_indispos, 
            aes(x=Date, y=unexpected_tot_indispo, color = 'Old')) +
  geom_line(data= plant_indispos, 
            aes(x=Date, y=unexpected_nuclear_indispo, color = 'Old')) +
  labs(title = "Unplanned Unavailabilities", x = "Day", y = "Total Unavailability Capacity") +
  theme_minimal()

#planned
ggplot() +
  geom_line(data= monthly_indispo_day_ahead, 
            aes(x=Date, y=Tot_Planned_Unavailability, color = 'New')) +
  geom_line(data= plant_indispos, 
            aes(x=Date, y=tot_indispo, color = 'Old')) +
  geom_line(data= plant_indispos, 
            aes(x=Date, y=nuclear_indispo, color = 'Old')) +
  labs(title = "Planned Unavailabilities", x = "Day", y = "Total Unavailability Capacity") +
  theme_minimal()
