

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)
indispo_day_ahead <- read_parquet("indispo_day_ahead_hourly_bis.parquet") %>% 
  mutate(date = as.Date(indispo_day))



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/macro_exercise")

spain_nuclear_monthly_indispo <- read_parquet('spain_nuclear_monthly_indispo.parquet')
spain_monthly_indispo <- read_parquet('spain_monthly_indispo.parquet')
unexpected_monthly_indispo <- read_parquet('unexpected_monthly_indispo.parquet')
unexpected_nuclear_monthly_indispo <- read_parquet('unexpected_nuclear_monthly_indispo.parquet')
#merge indispos
spain_monthly_indispo <- spain_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(tot_indispo = Unavailable.Capacity)
spain_nuclear_monthly_indispo <- spain_nuclear_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(nuclear_indispo = Unavailable.Capacity)
unexpected_monthly_indispo <- unexpected_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(unexpected_tot_indispo = Unavailable.Capacity)
unexpected_nuclear_monthly_indispo <- unexpected_nuclear_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(unexpected_nuclear_indispo = Unavailable.Capacity)


plant_indispos <- merge(spain_monthly_indispo, spain_nuclear_monthly_indispo, 
                        by= c('month', 'year'))
plant_indispos <- left_join(plant_indispos, unexpected_monthly_indispo, 
                            by= c('month', 'year'))
plant_indispos <- left_join(plant_indispos, unexpected_nuclear_monthly_indispo, 
                            by= c('month', 'year')) %>% 
  replace_na(list(unexpected_tot_indispo = 0, unexpected_nuclear_indispo = 0)) %>% 
  mutate(date = as.Date(paste0("01-", month, "-", year), format = "%d-%m-%Y"))

plant_indispos$date
colnames(indispo_day_ahead)

#planned
indispo_day_ahead_monthly <- indispo_day_ahead %>% 
  mutate(month = month(date), year = year(date)) %>% 
  group_by(month, year) %>% 
  summarise(Tot_Planned_Unavailability = mean(Tot_Planned_Unavailability),
            Tot_Unplanned_Unavailability = mean(Tot_Unplanned_Unavailability)) %>%
  mutate(date = as.Date(paste0("01-", month, "-", year), format = "%d-%m-%Y"))

ggplot()+
  geom_line(data = indispo_day_ahead_monthly, aes(x = date, y = Tot_Planned_Unavailability), color = "red")+
  geom_line(data = plant_indispos, aes(x = date, y = tot_indispo), color = "blue")+
  geom_line(data = plant_indispos, aes(x = date, y = nuclear_indispo), color = "green")+
  theme_minimal()
#unplanned
ggplot()+ 
  geom_line(data = indispo_day_ahead_monthly, aes(x = date, y = Tot_Unplanned_Unavailability), color = "red")+
  geom_line(data = plant_indispos, aes(x = date, y = unexpected_tot_indispo), color = "purple")+
  geom_line(data = plant_indispos, aes(x = date, y = unexpected_nuclear_indispo), color = "orange")+
  theme_minimal()
colnames(plant_indispos)



