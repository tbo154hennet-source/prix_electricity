library(sf)


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Temperature_instrument')
# Reading the GeoJSON file
geojson_file <- "stations_info.json"
geo_data <- st_read(geojson_file)

communes <- read.csv('communes2024.csv')

stations <- merge(geo_data, communes, 
                  by.x = 'COMMUNE', by.y = 'nom_commune_postal')
temp_data_1 <- read.csv2('Q_01_previous-1950-2022_RR-T-Vent.csv')
temp_data_2 <- read.csv2('Q_01_latest-2023-2024_RR-T-Vent.csv')
temp_data <- rbind(temp_data_1, temp_data_2)

colnames(stations)
colnames(temp_data)
library(dplyr)
library(tidyr)

mini_stations <- stations %>% 
  select("NOM_USUEL", "COMMUNE", "population")

mini_temp_data <- temp_data %>% 
  select("NOM_USUEL","AAAAMMJJ", "TM", "FFM", "RR")

mini_temp_data$Date <- as.Date(as.character(mini_temp_data$AAAAMMJJ), format = "%Y%m%d")

temp_instrument_data <- merge(mini_stations, mini_temp_data)

colnames(temp_instrument_data)
library(lubridate)

# test <- sample_n(temp_instrument_data, 100)
# 
# colnames(test)

#quantitative instrument for temperature

country_temp_instrument <- st_drop_geometry(temp_instrument_data) %>%
  group_by(Date) %>%
  summarise(T_avg = weighted.mean(as.numeric(TM),
                                  population, na.rm = TRUE))    


country_temp_instrument <- country_temp_instrument %>% 
  mutate(month = month(Date),
         year= year(Date)) %>% 
  filter(year > 2010)

country_temp_instrument <- country_temp_instrument %>% 
  group_by(month) %>% 
  mutate(T_month = mean(T_avg), deviation = T_avg - T_month)

write.csv(country_temp_instrument,
          'daily_temperature_deviation.csv')

temp_deviation <- country_temp_instrument %>% 
  group_by(month, year) %>% 
  summarize(T_year_month = mean(T_avg) - T_month )%>% 
  distinct()

cold_months <- c(1, 2, 3, 11, 12)
hot_months <- c(6, 7, 8)

cold_excess <- temp_deviation %>% 
 filter( month %in% cold_months) %>% 
  rename(cold_excess_instrument = T_year_month)

heat_excess <- temp_deviation %>% 
  filter( month %in% hot_months)%>% 
  rename(heat_excess_instrument = T_year_month)

cold_excess_instrument <- left_join(temp_deviation, cold_excess)%>% 
  mutate_all(~ replace_na(., 0))

heat_excess_instrument <- left_join(temp_deviation, heat_excess)%>% 
  mutate_all(~ replace_na(., 0))

temperature_instrument <- merge(cold_excess_instrument,
                                heat_excess_instrument )

#CDD and HDDs
#non-linear count 
#ajoute un niveau de détail


excess_CDD <- country_temp_instrument %>%
  filter(T_avg < T_month ) %>%
  group_by(month, year) %>% 
  summarise(count = n()) %>% 
  filter( month %in% cold_months)%>% 
  rename(CDD_excess_instrument = count)

excess_HDD <- country_temp_instrument %>%
  filter(T_avg > T_month ) %>%
  group_by(month, year) %>% 
  summarise(count = n())%>% 
  filter( month %in% hot_months)%>% 
  rename(HDD_excess_instrument = count)

CDD_excess_instrument <- left_join(temp_deviation, excess_CDD)%>% 
  mutate_all(~ replace_na(., 0))

HDD_excess_instrument <- left_join(temp_deviation, excess_HDD)%>% 
  mutate_all(~ replace_na(., 0))

temperature_instrument <- merge(temperature_instrument,
                                CDD_excess_instrument)

temperature_instrument <- merge(temperature_instrument,
                                HDD_excess_instrument)

save_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'

setwd(save_path)
write.csv(temperature_instrument, 
          'temperature_instrument.csv')

