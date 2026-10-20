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
  dplyr::select(NOM_USUEL, COMMUNE, population)

mini_temp_data <- temp_data %>% 
  dplyr::select(NOM_USUEL,AAAAMMJJ, TM, FFM, RR)

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

getwd()
write.csv(country_temp_instrument,
          'daily_temperature_deviation.csv')


cold_months <- c(1, 2, 3, 11, 12)
hot_months <- c(6, 7, 8)

mean_temp_instrument <- country_temp_instrument %>%
  group_by(month, year) %>%
  summarise(T_avg = mean(T_avg),
            deviation = mean(deviation),
              T_month = mean(T_month))

#on veut qu'ils aient un effet dans le même sens pour pouvoir
#en faire une unique série
# en été une déviation positive (canicule) augmente la demande d'éléc
#en hiver une déviation positive (redoux) diminue la demande d'éléc

mean_temp_instrument <- mean_temp_instrument %>% 
  mutate(cold_excess_instrument = -ifelse(month %in% cold_months, deviation, NA),
         heat_excess_instrument = ifelse(month %in% hot_months, deviation, NA)
         )


#CDD and HDDs
#non-linear count 
#ajoute un niveau de détail
T_base_cold <- 10
T_base_hot <- 20


excess_HDD <- country_temp_instrument %>%
  filter(T_avg > T_base_hot ) %>%
  group_by(month, year) %>% 
  summarise(count = n(), intensity =  sum(pmax(0, (T_base_hot - T_avg)^2)))%>%  
  rename(HDD = count, HDD_intensity = intensity) %>% 
# compare with the avg number of HDD per year
  ungroup() %>% 
  group_by(month) %>%
  mutate(HDD_avg = mean(HDD),
         HDD_intensity_avg = mean(HDD_intensity)
         ) %>%
  ungroup() %>%
  mutate(HDD_excess_instrument = ifelse(month %in% hot_months, HDD - HDD_avg, NA),
         HDD_intensity_instrument =ifelse(month %in% hot_months, HDD_intensity - HDD_intensity_avg, NA) 
         ) 

excess_CDD <- country_temp_instrument %>%
  filter(T_avg < T_base_cold) %>%
  group_by(month, year) %>% 
  summarise(count = n(), intensity =  sum(pmax(0, (T_base_cold  - T_avg)^2))) %>%
  rename(CDD = count, CDD_intensity = intensity) %>%
  # compare with the avg number of CDD per year
  ungroup() %>%
  group_by(month) %>%
  mutate(CDD_avg = mean(CDD),
         CDD_intensity_avg = mean(CDD_intensity)
         ) %>%
  ungroup() %>%
  mutate(CDD_excess_instrument = -ifelse(month %in% cold_months, CDD - CDD_avg, NA),
         CDD_intensity_instrument =-ifelse(month %in% cold_months, CDD_intensity - CDD_intensity_avg, NA) 
         )


# CDD_excess_instrument <- left_join(temp_deviation, excess_CDD)#%>% 
#   #mutate_all(~ replace_na(., 0))
# 
# HDD_excess_instrument <- left_join(temp_deviation, excess_HDD)#%>% 
#   #mutate_all(~ replace_na(., 0))

temperature_instrument <- left_join(mean_temp_instrument,
                                excess_CDD)

temperature_instrument <- left_join(temperature_instrument,
                                excess_HDD) %>% 
  replace_na(list(CDD_excess_instrument = 0, HDD_excess_instrument = 0,
                  CDD_intensity_instrument = 0, HDD_intensity_instrument = 0,
                  cold_excess_instrument = 0, heat_excess_instrument = 0))

colnames(temperature_instrument)

ggplot(temperature_instrument %>% 
         mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
         arrange(date)
         , aes(x = date)) +
  geom_line(aes(y = T_avg - mean(temperature_instrument$T_avg), color = "T_avg")) +
  #geom_line(aes(y = T_month, color = "T_month")) +
  #geom_line(aes(y = deviation, color = "deviation")) +
  geom_line(aes(y = cold_excess_instrument, color = "cold_excess_instrument", linetype = "Standard")) +
  geom_line(aes(y = heat_excess_instrument, color = "heat_excess_instrument", linetype = "Standard")) +
  geom_line(aes(y = CDD_excess_instrument, color = "cold_excess_instrument", linetype = "Non-linear")) +
  geom_line(aes(y = HDD_excess_instrument, color = "heat_excess_instrument", linetype = "Non-linear")) +
  labs(title = "Temperature Instrumentation",
       x = "Year",
       y = "Temperature") +
  scale_color_manual(values = c( "cold_excess_instrument" = "cyan",
                                 "heat_excess_instrument" = "green",
                                 # "CDD_excess_instrument" = "green",
                                 # "HDD_excess_instrument" = "orange",
                                 "T_avg" = "blue", 
                                 "T_month" = "red"
                                )) +
  theme_minimal()



save_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'

setwd(save_path)

write.csv(temperature_instrument, 
          'temperature_instrument.csv')



