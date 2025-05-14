
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(data.table)
library(ggplot2)


# Définir le répertoire de travail
setwd("C:\\Users\\DBT3KD\\Desktop\\Etude_macro_elec\\prix_electricite_gitlab\\day_ahead\\daily_temp\\outputs")

# Charger les données
full_pop <- as.data.table(read_parquet("temperature_pop.parquet"))

# Échantillonner 100 000 lignes aléatoirement
sample_pop <- full_pop[sample(.N, 100000)]

# Créer un tableau unique des coordonnées avec un identifiant
gdf_points <- unique(sample_pop[, .(latitude, longitude)])
gdf_points[, id := .I]

# Fusionner l'ID dans full_pop
full_pop <- merge(full_pop, gdf_points, by = c("latitude", "longitude"), all.x = TRUE)



# Agrégation par date
temperature_openmeteo_data <- full_pop[, .(
  temperature_2m = mean(temperature_2m, na.rm = TRUE),
  relative_humidity_2m = mean(relative_humidity_2m, na.rm = TRUE),
  precipitation = sum(precipitation, na.rm = TRUE)
), by = date]


setDT(temperature_openmeteo_data)

# Extraire année et mois, puis agréger
temperature_openmeteo_data[, `:=`(
  year = year(date),
  month = month(date)
)]

temperature_openmeteo_data_monthly <- temperature_openmeteo_data[, .(
  temperature_2m = mean(temperature_2m, na.rm = TRUE),
  relative_humidity_2m = mean(relative_humidity_2m, na.rm = TRUE),
  precipitation = sum(precipitation, na.rm = TRUE),
  Date = as.Date(paste0(unique(year), "-", unique(month), "-01"))
), by = .(year, month)]


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(temperature_openmeteo_data, "temperature_openmeteo_data.parquet")

# Conversion en numérique
# temperature_openmeteo_data_monthly[, temperature_2m := as.numeric(temperature_2m)]
# temp_instrument_data[, T_avg := as.numeric(T_avg)]




# Définir le répertoire de travail

setwd("C:\\Users\\DBT3KD\\Desktop\\Etude_macro_elec\\prix_electricite_gitlab\\day_ahead\\daily_temp\\outputs")
full_plant <- read_parquet("temperature_plants.parquet")

full_plant <- full_plant %>% 
  distinct(date, latitude, longitude, .keep_all = TRUE)

setDT(full_plant)
plants_data <- read.csv("plants_power.csv")

thermal_plants = c("coal" ,"gas"  , "biomass",                 
            "gas;oil", "coal;diesel;gas_natural")
#unique(plants_data$source)
# Villalcampo apparaît deux fois I et II
plants_data <- plants_data %>% 
  group_by(latitude, longitude) %>%
  summarise( n = n(),
            plant_name = ifelse(n >1, "Villalcampo I et II",plant_name),
            source =  ifelse(n >1, "hydro",source),
            power_mw = sum(power_mw, na.rm = TRUE)
            ) %>% 
  select(-n) %>% 
  ungroup() %>% 
#merge all the thermal plants together
  mutate(source = ifelse(source %in% thermal_plants, "thermal", source))


#sum(is.na(plants_data$power_mw))

full_plant <- merge(full_plant, plants_data,
                    by = c("latitude", "longitude"))


# Agrégation par date
temperature_openmeteo_plant_data <- full_plant[, .(
  temperature_2m = weighted.mean(temperature_2m, power_mw, na.rm = TRUE),
  relative_humidity_2m = weighted.mean(relative_humidity_2m, power_mw, na.rm = TRUE),
  precipitation = weighted.mean(precipitation, power_mw, na.rm = TRUE) # pas besoin de pondérer une somme
), by = date]


setDT(temperature_openmeteo_plant_data)

# Extraire année et mois, puis agréger
temperature_openmeteo_plant_data[, `:=`(
  year = year(date),
  month = month(date)
)]

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(temperature_openmeteo_plant_data, "temperature_openmeteo_plant_data.parquet")


temperature_openmeteo_plant_data_monthly <- temperature_openmeteo_plant_data[, .(
  temperature_2m = mean(temperature_2m, na.rm = TRUE),
  relative_humidity_2m = mean(relative_humidity_2m, na.rm = TRUE),
  precipitation = sum(precipitation, na.rm = TRUE),
  Date = as.Date(paste0(unique(year), "-", unique(month), "-01"))
), by = .(year, month)]



library(readxl)

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

temp_instrument_data <- t(read_excel('temperature_data.xlsx'))
temp_instrument_data <- as.data.frame(temp_instrument_data[c(-1,-2),])
colnames(temp_instrument_data) <- c("T_avg", "T_min", "T_max")
temp_instrument_data$Date <- rownames(temp_instrument_data)
temp_instrument_data <- temp_instrument_data %>% 
  mutate(Date = as.Date(paste0(Date,"-01"), format = "%Y-%m-%d"),
         month = month(Date),
         year = year(Date)) %>% 
  filter(year >= 2015)



ggplot()+
  geom_line(data = temperature_openmeteo_data_monthly, aes(x = Date, y = temperature_2m, color = "Open Meteo Population")) +
  geom_line(data = temperature_openmeteo_plant_data_monthly, aes(x = Date, y = temperature_2m, color = "Open Meteo Plant")) +
  geom_line(data = temp_instrument_data, aes(x = Date, y = as.numeric(T_avg), color = "World Bank")) +
  theme_minimal()

colnames(temperature_openmeteo_data_monthly)
colnames(temperature_openmeteo_data)


#build the final daily data for day ahead specification
temperature_openmeteo_plant_data_by_source <- full_plant[, .(
  temperature_2m = weighted.mean(temperature_2m, power_mw, na.rm = TRUE),
  relative_humidity_2m = weighted.mean(relative_humidity_2m, power_mw, na.rm = TRUE),
  precipitation = weighted.mean(precipitation, power_mw, na.rm = TRUE)  # pondéré si tu le souhaites
), by = .(date, source)]

# Extraire année et mois, puis agréger
temperature_openmeteo_plant_data_by_source[, `:=`(
  year = year(date),
  month = month(date)
)]

temperature_openmeteo_plant_data_wide <- dcast(
  temperature_openmeteo_plant_data_by_source,
  date ~ source,
  value.var = c("temperature_2m", "relative_humidity_2m", "precipitation"),
  sep = "_"
)
#View(temperature_openmeteo_plant_data_wide[1:100,])



temperature_openmeteo_data_tot <- merge(temperature_openmeteo_data %>% 
                                                  select(date, temperature_2m, precipitation, relative_humidity_2m ) %>%
                                                  rename(temperature_demand= temperature_2m,
                                                         precipitation_demand= precipitation,
                                                         relative_humidity_demand= relative_humidity_2m
                                                         )
                                                ,
                                                temperature_openmeteo_plant_data %>% 
                                                  select(date, temperature_2m, precipitation, relative_humidity_2m) %>%
                                                  rename(temperature_supply= temperature_2m,
                                                         precipitation_supply= precipitation,
                                                         relative_humidity_supply= relative_humidity_2m
                                                         )
                                                ,
                                                by = c("date")
)
temperature_openmeteo_data_tot <- merge(temperature_openmeteo_data_tot,
                                        temperature_openmeteo_plant_data_wide,
  by = c("date")
)
#View(temperature_openmeteo_data_tot[1:100,])

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(temperature_openmeteo_data_tot, "temperature_opendata_tot.parquet")









#build adapted temperature instrument for spain


temperature_openmeteo_data_monthly_tot <- merge(temperature_openmeteo_data_monthly %>% 
                                                  select(year, month, temperature_2m) %>%
                                                  rename(temperature_demand= temperature_2m)
                                                  ,
                                                temperature_openmeteo_plant_data_monthly %>% 
                                                  select(year, month, temperature_2m) %>%
                                                  rename(temperature_supply= temperature_2m)
                                                  ,
                                                by = c("year", "month")
                                                )

cold_months <- c(1, 2, 11, 12)
hot_months <- c(6, 7, 8, 9)

#class(temp_instrument_data$T_avg)

mean_temperatures <- temperature_openmeteo_data_monthly_tot %>% 
  group_by(month) %>% 
  summarise(mean_temp_demand = mean(as.numeric(temperature_demand), na.rm = TRUE),
            mean_temp_supply = mean(as.numeric(temperature_supply), na.rm = TRUE))


mean_temp_instrument <- temperature_openmeteo_data_monthly_tot %>% 
  mutate(cold_excess_instrument_demand = -ifelse(month %in% cold_months, 
                                          as.numeric(temperature_demand) - mean_temperatures[month, 'mean_temp_demand'][[1]], 0),
         heat_excess_instrument_demand = ifelse(month %in% hot_months,
                                         as.numeric(temperature_demand) - mean_temperatures[month, 'mean_temp_demand'][[1]], 0),
         cold_excess_instrument_supply = -ifelse(month %in% cold_months, 
                                          as.numeric(temperature_supply) - mean_temperatures[month, 'mean_temp_supply'][[1]], 0),
         heat_excess_instrument_supply = ifelse(month %in% hot_months,
                                         as.numeric(temperature_supply) - mean_temperatures[month, 'mean_temp_supply'][[1]], 0),
         temp_excess_instrument_demand = cold_excess_instrument_demand + heat_excess_instrument_demand,
         temp_excess_instrument_supply = cold_excess_instrument_supply + heat_excess_instrument_supply,
         temperature_spatial_mismatch = cold_excess_instrument_supply - cold_excess_instrument_demand + 
           heat_excess_instrument_supply - heat_excess_instrument_demand,
         Date = as.Date(paste0(year, "-", month, "-01"))
         ) 

getwd()
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Temperature_instrument/esp')
write_parquet(mean_temp_instrument, "temperature_instrument_opendata_meteo.parquet")

ggplot(data = mean_temp_instrument)+
  geom_line(aes(x = Date, y = (temperature_demand - mean(temperature_demand))/10, color = "temperature", linetype = "demand")) +
  geom_line(aes(x = Date, y = (temperature_supply- mean(temperature_supply))/10, color = "temperature", linetype = "supply")) +
  geom_line(aes(x = Date, y = cold_excess_instrument_demand, color = "cold_excess", linetype = "demand")) +
  geom_line(aes(x = Date, y = heat_excess_instrument_demand, color = "heat_excess", linetype = "demand")) +
  geom_line(aes(x = Date, y = cold_excess_instrument_supply, color = "cold_excess", linetype = "supply")) +
  geom_line(aes(x = Date, y = heat_excess_instrument_supply, color = "heat_excess", linetype = "supply")) +
  geom_line(aes(x = Date, y = temperature_spatial_mismatch
                  , color = "spatial_mismatch")) +
  labs(
    title = "Temperature Instrument",
    x = "Time",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

mean_temp_instrument %>% 
  select(temp_excess_instrument_supply, temp_excess_instrument_demand, temperature_spatial_mismatch) %>%
  cor(use = "pairwise.complete")


