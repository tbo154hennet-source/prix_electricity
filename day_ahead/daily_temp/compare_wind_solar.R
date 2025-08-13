
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(data.table)
library(ggplot2)


# Définir le répertoire de travail
setwd("C:\\Users\\DBT3KD\\Desktop\\Etude_macro_elec\\prix_electricite_gitlab\\day_ahead\\daily_temp\\outputs")

# Charger les données
sol_data <- as.data.table(read_parquet("sol_data.parquet"))
# Échantillonner 100 000 lignes aléatoirement
sample_sol <- sol_data[sample(.N, 100000)]
# Créer un tableau unique des coordonnées avec un identifiant
gdf_points <- unique(sample_sol[, .(latitude, longitude)])
gdf_points[, id := .I]
# Fusionner l'ID dans full_sol
sol_data <- merge(sol_data, gdf_points, by = c("latitude", "longitude"), all.x = TRUE)

# Charger les données
wind_data <- as.data.table(read_parquet("wind_data.parquet"))
# Échantillonner 100 000 lignes aléatoirement
sample_wind <- wind_data[sample(.N, 100000)]
# Créer un tableau unique des coordonnées avec un identifiant
gdf_points <- unique(sample_wind[, .(latitude, longitude)])
gdf_points[, id := .I]
# Fusionner l'ID dans full_wind
wind_data <- merge(wind_data, gdf_points, by = c("latitude", "longitude"), all.x = TRUE)


# Agrégation par date
sol_openmeteo_data <- sol_data[, .(
  #wind_speed_100m = mean(wind_speed_100m, na.rm = TRUE)
  cloud_cover = mean(cloud_cover, na.rm = TRUE),
  global_tilted_irradiance = mean(global_tilted_irradiance, na.rm = TRUE)
), by = date]


# Agrégation par date
wind_openmeteo_data <- wind_data[, .(
  wind_speed_100m = mean(wind_speed_100m, na.rm = TRUE)
  #cloud_cover = mean(cloud_cover, na.rm = TRUE),
  #global_tilted_irradiance = mean(global_tilted_irradiance, na.rm = TRUE)
), by = date]

colnames(wind_openmeteo_data)
colnames(sol_openmeteo_data)

wind_solar_data <- merge(wind_openmeteo_data, sol_openmeteo_data,
                         by = c("date")
)

setDT(wind_solar_data)

# Extraire année et mois, puis agréger
wind_solar_data[, `:=`(
  year = year(date),
  month = month(date)
)]

wind_solar_data_monthly <- wind_solar_data[, .(
  wind_speed_100m = mean(wind_speed_100m, na.rm = TRUE),
  cloud_cover = mean(cloud_cover, na.rm = TRUE),
  global_tilted_irradiance = mean(global_tilted_irradiance, na.rm = TRUE),
  Date = as.Date(paste0(unique(year), "-", unique(month), "-01"))
), by = .(year, month)]


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(wind_solar_data, "wind_solar_data.parquet")
wind_solar_data <- read_parquet("wind_solar_data.parquet")
#difference is 
# effective wind and solar energy including curtailment
#whereas I might have sold energy from other plants
#wind speed and sun exposition better for potential


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet( "price_indispo_merge_esp.parquet")
colnames(price_indispo_merge_esp)

price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)
colnames(price_indispo_merge_esp)
wind_solar_data_tot <- merge(
  wind_solar_data,
  price_indispo_merge_esp %>% 
    select(day_ahead_forecast, intraday_update_renewables, unpredicted_renewables,
           wind_solar_tot, gen_tot, temperature_demand, precipitation_demand,
           relative_humidity_demand, Fore_Wind, Fore_Solar,
            weekday, creneau,datetime),
  by.x = "date", by.y = "datetime"
)


colnames(wind_solar_data_tot)
#on essaye d'expliquer la prod de renouvelables avec mes infos et on regarde le R2
#est ce que bien encapsulé

model_wind <- lm(data =wind_solar_data_tot,
                   Fore_Wind ~ wind_speed_100m + global_tilted_irradiance)

model_windc <- lm(data =wind_solar_data_tot,
                    Fore_Wind ~ wind_speed_100m + global_tilted_irradiance +
                      temperature_demand + precipitation_demand +
                      relative_humidity_demand)

model_windfe <- lm(data =wind_solar_data_tot,
               Fore_Wind ~ wind_speed_100m + global_tilted_irradiance +
                 temperature_demand + precipitation_demand +
                 relative_humidity_demand + 
                 as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
                 )

model_solar <- lm(data =wind_solar_data_tot,
                  Fore_Solar ~ wind_speed_100m + global_tilted_irradiance)

model_solarc <- lm(data =wind_solar_data_tot,
                   Fore_Solar ~ wind_speed_100m + global_tilted_irradiance +
                    temperature_demand + precipitation_demand +
                    relative_humidity_demand)

model_solarfe <- lm(data =wind_solar_data_tot,
                    Fore_Solar ~ wind_speed_100m + global_tilted_irradiance +
                     temperature_demand + precipitation_demand +
                     relative_humidity_demand + 
                     as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)





