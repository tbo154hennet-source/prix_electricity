# Les cellules photovoltaïques ont un coefficient de température négatif :
#   typiquement –0,4 % à –0,5 % de rendement par °C au-dessus de 25°C.
# 
# À 40°C (température de module courante l’été), cela correspond à une perte de ~6–8 % de production instantanée.
# 
# En conditions extrêmes, les pertes peuvent dépasser 10–12 %.
# 👉 Le solaire photovoltaïque est de loin la technologie dont l’efficacité diminue le plus avec la montée des températures.
# 
# 
# La densité de l’air diminue avec la chaleur → puissance légèrement réduite.
# Impact faible à modéré (souvent < 1–2 % par 10°C).
# 
# Idem pour les dentrales thermiques
# Les pertes viennent surtout des contraintes de refroidissement :
# efficacité légèrement plus faible de la turbine (cycle vapeur → baisse 0,1–0,2 %/°C)




#NOTA BENE 
#The spatial distribution of demand changes along the day
#whereas it is constant for supply

#in particular one can use an east to west demand shift
#quand le soleil tombe les gens allument la lumière
#difficile à identifier car pendant la montée en charge de la soirée





library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(data.table)
library(ggplot2)


#je ne me concentre plus sur l'efficacité réduite des PV 
#et non plus sur les centrales thermiques


load_batches <- function(folder) {
  
  # Vérifier dossier
  if (!dir.exists(folder)) {
    stop("Le dossier n'existe pas : ", folder)
  }
  
  # Trouver fichiers parquet
  files <- list.files(folder, pattern = "\\.parquet$", full.names = TRUE)
  
  if (length(files) == 0) {
    stop("Aucun fichier .parquet trouvé dans : ", folder)
  }
  
  # Charger + empiler
  dt <- rbindlist(
    lapply(files, function(f) {
      tryCatch(
        read_parquet(f),
        error = function(e) {
          message("Erreur lecture fichier: ", f)
          message(e)
          return(NULL)
        }
      )
    }),
    use.names = TRUE, fill = TRUE
  )
  
  setDT(dt)
  
  # Supprimer doublons latitude/longitude/date
  dt <- unique(dt, by = c("latitude", "longitude", "date"))
  
  return(dt)
}

solar_temp_dt <- load_batches("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs/solar_temp_batches_bis")

solar_all_dt <- merge(solar_dt, solar_temp_dt, 
                      by = c("latitude", "longitude", "date"), all.x = TRUE)
colnames(solar_all_dt)
sol_temp_openmeteo_data <- solar_all_dt[, .(
  temperature_ss = weighted.mean(temperature_2m, capacity_mw, na.rm = TRUE),
  relative_humidity_2m_ss = weighted.mean(relative_humidity_2m, capacity_mw, na.rm = TRUE),
  precipitation_ss = weighted.mean(precipitation, capacity_mw, na.rm = TRUE)
), by = date]
#ss solar supply
sol_all_openmeteo_data <- solar_all_dt[, .(
  wind_speed_100m_ss = mean(wind_speed_100m, na.rm = TRUE),
  cloud_cover_ss = weighted.mean(cloud_cover,capacity_mw, na.rm = TRUE),
  global_tilted_irradiance_ss = weighted.mean(global_tilted_irradiance,capacity_mw, na.rm = TRUE),
  temperature_ss = weighted.mean(temperature_2m, capacity_mw, na.rm = TRUE),
  relative_humidity_2m_ss = weighted.mean(relative_humidity_2m, capacity_mw, na.rm = TRUE),
  precipitation_ss = weighted.mean(precipitation, capacity_mw, na.rm = TRUE)
), by = date]



setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")
# Charger les données
temperature_demand_data <- as.data.table(read_parquet("temperature_pop.parquet"))

gdf_demand_points <- read.csv("demand_points_corrected.csv") %>% 
  select(c("latitude", "longitude", "factor", "comunidad"))
dim(gdf_demand_points)
colnames(gdf_demand_points)
# Fusionner l'ID dans full_wind
temperature_demand_data <- merge(temperature_demand_data, gdf_demand_points, by = c("latitude", "longitude"), all.x = TRUE)


# Agrégation par date
temperature_demand_openmeteo_data <- temperature_demand_data[, .(
  temperature = mean(temperature_2m*factor, na.rm = TRUE),
  relative_humidity_2m = mean(relative_humidity_2m*factor, na.rm = TRUE),
  precipitation = mean(precipitation*factor, na.rm = TRUE)
), by = date]



colnames(temperature_demand_openmeteo_data) <- c("date", paste0(setdiff(colnames(temperature_demand_openmeteo_data), "date"), "_demand"))
#colnames(sol_temp_openmeteo_data ) <- c("date", paste0(setdiff(colnames(sol_temp_openmeteo_data ), "date"), "_ss"))



tempall_data_data <-  merge(temperature_demand_openmeteo_data, 
                            sol_all_openmeteo_data,
                          by = c("date")
)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(tempall_data_data, "tempall_data_data.parquet")



solar_data_tot <- merge(
  tempall_data_data,
  price_indispo_merge_esp %>% 
    select(day_ahead_forecast, intraday_update_renewables, unpredicted_renewables,
           wind_solar_tot, gen_tot, 
           relative_humidity_demand, Fore_Wind, Fore_Solar,year, month,
           weekday, creneau,datetime),
  by.x = "date", by.y = "datetime"
)

solar_data_tot <- merge(
  solar_data_tot,
  demand_openmeteo_data,
  by = "date"
)
colnames(demand_openmeteo_data)
mapping <- c(
  # --- WIND ---
  "wind_speed_100m_ss"                = "wind_speed_100m_supply",
  # --- SOLAR ---
  "global_tilted_irradiance_ss"       = "global_tilted_irradiance_supply",
  # --- WEATHER SUPPLY ---
  "temperature_ss"                 = "temperature_supply"
)

rev_mapping <- setNames(names(mapping), mapping)

# colnames(solar_data_tot)

solar_data_tot <- solar_data_tot %>%
  rename(all_of(rev_mapping)) %>% 
  mutate(
    temp_supply_above25 = pmax(temperature_supply - 25, 0),
    temp_supply_below25 = pmax(25 - temperature_supply, 0),
    
    temp_demand_above25 = pmax(temperature_demand - 25, 0),
    temp_demand_below25 = pmax(25 - temperature_demand, 0)
  )
#solar_data_tot_test <- sample_n(solar_data_tot, 100)



m_solarss <- lm(data =solar_data_tot,
                log(Fore_Solar+1 ) ~ wind_speed_100m_supply + log(global_tilted_irradiance_supply +1) + #cloud_cover_ss + 
                   relative_humidity_2m_ss + precipitation_ss+
                  #temperature_supply +
                  temp_supply_above25 + temp_supply_below25 +
                  # wind_speed_100m_ss + global_tilted_irradiance_ss + #cloud_cover_ss + 
                  # temperature_ss + relative_humidity_2m_ss + precipitation_ss+
                  #temperature_demand + precipitation_demand +relative_humidity_demand + 
                  as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)


m_solarssp <- lm(data =solar_data_tot,
                 log(Fore_Solar+1 ) ~ wind_speed_100m_supply + log(global_tilted_irradiance_supply +1) + #cloud_cover_ss + 
                   log(global_tilted_irradiance_demand +1) +
                   relative_humidity_2m_ss + precipitation_ss+
                   temperature_supply + temperature_demand+
                   #temp_supply_above25 + temp_supply_below25 +
                   # wind_speed_100m_ss + global_tilted_irradiance_ss + #cloud_cover_ss + 
                   # temperature_ss + relative_humidity_2m_ss + precipitation_ss+
                   #temperature_demand + precipitation_demand +relative_humidity_demand + 
                   as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)
m_solarssc<- lm(data =solar_data_tot ,
                log(Fore_Solar+1) ~ wind_speed_100m_supply + log(global_tilted_irradiance_supply+1) + #cloud_cover_ss + 
                  #temperature_supply + 
                  log(global_tilted_irradiance_demand +1) +temp_supply_above25 + temp_supply_below25 +
                  relative_humidity_2m_ss + precipitation_ss+
                  temp_demand_above25 + temp_demand_below25 +
                  precipitation_demand +relative_humidity_2m_demand + 
                  as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

solar_data_tot %>% 
  select(temperature_demand, temperature_supply) %>% 
  cor()

stargazer(m_solarss,m_solarssp, m_solarssc,
          type = "text",
          keep = c(
            "wind_speed_100m_supply",
            "wind_speed_100m_demand",
            "global_tilted_irradiance_supply",
            "global_tilted_irradiance_demand",
            "temperature_supply",
            "temperature_demand",
            "temp_supply",
            "temp_demand"
          ))

stargazer(
  #m_wind, 
  m_windc, m_windfe,
  #m_solar, 
  m_solarc, m_solarfe,
  m_solarssc ,
  type = "text",
  keep = c(
    "wind_speed_100m_supply",
    "wind_speed_100m_demand",
    "global_tilted_irradiance_supply",
    "global_tilted_irradiance_demand",
    "temperature_supply",
    "temperature_demand"
  ),
  column.labels = c("Wind",  "Wind FE",
                    "Solar",  "Solar FE",  "Solar FE"),
  dep.var.labels = c("Production"),
  title = "Regression Results for Wind and Solar Models",
  digits = 3
)

# ################## PLANT part

# # Définir le répertoire de travail
# 
# setwd("Z:\\prix_electricite_gitlab\\prix_electricite_gitlab\\day_ahead\\daily_temp\\outputs")
# full_plant <- read_parquet("temperature_plants.parquet")
# 
# full_plant <- full_plant %>% 
#   distinct(date, latitude, longitude, .keep_all = TRUE)
# 
# setDT(full_plant)
# plants_data <- read.csv("plants_power.csv")
# 
# thermal_plants = c("coal" ,"gas"  , "biomass",                 
#             "gas;oil", "coal;diesel;gas_natural")
# #unique(plants_data$source)
# # Villalcampo apparaît deux fois I et II
# plants_data <- plants_data %>% 
#   group_by(latitude, longitude) %>%
#   summarise( n = n(),
#             plant_name = ifelse(n >1, "Villalcampo I et II",plant_name),
#             source =  ifelse(n >1, "hydro",source),
#             power_mw = sum(power_mw, na.rm = TRUE)
#             ) %>% 
#   select(-n) %>% 
#   ungroup() %>% 
# #merge all the thermal plants together
#   mutate(source = ifelse(source %in% thermal_plants, "thermal", source))
# 
# 
# #sum(is.na(plants_data$power_mw))
# 
# full_plant <- merge(full_plant, plants_data,
#                     by = c("latitude", "longitude"))
# 
# 
# # Agrégation par date
# temperature_openmeteo_plant_data <- full_plant[, .(
#   temperature_2m = weighted.mean(temperature_2m, power_mw, na.rm = TRUE),
#   relative_humidity_2m = weighted.mean(relative_humidity_2m, power_mw, na.rm = TRUE),
#   precipitation = weighted.mean(precipitation, power_mw, na.rm = TRUE) # pas besoin de pondérer une somme
# ), by = date]
# 
# 
# setDT(temperature_openmeteo_plant_data)
# 
# # Extraire année et mois, puis agréger
# temperature_openmeteo_plant_data[, `:=`(
#   year = year(date),
#   month = month(date)
# )]
# 
# setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
# write_parquet(temperature_openmeteo_plant_data, "temperature_openmeteo_plant_data.parquet")
# 
# 
# temperature_openmeteo_plant_data_monthly <- temperature_openmeteo_plant_data[, .(
#   temperature_2m = mean(temperature_2m, na.rm = TRUE),
#   relative_humidity_2m = mean(relative_humidity_2m, na.rm = TRUE),
#   precipitation = sum(precipitation, na.rm = TRUE),
#   Date = as.Date(paste0(unique(year), "-", unique(month), "-01"))
# ), by = .(year, month)]
# 


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

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
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















#cas français

# Définir le répertoire de travail
setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")
# Charger les données
full_pop <- as.data.table(read_parquet("temperature_pop_fr.parquet"))

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


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(temperature_openmeteo_data, "temperature_openmeteo_data_fr.parquet")



