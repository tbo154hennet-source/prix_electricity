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

solar_dt <- load_batches("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs/solar_batches")
setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")
list.files()

solar_temp_dt <- load_batches("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs/solar_temp_batches")

solar_all_dt <- merge(solar_dt, solar_temp_dt, 
                      by = c("latitude", "longitude", "date"), all.x = TRUE)

setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")

df_solar_spain_big <- read.csv("df_solar_spain_big.csv")%>% 
  dplyr::select(generation_gwh_avg, efficiency, capacity_mw, latitude, longitude, name, gppd_idnr)

setDT(df_solar_spain_big)
df_solar_spain_big <- df_solar_spain_big[
  , {
    eff  <- as.numeric(efficiency)
    cap  <- as.numeric(capacity_mw)
    gen <- generation_gwh_avg
    
    ok   <- !is.na(eff) & !is.na(cap)
    
    .(
      capacity_mw = sum(cap[ok], na.rm = TRUE),
      efficiency  = if (sum(ok) > 0) weighted.mean(eff[ok], w = cap[ok]) else NA_real_,
      generation_gwh_avg = sum(gen[ok], na.rm = TRUE)
    )
  },
  by = .(latitude, longitude)
]
solar_all_dt <- merge(solar_all_dt, df_solar_spain_big, 
                      by = c("latitude", "longitude"), all.x = TRUE)


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
temperature_demand_data <- as.data.table(read_parquet("temperature_pop_all.parquet"))

gdf_demand_points <- read.csv("demand_points_corrected.csv") %>% 
  dplyr::select(c("latitude", "longitude", "factor", "comunidad"))
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




# ################## PLANT part

# Définir le répertoire de travail

setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")
full_plant <- read_parquet("temperature_plants_all.parquet")

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
  dplyr::select(-n) %>%
  ungroup() %>%
#merge all the thermal plants together
  mutate(source = ifelse(source %in% thermal_plants, "thermal", source))


#sum(is.na(plants_data$power_mw))

full_plant <- merge(full_plant, plants_data,
                    by = c("latitude", "longitude"))


# Agrégation par date
temperature_openmeteo_plant_data <- full_plant[, .(
  temperature_sth = weighted.mean(temperature_2m, power_mw, na.rm = TRUE),
  relative_humidity_2m_sth = weighted.mean(relative_humidity_2m, power_mw, na.rm = TRUE),
  precipitation_sth = weighted.mean(precipitation, power_mw, na.rm = TRUE) # pas besoin de pondérer une somme
), by = date]
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(temperature_openmeteo_plant_data, "temperature_openmeteo_plant_data.parquet")



# 
# 
# temperature_openmeteo_plant_data_monthly <- temperature_openmeteo_plant_data[, .(
#   temperature_2m = mean(temperature_2m, na.rm = TRUE),
#   relative_humidity_2m = mean(relative_humidity_2m, na.rm = TRUE),
#   precipitation = sum(precipitation, na.rm = TRUE),
#   Date = as.Date(paste0(unique(year), "-", unique(month), "-01"))
# ), by = .(year, month)]
# 

tempall_data_data <-  merge(temperature_demand_openmeteo_data, 
                            sol_all_openmeteo_data,
                            by = c("date")
)
colnames(temperature_openmeteo_plant_data)
tempall_data_data <-  merge(tempall_data_data, 
                            temperature_openmeteo_plant_data,
                            by = c("date")
)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(tempall_data_data, "tempall_data_data.parquet")




library(readxl)

setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")

temp_instrument_data <- t(read_excel('temperature_data.xlsx'))
temp_instrument_data <- as.data.frame(temp_instrument_data[c(-1,-2),])
colnames(temp_instrument_data) <- c("T_avg", "T_min", "T_max")
temp_instrument_data$Date <- rownames(temp_instrument_data)
temp_instrument_data <- temp_instrument_data %>% 
  mutate(Date = as.Date(paste0(Date,"-01"), format = "%Y-%m-%d"),
         month = month(Date),
         year = year(Date)) %>% 
  filter(year >= 2015)



temperature_by_date <- tempall_data_data %>% 
  mutate(day = as.Date(date)) %>% 
  group_by(day) %>% 
  summarise(temperature_demand = mean(temperature_demand, na.rm = TRUE),
            temperature_ss = mean(temperature_ss, na.rm = TRUE))

ggplot()+
  geom_line(data = temperature_by_date %>%  filter(day <= as.Date("2016-01-01")), aes(x = day, y =  temperature_demand, color = "Open Meteo Demand")) +
  geom_line(data = temperature_by_date %>%  filter(day <= as.Date("2016-01-01")), aes(x = day, y = temperature_ss, color = "Open Meteo Supply")) +
  geom_line(data = temp_instrument_data%>%  filter(Date <= as.Date("2016-01-01")), aes(x = Date, y = as.numeric(T_avg), color = "World Bank")) +
  theme_minimal()




#build adapted temperature instrument for spain


# 
# cold_months <- c(1, 2, 11, 12)
# hot_months <- c(6, 7, 8, 9)
# 
# #class(temp_instrument_data$T_avg)
# 
# mean_temperatures <- temperature_openmeteo_data_monthly_tot %>% 
#   group_by(month) %>% 
#   summarise(mean_temp_demand = mean(as.numeric(temperature_demand), na.rm = TRUE),
#             mean_temp_supply = mean(as.numeric(temperature_supply), na.rm = TRUE))
# 
# 
# mean_temp_instrument <- temperature_openmeteo_data_monthly_tot %>% 
#   mutate(cold_excess_instrument_demand = -ifelse(month %in% cold_months, 
#                                           as.numeric(temperature_demand) - mean_temperatures[month, 'mean_temp_demand'][[1]], 0),
#          heat_excess_instrument_demand = ifelse(month %in% hot_months,
#                                          as.numeric(temperature_demand) - mean_temperatures[month, 'mean_temp_demand'][[1]], 0),
#          cold_excess_instrument_supply = -ifelse(month %in% cold_months, 
#                                           as.numeric(temperature_supply) - mean_temperatures[month, 'mean_temp_supply'][[1]], 0),
#          heat_excess_instrument_supply = ifelse(month %in% hot_months,
#                                          as.numeric(temperature_supply) - mean_temperatures[month, 'mean_temp_supply'][[1]], 0),
#          temp_excess_instrument_demand = cold_excess_instrument_demand + heat_excess_instrument_demand,
#          temp_excess_instrument_supply = cold_excess_instrument_supply + heat_excess_instrument_supply,
#          temperature_spatial_mismatch = cold_excess_instrument_supply - cold_excess_instrument_demand + 
#            heat_excess_instrument_supply - heat_excess_instrument_demand,
#          Date = as.Date(paste0(year, "-", month, "-01"))
#          ) 
# 
# getwd()
# setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Temperature_instrument/esp')
# write_parquet(mean_temp_instrument, "temperature_instrument_opendata_meteo.parquet")
# 
# ggplot(data = mean_temp_instrument)+
#   geom_line(aes(x = Date, y = (temperature_demand - mean(temperature_demand))/10, color = "temperature", linetype = "demand")) +
#   geom_line(aes(x = Date, y = (temperature_supply- mean(temperature_supply))/10, color = "temperature", linetype = "supply")) +
#   geom_line(aes(x = Date, y = cold_excess_instrument_demand, color = "cold_excess", linetype = "demand")) +
#   geom_line(aes(x = Date, y = heat_excess_instrument_demand, color = "heat_excess", linetype = "demand")) +
#   geom_line(aes(x = Date, y = cold_excess_instrument_supply, color = "cold_excess", linetype = "supply")) +
#   geom_line(aes(x = Date, y = heat_excess_instrument_supply, color = "heat_excess", linetype = "supply")) +
#   geom_line(aes(x = Date, y = temperature_spatial_mismatch
#                   , color = "spatial_mismatch")) +
#   labs(
#     title = "Temperature Instrument",
#     x = "Time",
#     y = "Temperature (°C)"
#   ) +
#   theme_minimal()
# 
# mean_temp_instrument %>% 
#   dplyr::select(temp_excess_instrument_supply, temp_excess_instrument_demand, temperature_spatial_mismatch) %>%
#   cor(use = "pairwise.complete")
# 
# 













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



