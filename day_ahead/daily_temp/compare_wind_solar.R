
library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)
library(data.table)
library(ggplot2)


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

wind_dt <- load_batches("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs/wind_batches")
solar_dt <- load_batches("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs/solar_batches")


#il faut weight les points selon les capacity 
# Définir le répertoire de travail
setwd("Z:/prix_electricite_gitlab/day_ahead/daily_temp/outputs")
df_wind_spain_big <- read.csv("df_wind_spain_big.csv")%>% 
  select(generation_gwh_avg, efficiency, capacity_mw, latitude, longitude, name, gppd_idnr)

setDT(df_wind_spain_big)
df_wind_spain_big <- df_wind_spain_big[
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


df_solar_spain_big <- read.csv("df_solar_spain_big.csv")%>% 
  select(generation_gwh_avg, efficiency, capacity_mw, latitude, longitude, name, gppd_idnr)

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
wind_dt <- merge(wind_dt, df_wind_spain_big, 
                 by = c("latitude", "longitude"), all.x = TRUE)

solar_dt <- merge(solar_dt, df_solar_spain_big, 
                 by = c("latitude", "longitude"), all.x = TRUE)



# Charger les données
demand_data <- as.data.table(read_parquet("wind_sol_demand_data.parquet"))
# Échantillonner 100 000 lignes aléatoirement
# sample_demand <- wind_dt[sample(.N, 100000)]
# # Créer un tableau unique des coordonnées avec un identifiant
# gdf_points <- unique(sample_demand[, .(latitude, longitude)])
# gdf_points[, id := .I]
colnames(demand_data)
gdf_demand_points <- read.csv("demand_points_corrected.csv") %>% 
  select(c("latitude", "longitude", "factor", "comunidad"))
dim(gdf_demand_points)
colnames(gdf_demand_points)
# Fusionner l'ID dans full_wind
demand_data <- merge(demand_data, gdf_demand_points, by = c("latitude", "longitude"), all.x = TRUE)

demand_openmeteo_data <- demand_data[, .(
  wind_speed_100m = weighted.mean(wind_speed_100m*factor, na.rm = TRUE),
  cloud_cover = mean(cloud_cover*factor, na.rm = TRUE),
  global_tilted_irradiance = mean(global_tilted_irradiance*factor, na.rm = TRUE)
), by = date]





#Pour demand prendre les poids issus de la densité corrigée
#poids ajustés sur période de deux ans

# Agrégation par date
sol_openmeteo_data <- solar_dt[, .(
  #wind_speed_100m = mean(wind_speed_100m, na.rm = TRUE)
  cloud_cover = weighted.mean(cloud_cover,capacity_mw, na.rm = TRUE),
  global_tilted_irradiance = weighted.mean(global_tilted_irradiance,capacity_mw, na.rm = TRUE)
), by = date]
#wind_openmeteo_data_test <- head(wind_openmeteo_data, 100)
# Agrégation par date
wind_openmeteo_data <- wind_dt[, .(
  wind_speed_100m = weighted.mean(wind_speed_100m, capacity_mw, na.rm = TRUE) 
  #cloud_cover = mean(cloud_cover, na.rm = TRUE),
  #global_tilted_irradiance = mean(global_tilted_irradiance, na.rm = TRUE)
), by = date]

supply_openmeteo_data <- merge(wind_openmeteo_data, sol_openmeteo_data,
                               by = c("date")
)


colnames(demand_openmeteo_data) <- c("date", paste0(setdiff(colnames(demand_openmeteo_data), "date"), "_demand"))
colnames(supply_openmeteo_data) <- c("date", paste0(setdiff(colnames(supply_openmeteo_data), "date"), "_supply"))

wind_solar_data <-  merge(demand_openmeteo_data, 
                          supply_openmeteo_data,
                          by = c("date")
)


wind_solar_data_test <- head(wind_solar_data, 100)


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

price_indispo_merge_esp$year <- year(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$month <- month(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)
colnames(price_indispo_merge_esp)
wind_solar_data_tot <- merge(
  wind_solar_data,
  price_indispo_merge_esp %>% 
    select(day_ahead_forecast, intraday_update_renewables, unpredicted_renewables,
           wind_solar_tot, gen_tot, temperature_demand, precipitation_demand,
           relative_humidity_demand, Fore_Wind, Fore_Solar,year, month,
            weekday, creneau,datetime),
  by.x = "date", by.y = "datetime"
)

colnames(wind_solar_data_tot)

#on essaye d'expliquer la prod de renouvelables avec mes infos et on regarde le R2
#est ce que bien encapsulé

wind_solar_data_tot %>% 
  select(wind_speed_100m_supply, wind_speed_100m_demand,
           global_tilted_irradiance_supply, global_tilted_irradiance_demand) %>% 
  cor()

m_wind <- lm(data =wind_solar_data_tot,
                   Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                   global_tilted_irradiance_supply + global_tilted_irradiance_demand)

m_windc <- lm(data =wind_solar_data_tot,
                    Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                    global_tilted_irradiance_supply + global_tilted_irradiance_demand +
                      temperature_demand + precipitation_demand +
                      relative_humidity_demand)

m_windfe <- lm(data =wind_solar_data_tot,
               Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                 global_tilted_irradiance_supply + global_tilted_irradiance_demand +
                 temperature_demand + precipitation_demand +
                 relative_humidity_demand + 
                 as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
                 )

m_solar <- lm(data =wind_solar_data_tot,
                  Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                    global_tilted_irradiance_supply + global_tilted_irradiance_demand )

m_solarc <- lm(data =wind_solar_data_tot,
                   Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                     global_tilted_irradiance_supply + global_tilted_irradiance_demand +
                    temperature_demand + precipitation_demand +
                    relative_humidity_demand)

m_solarfe <- lm(data =wind_solar_data_tot,
                    Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                      global_tilted_irradiance_supply + global_tilted_irradiance_demand + +
                     temperature_demand + precipitation_demand +
                     relative_humidity_demand + 
                     as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

stargazer(
  #m_wind, 
  m_windc, m_windfe,
  #m_solar, 
  m_solarc, m_solarfe,
  type = "text",
  keep = c(
    "wind_speed_100m_supply",
    "wind_speed_100m_demand",
    "global_tilted_irradiance_supply",
    "global_tilted_irradiance_demand"
  ),
  column.labels = c("Wind",  "Wind FE",
                    "Solar",  "Solar FE"),
  dep.var.labels = c("Production"),
  title = "Regression Results for Wind and Solar Models",
  digits = 3
)

#Good result because we manage to kill the demand variable
#that supply wind speed is the only relevant
#in demand doesn't play
#same for irradiance

#alors même qu'ils sont corrélés respectivement à 90 et 98 %


