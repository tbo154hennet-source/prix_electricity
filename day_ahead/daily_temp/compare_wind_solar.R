
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

wind_dt <- load_batches("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/daily_temp/outputs/wind_batches")%>% 
  arrange(date)


solar_dt <- load_batches("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/daily_temp/outputs/solar_batches")%>% 
  arrange(date)

tail(wind_dt)

#il faut weight les points selon les capacity 
# Définir le répertoire de travail
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/daily_temp/outputs")
df_wind_spain_big <- read.csv("df_wind_spain_big.csv")%>% 
  dplyr::select(generation_gwh_avg, efficiency, capacity_mw, latitude, longitude, name, gppd_idnr)

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


wind_dt <- merge(wind_dt, df_wind_spain_big, 
                 by = c("latitude", "longitude"), all.x = TRUE)

solar_dt<- merge(solar_dt, df_solar_spain_big, 
                 by = c("latitude", "longitude"), all.x = TRUE)

# 
# head(solar_dt)
# tail(solar_dt)

# Charger les données
demand_data <- as.data.table(read_parquet("wind_sol_demand_data_all.parquet"))
# Échantillonner 100 000 lignes aléatoirement
# sample_demand <- wind_dt[sample(.N, 100000)]
# # Créer un tableau unique des coordonnées avec un identifiant
# gdf_points <- unique(sample_demand[, .(latitude, longitude)])
# gdf_points[, id := .I]
colnames(demand_data)
gdf_demand_points <- read.csv("demand_points_corrected.csv") %>% 
  dplyr::select(c("latitude", "longitude", "factor", "comunidad"))
dim(gdf_demand_points)
colnames(gdf_demand_points)
# Fusionner l'ID dans full_wind
demand_data <- merge(demand_data, gdf_demand_points, by = c("latitude", "longitude"), all.x = TRUE)

demand_openmeteo_data <- demand_data[, .(
  wind_speed_100m = weighted.mean(wind_speed_100m*factor, na.rm = TRUE),
  cloud_cover = mean(cloud_cover*factor, na.rm = TRUE),
  global_tilted_irradiance = mean(global_tilted_irradiance*factor, na.rm = TRUE)
), by = date]
colnames(demand_openmeteo_data) <- c("date", paste0(setdiff(colnames(demand_openmeteo_data), "date"), "_demand"))




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



colnames(supply_openmeteo_data) <- c("date", paste0(setdiff(colnames(supply_openmeteo_data), "date"), "_supply"))

wind_solar_data <-  merge(demand_openmeteo_data, 
                          supply_openmeteo_data,
                          by = c("date")
)

tail(wind_solar_data, 10)
wind_solar_data_test <- tail(wind_solar_data, 100)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(wind_solar_data, "wind_solar_data.parquet")





