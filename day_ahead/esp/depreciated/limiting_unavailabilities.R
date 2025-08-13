library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)
library(data.table)


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant/esp')
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
generation_per_plant <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


# Extract the first value between single quotes
names_extracted <- gsub("^\\('([^']+)'.*", "\\1", colnames(generation_per_plant))

colnames(generation_per_plant) <- names_extracted

# Sort the columns by alphabetical order
generation_per_plant <- generation_per_plant[, order(colnames(generation_per_plant))]

# Get duplicated column names
duplicated_names <- names(generation_per_plant )[duplicated(names(generation_per_plant ))]

for (col in duplicated_names){
  # Get the indexes of the duplicated columns
  indexes <- which(colnames(generation_per_plant) == col)
  
  # Fusionner les colonnes avec le même nom en prenant les valeurs non-NA
  generation_per_plant[[indexes[1]]] <- coalesce(generation_per_plant[[indexes[1]]],
                                                 generation_per_plant[[indexes[2]]])
  # Remove the column by its index
  generation_per_plant <- generation_per_plant[, -indexes[2]]
}


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant')
write_parquet(generation_per_plant, "generation_by_plant_esp.parquet")
generation_per_plant <- read_parquet("generation_by_plant_esp.parquet")
setDT(generation_per_plant)# Convertir en data.table


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
publi_bout_indispo_ENTSOE_data <- read_parquet( "save_indispo_with_pub_status_esp.parquet") %>% 
  arrange(datetime_pub)


# Fonction pour générer les heures pleines
generate_hourly_dates <- function(start_datetime, end_datetime) {
  start_datetime <- as.POSIXct(start_datetime, tz = "Europe/Brussels")
  end_datetime <- as.POSIXct(end_datetime, tz = "Europe/Brussels")
  
  # Trouver la première et la dernière heure pleine
  start_hourly <- ceiling(as.numeric(start_datetime) / 3600) * 3600
  end_hourly <- floor(as.numeric(end_datetime) / 3600) * 3600
  
  # Générer la séquence d'heures pleines
  seq(from = as.POSIXct(start_hourly, origin = "1970-01-01", tz = "Europe/Brussels"),
      to = as.POSIXct(end_hourly, origin = "1970-01-01", tz = "Europe/Brussels"), by = "hour")
}


setDT(publi_bout_indispo_ENTSOE_data)  # Convertir en data.table

# 1. Générer toutes les dates de prédiction pour chaque indispo
# 1. GÃ©nÃ©rer toutes les dates de prÃ©diction pour chaque indispo
expanded_indispo <- publi_bout_indispo_ENTSOE_data[, .(
  datetime_slot = generate_hourly_dates(start_datetime, end_datetime),
  production_resource_name
), by = .(production_resource_name, start_datetime, end_datetime, mrid)]

# 2. Filtrer seulement les centrales qui existent dans generation_per_plant
filtered_indispo <- expanded_indispo[expanded_indispo$production_resource_name %in% colnames(generation_per_plant), ]
filtered_indispo <- filtered_indispo[, unique(names(filtered_indispo)), with = FALSE]

#setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(filtered_indispo , "filtered_indispo_esp.parquet")


# indispo_row <- filtered_indispo[1:10,]
# # 3. Effectuer le merge avec generation_per_plant
# # CrÃ©er une fonction pour faire le merge dynamique pour chaque ligne de filtered_indispo
# merge_dynamic <- function(indispo_row, index) {
#   centrale <- indispo_row$production_resource_name
#   
#   #index <- which(bout_indispo_ENTSOE_data$mrid == indispo_row$mrid)
#   if (index %% 200== 0){
#     print(index)
#   }
#   
#   
#   # VÃ©rifie si la colonne existe dans generation_per_plant
#   if (centrale %in% colnames(generation_per_plant)) {
#     
#     # RÃ©cupÃ¨re la colonne dynamique Ã  partir de generation_per_plant
#     merged_data <- merge(
#       data.table(indispo_row),
#       generation_per_plant[, .(datetime_slot = `__index_level_0__`, production = get(centrale))],
#       by = "datetime_slot",
#       allow.cartesian = TRUE
#     )
#     
#     # Renommer la colonne dynamique en 'production'
#     setnames(merged_data, "production", "production")
#     
#     return(merged_data)
#   } else {
#     return(NULL)  # Si la colonne n'existe pas, on renvoie NULL
#   }
# }
# 
# getwd()
# 
# # Appliquer la fonction à chaque ligne de filtered_indispo et combiner les résultats
# long_indispo_with_production <- rbindlist(
#   lapply(1:nrow(filtered_indispo), function(i) {
#     merge_dynamic(filtered_indispo[i, ], i)
#   }),
#   use.names = TRUE, fill = TRUE
# )
# setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
# write_parquet(long_indispo_with_production , "long_indispo_with_production.parquet")
# 
# 
# long_indispo_with_production <- read_parquet("long_indispo_with_production.parquet")
# 
# 
# colnames(long_indispo_with_production)
# colnames(publi_bout_indispo_ENTSOE_data)
# # Résultat : long_indispo_with_production avec les données fusionnées dynamiquement
# 
# long_final_indispo <- long_indispo_with_production %>%
#   left_join(publi_bout_indispo_ENTSOE_data %>% 
#               select(publication_status, mrid, start_datetime,
#                      end_datetime, plant_type, production_resource_name, 
#                      Unavailable.Capacity, Installed.Capacity)
#               , by = c("start_datetime", "end_datetime",
#                                                    "production_resource_name", "mrid"))
# getwd()
# write_parquet(long_final_indispo, "long_final_indispo.parquet")
# 




