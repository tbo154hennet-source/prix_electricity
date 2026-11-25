
library(data.table)

#setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "data/entsoe/Unavailabilities_plants/formatted_OMIE"))
setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "Intra_day_micro/outputs/formatted_OMIE"))
#all_matched_indispo <- read_parquet("all_matched_indispo.parquet")

all_matched_indispo <- read_parquet("all_matched_indispo.parquet") %>% 
  arrange(datetime_debut) %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"),
         publication_duration = as.numeric(datetime_pub - datetime_debut, units = "hours"))

colnames(all_matched_indispo)
all_matched_indispo <- 
  all_matched_indispo[!duplicated(all_matched_indispo$mrid), ] %>% 
  filter(duration > 1 & duration < 24*365)

all_matched_indispo <- 
  all_matched_indispo[!duplicated(all_matched_indispo$mrid), ] %>% 
  filter(publication_duration < 24*7)

# Convertir le dataframe en data.table
setDT(all_matched_indispo)

# Convertir en data.table pour plus de rapidité
all_matched_indispo <- as.data.table(all_matched_indispo)



# Fonction pour trouver le prochain midi après une date donnée
next_noon <- function(dt) {
  #dt <- as.POSIXct(dt, tz = "Europe/Brussels")  # Assurez-vous que dt est bien en POSIXct
  noon_today <- as.POSIXct(format(dt, "%Y-%m-%d 12:00:00"), tz = "Europe/Brussels")
  
  # Si l'heure actuelle est déjà après midi, prendre midi du jour suivant
  if (dt >= noon_today) {
    noon_today <- noon_today + days(1)
  }
  
  return(noon_today)
}


published_indispo_dt <- function(pub_datetime, start_datetime, end_datetime, mrid) {
  
  pub_datetime <- as.POSIXct(pub_datetime, tz = 'Europe/Brussels')
  start_datetime <- as.POSIXct(start_datetime, tz = 'Europe/Brussels')
  end_datetime <- as.POSIXct(end_datetime, tz = 'Europe/Brussels')
  
  ref_datetime <- next_noon(pub_datetime)
  date_pub <- format(ref_datetime, "%Y-%m-%d")
  date_obj <- as.Date(date_pub)
  end_time_effect <- as.POSIXct(paste(date_obj + 1, "00:00:00"), tz = 'Europe/Brussels')
  
  result <- list()
  
  if (end_datetime < end_time_effect) {
    result <- list(list(mrid, start_datetime, end_datetime, "Uneffective"))
  } else {
    #if (start_datetime <= pub_datetime) {
    if (start_datetime <= end_time_effect) {
      result <- list(
        list(mrid, start_datetime, end_time_effect, "Unpublished"),
        list(mrid, end_time_effect, end_datetime, "Published")
      )
      # } else if (start_datetime <= end_time_effect) {
      #   result <- list(
      #     list(mrid, start_datetime, end_time_effect, "Unpublished"),
      #     list(mrid, end_time_effect, end_datetime, "Published")
      #   )
    } else {
      result <- list(list(mrid, start_datetime, end_datetime, "Published"))
    }
  }
  
  return(result)
}

# row <- all_matched_indispo %>%
#   filter(mrid =="-_XFGyCrYJNPOdFg_M6MNQ")
# row <- all_matched_indispo[1,]
# pub_datetime <- row$datetime_pub
# start_datetime <- row$datetime_debut
# end_datetime <- row$datetime_fin
# res <- row[, .(published_indispo_dt(datetime_pub, datetime_debut, datetime_fin, mrid)), by = .(mrid)]

res <- all_matched_indispo[, .(published_indispo_dt(datetime_pub, datetime_debut, datetime_fin, mrid)), by = .(mrid)]
res <- rbindlist(res$V1)  # Fusionner proprement en data.table


# Renommer les colonnes
setnames(res, c("V1", "V2", "V3", "V4"), c("mrid", "start_datetime", "end_datetime", "publication_status"))

# res %>%
#   filter(publication_status =="Unpublished")
# res %>%
#   filter(publication_status =="Uneffective")

publi_all_matched_indispo <- merge(all_matched_indispo, res, by = "mrid")
colnames(publi_all_matched_indispo)
nrow(publi_all_matched_indispo)
nrow(all_matched_indispo)

# publi_all_matched_indispo <- publi_all_matched_indispo %>% 
#   mutate(publication_status = ifelse(businesstype== "Planned maintenance", "Planned", publication_status))


save_path_data <- file.path(Sys.getenv("REPLICATION_ROOT", "."), "day_ahead/outputs/esp")
setwd(save_path_data)
#write_parquet(res, "save_pub_status.parquet")
write_parquet(publi_all_matched_indispo, "day_ahead_indispo_formatted.parquet")








# 
# setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "data/entsoe/Generation_by_plant/esp"))
# # Lister tous les fichiers dans le dossier avec l'extension .parquet
# parquet_files <- list.files(pattern = "\\.parquet$")
# 
# # Lire tous les fichiers parquet et les fusionner dans un seul dataframe
# generation_per_plant <- parquet_files %>%
#   lapply(read_parquet) %>% # Lire chaque fichier parquet
#   bind_rows() %>%  # Fusionner tous les dataframes
#   arrange(`__index_level_0__`)
# 
# 
# # Extract the first value between single quotes
# names_extracted <- gsub("^\\('([^']+)'.*", "\\1", colnames(generation_per_plant))
# 
# colnames(generation_per_plant) <- names_extracted
# 
# # Sort the columns by alphabetical order
# generation_per_plant <- generation_per_plant[, order(colnames(generation_per_plant))]
# 
# # Get duplicated column names
# duplicated_names <- names(generation_per_plant )[duplicated(names(generation_per_plant ))]
# 
# for (col in duplicated_names){
#   # Get the indexes of the duplicated columns
#   indexes <- which(colnames(generation_per_plant) == col)
#   
#   # Fusionner les colonnes avec le même nom en prenant les valeurs non-NA
#   generation_per_plant[[indexes[1]]] <- coalesce(generation_per_plant[[indexes[1]]],
#                                                  generation_per_plant[[indexes[2]]])
#   # Remove the column by its index
#   generation_per_plant <- generation_per_plant[, -indexes[2]]
# }
# 
# 
# setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "Intra_day_micro/outputs/formatted_OMIE"))
# write_parquet(generation_per_plant, "generation_by_plant_esp.parquet")






#setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "data/entsoe/Generation_by_plant"))
setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "Intra_day_micro/outputs/formatted_OMIE"))
#write_parquet(generation_per_plant, "generation_by_plant_esp.parquet")
generation_per_plant <- read_parquet("generation_by_plant_esp.parquet")
setDT(generation_per_plant)# Convertir en data.table

generation_per_plant <- generation_per_plant %>% 
  mutate(`ABOÑO 1`= ifelse(is.na(`ABOÑO 1`),`ABO¿O 1` , `ABOÑO 1`),
         `ABOÑO 2` = ifelse(is.na(`ABOÑO 2`),`ABO¿O 2` , `ABOÑO 2`)
                           )



table <- read.csv( "abbreviations.csv")

centrale_matching <- read.csv("centrale_matching_plant.csv")
colnames(centrale_matching) <- c("centrale_name","psr_resource_name",
                                 "plant_type",  "Installed.Capacity")

matching_table <- merge(table, centrale_matching,
                        by = "psr_resource_name", all.x = TRUE) %>% 
  filter(!is.na(centrale_name) & !is.na(Installed.Capacity)) %>% 
  filter(centrale_name!= "ABO¿O 2" & centrale_name!="ABO¿O 1") %>% 
  distinct(psr_resource_name, centrale_name, .keep_all = TRUE) %>% 
  rename(production_resource_name = centrale_name)
# sum(duplicated(matching_table$psr_resource_name))
# matching_table[duplicated(matching_table$psr_resource_name),]



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


setwd(save_path_data)
#write_parquet(res, "save_pub_status.parquet")
publi_all_matched_indispo <- read_parquet( "day_ahead_indispo_formatted.parquet")
publi_all_matched_indispo  <- publi_all_matched_indispo  %>%
  distinct(psr_resource_name, centrale_name, datetime_pub,
           start_datetime, end_datetime, .keep_all = TRUE)


colnames(publi_all_matched_indispo)
setDT(publi_all_matched_indispo)  # Convertir en data.table

publi_all_matched_indispo <- merge(publi_all_matched_indispo, matching_table[3:nrow(matching_table),] %>% #enlever la double possibilité pour ABONO 
                                     dplyr::select(psr_resource_name, production_resource_name) %>% distinct(),
                          by = "psr_resource_name")



# 1. Générer toutes les dates de prédiction pour chaque indispo
# 1. GÃƒÂ©nÃƒÂ©rer toutes les dates de prÃƒÂ©diction pour chaque indispo
expanded_indispo <- publi_all_matched_indispo[, .(
  datetime_slot = generate_hourly_dates(start_datetime, end_datetime)
), by = .(production_resource_name, start_datetime, end_datetime, mrid)]



colnames(publi_all_matched_indispo)
#colnames(expanded_indispo)


# 2. Filtrer seulement les centrales qui existent dans generation_per_plant
filtered_indispo <- expanded_indispo[expanded_indispo$production_resource_name %in% colnames(generation_per_plant), ]
filtered_indispo <- filtered_indispo[, unique(names(filtered_indispo)), with = FALSE]


setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "day_ahead/outputs/esp"))
write_parquet(filtered_indispo , "filtered_indispo_esp_OMIE_2026.parquet")

