

#on regarde une typologie d'indispo différente
#qui typiquement est plus longue
#mais dure médiane reste de 10h

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data_pre_2018.parquet") %>% 
  arrange(datetime_debut) %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"),
         publication_duration = as.numeric(datetime_pub - datetime_debut, units = "hours"))



summary(bout_indispo_ENTSOE_data$duration)
#enlève les indispos trop longues qui n'ont pas de sens

bout_indispo_ENTSOE_data <- 
  bout_indispo_ENTSOE_data[!duplicated(bout_indispo_ENTSOE_data$mrid), ] %>% 
  filter(duration > 1 & duration < 24*365)

#on regarde el temps pour annoncer
summary(bout_indispo_ENTSOE_data$publication_duration)
  
library(dplyr)

bout_indispo_ENTSOE_data <- 
  bout_indispo_ENTSOE_data[!duplicated(bout_indispo_ENTSOE_data$mrid), ] %>% 
  filter(publication_duration < 24*7)


View(bout_indispo_ENTSOE_data %>% 
       ungroup() %>% 
  slice_min(publication_duration, n = 10) %>% 
    select(production_resource_name,publication_duration, duration,
           datetime_debut, datetime_fin, datetime_pub, plant_type, Unavailable.Capacity))

View(bout_indispo_ENTSOE_data %>% 
       ungroup() %>% 
       slice_max(publication_duration, n = 10) %>% 
       select(production_resource_name,publication_duration, duration,
              datetime_debut, datetime_fin, datetime_pub, plant_type, Unavailable.Capacity))


#Mediane 11h d'indispo
#donc published/ unpublished encore une distinction pertinente
#trois modalités published/unpublished/uneffective

#day ahead market
#donc indispo peut affecter le marché du lendemain


#doubler dans le dataset les indispos qui sont unpublished
#avant de devenir published à partir du lendemain

row <- bout_indispo_ENTSOE_data[1,]

library(data.table)

# Convertir le dataframe en data.table
setDT(bout_indispo_ENTSOE_data)


# Convertir en data.table pour plus de rapidité
bout_indispo_ENTSOE_data <- as.data.table(bout_indispo_ENTSOE_data)


View(bout_indispo_ENTSOE_data %>%  
       filter( publication_duration > -5 &publication_duration < 5 & duration > 48) %>% 
       select(datetime_pub, datetime_debut, datetime_fin,mrid, publication_duration))


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


library(data.table)

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

# row <- bout_indispo_ENTSOE_data %>% 
#   filter(mrid =="-_XFGyCrYJNPOdFg_M6MNQ")
# pub_datetime <- row$datetime_pub
# start_datetime <- row$datetime_debut
# end_datetime <- row$datetime_fin


res <- bout_indispo_ENTSOE_data[, .(published_indispo_dt(datetime_pub, datetime_debut, datetime_fin, mrid)), by = .(mrid)]
res <- rbindlist(res$V1)  # Fusionner proprement en data.table

# Renommer les colonnes
setnames(res, c("V1", "V2", "V3", "V4"), c("mrid", "start_datetime", "end_datetime", "publication_status"))

# res %>%
#   filter(publication_status =="Unpublished")
# res %>%
#   filter(publication_status =="Uneffective")

publi_bout_indispo_ENTSOE_data <- merge(bout_indispo_ENTSOE_data, res, by = "mrid")
colnames(publi_bout_indispo_ENTSOE_data)
nrow(publi_bout_indispo_ENTSOE_data)
nrow(bout_indispo_ENTSOE_data)

publi_bout_indispo_ENTSOE_data <- publi_bout_indispo_ENTSOE_data %>% 
  mutate(publication_status = ifelse(businesstype== "Planned maintenance", "Planned", publication_status))

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs"
setwd(save_path_data)
write_parquet(res, "save_pub_status.parquet")
write_parquet(publi_bout_indispo_ENTSOE_data, "save_indispo_with_pub_status_esp.parquet")

#on veut retrouver un concept d'indispo limitante
#on compare la production effective moyenne sur toute la période
#avec l'indispo annoncée

#nota bene pour les indispos de longue durée
#je risque de perdre les indispos limitantes 










