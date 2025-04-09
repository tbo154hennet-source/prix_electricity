library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(ggplot2)

Sys.setenv(lang = "en_US")

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)

actual_ENTSOE_indispo <- read_parquet('actual_ENTSOE_indispo_theoretical.parquet') %>% 
  filter(as.numeric(first_affec_tim_period) < as.numeric(last_affec_tim_period)) %>%
  ungroup()



write_parquet(actual_ENTSOE_indispo, 'non_duplicated_indispo_theoretical.parquet')



non_duplicated_indispo <- read_parquet('non_duplicated_indispo_theoretical.parquet')

# get long format for every hour of the day
# get the corresponding line with unavailability

long_format <- function(row, result_list) {
  
  # Convert first_affec_tim_period and last_affec_tim_period to numeric
  first_hour <- as.numeric(row[['first_affec_tim_period']])
  last_hour <- as.numeric(row[['last_affec_tim_period']])
  
  # Create a sequence of hours between first_hour and last_hour
  hours <- seq(first_hour, last_hour)
  
  # Convert the row to a dataframe
  row_df <- as.data.frame(row, stringsAsFactors = FALSE)
  
  # Stocker les nouvelles lignes dans une sous-liste temporaire
  temp_list <- vector("list", length(hours))
  
  # Créer les nouvelles lignes
  for (i in seq_along(hours)) {
    temp_list[[i]] <- row_df %>%
      select(all_of(final_headers)) %>%
      mutate(creneau = hours[i])
  }
  
  # Ajouter les résultats à la liste principale
  result_list[[length(result_list) + 1]] <- bind_rows(temp_list)
  
  return(result_list)  # Retourner la liste mise à jour
}

final_headers <- c("mrid", "Installed.Capacity" ,"Unavailable.Capacity" , "plant_type",                  
                   "production_resource_id","production_resource_name","production_resource_psr_name",             
                   "datetime_pub", "datetime_debut" , "datetime_fin" ,                  
                   "duration",                 
                   "auction_before","auction_after","in_auction_time",                
                   "first_affec_tim_period","last_affec_tim_period",          
                   "electricity_delivery_date")
# Initialisation de la liste
result_list <- list()

# Boucle optimisée avec lapply()

counter <- 0

# row <- non_duplicated_indispo[2, ]

# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(non_duplicated_indispo)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    long_format(non_duplicated_indispo[i, ], result_list)
  }, error = function(e) {
    print(non_duplicated_indispo[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
long_format_indispo <- bind_rows(result_list)



save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(long_format_indispo, 'long_format_indispo_theoretical.parquet')



# save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
# setwd(save_path_entsoe_output)
# 
# long_format_indispo <- read_parquet('long_format_indispo_theoretical.parquet')
#test <- long_format_indispo %>% filter(electricity_delivery_date=='2021-09-25')

long_format_indispo %>% 
  group_by(datetime_pub > datetime_fin) %>% 
  summarise(n = n())

long_format_indispo %>% 
  group_by(datetime_pub > datetime_debut) %>% 
  summarise(n = n())

# on se propose d'enlever les indispos pour lesquelles les date de publi et de debut sont
#vraiment trop éloignées
# a savoir plus de 1 jour



final_publi_ENTSOE_indispo <- long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_publi_ENTSOE_indispo, 'final_publi_ENTSOE_indispo_theoretical.parquet')




#je sépare la production nucléaire
#où je n'ai pas le problème de active shocks
# me permet de faire des tests de robustesse


non_duplicated_indispo_nuclear <- non_duplicated_indispo %>% 
  filter(plant_type == "Nuclear")
result_list <- list()
result_list <- lapply(seq_len(nrow(non_duplicated_indispo_nuclear)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    long_format(non_duplicated_indispo_nuclear[i, ], result_list)
  }, error = function(e) {
    print(non_duplicated_indispo_nuclear[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
long_format_indispo_nuclear <- bind_rows(result_list)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(long_format_indispo_nuclear, 'long_format_nuclear_indispo_theoretical.parquet')



final_publi_nuclear_ENTSOE_indispo <- long_format_indispo_nuclear %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_publi_nuclear_ENTSOE_indispo, 'final_publi_nuclear_ENTSOE_indispo_theoretical.parquet')




save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

long_format_indispo <-  read_parquet( 'long_format_indispo.parquet')
long_format_indispo_nuclear <- read_parquet('long_format_nuclear_indispo.parquet')

long_format_indispo_theoretical <-  read_parquet( 'long_format_indispo_theoretical.parquet')
long_format_indispo_nuclear_theoretical <- read_parquet('long_format_nuclear_indispo_theoretical.parquet')




library(dplyr)
long_format_indispo_non_published <- long_format_indispo_theoretical %>% 
  anti_join(long_format_indispo, by = c("production_resource_name", "datetime_pub", 
                                                  "datetime_debut", "datetime_fin", "auction_before", "creneau"))

long_format_indispo_pre_published <- long_format_indispo%>%
  anti_join(long_format_indispo_theoretical ,
            by = c("production_resource_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))

dim(long_format_indispo_non_published)
dim(long_format_indispo_pre_published)



final_not_publi_ENTSOE_indispo <- long_format_indispo_non_published  %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(long_format_indispo_non_published , 'long_format_indispo_non_published.parquet')


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)

write_parquet(final_not_publi_ENTSOE_indispo, 'final_not_publi_ENTSOE_indispo.parquet')




# Définir le chemin de sauvegarde
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

# Charger les fichiers Parquet
long_format_nuclear_non_published <- long_format_indispo_nuclear_theoretical %>% 
  anti_join(long_format_indispo_nuclear, by = c("production_resource_name", "datetime_pub", 
                                                "datetime_debut", "datetime_fin", "auction_before", "creneau"))

long_format_nuclear_pre_published <- long_format_indispo_nuclear %>%
  anti_join(long_format_indispo_nuclear_theoretical, 
            by = c("production_resource_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))

# Vérifier les dimensions
dim(long_format_nuclear_non_published)
dim(long_format_nuclear_pre_published)

# Agréger les données
final_not_publi_ENTSOE_nuclear <- long_format_nuclear_non_published %>% 
  group_by(electricity_delivery_date, auction_before, auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

# Sauvegarder les résultats
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(long_format_nuclear_non_published, 'long_format_nuclear_non_published.parquet')

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)

write_parquet(final_not_publi_ENTSOE_nuclear, 'final_not_publi_ENTSOE_nuclear.parquet')



