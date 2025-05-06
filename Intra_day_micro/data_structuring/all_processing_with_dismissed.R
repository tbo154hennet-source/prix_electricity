

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_all_output)

actual_OMIE_all_indispo <- read_parquet('actual_OMIE_indispo_all.parquet') %>% 
  rename(production_resource_psr_name = centrale_name) %>% 
  ungroup()

actual_OMIE_all_indispo <- actual_OMIE_all_indispo %>% 
  filter(end_before_pub == "Normal: publi before end of incident")%>% 
  filter(as.numeric(first_affec_tim_period) < as.numeric(last_affec_tim_period)) %>% 
  arrange(production_resource_psr_name, electricity_delivery_date)

#attention à abono lorsqu'il faudra mergere avec le production dataset

colnames(actual_OMIE_all_indispo)
non_duplicated_indispo <- actual_OMIE_all_indispo %>% 
  group_by(production_resource_psr_name, electricity_delivery_date,
           auction_before, auction_after)%>%
  arrange(datetime_pub, datetime_debut, datetime_fin) %>%
  slice_head(n = 1) %>%
  ungroup()


non_duplicated_indispo %>% 
  group_by(plant_type) %>% 
  summarise(n = n())

getwd()
write_parquet(non_duplicated_indispo, 'non_duplicated_indispo_OMIE_all.parquet')



non_duplicated_indispo <- read_parquet('non_duplicated_indispo_OMIE_all.parquet')

# get long format for every hour of the day
# get the corresponding line with unavailability

# i <-3
# row <- non_duplicated_indispo_nuclear[i, ]

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

final_headers <- c("mrid", "Installed.Capacity" ,"Unavailable.Capacity" , "Available.Capacity",
                   "plant_type", "production_resource_psr_name",            
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
long_format_indispo <-  bind_rows(result_list) %>%    group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%     arrange(desc(datetime_pub)) %>%    slice_head(n=1)



save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo, 'long_format_indispo_OMIE_all.parquet')


final_publi_OMIE_all_indispo <- long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)), 
            Available.Capacity = sum(as.numeric(Available.Capacity))
            ) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(final_publi_OMIE_all_indispo, 'final_publi_OMIE_all_indispo.parquet')




#je sépare la production nucléaire
#où je n'ai pas le problème de active shocks
# me permet de faire des tests de robustesse


non_duplicated_indispo_nuclear <- non_duplicated_indispo %>% 
  filter(plant_type == "Nuclear")

counter <-0
result_list <- list()
# Boucle optimisée avec lapply()
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
long_format_indispo_nuclear <-  bind_rows(result_list) %>%    group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%     arrange(desc(datetime_pub)) %>%    slice_head(n=1)

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo_nuclear, 'long_format_nuclear_indispo_OMIE_all.parquet')



final_publi_nuclear_OMIE_all_indispo <- long_format_indispo_nuclear %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)),         
            Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(final_publi_nuclear_OMIE_all_indispo, 'final_publi_nuclear_OMIE_all_indispo_OMIE_all.parquet')




#theoretical and non published


save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_all_output)
actual_OMIE_all_indispo_theoretical <- read_parquet('actual_OMIE_indispo_theoretical_all.parquet') %>% 
  filter(as.numeric(first_affec_tim_period) < as.numeric(last_affec_tim_period)) %>%
  ungroup() %>% 
  rename(production_resource_psr_name = centrale_name)


write_parquet(actual_OMIE_all_indispo_theoretical, 'non_duplicated_indispo_theoretical_OMIE_all.parquet')
non_duplicated_indispo_theoretical <- read_parquet('non_duplicated_indispo_theoretical_OMIE_all.parquet')


non_duplicated_indispo_theoretical %>% 
  group_by(plant_type) %>% 
  summarise(n = n())

counter <- 0

#test <- non_duplicated_indispo_theoretical[1:10, ]
result_list <- list()
# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(non_duplicated_indispo_theoretical)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    long_format(non_duplicated_indispo_theoretical[i, ], result_list)
  }, error = function(e) {
    print(non_duplicated_indispo_theoretical[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

length(result_list)
flat_result_list <- unlist(result_list, recursive = FALSE)
long_format_indispo_theoretical <- bind_rows(flat_result_list)

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo_theoretical, 'long_format_indispo_theoretical_OMIE_all.parquet')




final_publi_OMIE_all_indispo <- long_format_indispo_theoretical %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)),             
            Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(final_publi_OMIE_all_indispo, 'final_publi_OMIE_all_indispo_theoretical.parquet')




#je sépare la production nucléaire
#où je n'ai pas le problème de active shocks
# me permet de faire des tests de robustesse


non_duplicated_indispo_nuclear_theoretical <- non_duplicated_indispo_theoretical %>% 
  filter(plant_type == "Nuclear")
result_list <- list()
result_list <- lapply(seq_len(nrow(non_duplicated_indispo_nuclear_theoretical)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    long_format(non_duplicated_indispo_nuclear_theoretical[i, ], result_list)
  }, error = function(e) {
    print(non_duplicated_indispo_nuclear_theoretical[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
long_format_indispo_nuclear_theoretical <-  bind_rows(result_list) %>%    group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%     arrange(desc(datetime_pub)) %>%    slice_head(n=1)

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo_nuclear_theoretical, 'long_format_nuclear_indispo_theoretical_OMIE_all.parquet')



final_publi_nuclear_OMIE_all_indispo <- long_format_indispo_nuclear_theoretical %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)),            
            Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(final_publi_nuclear_OMIE_all_indispo, 'final_publi_nuclear_OMIE_all_indispo_theoretical.parquet')






save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

long_format_indispo <-  read_parquet( 'long_format_indispo_OMIE_all.parquet')
long_format_indispo_nuclear <- read_parquet('long_format_nuclear_indispo_OMIE_all.parquet')

long_format_indispo_theoretical <-  read_parquet( 'long_format_indispo_theoretical_OMIE_all.parquet')
long_format_indispo_nuclear_theoretical <- read_parquet('long_format_nuclear_indispo_theoretical_OMIE_all.parquet')




library(dplyr)
long_format_indispo_non_published <- long_format_indispo_theoretical %>% 
  anti_join(long_format_indispo, by = c("production_resource_psr_name", "datetime_pub", 
                                        "datetime_debut", "datetime_fin", "auction_before", "creneau"))%>%    
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%   
  arrange(desc(datetime_pub)) %>%    
  slice_head(n=1)


long_format_indispo_pre_published <- long_format_indispo%>%
  anti_join(long_format_indispo_theoretical ,
            by = c("production_resource_psr_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))%>%    
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%   
  arrange(desc(datetime_pub)) %>%    
  slice_head(n=1)


dim(long_format_indispo_non_published)
dim(long_format_indispo_pre_published)



final_not_publi_OMIE_all_indispo <- long_format_indispo_non_published  %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)),             Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo_non_published , 'long_format_indispo_non_published_OMIE_all.parquet')


save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)

write_parquet(final_not_publi_OMIE_all_indispo, 'final_not_publi_OMIE_all_indispo.parquet')





# Définir le chemin de sauvegarde
save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)



# Charger les fichiers Parquet
long_format_nuclear_non_published <- long_format_indispo_nuclear_theoretical %>% 
  anti_join(long_format_indispo_nuclear, by = c("production_resource_psr_name", "datetime_pub", 
                                                "datetime_debut", "datetime_fin", "auction_before", "creneau")) %>%    
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%   
  arrange(desc(datetime_pub)) %>%    
  slice_head(n=1)


long_format_nuclear_pre_published <- long_format_indispo_nuclear %>%
  anti_join(long_format_indispo_nuclear_theoretical, 
            by = c("production_resource_psr_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))%>%    
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%   
  arrange(desc(datetime_pub)) %>%    
  slice_head(n=1)


# Vérifier les dimensions
dim(long_format_nuclear_non_published)
dim(long_format_nuclear_pre_published)

# Agréger les données
final_not_publi_OMIE_all_nuclear <- long_format_nuclear_non_published %>% 
  group_by(electricity_delivery_date, auction_before, auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)),            
            Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

# Sauvegarder les résultats
save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_nuclear_non_published, 'long_format_nuclear_non_published_OMIE_all.parquet')

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)

write_parquet(final_not_publi_OMIE_all_nuclear, 'final_not_publi_OMIE_all_nuclear.parquet')



save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_all_output)

non_duplicated_indispo <- non_duplicated_indispo %>%
  mutate(source_type = "actual")

non_duplicated_indispo_theoretical <- non_duplicated_indispo_theoretical %>%
  mutate(source_type = "theoretical")

non_duplicated_indispo_tot <- bind_rows(
  non_duplicated_indispo,
  non_duplicated_indispo_theoretical
) %>%
  group_by(mrid) %>%
  arrange(mrid, 
          # Prioriser "actual" si publication_date < startdate
          if_else(datetime_pub < datetime_debut, 
                  source_type == "actual", 
                  source_type == "theoretical"),
          .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

uneffective_slots <- function(row, result_list) {
  auction_after <- as.numeric(row["auction_after"])
  first_affec_time_period <- as.numeric(row["first_affec_tim_period"])
  last_affec_time_period <- as.numeric(row["last_affec_tim_period"])
  
  last_trade_time_period <- 24
  if (auction_after <= 3) {
    first_traded_time_period <- 1
  } else if (auction_after <= 4) {
    first_traded_time_period <- 5 +1
  } else if (auction_after <= 5) {
    first_traded_time_period <- 8
  } else if (auction_after <= 6) {
    first_traded_time_period <- 13+1
  } else {
    first_traded_time_period <- 21+1
  }
  
  if (first_affec_time_period > first_traded_time_period) {
    row["first_affec_tim_period"] <- first_traded_time_period
    row["last_affec_tim_period"] <- first_affec_time_period - 1
    result_list[[length(result_list) + 1]] <- row  # Ajouter à la liste
  }
  
  if (last_affec_time_period < last_trade_time_period) {
    row["first_affec_tim_period"] <- last_affec_time_period + 1
    row["last_affec_tim_period"] <- last_trade_time_period
    result_list[[length(result_list) + 1]] <- row  # Ajouter à la liste
  }
  
  return(result_list)  # Retourner la liste mise à jour
}


# Initialisation de la liste
result_list <- list()
counter <- 0

# row <- actual_OMIE_all_indispo %>% 
#   filter(mrid == "AniRluU1-Uve8W2nqHqB9w")


#test <- uneffective_slots(row, result_list)

# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(non_duplicated_indispo_tot)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    uneffective_slots(non_duplicated_indispo_tot[i, ], result_list)
  }, error = function(e) {
    print(non_duplicated_indispo_tot[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
uneffective_indispo <- do.call(bind_rows, result_list)


save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_all_output)

write_parquet(uneffective_indispo, 'uneffective_indispo_OMIE_all.parquet')


# Initialisation de la liste
result_list <- list()

# Boucle optimisée avec lapply()

counter <- 0


# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(uneffective_indispo)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    long_format(uneffective_indispo[i, ], result_list)
  }, error = function(e) {
    print(uneffective_indispo[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
long_format_indispo_uneffective <-  bind_rows(result_list) %>%    group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%     arrange(desc(datetime_pub)) %>%    slice_head(n=1)

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo_uneffective, 'long_format_indispo_uneffective_OMIE_all.parquet')


final_uneffective_OMIE_all_indispo <- long_format_indispo_uneffective %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)), 
            Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(final_uneffective_OMIE_all_indispo, 'final_uneffective_OMIE_all_indispo.parquet')



uneffective_indispo_nuclear <- uneffective_indispo %>% 
  filter(plant_type == "Nuclear")

counter <-0
result_list <- list()
# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(uneffective_indispo_nuclear)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    long_format(uneffective_indispo_nuclear[i, ], result_list)
  }, error = function(e) {
    print(uneffective_indispo_nuclear[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
long_format_indispo_nuclear_uneffective <-  bind_rows(result_list) %>%    group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>%     arrange(desc(datetime_pub)) %>%    slice_head(n=1)

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)

write_parquet(long_format_indispo_nuclear_uneffective, 'long_format_indispo_nuclear_uneffective_OMIE_all.parquet')


final_uneffective_nuclear_OMIE_all_indispo <- long_format_indispo_nuclear %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity)),           
            Available.Capacity = sum(as.numeric(Available.Capacity))) %>%
  ungroup()

save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(final_uneffective_nuclear_OMIE_all_indispo, 'final_uneffective_nuclear_OMIE_all_indispo.parquet')




#predict with xgb model




# Combine the two dataframes
all_indispo <- rbind(long_format_indispo, 
                     long_format_indispo_non_published, 
                     long_format_indispo_uneffective %>% 
                       mutate(first_affec_tim_period = as.character(first_affec_tim_period),
                              last_affec_tim_period = as.character(last_affec_tim_period)
                       )
)

#deal with ABONO again
all_indispo <- all_indispo %>% 
  mutate(production_resource_psr_name = ifelse(production_resource_psr_name == "ABOÑO 1", 
                                               "ABONO1" , production_resource_psr_name),
         production_resource_psr_name = ifelse(production_resource_psr_name == "ABO¿O 2", 
                                               "ABONO2" , production_resource_psr_name),
  ) 


centrales <- colnames(generation_per_plant %>% 
                        select(-c('__index_level_0__')))# Replace with your actual plant names
indispo_centrales <- all_indispo %>% 
  distinct(production_resource_psr_name) %>% 
  pull(production_resource_psr_name)

active_centrales <- intersect(centrales, indispo_centrales)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs")

models <- readRDS('rf_models_xgb_final.rds')
models_conservative <- readRDS('rf_models_xgb_final_without_pred_prod.rds')



rf_predictions_data <- NULL
centrale <- active_centrales[length(active_centrales)-1]
for (centrale in active_centrales){
  
  tryCatch({
    print(centrale)
    prediction_centrale <- get_predictions(centrale)%>% 
      mutate(centrale = !!centrale)
    colnames(prediction_centrale) <- c('__index_level_0__', 'month','wind_solar_tot','gen_tot',
                                       'day_of_week','creneau','previous_day_prod', 'centrale_prod', 
                                       'prediction_prod', 'prediction_prod_conservative', 'centrale')
    
    if (is.null(rf_predictions_data)){
      rf_predictions_data <- prediction_centrale 
    }else{
      rf_predictions_data <- rbind(rf_predictions_data, prediction_centrale)
    } 
  }, error = function(e) {
    print(paste("Error for", centrale))
  })
  
}


getwd()
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
write_parquet(rf_predictions_data, 'xgb_predictions_data_OMIE_all.parquet')


process_indispo_rf <- function(indispo_df, rf_predictions_data) {
  indispo_df %>%
    mutate(datetime_slot = as.POSIXct(electricity_delivery_date) +
             as.difftime(creneau - 1, units = "hours")) %>%
    merge(rf_predictions_data,
          by.y = c('__index_level_0__', 'centrale', 'creneau'),
          by.x = c('datetime_slot', 'production_resource_psr_name', 'creneau')) %>%
    rename(datetime = datetime_slot) %>%
    filter(prediction_prod_conservative > 50) %>%
    #get predicted wanted change
    mutate(predicted_real_unavailability = ifelse(
      Unavailable.Capacity > 0,
      pmin(prediction_prod - Available.Capacity, Unavailable.Capacity),
      -pmin(prediction_prod - Available.Capacity, -Unavailable.Capacity)
    )) %>%
    #check same sign
    filter(Unavailable.Capacity * predicted_real_unavailability > 0) %>%
    #only large unavailabilities
    filter(abs(predicted_real_unavailability) > 100)
}

# Add a dummy column to differentiate the sources
long_format_indispo$source <- "published"
long_format_indispo_non_published$source <- "non_published"
long_format_indispo_uneffective$source <- "uneffective"

long_format_indispo_rf <- process_indispo_rf(long_format_indispo, rf_predictions_data)%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)

long_format_indispo_non_published_rf <- process_indispo_rf(long_format_indispo_non_published, rf_predictions_data)%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)
long_format_indispo_uneffective_rf <- process_indispo_rf(long_format_indispo_uneffective, rf_predictions_data)%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)
getwd()
write_parquet(long_format_indispo_rf, 'long_format_indispo_rf_OMIE_all.parquet')
write_parquet(long_format_indispo_non_published_rf, 'long_format_indispo_non_published_rf_OMIE_all.parquet')
write_parquet(long_format_indispo_uneffective_rf, 'long_format_indispo_uneffective_rf_OMIE_all.parquet')


all_rf_indispo <- rbind(
  long_format_indispo_rf,
  long_format_indispo_non_published_rf,
  long_format_indispo_uneffective_rf %>% 
    mutate(first_affec_tim_period = as.character(first_affec_tim_period),
           last_affec_tim_period = as.character(last_affec_tim_period)))

all_rf_indispo %>% 
  group_by(Unavailable.Capacity >0) %>% 
  summarise(n = n())

ggplot()+
  geom_point(data = all_rf_indispo  %>% 
               mutate(Unavailable.Capacity = as.numeric(Unavailable.Capacity)),
             aes(x = predicted_real_unavailability, y = Unavailable.Capacity, color = source)) +
  theme_minimal()

getwd()
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
write_parquet(all_rf_indispo, 'all_rf_indispo_OMIE_all.parquet')


final_real_indispo_rf <- long_format_indispo_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()

final_not_publi_rf <- long_format_indispo_non_published_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()


final_uneffective_rf <- long_format_indispo_uneffective_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()




save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_entsoe_output)
getwd()
write_parquet(final_real_indispo_rf, 'final_real_indispo_rf_OMIE_all.parquet')
write_parquet(final_not_publi_rf, 'final_not_publi_rf_OMIE_all.parquet')
write_parquet(final_uneffective_rf, 'final_uneffective_rf_OMIE_all.parquet')


#get only one file for unavailabilities


save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)

final_publi_OMIE_all_indispo <- read_parquet('final_publi_OMIE_all_indispo.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau) 

final_publi_nuclear_OMIE_all_indispo <- read_parquet('final_publi_nuclear_OMIE_all_indispo_OMIE_all.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_real_indispo_rf <- read_parquet('final_real_indispo_rf_OMIE.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)


#unpublished indispo 
final_not_publi_OMIE_all_indispo <- read_parquet('final_not_publi_OMIE_all_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_nuclear_OMIE_all_indispo <- read_parquet('final_not_publi_OMIE_all_nuclear.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_rf <- read_parquet('final_not_publi_rf_OMIE_all.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

#uneffective_indispo
final_uneffective_OMIE_all_indispo <- read_parquet('final_uneffective_OMIE_all_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_uneffective_nuclear_OMIE_all_indispo <- read_parquet('final_uneffective_nuclear_OMIE_all_indispo.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_uneffective_rf <- read_parquet('final_uneffective_rf_OMIE_all.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)



tot_publi_OMIE_all_indispo <- final_publi_OMIE_all_indispo %>% 
  rename(Published.Unavailable.Capacity = Unavailable.Capacity)  %>%
  left_join(final_publi_nuclear_OMIE_all_indispo %>% 
              rename(Published.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity')))  %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, as.numeric(creneau)-1)
                               , format="%Y-%m-%d %H", tz = 'Europe/Brussels')) %>%
  left_join(final_real_indispo_rf %>%
              mutate(Published.Predicted.Unavailable.Capacity = predicted_real_unavailability)%>%
              select(-predicted_real_unavailability))


tot_publi_OMIE_all_indispo <- tot_publi_OMIE_all_indispo %>% 
  full_join(final_not_publi_OMIE_all_indispo %>% 
              rename(Unpublished.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_not_publi_nuclear_OMIE_all_indispo %>% 
              rename(Unpublished.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>%
  left_join(final_not_publi_rf %>%
              mutate(Unpublished.Predicted.Unavailable.Capacity = predicted_real_unavailability) %>%
              select(-predicted_real_unavailability)
  )

tot_publi_OMIE_all_indispo <- tot_publi_OMIE_all_indispo %>%
  full_join(final_uneffective_OMIE_all_indispo %>%
              rename(Uneffective.Unavailable.Capacity = Unavailable.Capacity) %>%
              select(-c('Installed.Capacity'))) %>%
  full_join(final_uneffective_nuclear_OMIE_all_indispo %>%
              rename(Uneffective.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>%
              select(-c('Installed.Capacity'))) %>%
  left_join(final_uneffective_rf %>%
              mutate(Uneffective.Predicted.Unavailable.Capacity = predicted_real_unavailability) %>%
              select(-predicted_real_unavailability))


getwd()
write_parquet(tot_publi_OMIE_all_indispo, 'tot_publi_OMIE_all_indispo.parquet')




tot_publi_OMIE_all_indispo <- read_parquet('tot_publi_OMIE_all_indispo.parquet')

save_path_OMIE_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_output)
price_diff_data <- read_parquet('price_diff_data.parquet')

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')

first_stage_data_all <- price_diff_data  %>% 
  mutate(electricity_delivery_date = as.character(delivery_date)) %>% 
  left_join(tot_publi_OMIE_all_indispo  %>%
              #some minor changes to get good join
              select(-datetime) 
            , by = c('auction_before', 'auction_after', 'creneau', "electricity_delivery_date")) %>% 
  arrange(datetime)


#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

first_stage_data_all <- first_stage_data_all%>% 
  arrange(datetime, creneau, auction_before) %>%
  left_join(renewable_forecasts, by=c('datetime' = '__index_level_0__'))

# Identify all capacity-related columns dynamically
capacity_cols <- colnames(first_stage_data_all)[grepl("Capacity", colnames(first_stage_data_all))]

# Drop rows with NA in log_price column
first_stage_data_all  <- 
  first_stage_data_all[!is.na(first_stage_data_all$log_price_diff), ]%>%
  mutate(across(all_of(capacity_cols), ~ replace_na(.x, 0))) 
#%>%  mutate(across(all_of(capacity_cols), ~ .x / gen_tot, .names = "{.col}_Share"))

getwd()

save_path_OMIE_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_output)
write_parquet(first_stage_data_all, 'first_stage_data_all_OMIE_all.parquet')









