

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_IIP_output)

actual_IIP_indispo <- read_parquet('actual_IIP_indispo.parquet') %>% 
  ungroup()

actual_IIP_indispo <- actual_IIP_indispo %>% 
  filter(end_before_pub == "Normal: publi before end of incident")%>% 
  filter(as.numeric(first_affec_tim_period) < as.numeric(last_affec_tim_period))


non_duplicated_indispo <- actual_IIP_indispo %>% 
  group_by(production_resource_name, electricity_delivery_date,
           auction_before, auction_after)%>%
  arrange(datetime_pub, datetime_debut, datetime_fin) %>%
  slice_head(n = 1) %>%
  ungroup()


non_duplicated_indispo %>% 
  group_by(plant_type) %>% 
  summarise(n = n())

getwd()
write_parquet(non_duplicated_indispo, 'non_duplicated_indispo_IIP.parquet')



non_duplicated_indispo <- read_parquet('non_duplicated_indispo_IIP.parquet')

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
                   "production_resource_name",            
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



save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo, 'long_format_indispo_IIP.parquet')


final_publi_IIP_indispo <- long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)
write_parquet(final_publi_IIP_indispo, 'final_publi_IIP_indispo.parquet')




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
long_format_indispo_nuclear <- bind_rows(result_list)

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo_nuclear, 'long_format_nuclear_indispo_IIP.parquet')



final_publi_nuclear_IIP_indispo <- long_format_indispo_nuclear %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)
write_parquet(final_publi_nuclear_IIP_indispo, 'final_publi_nuclear_IIP_indispo_IIP.parquet')




#theoretical and non published


save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_IIP_output)
actual_IIP_indispo_theoretical <- read_parquet('actual_IIP_indispo_theoretical.parquet') %>% 
  filter(as.numeric(first_affec_tim_period) < as.numeric(last_affec_tim_period)) %>%
  ungroup()


write_parquet(actual_IIP_indispo_theoretical, 'non_duplicated_indispo_theoretical_IIP.parquet')
non_duplicated_indispo_theoretical <- read_parquet('non_duplicated_indispo_theoretical_IIP.parquet')

non_duplicated_indispo_theoretical %>% 
  group_by(plant_type) %>% 
  summarise(n = n())

counter <- 0

# row <- non_duplicated_indispo[2, ]

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

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo_theoretical, 'long_format_indispo_theoretical_IIP.parquet')




final_publi_IIP_indispo <- long_format_indispo_theoretical %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)
write_parquet(final_publi_IIP_indispo, 'final_publi_IIP_indispo_theoretical.parquet')




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
long_format_indispo_nuclear_theoretical <- bind_rows(result_list)

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo_nuclear_theoretical, 'long_format_nuclear_indispo_theoretical_IIP.parquet')



final_publi_nuclear_IIP_indispo <- long_format_indispo_nuclear_theoretical %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)
write_parquet(final_publi_nuclear_IIP_indispo, 'final_publi_nuclear_IIP_indispo_theoretical.parquet')






save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

long_format_indispo <-  read_parquet( 'long_format_indispo_IIP.parquet')
long_format_indispo_nuclear <- read_parquet('long_format_nuclear_indispo_IIP.parquet')

long_format_indispo_theoretical <-  read_parquet( 'long_format_indispo_theoretical_IIP.parquet')
long_format_indispo_nuclear_theoretical <- read_parquet('long_format_nuclear_indispo_theoretical_IIP.parquet')




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



final_not_publi_IIP_indispo <- long_format_indispo_non_published  %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo_non_published , 'long_format_indispo_non_published_IIP.parquet')


save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)

write_parquet(final_not_publi_IIP_indispo, 'final_not_publi_IIP_indispo.parquet')





# Définir le chemin de sauvegarde
save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)



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
final_not_publi_IIP_nuclear <- long_format_nuclear_non_published %>% 
  group_by(electricity_delivery_date, auction_before, auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

# Sauvegarder les résultats
save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_nuclear_non_published, 'long_format_nuclear_non_published_IIP.parquet')

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)

write_parquet(final_not_publi_IIP_nuclear, 'final_not_publi_IIP_nuclear.parquet')



save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_IIP_output)

actual_IIP_indispo <- read_parquet('non_duplicated_indispo_theoretical_IIP.parquet')


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

# row <- actual_IIP_indispo %>% 
#   filter(mrid == "AniRluU1-Uve8W2nqHqB9w")


#test <- uneffective_slots(row, result_list)

# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(actual_IIP_indispo)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    uneffective_slots(actual_IIP_indispo[i, ], result_list)
  }, error = function(e) {
    print(actual_IIP_indispo[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
uneffective_indispo <- do.call(bind_rows, result_list)


save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_IIP_output)

write_parquet(uneffective_indispo, 'uneffective_indispo_IIP.parquet')

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
long_format_indispo_uneffective <- bind_rows(result_list)

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo_uneffective, 'long_format_indispo_uneffective_IIP.parquet')


final_uneffective_IIP_indispo <- long_format_indispo_uneffective %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)
write_parquet(final_publi_IIP_indispo, 'final_uneffective_IIP_indispo.parquet')



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
long_format_indispo_nuclear_uneffective <- bind_rows(result_list)

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_IIP_output)

write_parquet(long_format_indispo_nuclear_uneffective, 'long_format_indispo_nuclear_uneffective_IIP.parquet')


final_uneffective_nuclear_IIP_indispo <- long_format_indispo_nuclear %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_IIP_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_IIP_output)
write_parquet(final_uneffective_nuclear_IIP_indispo, 'final_uneffective_nuclear_IIP_indispo.parquet')





