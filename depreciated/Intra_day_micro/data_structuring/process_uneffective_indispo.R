library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(ggplot2)

Sys.setenv(lang = "en_US")

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)

actual_ENTSOE_indispo <- read_parquet('non_duplicated_indispo_theoretical.parquet')

colnames(actual_ENTSOE_indispo)


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

# row <- actual_ENTSOE_indispo %>% 
#   filter(mrid == "AniRluU1-Uve8W2nqHqB9w")


#test <- uneffective_slots(row, result_list)

# Boucle optimisée avec lapply()
result_list <- lapply(seq_len(nrow(actual_ENTSOE_indispo)), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  
  tryCatch({
    uneffective_slots(actual_ENTSOE_indispo[i, ], result_list)
  }, error = function(e) {
    print(actual_ENTSOE_indispo[i, "mrid"])
    return(NULL)  # Ignorer les erreurs
  })
})

# Conversion finale en dataframe
uneffective_indispo <- do.call(bind_rows, result_list)


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)

write_parquet(uneffective_indispo, 'uneffective_indispo.parquet')



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
                   "electricity_delivery_date"  )
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
long_format_indispo <- bind_rows(result_list)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(long_format_indispo, 'long_format_indispo_uneffective.parquet')


final_uneffective_ENTSOE_indispo <- long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_publi_ENTSOE_indispo, 'final_uneffective_ENTSOE_indispo.parquet')







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
long_format_indispo_nuclear <- bind_rows(result_list)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(long_format_indispo_nuclear, 'long_format_indispo_nuclear_uneffective.parquet')


final_uneffective_nuclear_ENTSOE_indispo <- long_format_indispo_nuclear %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_uneffective_nuclear_ENTSOE_indispo, 'final_uneffective_nuclear_ENTSOE_indispo.parquet')




