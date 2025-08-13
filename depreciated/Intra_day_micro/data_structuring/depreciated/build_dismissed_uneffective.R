
save_path_entsoe <- 'Z:/prix_electricite_gitlab/Intra_day_micro/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_quat.R")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE_dismissed_unavailabilities/ES/formatted")
dismissed_unexpected_indispo_ENTSOE_data <- read_parquet("dismissed_indispo_ENTSOE_data.parquet")

colnames(dismissed_unexpected_indispo_ENTSOE_data)

result_df <- NULL

#on fait la même chose pour les dismissed unavailabilities non-publiées

counter <- 0  # Initialiser un compteur

test <- apply(dismissed_unexpected_indispo_ENTSOE_data, 1, function(row) {
  counter <<- counter + 1  # Incrémenter le compteur
  
  # Afficher un message toutes les 200 lignes
  if (counter %% 200 == 0) {
    message("Lignes traitées : ", counter)
  }
  tryCatch({
    # Appeler la fonction originale
    pub_auction_window_uneffective(row)
  }, error = function(e) {
    # Afficher un message d'erreur
    print(row["mrid"]) 
    })
  
})

dismissed_uneffective_auction_ENTSOE_indispo <- result_df %>% 
  left_join(dismissed_unexpected_indispo_ENTSOE_data,  by = "mrid")

dismissed_uneffective_auction_ENTSOE_indispo <- dismissed_uneffective_auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)

# je decide d'enlever les evene;ents de moins d'une heure
dismissed_uneffective_auction_ENTSOE_indispo <- dismissed_uneffective_auction_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

dismissed_uneffective_auction_ENTSOE_indispo <- dismissed_uneffective_auction_ENTSOE_indispo %>% 
  filter(duration >= 1)

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)
write_parquet(dismissed_uneffective_auction_ENTSOE_indispo, 'dismissed_uneffective_auction_ENTSOE_indispo.parquet')




dismissed_uneffective_non_duplicated_indispo <- dismissed_uneffective_auction_ENTSOE_indispo %>% 
  group_by(production_resource_psr_name,
           production_resource_id, electricity_delivery_date,
           auction_before, auction_after)%>%
  filter(n()==1) %>%
  ungroup()



colnames(dismissed_uneffective_non_duplicated_indispo)


final_headers <- c("mrid", "Installed.Capacity" ,"Unavailable.Capacity" , "plant_type",                  
                   "production_resource_id","production_resource_name","production_resource_psr_name",             
                   "datetime_pub", "datetime_debut" , "datetime_fin" ,                  
                   "duration",                 
                   "auction_before","auction_after","in_auction_time",                
                   "first_affec_tim_period","last_affec_tim_period",          
                   "electricity_delivery_date")
# Exclude elements "c" and "d"
headers <- final_headers[-which(final_headers %in% 
                                  c("auction_before","auction_after", "electricity_delivery_date",                
                                    "first_affec_tim_period","last_affec_tim_period"))]

dismissed_uneffective_non_duplicated_indispo <- dismissed_uneffective_non_duplicated_indispo %>%
  select(all_of(final_headers))
getwd()
write_parquet(dismissed_uneffective_non_duplicated_indispo, 'dismissed_uneffective_non_duplicated_indispo.parquet')


dismissed_uneffective_long_format_indispo <- NULL
long_format <- function(row){
  
  # Convert first_affec_tim_period and last_affec_tim_period to numeric
  first_hour <- as.numeric(row[['first_affec_tim_period']])
  last_hour <- as.numeric(row[['last_affec_tim_period']])
  
  # Create a sequence of hours between first_hour and last_hour
  hours <- seq(first_hour, last_hour)
  
  # Convert the row to a dataframe
  row_df <- as.data.frame(t(row), stringsAsFactors = FALSE)
  
  # Iterate over each hour
  for (hour in hours) {
    # Create a dataframe for the current hour
    line <- row_df %>% 
      select(all_of(final_headers)) %>% 
      mutate(creneau = hour)
    
    # Append the line to the result
    dismissed_uneffective_long_format_indispo <<- rbind(dismissed_uneffective_long_format_indispo, line)
    
    
  }
}


result_hours <- apply(dismissed_uneffective_non_duplicated_indispo, 1, long_format)

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(dismissed_uneffective_long_format_indispo, 'dismissed_uneffective_long_format_indispo.parquet')

final_dismissed_uneffective_ENTSOE_indispo <- dismissed_uneffective_long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_dismissed_uneffective_ENTSOE_indispo, 'final_dismissed_uneffective_ENTSOE_indispo.parquet')


#pareil pour le versant uniquement nucléaire

dismissed_uneffective_nuclear_non_duplicated_indispo <- dismissed_uneffective_non_duplicated_indispo %>% 
  filter(plant_type == "Nuclear")
dismissed_uneffective_nuclear_long_format_indispo <- NULL

long_format_nuclear <- function(row){
  
  # Convert first_affec_tim_period and last_affec_tim_period to numeric
  first_hour <- as.numeric(row[['first_affec_tim_period']])
  last_hour <- as.numeric(row[['last_affec_tim_period']])
  
  # Create a sequence of hours between first_hour and last_hour
  hours <- seq(first_hour, last_hour)
  
  # Convert the row to a dataframe
  row_df <- as.data.frame(t(row), stringsAsFactors = FALSE)
  
  # Iterate over each hour
  for (hour in hours) {
    # Create a dataframe for the current hour
    line <- row_df %>% 
      select(all_of(final_headers)) %>% 
      mutate(creneau = hour)
    
    # Append the line to the result
    dismissed_uneffective_nuclear_long_format_indispo <<- rbind(dismissed_uneffective_nuclear_long_format_indispo, line)
    
    
  }
}


result_hours <- apply(dismissed_uneffective_nuclear_non_duplicated_indispo, 1, long_format_nuclear)

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(dismissed_uneffective_nuclear_long_format_indispo, 'dismissed_uneffective_nuclear_long_format_indispo.parquet')

final_dismissed_uneffective_nuclear_ENTSOE_indispo <- dismissed_uneffective_nuclear_long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_dismissed_uneffective_nuclear_ENTSOE_indispo, 'final_dismissed_uneffective_nuclear_ENTSOE_indispo.parquet')


