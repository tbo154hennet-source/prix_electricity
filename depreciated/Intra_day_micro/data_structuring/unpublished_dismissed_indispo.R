


library(arrow)
library(purrr)
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
Sys.setenv(lang = "en_US")

save_path_entsoe <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_ter.R")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")
dismissed_unexpected_indispo_ENTSOE_data <- read_parquet("dismissed_indispo_ENTSOE_data.parquet")

result_df <- NULL

#on fait la même chose pour les dismissed unavailabilities non-publiées

test <- apply(dismissed_unexpected_indispo_ENTSOE_data, 1, pub_auction_window_theoretical)

dismissed_theoretical_auction_ENTSOE_indispo <- result_df %>% 
  left_join(dismissed_unexpected_indispo_ENTSOE_data,  by = "mrid")

dismissed_theoretical_auction_ENTSOE_indispo <- dismissed_theoretical_auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)

# je decide d'enlever les evene;ents de moins d'une heure
dismissed_theoretical_auction_ENTSOE_indispo <- dismissed_theoretical_auction_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

dismissed_theoretical_auction_ENTSOE_indispo <- dismissed_theoretical_auction_ENTSOE_indispo %>% 
  filter(duration >= 1)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)
write_parquet(dismissed_theoretical_auction_ENTSOE_indispo, 'dismissed_theoretical_auction_ENTSOE_indispo.parquet')



dismissed_theoretical_non_duplicated_indispo <- dismissed_theoretical_auction_ENTSOE_indispo %>% 
  group_by(production_resource_psr_name,
           production_resource_id, electricity_delivery_date,
           auction_before, auction_after)%>%
  filter(n()==1) %>%
  ungroup()



colnames(dismissed_theoretical_non_duplicated_indispo)


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

dismissed_theoretical_non_duplicated_indispo <- dismissed_theoretical_non_duplicated_indispo %>%
  select(all_of(final_headers))
getwd()
write_parquet(dismissed_theoretical_non_duplicated_indispo, 'dismissed_theoretical_non_duplicated_indispo.parquet')


dismissed_theoretical_long_format_indispo <- NULL
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
    dismissed_theoretical_long_format_indispo <<- rbind(dismissed_theoretical_long_format_indispo, line)
    
    
  }
}


result_hours <- apply(dismissed_theoretical_non_duplicated_indispo, 1, long_format)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(dismissed_theoretical_long_format_indispo, 'dismissed_theoretical_long_format_indispo.parquet')

final_dismissed_theoretical_ENTSOE_indispo <- dismissed_theoretical_long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_dismissed_theoretical_ENTSOE_indispo, 'final_dismissed_theoretical_ENTSOE_indispo.parquet')


#pareil pour le versant uniquement nucléaire

dismissed_theoretical_nuclear_non_duplicated_indispo <- dismissed_theoretical_non_duplicated_indispo %>% 
  filter(plant_type == "Nuclear")
dismissed_theoretical_nuclear_long_format_indispo <- NULL

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
    dismissed_theoretical_nuclear_long_format_indispo <<- rbind(dismissed_theoretical_nuclear_long_format_indispo, line)
    
    
  }
}


result_hours <- apply(dismissed_theoretical_nuclear_non_duplicated_indispo, 1, long_format_nuclear)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(dismissed_theoretical_nuclear_long_format_indispo, 'dismissed_theoretical_nuclear_long_format_indispo.parquet')

final_dismissed_theoretical_nuclear_ENTSOE_indispo <- dismissed_theoretical_nuclear_long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_dismissed_theoretical_nuclear_ENTSOE_indispo, 'final_dismissed_theoretical_nuclear_ENTSOE_indispo.parquet')


#get unpublished grandeurs

library(dplyr)
dismissed_long_format_indispo_non_published <- dismissed_theoretical_long_format_indispo %>% 
  anti_join(dismissed_long_format_indispo, by = c("production_resource_name", "datetime_pub", 
                                        "datetime_debut", "datetime_fin", "auction_before", "creneau"))

dismissed_long_format_indispo_pre_published <- dismissed_long_format_indispo%>%
  anti_join(dismissed_theoretical_long_format_indispo ,
            by = c("production_resource_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))



final_not_publi_dismissed_ENTSOE_indispo <- dismissed_long_format_indispo_non_published  %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(dismissed_long_format_indispo_non_published , 'dismissed_long_format_indispo_non_published.parquet')


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)

write_parquet(final_not_publi_dismissed_ENTSOE_indispo, 'final_not_publi_dismissed_ENTSOE_indispo.parquet')


#same for nuclear

library(dplyr)
dismissed_nuclear_long_format_indispo_non_published <- dismissed_theoretical_nuclear_long_format_indispo %>% 
  anti_join(dismissed_nuclear_long_format_indispo, by = c("production_resource_name", "datetime_pub", 
                                                  "datetime_debut", "datetime_fin", "auction_before", "creneau"))

dismissed_nuclear_long_format_indispo_pre_published <- dismissed_nuclear_long_format_indispo%>%
  anti_join(dismissed_theoretical_nuclear_long_format_indispo ,
            by = c("production_resource_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))



final_not_publi_dismissed_nuclear_ENTSOE_indispo <- dismissed_nuclear_long_format_indispo_non_published  %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)

write_parquet(dismissed_nuclear_long_format_indispo_non_published , 'dismissed_nuclear_long_format_indispo_non_published.parquet')


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)

write_parquet(final_not_publi_dismissed_nuclear_ENTSOE_indispo, 'final_not_publi_dismissed_nuclear_ENTSOE_indispo.parquet')

# get an overview of the number of unavailabilities
#that we are talking about

nrow(dismissed_long_format_indispo_non_published)
nrow(dismissed_long_format_indispo)

nrow(dismissed_nuclear_long_format_indispo_non_published)
nrow(dismissed_nuclear_long_format_indispo)

