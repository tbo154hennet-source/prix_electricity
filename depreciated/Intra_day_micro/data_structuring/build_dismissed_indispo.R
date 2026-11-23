
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

read_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/ENTSOE_dismissed_unavailabilities'
setwd(read_path )
my_files <-  list.files(pattern = "\\.parquet$")

dismissed_indispo_ENTSOE_data <- NULL

for (file in my_files){
  excel_file <- read_parquet(file)
  print(file)
  if (is.null(dismissed_indispo_ENTSOE_data)){
    dismissed_indispo_ENTSOE_data <- excel_file
  }
  else{
    dismissed_indispo_ENTSOE_data <- rbind(dismissed_indispo_ENTSOE_data, excel_file)
  }
} 

colnames(dismissed_indispo_ENTSOE_data)
dismissed_indispo_ENTSOE_data$`created_doc_time` <- 
  with_tz(dismissed_indispo_ENTSOE_data$`created_doc_time`, tzone = 'Europe/Brussels')
dismissed_indispo_ENTSOE_data$start <- 
  with_tz(dismissed_indispo_ENTSOE_data$start, tzone = 'Europe/Brussels')
dismissed_indispo_ENTSOE_data$end <- 
  with_tz(dismissed_indispo_ENTSOE_data$end, tzone = 'Europe/Brussels')


my_columns <- c("avail_qty","biddingzone_domain",  "businesstype","end" ,  "nominal_power",                       
                "mrid","nominal_power" ,"plant_type","production_resource_id",      
                "production_resource_name", "production_resource_psr_name",
                "revision", "start","created_doc_time")

colnames(dismissed_indispo_ENTSOE_data%>%
           select(my_columns) )

dismissed_indispo_ENTSOE_data <- dismissed_indispo_ENTSOE_data %>%
  select(my_columns) %>% 
  mutate(Unavailable.Capacity = nominal_power - as.numeric(avail_qty)) %>% 
  rename(datetime_pub = `created_doc_time`,
         # create debut date
         datetime_debut = start,
         # create end date
         datetime_fin = end,
         Installed.Capacity = nominal_power)

colnames(dismissed_indispo_ENTSOE_data)

my_dismissed_indispo_ENTSOE_data <- dismissed_indispo_ENTSOE_data %>% 
  group_by(businesstype, mrid, Installed.Capacity, datetime_pub,
           Unavailable.Capacity, plant_type, production_resource_id,      
           production_resource_name, production_resource_psr_name,biddingzone_domain, revision) %>% 
  summarise(datetime_debut = min(datetime_debut),
            datetime_fin = max(datetime_fin)) %>% 
  unique()


my_dismissed_indispo_ENTSOE_data$duration <- my_dismissed_indispo_ENTSOE_data$datetime_fin- my_dismissed_indispo_ENTSOE_data$datetime_debut


dismissed_unexpected_indispo_ENTSOE_data <- my_dismissed_indispo_ENTSOE_data %>% 
  filter(businesstype == "Unplanned outage") %>% 
  unique()

dismissed_unexpected_indispo_ENTSOE_data <- dismissed_unexpected_indispo_ENTSOE_data %>% 
  #exclude events which publication delay is greater than 24 hours
  mutate(lapse = difftime(datetime_debut, datetime_pub, units = "hours")) %>% 
  filter(lapse < 24 & lapse > -24 )

dismissed_unexpected_indispo_ENTSOE_data <- dismissed_unexpected_indispo_ENTSOE_data[!(dismissed_unexpected_indispo_ENTSOE_data$mrid
                                                                                       %in% dismissed_unexpected_indispo_ENTSOE_data$mrid[duplicated(dismissed_unexpected_indispo_ENTSOE_data$mrid)]), ]

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")
write_parquet(dismissed_unexpected_indispo_ENTSOE_data, "dismissed_indispo_ENTSOE_data.parquet")


result_df <- NULL

test <- apply(dismissed_unexpected_indispo_ENTSOE_data, 1, pub_auction_window)

dismissed_auction_ENTSOE_indispo <- result_df %>% 
  left_join(dismissed_unexpected_indispo_ENTSOE_data,  by = "mrid")

dismissed_actual_ENTSOE_indispo <- dismissed_auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)

# je decide d'enlever les evene;ents de moins d'une heure
dismissed_actual_ENTSOE_indispo <- dismissed_actual_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

dismissed_actual_ENTSOE_indispo <- dismissed_actual_ENTSOE_indispo %>% 
  filter(duration >= 1)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)
write_parquet(dismissed_actual_ENTSOE_indispo, 'dismissed_actual_ENTSOE_indispo.parquet')



dismissed_non_duplicated_indispo <- dismissed_actual_ENTSOE_indispo %>% 
  group_by(production_resource_psr_name,
           production_resource_id, electricity_delivery_date,
           auction_before, auction_after)%>%
  filter(n()==1) %>%
  ungroup()



colnames(dismissed_non_duplicated_indispo)


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

dismissed_non_duplicated_indispo <- dismissed_non_duplicated_indispo %>%
  select(all_of(final_headers))
getwd()
write_parquet(dismissed_non_duplicated_indispo, 'dismissed_non_duplicated_indispo.parquet')


dismissed_long_format_indispo <- NULL
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
    dismissed_long_format_indispo <<- rbind(dismissed_long_format_indispo, line)
    
    
  }
}


result_hours <- apply(dismissed_non_duplicated_indispo, 1, long_format)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(dismissed_long_format_indispo, 'dismissed_long_format_indispo.parquet')

final_dismissed_ENTSOE_indispo <- dismissed_long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_dismissed_ENTSOE_indispo, 'final_dismissed_ENTSOE_indispo.parquet')



dismissed_nuclear_non_duplicated_indispo <- dismissed_non_duplicated_indispo %>% 
  filter(plant_type == "Nuclear")
dismissed_nuclear_long_format_indispo <- NULL

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
    dismissed_nuclear_long_format_indispo <<- rbind(dismissed_nuclear_long_format_indispo, line)
    
    
  }
}


result_hours <- apply(dismissed_nuclear_non_duplicated_indispo, 1, long_format_nuclear)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(dismissed_nuclear_long_format_indispo, 'dismissed_nuclear_long_format_indispo.parquet')

final_dismissed_nuclear_ENTSOE_indispo <- dismissed_nuclear_long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_dismissed_nuclear_ENTSOE_indispo, 'final_dismissed_nuclear_ENTSOE_indispo.parquet')



