library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

Sys.setenv(lang = "en_US")

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

actual_ENTSOE_indispo <- read_parquet('theoretical_ENTSOE_indispo.parquet') %>% 
  ungroup()

colnames(actual_ENTSOE_indispo)
# utiliser auction_before et after 2,6
# pour pouvoir retrouver lesquels sont dedoublonnes


# attention regarder l'identifiant de message potentiellement 
# des deuxiemes ou troisiemes versions
# qui ne traduisent que des ajustements d'indisponibilites deja connues

# n_bre <- nchar('REPSOLEYG------2021VI0915')
# 
# indispo_IIP_data %>% 
#   filter(substr(Message.ID, 1, n_bre)=='REPSOLEYG------2021VI0915')

# pour quelques exemples, n'a pas l'air d'etre le cas



#attention ref_day est calculé par rapport à la date de publication
# dans le cas où la date de publication est postérieure
# il faut s'assurer que ref_day est cohérent avec la date de début


#eliminer les dates de publication postérieures à la date de fin


actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  filter(end_before_pub == "Normal: publi before end of incident")



# on elimine les doublons d'evenements
# qui affectent une unite avec le meme nom
#  et affectent les memes auction
#  (ou sont le meme jour)

non_duplicated_rows <- actual_ENTSOE_indispo %>% 
  group_by(production_resource_psr_name,
           production_resource_id, electricity_delivery_date,
           auction_before, auction_after)%>%
  filter(n()==1) %>%
  ungroup()

duplicated_rows <- actual_ENTSOE_indispo %>% 
  group_by(production_resource_psr_name,
           production_resource_id, electricity_delivery_date,
           auction_before, auction_after)%>%
  filter(n() > 1) %>% 
  arrange(electricity_delivery_date, production_resource_psr_name)

# on retient le dernier pour ajustement et doublon
# same_start_date = all(difftime(start_datetime, first(start_datetime), unit= 'hours') < 1)


# et la periode d'indisponibilite est au moins 50 % la meme


# Fonction pour calculer l'intersection de deux intervalles
intersection <- function(interval1, interval2) {
  start_int <- max(int_start(interval1), int_start(interval2))
  end_int <- min(int_end(interval1), int_end(interval2))
  if (start_int < end_int) {
    return(interval(start_int, end_int))
  } else {
    return(NA)  # Pas d'intersection
  }
}

# Fonction pour calculer le pourcentage de chevauchement entre deux intervalles
chevauchement <- function(start_time_1, end_time_1, start_time_2, end_time_2) {
  interval_1 <- interval(start_time_1, end_time_1)
  interval_2 <- interval(start_time_2, end_time_2)
  
  intersected_interval <- intersection(interval_1, interval_2)
  
  if (is.na(intersected_interval)) {
    return(0)  # Pas de chevauchement
  }
  
  duration_intersect <- as.numeric(int_length(intersected_interval), units = "hours")
  duration_ini <- as.numeric(int_length(interval_1), units = "hours")
  
  return(duration_intersect / duration_ini)
}

duplicated_instances <- duplicated_rows %>%
  mutate( chevauche = chevauchement(datetime_debut, datetime_fin, 
                                    first(datetime_debut), first(datetime_fin)),
          message_dedouble = ifelse(chevauche > 0.3, 1, 0)
  ) 

# en fait les recouvrements d'intervalles partiels sont des cas si isoles
# que l'on va faire une politique brutale,
# on garde un dummy chevauchement pour traduire un parametre d'incertitude
# il est courant que les indisponibilites soient prolongees
# lorsque le probleme technique n'a pas ete regle


# keep only the latest publication
# most likely to have the correct information
high_overlap <-  duplicated_instances %>%
  filter(chevauche > 0.95) %>% 
  arrange(datetime_pub) %>% 
  slice_tail(n = 1)

# keep both since they are unapparently uncorrelated or successive
weak_overlap <-  duplicated_instances %>%
  filter(0.05 > chevauche) %>% 
  ungroup()








# un probleme mis sous le tapis
# probablement des fois ou les reacteurs sont operationnels avant l'heure donnee
# avec les lags, unknown to market participant, pas un probleme

non_duplicated_rows$chevauche <- 0
non_duplicated_rows$message_dedouble <- 0

non_duplicated_indispo <- rbind(non_duplicated_rows,
                                high_overlap,
                                weak_overlap)

colnames(non_duplicated_indispo)


final_headers <- c("mrid", "Installed.Capacity" ,"Unavailable.Capacity" , "plant_type",                  
 "production_resource_id","production_resource_name","production_resource_psr_name",             
                   "datetime_pub", "datetime_debut" , "datetime_fin" ,                  
                   "duration",                 
                   "auction_before","auction_after","in_auction_time",                
                   "first_affec_tim_period","last_affec_tim_period",          
                   "electricity_delivery_date","chevauche"  )
# Exclude elements "c" and "d"
headers <- final_headers[-which(final_headers %in% 
                                  c("auction_before","auction_after", "electricity_delivery_date",                
                                    "first_affec_tim_period","last_affec_tim_period"))]

non_duplicated_indispo <- non_duplicated_indispo %>%
  select(all_of(final_headers))

write_parquet(non_duplicated_indispo, 'non_duplicated_indispo_theoretical.parquet')


non_duplicated_indispo <- read_parquet('non_duplicated_indispo_theoretical.parquet')

# get long format for every hour of the day
# get the corresponding line with unavailability

long_format_indispo <- NULL
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
    long_format_indispo <<- rbind(long_format_indispo, line)
    
    
  }
}





# test <- non_duplicated_indispo[1:5,]
# 
# result_hours <- apply(test, 1, long_format)

result_hours <- apply(non_duplicated_indispo, 1, long_format)

long_format_indispo <- long_format_indispo %>%
  mutate(previous_day = as.character(as.Date(electricity_delivery_date)-1),
         pre_bid_time = as.POSIXct(paste(previous_day, "14:00:00"), tz = "UTC")) %>% 
  filter(auction_before != 1 | datetime_debut < pre_bid_time)


long_format_indispo_theoretical_credible <- long_format_indispo_theoretical %>% 
  mutate(lapse_pub = difftime(datetime_debut, datetime_pub, units = "hours")) %>% 
  filter(lapse_pub > -24 & lapse_pub < 24)

write_parquet(long_format_indispo_theoretical_credible, 'long_format_indispo_theoretical_credible.parquet')


#test <- long_format_indispo %>% filter(electricity_delivery_date=='2021-09-25')

# 1st_affec_tim_period","last_affec_tim_period"
# "chevauche", in auction time


long_format_indispo_credible <- long_format_indispo %>% 
  mutate(lapse_pub = difftime(datetime_debut, datetime_pub, units = "hours")) %>% 
  filter(lapse_pub > -24 & lapse_pub < 24)
write_parquet(long_format_indispo_credible, 'long_format_indispo_theoretical_credible.parquet')







non_duplicated_indispo_credible_nuclear <- non_duplicated_indispo %>% 
  filter(plant_type == "Nuclear")

long_format_indispo_nuclear <- NULL
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
    long_format_indispo_nuclear <<- rbind(long_format_indispo_nuclear, line)
    
    
  }
}

result_hours <- apply(non_duplicated_indispo_credible_nuclear, 1, long_format_nuclear)

#repérage d'une erreur dans le code
# voir intra daily schedule
#éliminer les jours tel que la publication précède 14h
nrow(long_format_indispo_nuclear)

long_format_indispo_nuclear <- long_format_indispo_nuclear %>%
  mutate(previous_day = as.character(as.Date(electricity_delivery_date)-1),
         pre_bid_time = as.POSIXct(paste(previous_day, "14:00:00"), tz = "UTC")) %>% 
  filter(auction_before != 1 | datetime_debut < pre_bid_time)

long_format_indispo_nuclear_theoretical_credible <- long_format_indispo_nuclear_theoretical %>% 
  mutate(lapse_pub = difftime(datetime_debut, datetime_pub, units = "hours")) %>% 
  filter(lapse_pub > -24 & lapse_pub < 24)

write_parquet(long_format_indispo_nuclear_theoretical_credible, 'long_format_indispo_nuclear_theoretical_credible.parquet')


write_parquet(long_format_indispo_nuclear, 'long_format_nuclear_indispo_theoretical.parquet')


final_theoretical_ENTSOE_indispo <- long_format_indispo_theoretical %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

final_theoretical_ENTSOE_nuclear_indispo <- long_format_indispo_nuclear_theoretical %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

write_parquet(final_theoretical_ENTSOE_indispo, 'final_theoretical_ENTSOE_indispo.parquet')
write_parquet(final_theoretical_ENTSOE_nuclear_indispo, 'final_theoretical_ENTSOE_nuclear_indispo.parquet')




