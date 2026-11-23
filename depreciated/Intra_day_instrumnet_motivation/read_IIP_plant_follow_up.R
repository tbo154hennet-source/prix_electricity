library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

Sys.setenv(lang = "en_US")

save_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'

setwd(save_path)
pure_indispo_data <- read_parquet('pure_indispo_data.parquet')

colnames(pure_indispo_data)
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






# a focus on short term temporary unavailabilities
pure_short_indispo <- pure_indispo_data %>% 
  filter(term=='short')

# je decide d'enlever les evene;ents de moins d'une heure
pure_short_indispo <- pure_short_indispo %>% 
  filter(duration >= 1/24)



# on elimine les doublons d'evenements
# qui affectent une unite avec le meme nom
#  et affectent les memes auction
#  (ou sont le meme jour)

non_duplicated_rows <- pure_short_indispo %>% 
  group_by(Affected.Asset.or.Unit,
           Affected.Asset.or.Unit.EIC.code, electricity_delivery_date)%>%
  filter(n()==1) %>%
  ungroup()

duplicated_rows <- pure_short_indispo %>% 
  group_by(Affected.Asset.or.Unit,
           Affected.Asset.or.Unit.EIC.code, electricity_delivery_date)%>%
  filter(n() > 1)

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

# keep both since they are unapparently uncorrelated or succesive
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

final_headers <- c("Message.ID" ,                   
    "Unavailable.Capacity", "Installed.Capacity", "Fuel.Type",                      
   "Affected.Asset.or.Unit", "Affected.Asset.or.Unit.EIC.code",
     "Market.Participant.code", "Market.Participant",             
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

write_parquet(non_duplicated_indispo, 'non_duplicated_indispo.parquet')


non_duplicated_indispo <- read_parquet('non_duplicated_indispo.parquet')

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

write_parquet(long_format_indispo, 'long_format_indispo.parquet')

#test <- long_format_indispo %>% filter(electricity_delivery_date=='2021-09-25')

# 1st_affec_tim_period","last_affec_tim_period"
# "chevauche", in auction time

final_indispo <- long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

#test <- final_indispo %>% filter(electricity_delivery_date=='2021-09-25')

write_parquet(final_indispo, 'final_indispo.parquet')





