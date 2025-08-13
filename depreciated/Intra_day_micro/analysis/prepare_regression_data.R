
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(tidyr)

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)


final_publi_ENTSOE_indispo <- read_parquet('final_publi_ENTSOE_indispo.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau) 

final_publi_nuclear_ENTSOE_indispo <- read_parquet('final_publi_nuclear_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

#unpublished indispo 
final_not_publi_ENTSOE_indispo <- read_parquet('final_not_publi_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_nuclear_ENTSOE_indispo <- read_parquet('final_not_publi_ENTSOE_nuclear_indispo.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

#uneffective_indispo
final_uneffective_ENTSOE_indispo <- read_parquet('final_publi_ENTSOE_indispo_uneffective.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_uneffective_nuclear_ENTSOE_indispo <- read_parquet('final_publi_nuclear_ENTSOE_indispo_uneffective.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)




#dismissed indispo

final_dismissed_ENTSOE_indispo<- read_parquet('final_dismissed_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_dismissed_ENTSOE_indispo<- read_parquet('final_not_publi_dismissed_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_dismissed_uneffective_ENTSOE_indispo<- read_parquet("final_dismissed_uneffective_ENTSOE_indispo.parquet") %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

#dismissed nuclear indispo

final_dismissed_nuclear_ENTSOE_indispo<- read_parquet('final_dismissed_nuclear_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_dismissed_nuclear_ENTSOE_indispo<- read_parquet('final_not_publi_dismissed_nuclear_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)


final_dismissed_nuclear_uneffective_ENTSOE_indispo<- read_parquet("final_dismissed_uneffective_nuclear_ENTSOE_indispo.parquet") %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)





# real compared with random forest indispo



save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)

final_real_indispo_rf <- read_parquet('final_real_indispo_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_rf <- read_parquet('final_not_publi_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_uneffective_rf <- read_parquet('final_uneffective_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

# real compared with random forest indispo
# with dismissed indispo

final_real_indispo_rf_dismissed <- read_parquet('final_dismissed_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau) 

final_dismissed_not_publi_rf <- read_parquet('final_dismissed_not_publi_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau) 

final_dismissed_uneffective_rf <- read_parquet('final_dismissed_uneffective_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

#marginal limit

# final_real_indispo_limitantes <- read_parquet('final_real_indispo_limitantes.parquet') %>%
#   mutate(creneau = as.character(creneau)) %>%
#   arrange(electricity_delivery_date, auction_before, creneau) 
# 
# final_real_theoretical_indispo_limitantes <- read_parquet('final_real_theoretical_indispo_limitantes.parquet') %>%
#   mutate(creneau = as.character(creneau)) %>%
#   arrange(electricity_delivery_date, auction_before, creneau)
#rajouter les uneffective à partir des day-ahead


# final_indispo_post <- final_uneffective_ENTSOE_indispo %>% 
#   rename(Uneffective.Unavailable.Capacity = Unavailable.Capacity) %>% 
#   select(-c('Installed.Capacity')) %>% 
#   full_join(final_uneffective_nuclear_ENTSOE_indispo %>% 
#               rename(Uneffective.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
#               select(-c('Installed.Capacity')))%>% 
#   left_join(final_uneffective_rf %>%
#               mutate(Uneffective.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
#               select(-predicted_real_unavailability))




tot_publi_ENTSOE_indispo <- final_publi_ENTSOE_indispo %>% 
  rename(Published.Unavailable.Capacity = Unavailable.Capacity)  %>%
  left_join(final_publi_nuclear_ENTSOE_indispo %>% 
              rename(Published.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity')))  %>% 
 mutate(datetime = as.POSIXct(paste(electricity_delivery_date, as.numeric(creneau)-1)
         , format="%Y-%m-%d %H", tz = 'Europe/Brussels')) %>% 
  left_join(final_real_indispo_rf %>% 
              mutate(Published.Predicted.Unavailable.Capacity = -predicted_real_unavailability)%>% 
              select(-predicted_real_unavailability))


tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  full_join(final_not_publi_ENTSOE_indispo %>% 
              rename(Unpublished.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_not_publi_nuclear_ENTSOE_indispo %>% 
              rename(Unpublished.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  left_join(final_not_publi_rf %>%
              mutate(Unpublished.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
              select(-predicted_real_unavailability)
  )

tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>%
  full_join(final_uneffective_ENTSOE_indispo %>%
              rename(Uneffective.Unavailable.Capacity = Unavailable.Capacity) %>%
              select(-c('Installed.Capacity'))) %>%
  full_join(final_uneffective_nuclear_ENTSOE_indispo %>%
              rename(Uneffective.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>%
              select(-c('Installed.Capacity'))) %>%
  left_join(final_uneffective_rf %>%
              mutate(Uneffective.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
              select(-predicted_real_unavailability))




setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
final_indispo_tot_wide_esp_da <- read_parquet("final_indispo_tot_wide_esp.parquet")%>%
  mutate(creneau = as.character(hour(datetime_slot) + 1),
         electricity_delivery_date = as.character(as.Date(datetime_slot)),
         auction_before = "0",
         auction_after = "1",
         Uneffective.Unavailable.Capacity = Published.Unavailable.Capacity +Unpublished.Unavailable.Capacity,
         Uneffective.Unavailable.Nuclear.Capacity = Published.Unavailable.Nuclear.Capacity +Unpublished.Unavailable.Nuclear.Capacity,
         Uneffective.Predicted.Unavailable.Capacity = Published.Predicted.Unavailable.Capacity +Unpublished.Predicted.Unavailable.Capacity
  ) %>%
  arrange(datetime_slot)



tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  full_join(final_indispo_tot_wide_esp_da %>% 
              filter(Uneffective.Unavailable.Capacity >0 | 
      Uneffective.Unavailable.Nuclear.Capacity >0 | Uneffective.Predicted.Unavailable.Capacity >0) %>% 
        select(Uneffective.Unavailable.Capacity, Uneffective.Unavailable.Nuclear.Capacity ,
               Uneffective.Predicted.Unavailable.Capacity, electricity_delivery_date, creneau, 
               auction_before, auction_after)) 
  
  
# %>% 
#   left_join(final_real_indispo_limitantes %>% 
#             rename(Limiting.Unavailable.Capacity = Unavailable.Capacity))


tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  full_join(final_dismissed_ENTSOE_indispo %>% 
              rename(Published.Dismissed.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_not_publi_dismissed_ENTSOE_indispo %>% 
              rename(Unpublished.Dismissed.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_dismissed_uneffective_ENTSOE_indispo %>%
              rename(Dismissed.Uneffective.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity')))

tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  full_join(final_dismissed_nuclear_ENTSOE_indispo %>% 
              rename(Published.Dismissed.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_not_publi_dismissed_nuclear_ENTSOE_indispo %>% 
              rename(Unpublished.Dismissed.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_dismissed_nuclear_uneffective_ENTSOE_indispo %>%
              rename(Dismissed.Uneffective.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity')))

tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  left_join(final_real_indispo_rf_dismissed  %>% 
              mutate(Published.Dismissed.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
              select(-predicted_real_unavailability) %>% 
              rename(Published.Dismissed.Excess.Capacity =  Excess.Capacity
                     )) %>% 
  left_join(final_dismissed_not_publi_rf  %>% 
              mutate(Unpublished.Dismissed.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
              select(-predicted_real_unavailability) %>% 
              rename(Unpublished.Dismissed.Excess.Capacity =  Excess.Capacity
                     )) %>%
  
  left_join(final_dismissed_uneffective_rf  %>%
              mutate(Uneffective.Dismissed.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
              select(-predicted_real_unavailability) %>% 
              rename(Uneffective.Dismissed.Excess.Capacity =  Excess.Capacity
                     )) %>% 
     
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, as.numeric(creneau)-1)
                               , format="%Y-%m-%d %H", tz = 'Europe/Brussels'),
         creneau= as.factor(creneau))



#paste(electricity_delivery_date, creneau-1)



# # Générer une séquence complète de dates entre la première et la dernière date
# full_dates <- tibble(datetime = seq(min(tot_publi_ENTSOE_indispo$datetime, na.rm=TRUE),
#                                     max(tot_publi_ENTSOE_indispo$datetime, na.rm=TRUE), by = "hour"))
# 
# 
# auction_combinations <- tibble(auction_before = as.character(c(1:6)))  # Conversion en tibble
# creneaux_combinations <- tibble(creneau = as.character(c(1:24)))  # Conversion en tibble
#   
# # Croiser toutes les dates avec les combinaisons d'enchères
# crossed_data <- full_dates %>%
#   cross_join(auction_combinations) %>%
#   cross_join(creneaux_combinations)
# 
# # Fusionner le DataFrame original avec la séquence complète de dates
# complete_data <- crossed_data  %>%
#   left_join(tot_publi_ENTSOE_indispo , by = c("datetime", "auction_before", "creneau")) %>%
#   replace_na(list(
#                   Published.Unavailable.Capacity = 0,
#                   Published.Unavailable.Nuclear.Capacity = 0,
#                   Unpublished.Unavailable.Capacity = 0,
#                   Unpublished.Unavailable.Nuclear.Capacity = 0,
#                   Predicted.Unavailable.Capacity = 0
#                   ))
# 
# write_parquet(complete_data, 'complete_published_indispo_data.parquet')
# 
# colnames(complete_data)


auction_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/intra_day_prices/OMIE-auction-market-data'
setwd(auction_path)
intradaily_price_data <- read_parquet('auction_price_data_all.parquet')


load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/esp"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_prices <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`) 

day_ahead_prices<- day_ahead_prices[,1:2]
colnames(day_ahead_prices) <- c("price", "datetime")
day_ahead_prices<- day_ahead_prices %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         creneau = hour(datetime) +1,
         auction = "0",
         delivery_date = as.Date(datetime),
         auction_date_plus_one = delivery_date, 
         price = as.character(price)) %>% 
  filter(delivery_date >= as.Date('2018-01-01'))


# nrow(intradaily_price_data)
# nrow(intradaily_price_data %>% 
#       filter(auction==1 & delivery_date < as.Date('2025-01-01') ))

price_data <- bind_rows(intradaily_price_data, day_ahead_prices)
# nrow(price_data)


price_diff_data <- price_data %>%
  group_by(delivery_date, creneau) %>%
  arrange(delivery_date, creneau, auction) %>%
  mutate(log_price_diff = log(as.numeric(price)) - lag(log(as.numeric(price))),
         price_diff = as.numeric(price) - lag(as.numeric(price)),
         auction_before = lag(auction),
         auction_after = auction,
         creneau = as.factor(creneau)) %>% 
  ungroup() %>% 
  filter(datetime < as.POSIXct('2024-06-01', tz='Europe/Brussels')) %>% 
  arrange(datetime)


# la difference correspond au nombre de creneaux tradés dans une journee

price_diff_data[, c("auction_before", "auction_after")] %>% 
  group_by(auction_before, auction_after) %>%
  summarise(n= n()) %>% 
  arrange(desc(n))

# test <- price_diff_data %>% 
#   filter(auction_before == 0 & auction_after ==7)
# 
# test2 <- price_diff_data %>% 
#   filter(auction_before == 7 & auction_after ==7)

first_stage_data <- price_diff_data  %>% 
  mutate(electricity_delivery_date = as.character(delivery_date)) %>% 
  left_join(tot_publi_ENTSOE_indispo %>%
              #some minor changes to get good join
              select(-datetime) 
            , by = c('auction_before', 'auction_after', 'creneau', "electricity_delivery_date")) %>% 
  arrange(datetime)

first_stage_data <- first_stage_data %>% 
  arrange(datetime, creneau, auction_before)
colnames(first_stage_data)


#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')
colnames(renewable_forecasts)

#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

first_stage_data <- first_stage_data %>%
  left_join(renewable_forecasts, by=c('datetime' = '__index_level_0__'))

# Identify all capacity-related columns dynamically
capacity_cols <- colnames(first_stage_data)[grepl("Capacity", colnames(first_stage_data))]

# Drop rows with NA in log_price column
first_stage_data  <- 
  first_stage_data[!is.na(first_stage_data$log_price_diff), ]%>%
  mutate(across(all_of(capacity_cols), ~ replace_na(.x, 0))) 
#%>%  mutate(across(all_of(capacity_cols), ~ .x / gen_tot, .names = "{.col}_Share"))

getwd()

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(first_stage_data, 'first_stage_data.parquet')


