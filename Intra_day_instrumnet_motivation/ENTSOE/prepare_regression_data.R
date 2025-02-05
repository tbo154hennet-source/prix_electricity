
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(tidyr)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)


final_publi_ENTSOE_indispo <- read_parquet('final_publi_ENTSOE_indispo.parquet') %>% 
  arrange(electricity_delivery_date, auction_before, creneau)

final_publi_nuclear_ENTSOE_indispo <- read_parquet('final_publi_nuclear_ENTSOE_indispo.parquet') %>% 
  arrange(electricity_delivery_date, auction_before, creneau)

#unpublished indispo 
final_not_publi_ENTSOE_indispo <- read_parquet('final_not_publi_ENTSOE_indispo.parquet') %>% 
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_not_publi_nuclear_ENTSOE_indispo <- read_parquet('final_not_publi_ENTSOE_nuclear_indispo.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_real_indispo_rf <- read_parquet('final_real_indispo_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau) %>% 
  select(-c("wind_solar_tot","gen_tot" ))

final_real_theoretical_indispo_rf <- read_parquet('final_real_theoretical_indispo_rf.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau) %>% 
  select(-c("wind_solar_tot","gen_tot" ))

# real compared with random forest indispo

final_real_indispo_limitantes <- read_parquet('final_real_indispo_limitantes.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)

final_real_theoretical_indispo_limitantes <- read_parquet('final_real_theoretical_indispo_limitantes.parquet') %>%
  mutate(creneau = as.character(creneau)) %>%
  arrange(electricity_delivery_date, auction_before, creneau)


tot_publi_ENTSOE_indispo <- final_publi_ENTSOE_indispo %>% 
  rename(Published.Unavailable.Capacity = Unavailable.Capacity)  %>%
  left_join(final_publi_nuclear_ENTSOE_indispo %>% 
              rename(Published.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1)
                               , format="%Y-%m-%d %H", tz = 'Europe/Brussels'),
         creneau= as.factor(creneau))

tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  full_join(final_not_publi_ENTSOE_indispo %>% 
              rename(Unpublished.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  full_join(final_not_publi_nuclear_ENTSOE_indispo %>% 
              rename(Unpublished.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity')))


tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  left_join(final_real_indispo_rf %>% 
              mutate(Predicted.Unavailable.Capacity = -predicted_real_unavailability)%>% 
              select(-predicted_real_unavailability)) %>% 
  left_join(final_real_indispo_limitantes %>% 
            rename(Limiting.Unavailable.Capacity = Unavailable.Capacity))


tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  left_join(final_real_theoretical_indispo_rf %>% 
              mutate(Unpublished.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>% 
              select(-predicted_real_unavailability)) %>% 
  left_join(final_real_theoretical_indispo_limitantes %>% 
              rename(Unpublished.Limiting.Unavailable.Capacity = Unavailable.Capacity))



colnames(tot_publi_ENTSOE_indispo)

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
day_ahead_prices$`__index_level_0__`

day_ahead_prices<- day_ahead_prices[,1:2]
colnames(day_ahead_prices) <- c("price", "datetime")
day_ahead_prices<- day_ahead_prices %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         creneau = hour(datetime) +1,
         auction = "0",
         delivery_date = as.Date(datetime),
         auction_date_plus_one = delivery_date + 1, 
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
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts.parquet')
gen_tot <- renewable_forecasts %>%
  select(`__index_level_0__`, gen_tot, wind_solar_tot)

#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

first_stage_data <- first_stage_data %>%
  left_join(gen_tot, by=c('datetime' = '__index_level_0__'))



# Drop rows with NA in log_price column
first_stage_data  <- 
  first_stage_data[!is.na(first_stage_data$log_price_diff), ]%>%
  replace_na(list(Published.Unavailable.Capacity = 0,
                  Published.Unavailable.Nuclear.Capacity = 0,
                  Unpublished.Unavailable.Capacity = 0,
                  Unpublished.Unavailable.Nuclear.Capacity = 0,
                  Predicted.Unavailable.Capacity = 0,
                  Limiting.Unavailable.Capacity = 0,
                  Unpublished.Predicted.Unavailable.Capacity = 0,
                  Unpublished.Limiting.Unavailable.Capacity = 0
                  )) %>% 
  mutate( Published.Unavailable.Nuclear.Share = Published.Unavailable.Nuclear.Capacity/ gen_tot,
          Published.Unavailable.Share = Published.Unavailable.Capacity/ gen_tot,
          Unpublished.Unavailable.Nuclear.Share = Unpublished.Unavailable.Nuclear.Capacity/ gen_tot,
          Unpublished.Unavailable.Share = Unpublished.Unavailable.Capacity/ gen_tot,
          Predicted.Unavailable.Share = Predicted.Unavailable.Capacity/ gen_tot,
          Limiting.Unavailable.Share = Limiting.Unavailable.Capacity/ gen_tot,
          Unpublished.Predicted.Unavailable.Share = Unpublished.Predicted.Unavailable.Capacity/ gen_tot,
          Unpublished.Limiting.Unavailable.Share = Unpublished.Limiting.Unavailable.Capacity/ gen_tot
          )

getwd()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(first_stage_data, 'first_stage_data.parquet')



