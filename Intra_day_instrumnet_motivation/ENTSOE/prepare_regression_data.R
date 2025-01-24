
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



# real compared with random forest indispo



tot_publi_ENTSOE_indispo <- final_publi_ENTSOE_indispo %>% 
  rename(Published.Unavailable.Capacity = Unavailable.Capacity)  %>%
  left_join(final_publi_nuclear_ENTSOE_indispo %>% 
              rename(Published.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1)
                               , format="%Y-%m-%d %H", tz = 'UTC'),
         creneau= as.factor(creneau))

tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  left_join(final_not_publi_ENTSOE_indispo %>% 
              rename(Unpublished.Unavailable.Capacity = Unavailable.Capacity)  %>% 
              select(-c('Installed.Capacity'))) %>% 
  left_join(final_not_publi_nuclear_ENTSOE_indispo %>% 
              rename(Unpublished.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity')))


# use the auction 7 concept for the final indispo data

tot_publi_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  mutate( auction_after = ifelse(auction_before == 6 & auction_after == 2,
                                 7, auction_after))

# Générer une séquence complète de dates entre la première et la dernière date
full_dates <- tibble(datetime = seq(min(tot_publi_ENTSOE_indispo$datetime, na.rm=TRUE),
                                    max(tot_publi_ENTSOE_indispo$datetime, na.rm=TRUE), by = "hour"))


auction_combinations <- tibble(auction_before = as.character(c(1:6)))  # Conversion en tibble

# Croiser toutes les dates avec les combinaisons d'enchères
crossed_data <- full_dates %>%
  cross_join(auction_combinations)


# Fusionner le DataFrame original avec la séquence complète de dates
complete_data <- crossed_data  %>%
  left_join(tot_publi_ENTSOE_indispo , by = c("datetime", "auction_before")) %>%
  replace_na(list(Unavailable.Capacity = 0,
                  Unavailable.Nuclear.Capacity = 0))

write_parquet(complete_data, 'complete_published_indispo_data.parquet')

colnames(complete_data)
# complete_data %>% 
#   mutate(electricity_delivery_date = as.Date(datetime)) %>%
#   filter(electricity_delivery_date >= as.Date('2018-01-01')) %>% 
#   nrow()

auction_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/intra_day_prices/OMIE-auction-market-data'
setwd(auction_path)
price_data <- read_parquet('auction_price_data_all.parquet')

#rows_with_na <- price_data[!complete.cases(price_data), ]
#small number of nas


price_diff_data <- price_data %>%
  mutate(delivery_date = as.Date(datetime)) %>% 
  group_by(delivery_date, creneau) %>%
  arrange(delivery_date, creneau, auction) %>%
  mutate(log_price_diff = log(as.numeric(price)) - lag(log(as.numeric(price))),
         price_diff = as.numeric(price) - lag(as.numeric(price)),
         auction_before = lag(auction),
         auction_after = auction,
         creneau = as.factor(creneau))


first_stage_data <- left_join(price_diff_data, complete_data,
                              by= c('auction_before', 'auction_after', 'datetime', 'creneau'))



#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts.parquet')
gen_tot <- renewable_forecasts %>% 
  select(`__index_level_0__`, gen_tot) 

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
                  Unpublished.Unavailable.Nuclear.Capacity = 0)) %>% 
  mutate( Published.Unavailable.Nuclear.Share = Published.Unavailable.Nuclear.Capacity/ gen_tot,
          Published.Unavailable.Share = Published.Unavailable.Capacity/ gen_tot,
          Unpublished.Unavailable.Nuclear.Share = Unpublished.Unavailable.Nuclear.Capacity/ gen_tot,
          Unpublished.Unavailable.Share = Unpublished.Unavailable.Capacity/ gen_tot)

getwd()
write_parquet(first_stage_data, 'first_stage_data.parquet')



