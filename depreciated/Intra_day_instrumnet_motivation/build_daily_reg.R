
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/outputs'
setwd(save_path_esp)
final_indispo <- read_parquet('final_indispo.parquet')
updated_final_indispo <- read_parquet('updated_final_indispo.parquet')
nuclear_final_indispo <- read_parquet('nuclear_final_indispo.parquet')
final_real_indispo_rf <- read_parquet( 
              'final_real_indispo_rf.parquet')


final_indispo <- final_indispo %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1)
                               , format="%Y-%m-%d %H", tz = 'UTC'),
         creneau= as.factor(creneau))
#final_indispo  <- tibble(final_indispo )
#test <- final_indispo %>% filter(electricity_delivery_date=='2021-09-25')

# Générer une séquence complète de dates entre la première et la dernière date
full_dates <- tibble(datetime = seq(min(final_indispo$datetime, na.rm=TRUE), max(final_indispo$datetime, na.rm=TRUE), by = "hour"))

updated_final_indispo_merge <- updated_final_indispo %>% 
  select(-c('Installed.Capacity')) %>%
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1),
               format="%Y-%m-%d %H", tz = 'UTC'), creneau= as.factor(creneau))

nuclear_final_indispo_merge <- nuclear_final_indispo %>% 
  select(-c('Installed.Capacity')) %>%
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1),
    format="%Y-%m-%d %H", tz = 'UTC'), creneau= as.factor(creneau)) %>% 
  rename(nuclear_real_unavailability = real_unavailability) %>% 
  select(-c('theoretical_unavailability', 'centrale_unavailability'))


# Fusionner le DataFrame original avec la séquence complète de dates
complete_data <- full_dates %>%
  left_join(final_indispo , by = "datetime") %>%
  replace_na(list(Unavailable.Capacity = 0))

complete_data <- left_join( complete_data, updated_final_indispo_merge) %>%
    replace_na(list(real_unavailability = 0))


complete_data <- left_join( complete_data, nuclear_final_indispo_merge) %>%
  replace_na(list(nuclear_real_unavailability = 0))

complete_data <- left_join(complete_data,
 final_real_indispo_rf  ) %>%
  replace_na(list(predicted_real_unavailability = 0))



write_parquet(complete_data, 'complete_indispo_data.parquet')

setwd(save_path_esp)
complete_data <- read_parquet( 'complete_indispo_data.parquet')

test <- complete_data %>% 
  filter(predicted_real_unavailability >0)

auction_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/intra_day_prices/OMIE-auction-market-data'
setwd(auction_path)
price_data <- read_parquet('auction_price_data_21_22.parquet')

price_diff_data <- price_data %>%
  mutate(delivery_date = as.Date(datetime)) %>% 
  group_by(delivery_date, creneau) %>%
  arrange(delivery_date, creneau, auction) %>%
  mutate(log_price_diff = log(as.numeric(price)) - lag(log(as.numeric(price))),
         price_diff = as.numeric(price) - lag(as.numeric(price)),
         auction_before = lag(auction),
         auction_after = auction,
         creneau = as.factor(creneau))


# use the auction 7 concept for the final indispo data

complete_data <- complete_data%>% 
  mutate( auction_after = ifelse(auction_before == 6 & auction_after == 2,
                                 7, auction_after))


first_stage_data <- left_join(price_diff_data, complete_data,
                              by= c('auction_before', 'auction_after', 'datetime', 'creneau'))

# complete_data %>% 
#   filter(nuclear_real_unavailability > 0) 
# test <- first_stage_data %>% 
#   filter(nuclear_real_unavailability > 0) 

first_stage_data <- first_stage_data %>% 
  select("price", 'price_diff', "delivery_date", "log_price_diff",           
         "auction_before", "auction_after", 'creneau',
         "gen_uncontrolled" ,"gen_tot", 
         "real_unavailability",   
        "nuclear_real_unavailability",  
       "predicted_real_unavailability",
         "Unavailable.Capacity", 'datetime' )


# Drop rows with NA in log_price column
first_stage_data  <- 
  first_stage_data[!is.na(first_stage_data$log_price_diff), ]%>%
  mutate(Unavailable.Capacity = 
           ifelse(is.na(Unavailable.Capacity), 0, Unavailable.Capacity ),
         real_unavailability = 
           ifelse(is.na(real_unavailability), 0, real_unavailability ),
         nuclear_real_unavailability = 
           ifelse(is.na(nuclear_real_unavailability), 0, nuclear_real_unavailability ),
         predicted_real_unavailability = 
           ifelse(is.na(predicted_real_unavailability), 0, predicted_real_unavailability ),
         Unavailable.Share = 
           ifelse(is.na(predicted_real_unavailability), 0, predicted_real_unavailability/400 ))







setwd(save_path_esp)
write_parquet(first_stage_data, 'first_stage_data.parquet')


