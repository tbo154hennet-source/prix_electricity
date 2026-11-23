
library(tidyr)
#build a specification with single change during the day and fixed effect by lant


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
all_rf_indispo <- read_parquet( 'all_rf_indispo_OMIE.parquet')

#je prends juste els journées où il y a effectivement une indisponibilité
#je ne considère donc pas la corrélation temporelle entre les journées puisque celles-ci ne se suivent pas
#je me ramène donc au cadre sun et abraham où t est un individu i
# que je peux clusterisé par plant (garde tout de même mon asymptote)


save_path_OMIE_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_output)
price_diff_data <- read_parquet('price_diff_data.parquet')

#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
setwd(save_path_data)
renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')

#add temperature  and other weather conditions
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
temperature_openmeteo_data <- read_parquet("temperature_opendata_tot.parquet")


intersect(colnames(all_rf_indispo), )

#garder uniquement les créneaux avec une unique indispo

all_rf_indispo_unique <- all_rf_indispo %>% 
  group_by(electricity_delivery_date, creneau) %>% 
  mutate(n = n()) %>% 
  filter(n ==1)


get_processed_data <- function(rf_data = all_rf_indispo_unique){
  first_stage_data <- price_diff_data  %>% 
    mutate(electricity_delivery_date = as.character(delivery_date),
           creneau = as.integer(creneau)) %>% 
    left_join(rf_data %>%
                #some minor changes to get good join
                select(-datetime) 
              , by = c('auction_before', 'auction_after', 'creneau', "electricity_delivery_date")) %>% 
    arrange(datetime) %>% 
    rename(month = month.x)
  
  
  first_stage_data %>% 
    group_by(is.na(predicted_real_unavailability)) %>% 
    summarise(n = n())
  #les journées avec au moins une indispo
  
  first_stage_data_test <- first_stage_data %>% 
    arrange(datetime, creneau, auction_before) %>% 
    group_by(datetime, creneau) %>%
    mutate(day_unavail = sum((predicted_real_unavailability>0), na.rm = TRUE)) %>% 
    filter(day_unavail > 0)
  
  
  
  #nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
  #sample start
  
  first_stage_data_test <- first_stage_data_test %>%
    left_join(renewable_forecasts %>% 
                select(-gen_tot, -wind_solar_tot), by=c('datetime' = '__index_level_0__'))
  
  first_stage_data_test <- first_stage_data_test %>%
    left_join(temperature_openmeteo_data, by=c('datetime' = 'date'))
  
  first_stage_data_event_study <- first_stage_data_test %>% 
    group_by(creneau, electricity_delivery_date, auction) %>%
    summarise(
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      across(!where(is.numeric), ~ .x[which(!is.na(.x))[1]]),
      .groups = "drop"
    )
  
  
  
  
  return(first_stage_data_test)
}

library(dplyr)

all_rf_indispo %>% 
  group_by(electricity_delivery_date, creneau, source) %>% 
  mutate(n = n()) %>%
  ungroup() %>%
  group_by(electricity_delivery_date, creneau) %>%
  mutate(
    unique_sources = n_distinct(source),
    source_group = if_else(unique_sources == 1, first(source), "mixed")
  ) %>%
  ungroup() %>% 
  group_by(unique_sources, source_group) %>% 
  summarise(n = n())


View(first_stage_data_event_study %>% 
       select(Unavailable.Capacity, predicted_real_unavailability, 
              auction_before, creneau, electricity_delivery_date
              ,ref_auction, event_time, price
              ))


first_stage_data_event_study <- first_stage_data_event_study %>%
  mutate(auction_before = replace_na(as.numeric(auction_before), -1)) %>% 
  group_by(creneau, electricity_delivery_date) %>%
  mutate(
    ref_auction = if(any(predicted_real_unavailability != 0)) {
      which(predicted_real_unavailability != 0)[1] -2
    } else {
      NA_integer_
    },
    event_time = auction_before - ref_auction
  ) %>%
  ungroup()

  
  
library(fixest)

# Create event time dummies (e.g., -5 to +5 days around shock)
# Let's say you have a variable `event_time` ranging from -5 to +5
colnames(first_stage_data_event_study)

model <- feols(as.numeric(price) ~ sunab(auction_before, event_time) +
               day_ahead_forecast + intraday_update_renewables + unpredicted_renewables+
                 temperature_supply + precipitation_supply +relative_humidity_supply|
                 auction_before  ,
      
      #cluster = ~plant_id,
      #fixef = c("auction_before ")
      data = first_stage_data_event_study
      )
summary(model)



first_stage_data_event_study_bis <- first_stage_data_event_study %>% 
  group_by(creneau, electricity_delivery_date) %>%
  mutate(predicted_real_unavailability = max(predicted_real_unavailability))

model <- feols(
  as.numeric(price) ~ i(event_time, predicted_real_unavailability) +
    day_ahead_forecast + intraday_update_renewables + unpredicted_renewables +
    temperature_supply + precipitation_supply + relative_humidity_supply |
    auction_before,
  cluster = ~creneau,
  data = first_stage_data_event_study_bis %>% 
    filter(event_time <= 3 & event_time >= -3))
)
dev.off()
par(mar = c(5, 5, 4, 2))  # bottom, left, top, right

summary(model)
iplot(model ,
      xlab = "Event Time",
      ylab = "Estimated Effect",
      main = "Event Study Plot",
      ref.line = TRUE)

feols(as.numeric(price) ~ sunab(auction_before, event_time) + controls | fixed_effects, 
      data = ..., cluster = ...)




all_rf_indispo_unique %>% 
  group_by(auction_before) %>% 
  summarise(n = n())

first_stage_data_event_study %>% 
  group_by(event_time) %>% 
  summarise(n = n()) 

first_stage_data_event_study %>% 
  group_by(ref_auction) %>% 
  summarise(n = n()) 
