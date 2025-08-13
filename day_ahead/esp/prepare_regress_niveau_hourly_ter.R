library(data.table)
library(dplyr)
library(tidyr)  
library(arrow)
library(lubridate)

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/day_ahead/outputs/rf'
setwd(save_path_entsoe_output)
rf_predictions_data_esp <- read_parquet('rf_predictions_data_xgb_esp.parquet') %>% 
  distinct() %>% 
  group_by(centrale, `__index_level_0__`) %>% 
  slice_head(n = 1) %>%  # Utilisation de slice_head au lieu de slice(1)
  ungroup() %>% 
  arrange(`__index_level_0__`)

colnames(rf_predictions_data_esp)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
filtered_indispo_esp <- read_parquet("filtered_indispo_esp_OMIE.parquet") %>% 
  arrange(datetime_slot)
colnames(filtered_indispo_esp)

#enlever cas limite de l'heure pile
filtered_indispo_esp <- filtered_indispo_esp%>% 
  filter(datetime_slot != end_datetime)


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)

publi_all_matched_indispo <- read_parquet("day_ahead_indispo_formatted.parquet")
colnames(publi_all_matched_indispo )
final_indispo_esp <- filtered_indispo_esp  %>%
  left_join(publi_all_matched_indispo  %>% 
              rename(production_resource_name = centrale_name) %>%
              dplyr::select(publication_status, mrid, start_datetime,
                     end_datetime, plant_type, production_resource_name, 
                     Unavailable.Capacity, Installed.Capacity)
            , by = c("start_datetime", "end_datetime",
                     "production_resource_name", "mrid")) 

long_final_indispo_esp <- left_join(
  final_indispo_esp, 
  rf_predictions_data_esp, 
  by = c("datetime_slot" = "__index_level_0__", 
         "production_resource_name" = "centrale")
)

# gérer le cas de bord voir l'exemple ci-dessous
# test2 <- long_final_indispo %>%
#   filter(datetime_slot== "2018-01-19 15:00:00" & production_resource_name == "COMPOSTI 4" )
long_final_indispo_esp  <- long_final_indispo_esp  %>% 
  filter(datetime_slot != end_datetime)


# long_indispo_limitantes <- long_final_indispo_esp %>%
#   filter(Installed.Capacity!= Unavailable.Capacity) %>%
#   filter(as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity) < centrale_prod*1.1) %>% 
#   filter(previous_day_prod > as.numeric(Installed.Capacity)*0.9)

long_indispo_nuclear <- long_final_indispo_esp %>%
  filter(plant_type == "Nuclear")

long_final_indispo_frpred <- long_final_indispo_esp %>%
  mutate(Predicted.Unavailable.Capacity = - predicted_real_unavailability) %>% 
  filter(Predicted.Unavailable.Capacity > 50)


long_indispo_nuclear %>% 
  group_by(publication_status) %>% 
  summarise(n= n())


long_final_indispo_frpred %>% 
  group_by(publication_status) %>% 
  summarise(n= n())


final_indispo_esp <- long_final_indispo_esp[,lapply(.SD, sum), 
                                          .SDcols = c("Unavailable.Capacity"),
                                          by = c("publication_status","datetime_slot")]

# final_indispo_esp_limitantes <- long_indispo_limitantes[,lapply(.SD, sum), 
#                                                        .SDcols = c("Unavailable.Capacity"),
#                                                        by = c("publication_status", "datetime_slot")]


final_indispo_esp_rfpred <- long_final_indispo_frpred[,lapply(.SD, sum), 
                                                     .SDcols = c("Predicted.Unavailable.Capacity", "Unavailable.Capacity"),
                                                     by = c("publication_status", "datetime_slot")]


final_indispo_esp_nuclear <- long_indispo_nuclear[,lapply(.SD, sum), 
                                                 .SDcols = c("Unavailable.Capacity"),
                                                 by = c("publication_status", "datetime_slot")]


final_indispo_tot_esp <- final_indispo_esp %>%  
                                  # final_indispo_esp_limitantes%>% 
                                  #   rename(Limiting.Capacity = Unavailable.Capacity),
                                  # by = c("publication_status", "datetime_slot")) %>% 
  left_join(final_indispo_esp_nuclear %>% 
              rename(Unavailable.Nuclear.Capacity = Unavailable.Capacity),
            by = c("publication_status",  "datetime_slot")) %>% 
  left_join(final_indispo_esp_rfpred%>% 
              rename(Cluster.Capacity = Unavailable.Capacity), 
            by = c("publication_status",  "datetime_slot"))


getwd()

#pour bien formatter le cluster_rf_id
# c'est une approximation
# final_indispo_tot_esp <- final_indispo_tot_esp %>%
#   mutate(date = as.Date(datetime_slot)) %>%  # Extraire la date
#   group_by(publication_status) %>%  # Grouper par jour
#   mutate(transitions = Cluster.Capacity != lag(Cluster.Capacity, default = first(Cluster.Capacity)),
#          n_unique_values = n_distinct(Cluster.Capacity, na.rm = TRUE),  # Count distinct values
#          n_transitions = sum(transitions, na.rm = TRUE),
#          #ex 2 valeurs d'indispo différentes
#          #il devrait y avoir une indispo
#          
#          # cas de bord n_transitions = 0, n_unique_values=0
#          Cluster.Capacity = ifelse( n_transitions > max(n_unique_values -1,0) , max(Cluster.Capacity, na.rm = TRUE), Cluster.Capacity)
#   ) %>%  # Fixer à la valeur max du jour
#   ungroup()



final_indispo_tot_wide_esp <- final_indispo_tot_esp %>% 
  group_by(datetime_slot, publication_status) %>% 
  summarise(across(c(Unavailable.Nuclear.Capacity, #Limiting.Capacity, 
                     Unavailable.Capacity, Predicted.Unavailable.Capacity, Cluster.Capacity), 
                   ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  pivot_wider(
    names_from = publication_status, 
    values_from = c(Unavailable.Nuclear.Capacity, #Limiting.Capacity, 
                    Unavailable.Capacity, Predicted.Unavailable.Capacity, Cluster.Capacity), 
    names_glue = "{publication_status}.{.value}"
  )
setDT(final_indispo_tot_wide_esp)



final_indispo_tot_wide_esp <- final_indispo_tot_wide_esp %>% 
  mutate(cl_id  = rleid(
                        #Planned.Unavailable.Capacity,
                        Published.Unavailable.Capacity, 
                        Unpublished.Unavailable.Capacity,
                        Uneffective.Unavailable.Capacity),
         cl_nu_id  = rleid(
                        #Planned.Unavailable.Nuclear.Capacity,
                        Published.Unavailable.Nuclear.Capacity, 
                        Unpublished.Unavailable.Nuclear.Capacity,
                        Uneffective.Unavailable.Nuclear.Capacity),
         # cl_lm_id  = rleid(Planned.Limiting.Capacity,
         #                   Published.Limiting.Capacity,
         #                   Unpublished.Limiting.Capacity,
         #                   Uneffective.Limiting.Capacity),
         cl_rf_id  = rleid(
                          #Planned.Cluster.Capacity,
                           Published.Cluster.Capacity,
                           Unpublished.Cluster.Capacity,
                           Uneffective.Cluster.Capacity)
  )


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")

#write_parquet(final_indispo_tot_wide_esp, "final_indispo_tot_wide_esp.parquet")
final_indispo_tot_wide_esp <- read_parquet("final_indispo_tot_wide_esp.parquet")

library(lubridate)

load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/esp"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
# Liste des noms de colonnes souhaités
colnames_target <- c( "price", "datetime")

# Lecture + renommage dans lapply
day_ahead_prices <- parquet_files %>%
  lapply(function(file) {
    df <- read_parquet(file)
    names(df) <- colnames_target
    return(df)
  }) %>%
  bind_rows() %>%
  arrange(datetime)


day_ahead_prices<- day_ahead_prices %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         delivery_date = as.Date(datetime),
         price = as.numeric(price),
         price_diff = price - lag(price),
         log_price_diff = log(price) - lag(log(price))
         ) %>% 
  #filter(delivery_date >= as.Date('2018-01-01')) %>% 
  arrange(datetime)


# day_ahead_prices[61,]
# View(final_indispo_tot_wide %>% 
#   filter(datetime_slot == "2018-01-03 13:00:00"))
price_indispo_merge_esp <- left_join(day_ahead_prices,
                                 final_indispo_tot_wide_esp,
      by = c("datetime" = "datetime_slot"))

#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')
colnames(renewable_forecasts)

#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

price_indispo_merge_esp <- price_indispo_merge_esp %>%
  left_join(renewable_forecasts, by=c('datetime' = '__index_level_0__'))


#add temperature  and other weather conditions

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
temperature_openmeteo_data <- read_parquet("temperature_opendata_tot.parquet")
colnames(temperature_openmeteo_data)

#question sou-jacente
#quelle est la temporalité d'ajustement au froid

mean_temperatures_hour <- temperature_openmeteo_data %>% 
  mutate(week = week(date),
         hour = hour(date)) %>%
  group_by(week, hour) %>% 
  summarise(
    mean_temp_demand = mean(as.numeric(temperature_demand), na.rm = TRUE),
    mean_temp_supply = mean(as.numeric(temperature_supply), na.rm = TRUE),
    mean_temp_nuclear = mean(as.numeric(temperature_2m_nuclear), na.rm = TRUE),
    .groups = "drop"
  )

cold_months <- c(1, 2, 11, 12)
hot_months <- c(6, 7, 8, 9)

temperature_openmeteo_data_test <- temperature_openmeteo_data %>%
  mutate(
    week = week(date),
    hour = hour(date),
    month = month(date),
    year = year(date),
    day_month = format(date, "%d-%m")
  ) %>%
  # Join hourly means
  left_join(mean_temperatures_hour, by = c("week", "hour")) %>%
  rename(hourly_mean_temp_demand = mean_temp_demand,
         hourly_mean_temp_supply = mean_temp_supply,
         hourly_mean_temp_nuclear = mean_temp_nuclear
         ) %>%
  mutate(
    cold_excess_supply = -ifelse(month %in% cold_months, 
                                      temperature_supply - hourly_mean_temp_supply, 0),
    heat_excess_supply = ifelse(month %in% hot_months, 
                                     temperature_supply - hourly_mean_temp_supply, 0),
    cold_excess_demand = -ifelse(month %in% cold_months, 
                                      temperature_demand - hourly_mean_temp_demand, 0),
    heat_excess_demand = ifelse(month %in% hot_months, 
                                     temperature_demand - hourly_mean_temp_demand, 0),
    cold_excess_nuclear = -ifelse(month %in% cold_months, 
                                      temperature_2m_nuclear - hourly_mean_temp_nuclear, 0),
    heat_excess_nuclear = ifelse(month %in% hot_months,
                                     temperature_2m_nuclear - hourly_mean_temp_nuclear, 0)
    
  )



price_indispo_merge_esp <- price_indispo_merge_esp %>%
  left_join(temperature_openmeteo_data_test %>% 
              dplyr::select(-year, -month), by=c('datetime' = 'date'))


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
wind_solar_openmeteo_data <- read_parquet("wind_solar_data.parquet")
colnames(wind_solar_openmeteo_data )

price_indispo_merge_esp <- price_indispo_merge_esp %>%
  left_join(wind_solar_openmeteo_data %>% 
              dplyr::select(-year, -month), by=c('datetime' = 'date'))

#add day ahead generation

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/esp")
# Get all files in the directory
# List only .parquet files in the directory
my_files <- list.files(pattern = "\\.parquet$")
# Read all files into a list of data frames
gen_list <- lapply(my_files, function(file) {
  message(file)  # equivalent to print
  read_parquet(file)
})

# Combine all data frames and remove duplicates
gen_data <- unique(do.call(rbind, gen_list))
# Rename columns
colnames(gen_data) <- c('gen_tot_da', 'gen_tot_da_conso', 'datetime')
gen_data$gen_tot_da_net <- gen_data$gen_tot_da -  gen_data$gen_tot_da_conso 



price_indispo_merge_esp <- price_indispo_merge_esp %>%
  left_join(gen_data, by=c('datetime'))


colnames(price_indispo_merge_esp)

# pour l'estimation en niveau,
#on rajoute le spot gas price
library(readxl)
gas_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(gas_path)

gas_prices <- read_excel("gas.xlsx") %>% 
  mutate(time = as.Date(time),
         spot_gas = as.numeric(paste0(
           `Gaz naturel, contrats à terme aux Pays-Bas (TTF), euros par megawatt-heure`
                                      ,".",virgule ))) %>% 
  dplyr::select(time, spot_gas)

colnames(price_indispo_merge_esp)
price_indispo_merge_esp <- price_indispo_merge_esp %>%
  left_join(gas_prices, by=c('delivery_date' = 'time'))
nrow(price_indispo_merge_esp)
price_indispo_merge_esp <- price_indispo_merge_esp %>% distinct()

save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
write_parquet(price_indispo_merge_esp, "price_indispo_merge_esp.parquet")


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/cross_border")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

price_merge_CBF <- read_parquet("price_merge_CBF_max.parquet")
colnames(price_merge_CBF)
crossborder_data <- price_merge_CBF %>% 
  filter(gen_tot_fr > 0 & gen_tot_esp > 0) %>%
  summarise(NTC_ES_FR = NTC_ES_FR,
            NTC_FR_ES = NTC_FR_ES,
            NTC_ES_PT = NTC_ES_PT,
            NTC_PT_ES = NTC_PT_ES,
    renewable_day_ahead_forecast_ex_es = renewable_day_ahead_forecast_pt +
                                                         renewable_day_ahead_forecast_fr,
    cbf_es = CBF_ES_pred,

    
    
    renewable_day_ahead_forecast_ex_fr = renewable_day_ahead_forecast_esp+
                                                         renewable_day_ahead_forecast_it_nord+
                                                         renewable_day_ahead_forecast_de+
                                                         renewable_day_ahead_forecast_be+
                                                         renewable_day_ahead_forecast_ch+
                                                         renewable_day_ahead_forecast_gb,
    cbf_fr = CBF_FR_pred,
    datetime = datetime
  ) %>%
  arrange(datetime) %>% 
  ungroup()


price_indispo_merge_esp_NTC <- merge(price_indispo_merge_esp, crossborder_data %>% 
                                       dplyr::select(NTC_ES_FR,NTC_FR_ES,                          
                                        NTC_ES_PT, NTC_PT_ES,
                                        renewable_day_ahead_forecast_ex_es, cbf_es,
                                                     datetime), 
                                     by ="datetime")


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
write_parquet(price_indispo_merge_esp_NTC, "price_indispo_merge_esp_NTC.parquet")

