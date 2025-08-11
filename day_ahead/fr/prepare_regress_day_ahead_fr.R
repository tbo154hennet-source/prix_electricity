library(tidyr)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/rf'
setwd(save_path_entsoe_output)
rf_predictions_data_fr <- read_parquet('rf_predictions_data_xgb_fr.parquet') %>% 
  distinct() %>% 
  group_by(centrale, `__index_level_0__`) %>% 
  slice_head(n = 1) %>%  
  ungroup()


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr")
filtered_indispo_fr <- read_parquet("filtered_indispo_fr.parquet")
colnames(filtered_indispo_fr)

filtered_indispo_fr <- filtered_indispo_fr%>% 
  filter(datetime_slot != end_datetime)


save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
publi_bout_indispo_ENTSOE_data_fr <- read_parquet("save_indispo_with_pub_status_fr.parquet")


final_indispo_fr <- filtered_indispo_fr  %>%
  left_join(publi_bout_indispo_ENTSOE_data_fr %>% 
              dplyr::select(publication_status, mrid, start_datetime,
                     end_datetime, plant_type, production_resource_name, 
                     Unavailable.Capacity, Installed.Capacity)
            , by = c("start_datetime", "end_datetime",
                     "production_resource_name", "mrid")) 

long_final_indispo_fr <- left_join(
  final_indispo_fr, 
  rf_predictions_data_fr, 
  by = c("datetime_slot" = "__index_level_0__", 
         "production_resource_name" = "centrale")
)



# long_indispo_limitantes <- long_final_indispo_fr %>%
#   filter(Installed.Capacity!= Unavailable.Capacity) %>%
#   filter(as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity) < centrale_prod*1.1) %>% 
#   filter(previous_day_prod > (as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity))*0.9)


long_indispo_nuclear <- long_final_indispo_fr %>%
  filter(plant_type == "Nuclear")

long_final_indispo_frpred <- long_final_indispo_fr %>%
  mutate(Predicted.Unavailable.Capacity = - predicted_real_unavailability) %>% 
  filter(Predicted.Unavailable.Capacity > 50)


long_indispo_nuclear %>% 
  group_by(publication_status) %>% 
  summarise(n= n())


long_final_indispo_frpred %>% 
  group_by(publication_status) %>% 
  summarise(n= n())

library(data.table)

final_indispo_fr <- long_final_indispo_fr[,lapply(.SD, sum), 
                                          .SDcols = c("Unavailable.Capacity"),
                                          by = c("publication_status","datetime_slot")]

# final_indispo_fr_limitantes <- long_indispo_limitantes[,lapply(.SD, sum), 
#                                                        .SDcols = c("Unavailable.Capacity"),
#                                                        by = c("publication_status", "datetime_slot")]
# 

final_indispo_fr_rfpred <- long_final_indispo_frpred[,lapply(.SD, sum), 
                                                     .SDcols = c("Predicted.Unavailable.Capacity", "Unavailable.Capacity"),
                                                     by = c("publication_status", "datetime_slot")]


final_indispo_fr_nuclear <- long_indispo_nuclear[,lapply(.SD, sum), 
                                                 .SDcols = c("Unavailable.Capacity"),
                                                 by = c("publication_status", "datetime_slot")]


final_indispo_tot_fr <- final_indispo_fr %>%
# , final_indispo_fr_limitantes%>% 
#                                     rename(Limiting.Capacity = Unavailable.Capacity),
#                                   by = c("publication_status", "datetime_slot"))
  left_join(final_indispo_fr_nuclear %>% 
              rename(Unavailable.Nuclear.Capacity = Unavailable.Capacity),
            by = c("publication_status",  "datetime_slot")) %>% 
  left_join(final_indispo_fr_rfpred%>% 
              rename(Cluster.Capacity = Unavailable.Capacity), 
            by = c("publication_status",  "datetime_slot"))

#pour bien formatter le cluster_rf_id
# c'est une approximation
final_indispo_tot_fr <- final_indispo_tot_fr %>%
  mutate(date = as.Date(datetime_slot)) %>%  # Extraire la date
  group_by(date, publication_status) %>%  # Grouper par jour
  mutate(transitions = Cluster.Capacity != lag(Cluster.Capacity, default = first(Cluster.Capacity)),
         n_unique_values = n_distinct(Cluster.Capacity, na.rm = TRUE),  # Count distinct values
         n_transitions = sum(transitions, na.rm = TRUE),
         #ex 2 valeurs d'indispo différentes
         #il devrait y avoir une indispo
         
         # cas de bord n_transitions = 0, n_unique_values=0
         Cluster.Capacity = ifelse( n_transitions > max(n_unique_values -1,0) , max(Cluster.Capacity, na.rm = TRUE), Cluster.Capacity)
  ) %>%  # Fixer à la valeur max du jour
  ungroup()



final_indispo_tot_wide_fr <- final_indispo_tot_fr %>% 
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
setDT(final_indispo_tot_wide_fr)



final_indispo_tot_wide_fr <- final_indispo_tot_wide_fr %>% 
  mutate(cl_id  = rleid(Planned.Unavailable.Capacity,
                        Published.Unavailable.Capacity, 
                        Unpublished.Unavailable.Capacity,
                        Uneffective.Unavailable.Capacity),
         # cl_lm_id  = rleid(Planned.Limiting.Capacity,
         #                   Published.Limiting.Capacity,
         #                   Unpublished.Limiting.Capacity,
         #                   Uneffective.Limiting.Capacity),
         cl_rf_id  = rleid(Planned.Cluster.Capacity,
                           Published.Cluster.Capacity,
                           Unpublished.Cluster.Capacity,
                           Uneffective.Cluster.Capacity),
         cl_nu_id = rleid(Planned.Unavailable.Nuclear.Capacity,
                          Published.Unavailable.Nuclear.Capacity,
                          Unpublished.Unavailable.Nuclear.Capacity,
                          Uneffective.Unavailable.Nuclear.Capacity)
  )

#j'ai repere un probleme avec rf_cluster
#des indispos peuvent dépasser le seuil de 50 que j'ai fixé selon les heures
#donc l'indispo Cluster Capacity varie artificiellement
#et crèe de la variabilité artificielle en faisant varier le numéro de cluster
#voir l'exemple

# View(final_indispo_tot_fr %>%
#        mutate(date = as.Date(datetime_slot)) %>%  # Extraire la date
#        filter(date == "2015-01-02") %>%
#        select(Cluster.Capacity, datetime_slot, publication_status))

# View(final_indispo_tot_fr %>%
#        mutate(date = as.Date(datetime_slot)) %>%  # Extraire la date
#        filter(date == "2015-01-02") %>%
#        group_by(date, publication_status) %>%  # Grouper par jour
#        mutate(transitions = Cluster.Capacity != lag(Cluster.Capacity, default = first(Cluster.Capacity))) %>%
#        summarise(
#          n_unique_values = n_distinct(Cluster.Capacity, na.rm = TRUE),  # Count distinct values
#          n_transitions = sum(transitions, na.rm = TRUE),
#          Cluster.Capacity = max(Cluster.Capacity, na.rm = TRUE)
#        ) %>%  # Fixer à la valeur max du jour
#        ungroup()
# )

# View(long_final_indispo_frpred %>% 
#        mutate(day = as.Date(datetime_slot)) %>% 
#        filter(day == as.Date("2015-01-02")) %>% 
#        arrange(datetime_slot) %>% 
#        filter(publication_status == "Planned"))


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr")

write_parquet(final_indispo_tot_wide_fr, "final_indispo_tot_wide_fr.parquet")
final_indispo_tot_wide_fr <- read_parquet("final_indispo_tot_wide_fr.parquet")



load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/fr"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
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

price_indispo_merge_fr <- left_join(day_ahead_prices,
                                    final_indispo_tot_wide_fr,
                                    by = c("datetime" = "datetime_slot"))



#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast"
setwd(save_path_data)

renewable_forecasts_fr <- read_parquet('renewable_forecasts_fr.parquet')

colnames(renewable_forecasts_fr)
# gen_tot <- renewable_forecasts_fr %>%
#   dplyr::select(`__index_level_0__`, gen_tot, day_ahead_forecast,
#          unpredicted_renewables, intraday_update_renewables)

#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

price_indispo_merge_fr  <- price_indispo_merge_fr %>%
  left_join(renewable_forecasts_fr, by=c('datetime' = '__index_level_0__'))



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
temperature_openmeteo_data <- read_parquet("temperature_openmeteo_data_fr.parquet")
colnames(temperature_openmeteo_data)

#question sou-jacente
#quelle est la temporalité d'ajustement au froid

mean_temperatures_hour <- temperature_openmeteo_data %>% 
  mutate(week = week(date),
         hour = hour(date)) %>%
  group_by(week, hour) %>% 
  summarise(
    mean_temp = mean(as.numeric(temperature_2m), na.rm = TRUE),
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
  rename(hourly_mean_temp = mean_temp
  ) %>%
  mutate(
    cold_excess_instrument = -ifelse(month %in% cold_months, 
                                     temperature_2m - hourly_mean_temp, 0),
    heat_excess_instrument = ifelse(month %in% hot_months, 
                                    temperature_2m - hourly_mean_temp, 0)
    
  )



price_indispo_merge_fr <- price_indispo_merge_fr %>%
  left_join(temperature_openmeteo_data_test %>% 
              dplyr::select(-year, -month), by=c('datetime' = 'date'))


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


price_indispo_merge_fr_ahead <- price_indispo_merge_fr %>%
  left_join(gas_prices, by=c('delivery_date' = 'time'))
price_indispo_merge_fr_ahead <- price_indispo_merge_fr_ahead %>%
  fill(spot_gas, .direction = "up")
price_indispo_merge_fr_ahead <- price_indispo_merge_fr_ahead %>% distinct()

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
write_parquet(price_indispo_merge_fr_ahead, "price_indispo_merge_fr.parquet")



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

price_merge_CBF <- read_parquet("price_merge_CBF_max.parquet")

# Suppose que ton dataframe s'appelle df

# Colonnes sortantes depuis FR (vers un autre pays)
cols_sortant <- grep("^NTC_FR_", names(price_merge_CBF), value = TRUE)

# Colonnes entrantes vers FR : se terminent par _FR, incluent NTC et NTYC
cols_entrant <- grep("^NTC_.+_FR$", names(price_merge_CBF), value = TRUE)

# Créer deux nouvelles colonnes avec les totaux
price_merge_CBF$NTC_FR_sortant <- rowSums(price_merge_CBF[, cols_sortant], na.rm = TRUE)
price_merge_CBF$NTC_FR_entrant <- rowSums(price_merge_CBF[, cols_entrant], na.rm = TRUE)

colnames(price_merge_CBF)
crossborder_data <- price_merge_CBF %>% 
  filter(gen_tot_fr > 0 & gen_tot_esp > 0) %>%
  summarise(NTC_ES_FR = NTC_ES_FR,
            NTC_FR_ES = NTC_FR_ES,
            
            renewable_day_ahead_forecast_ex_es = renewable_day_ahead_forecast_pt +
              renewable_day_ahead_forecast_fr,
            cbf_es = CBF_ES_pred,
            NTC_FR_sortant = NTC_FR_sortant,
            NTC_FR_entrant = NTC_FR_entrant,
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


price_indispo_merge_fr_NTC <- merge(price_indispo_merge_fr_ahead, crossborder_data %>% 
                                       dplyr::select( NTC_FR_sortant, NTC_FR_entrant,
                                                      NTC_FR_ES, NTC_ES_FR,
                                                     renewable_day_ahead_forecast_ex_fr, cbf_fr,
                                                     datetime), 
                                     by ="datetime")


save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
write_parquet(price_indispo_merge_fr_NTC, "price_indispo_merge_fr_NTC.parquet")


