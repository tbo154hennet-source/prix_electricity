
library(tidyr)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/rf'
setwd(save_path_entsoe_output)
rf_predictions_data_fr <- read_parquet('rf_predictions_data_fr.parquet') %>% 
  distinct() %>% 
  group_by(centrale, `__index_level_0__`) %>% 
  slice(1) %>% 
  ungroup()

colnames(rf_predictions_data_fr)

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
              select(publication_status, mrid, start_datetime,
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



long_indispo_limitantes <- long_final_indispo_fr %>%
  filter(Installed.Capacity!= Unavailable.Capacity) %>%
  filter(as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity) < centrale_prod*1.1) %>% 
  filter(previous_day_prod > (as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity))*0.9)


long_indispo_nuclear <- long_final_indispo_fr %>%
  filter(plant_type == "Nuclear")

long_final_indispo_frpred <- long_final_indispo_fr %>%
  mutate(Predicted.Unavailable.Capacity = - predicted_real_unavailability) %>% 
  filter(Predicted.Unavailable.Capacity > 50)


long_indispo_nuclear %>% 
  group_by(publication_status) %>% 
  summarise(n= n())

long_indispo_limitantes %>% 
  group_by(publication_status) %>% 
  summarise(n= n())

long_final_indispo_frpred %>% 
  group_by(publication_status) %>% 
  summarise(n= n())

library(data.table)

final_indispo_fr <- long_final_indispo_fr[,lapply(.SD, sum), 
                                        .SDcols = c("Unavailable.Capacity"),
                                        by = c("publication_status","datetime_slot")]

final_indispo_fr_limitantes <- long_indispo_limitantes[,lapply(.SD, sum), 
                                                        .SDcols = c("Unavailable.Capacity"),
                                                        by = c("publication_status", "datetime_slot")]


final_indispo_fr_rfpred <- long_final_indispo_frpred[,lapply(.SD, sum), 
                                                .SDcols = c("Predicted.Unavailable.Capacity", "Unavailable.Capacity"),
                                                by = c("publication_status", "datetime_slot")]


final_indispo_fr_nuclear <- long_indispo_nuclear[,lapply(.SD, sum), 
                                                  .SDcols = c("Unavailable.Capacity"),
                                                  by = c("publication_status", "datetime_slot")]


final_indispo_tot_fr <- left_join(final_indispo_fr , 
                               final_indispo_fr_limitantes%>% 
                                 rename(Limiting.Capacity = Unavailable.Capacity),
                               by = c("publication_status", "datetime_slot")) %>% 
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
  summarise(across(c(Unavailable.Nuclear.Capacity, Limiting.Capacity, 
                     Unavailable.Capacity, Predicted.Unavailable.Capacity, Cluster.Capacity), 
                   ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  pivot_wider(
    names_from = publication_status, 
    values_from = c(Unavailable.Nuclear.Capacity, Limiting.Capacity, 
                    Unavailable.Capacity, Predicted.Unavailable.Capacity, Cluster.Capacity), 
    names_glue = "{publication_status}.{.value}"
  )
setDT(final_indispo_tot_wide_fr)



final_indispo_tot_wide_fr <- final_indispo_tot_wide_fr %>% 
  mutate(cl_id  = rleid(Planned.Unavailable.Capacity,
                        Published.Unavailable.Capacity, 
                        Unpublished.Unavailable.Capacity,
                        Uneffective.Unavailable.Capacity),
         cl_lm_id  = rleid(Planned.Limiting.Capacity,
                            Published.Limiting.Capacity,
                           Unpublished.Limiting.Capacity,
                           Uneffective.Limiting.Capacity),
         cl_rf_id  = rleid(Planned.Cluster.Capacity,
                           Published.Cluster.Capacity,
                           Unpublished.Cluster.Capacity,
                           Uneffective.Cluster.Capacity)
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


load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/fr"
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
day_ahead_prices<- day_ahead_prices%>% 
  arrange(datetime) %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         delivery_date = as.Date(datetime),
         price = as.numeric(price),
         price_diff = price - lag(price),
         log_price_diff = log(price) - lag(log(price))
  )


price_indispo_merge_fr <- left_join(day_ahead_prices,
                                    final_indispo_tot_wide_fr,
                                 by = c("datetime" = "datetime_slot"))



#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast"
setwd(save_path_data)

renewable_forecasts_fr <- read_parquet('renewable_forecasts_fr.parquet')

colnames(renewable_forecasts_fr)
gen_tot <- renewable_forecasts_fr %>%
  select(`__index_level_0__`, gen_tot, day_ahead_forecast,
         unpredicted_renewables, intraday_update_renewables)

#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

price_indispo_merge_fr  <- price_indispo_merge_fr %>%
  left_join(gen_tot, by=c('datetime' = '__index_level_0__'))

# pour l'estimation en niveau,
#on rajoute le spot gas price

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp.parquet")



nrow(price_indispo_merge_fr)
nrow(price_indispo_merge_esp)

colnames(price_indispo_merge_fr)
colnames(price_indispo_merge_esp)

price_merge_CBF <- merge(price_indispo_merge_fr  %>% 
                           rename_with(~ paste0(., "_fr"), -c(datetime, year, month, day, delivery_date)),
                         price_indispo_merge_esp  %>% 
                           rename_with(~ paste0(., "_esp"), -c(datetime, year, month, day, delivery_date)),
                         by= c("datetime", "year", "month", "day", "delivery_date")
                         )

#exception ibérique
#Le 15 juin 2022, l’exception ibérique est entrée en vigueur, pour une durée initiale de 12 mois
#plafonne le prix du gaz utilisé dans la production d'électricité
#d'abord 55 euros
#puis extension un an plus tard jusqu'en décembre 2023 à 65 euros

# Si le dispositif a effectivement servi à amortir l'impact de la flambée des cours du gaz en 2022
# sur la note des consommateurs, il n'a de fait plus été activé depuis février 2023,
# lorsque les prix du marché gazier avaient retrouvé une certaine normalité.

class(price_merge_CBF$datetime)
exception_start_date <- as.POSIXct("15-06-2022 00:00:00", 
                             format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")
exception_end_date <- as.POSIXct("01-03-2023 00:00:00", 
                                   format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")
#exclure une valeur aberrante qui rend le graphique moins lisible

plot_data <- price_merge_CBF %>% 
  filter(price_fr < 1500)
library(ggplot2)
ggplot(plot_data)+
  geom_line(aes(x=datetime, y=price_fr, color = "France")) +
  geom_line(aes(x=datetime, y=price_esp, color = "Espagne")) +
  geom_vline(xintercept = exception_start_date, linetype = "dashed", color = "red") +
  geom_vline(xintercept = exception_end_date, linetype = "dashed", color = "red") +
  labs(title = "Day-ahead electricity prices in France and Spain",
       x = "Date", y = "Price (€/MWh)") +
  theme_minimal()


#regarder quelles sont les dates perdues dans le merge
#données 2024


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows")

cross_border_flows <- read_parquet("fr_esp_cross_border_flows.parquet")
colnames(cross_border_flows)


price_merge_CBF <- merge(price_merge_CBF, cross_border_flows, 
                         by.x ="datetime", by.y ="time")
getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

write_parquet(price_merge_CBF, "price_merge_CBF.parquet")




