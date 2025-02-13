library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
library(lmtest)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

non_duplicated_indispo <- read_parquet('non_duplicated_indispo.parquet')
non_duplicated_indispo <- unique(non_duplicated_indispo)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format")
long_format_indispo <- read_parquet('long_format_indispo.parquet') 

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
first_stage_data <- read_parquet('first_stage_data.parquet') 




first_stage_test_bis <- first_stage_data %>%
  mutate(published_unavailability_creneau = (Published.Unavailable.Share >0)) %>% 
  group_by(delivery_date, creneau) %>%
  mutate(creneau_unavailability = sum(published_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  group_by(delivery_date) %>%
  mutate(day_unavailability = sum(published_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  arrange(datetime)

my_test <- first_stage_test_bis %>% 
  filter(day_unavailability > 0) %>% 
  arrange(datetime)
my_test <- my_test %>% 
  group_by(electricity_delivery_date, creneau) %>%
  mutate(unavailability_trigger = 
           ifelse(auction_before ==0, published_unavailability_creneau, published_unavailability_creneau - 
                    lag(published_unavailability_creneau))) %>% 
  ungroup()


#même créneau des avec et sans trigger

trigger_data <- my_test %>% 
  #je veux comparer lorsqu'il y a à la fois un trigger et pas de trigger 
  #pour la même séance d'enchères
  group_by(delivery_date, auction_before) %>% 
  filter(n_distinct(unavailability_trigger) == 2) %>%  # Garde seulement les groupes avec trigger 0 et 1
  group_by(delivery_date, auction_before, unavailability_trigger, Published.Unavailable.Capacity) %>% 
  summarise(
    log_price_response = mean(log_price_diff, na.rm = TRUE),
    Published.Unavailable.Share = mean(Published.Unavailable.Share, na.rm = TRUE),
    count = n()
  ) %>% 
  ungroup()


# le poblème de trigger data c'est que ce sontsouvent
# un créneau contre 10 autres
#mauvaise comparabilité des deux groupes
#avec du bruit 

trigger_data_final <- trigger_data[sapply(trigger_data, function(col) !(is.infinite(col) | is.nan(col))) |> rowSums() == ncol(trigger_data), ]
trigger_data_final <- trigger_data_final[complete.cases(trigger_data_final), ]
trigger_data_final <-trigger_data_final %>% 
  group_by(delivery_date) %>%
  filter(n_distinct(unavailability_trigger) >= 2) %>% 
  ungroup()# Garde seulement les groupes avec trigger 0 et 1

trigger_data_final$published_unavailability_diff <- trigger_data_final$unavailability_trigger *
  trigger_data_final$Published.Unavailable.Share
trigger_model <- lm(data = trigger_data_final,
                    log_price_response ~ published_unavailability_diff)
summary(trigger_model)

#beaucoup d'indispos sont a priori not binding d'après le rf algo
# et donc zero predicted indispo during the data
#je rajoute du bruit dans l'estimation
#get rid fo these rows
first_stage_test_predicted <- first_stage_data %>%
  mutate(predicted_unavailability_creneau = (Predicted.Unavailable.Share >0)) %>% 
  group_by(delivery_date, creneau) %>%
  mutate(creneau_unavailability = sum(predicted_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  group_by(delivery_date) %>%
  mutate(day_unavailability = sum(predicted_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  arrange(datetime)

trigger_data_predicted <- first_stage_test_predicted %>% 
  filter(day_unavailability > 0) %>% 
  arrange(datetime) %>% 
  group_by(electricity_delivery_date, creneau) %>%
  mutate(unavailability_trigger = 
           ifelse(auction_before ==0, predicted_unavailability_creneau, predicted_unavailability_creneau - 
                    lag(predicted_unavailability_creneau))) %>% 
  ungroup() %>% 
  # je veux comparer lorsqu'il y a à la fois un trigger et pas de trigger
  # pour la même séance d'enchères
  group_by(delivery_date, auction_before) %>%
  filter(n_distinct(unavailability_trigger) == 2) %>%  # Garde seulement les groupes avec trigger 0 et 1
  group_by(delivery_date, auction_before, unavailability_trigger) %>%
  summarise(
    log_price_response = mean(log_price_diff, na.rm = TRUE),
    gen_tot = mean(gen_tot, na.rm = TRUE),
    Predicted.Unavailable.Share = mean(Predicted.Unavailable.Share, na.rm = TRUE),
    count = n()
  ) %>%
  ungroup()

trigger_data_predicted  <- trigger_data_predicted [sapply(trigger_data_predicted ,
                function(col) !(is.infinite(col) | is.nan(col))) |> rowSums() == ncol(trigger_data_predicted ), ]
trigger_data_predicted  <- trigger_data_predicted[complete.cases(trigger_data_predicted ), ]
trigger_data_predicted  <-trigger_data_predicted  %>% 
  group_by(delivery_date) %>%
  filter(n_distinct(unavailability_trigger) >= 2) %>% 
  ungroup()# Garde seulement les groupes avec trigger 0 et 1

trigger_data_predicted$predicted_unavailability_diff <- trigger_data_predicted$unavailability_trigger *
  trigger_data_predicted$Predicted.Unavailable.Share

trigger_model_predicted <- lm(data = trigger_data_predicted,
                    log_price_response ~ predicted_unavailability_diff + gen_tot)

summary(trigger_model_predicted)
# Load the lmtest package if it's not already loaded





save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

non_duplicated_nuclear_indispo <- read_parquet('non_duplicated_indispo.parquet') %>% 
  filter(as.Date(electricity_delivery_date) >= as.Date('2018-01-01')) %>% 
  filter(plant_type == 'Nuclear')


non_duplicated_nuclear_indispo <- unique(non_duplicated_nuclear_indispo)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format")

long_format_nuclear_indispo <- read_parquet('long_format_nuclear_indispo.parquet') 

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
first_stage_data <- read_parquet('first_stage_data.parquet') 




first_stage_test_bis_nuclear <- first_stage_data %>%
  mutate(published_nuclear_unavailability_creneau = (Published.Unavailable.Nuclear.Share >0)) %>% 
  group_by(delivery_date, creneau) %>%
  mutate(creneau_unavailability = sum(published_nuclear_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  group_by(delivery_date) %>%
  mutate(day_unavailability = sum(published_nuclear_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  arrange(datetime)

my_test_nuclear <- first_stage_test_bis_nuclear %>% 
  filter(day_unavailability > 0) %>% 
  arrange(datetime)



#il faut regarder si l'on est un seuil
# à savoir si le même creneau  de l'auction suivante 
# a une indisponibilité différente

my_test_nuclear <- my_test_nuclear %>% 
  group_by(electricity_delivery_date, creneau) %>%
  mutate(unavailability_trigger = 
           ifelse(auction_before ==0, published_nuclear_unavailability_creneau, published_nuclear_unavailability_creneau - 
                    lag(published_nuclear_unavailability_creneau))) %>% 
  ungroup()




#même créneau des avec et sans trigger

trigger_data_nuclear <- my_test_nuclear %>% 
  #je veux comparer lorsqu'il y a à la fois un trigger et pas de trigger 
  #pour la même séance d'enchères
  group_by(delivery_date, auction_before) %>% 
  filter(n_distinct(unavailability_trigger) == 2) %>%  # Garde seulement les groupes avec trigger 0 et 1
  group_by(delivery_date, auction_before, unavailability_trigger, Published.Unavailable.Nuclear.Capacity) %>% 
  summarise(
    log_price_response = mean(log_price_diff, na.rm = TRUE),
    gen_tot = mean(gen_tot, na.rm = TRUE),
    Published.Unavailable.Nuclear.Share = mean(Published.Unavailable.Nuclear.Share, na.rm = TRUE),
    count = n()
  ) %>% 
  ungroup()


# le poblème de trigger data c'est que ce sontsouvent
# un créneau contre 10 autres
#mauvaise comparabilité des deux groupes
#avec du bruit 


# trigger_data_analysis_nuclear <- trigger_data_nuclear %>% 
#   group_by(delivery_date, auction_before) %>% 
#   mutate(
#     log_price_response_diff = log_price_response[unavailability_trigger == 1] - log_price_response[unavailability_trigger == 0]
#   ) %>% 
#   ungroup() %>%
#   select(delivery_date, auction_before, log_price_response_diff) %>% 
#   distinct()

trigger_data_nuclear$published_nuclear_unavailability_diff <- trigger_data_nuclear$unavailability_trigger *
  trigger_data_nuclear$Published.Unavailable.Nuclear.Share

trigger_model_nuclear <- lm(data = trigger_data_nuclear,
                            log_price_response ~ published_nuclear_unavailability_diff + gen_tot)
summary(trigger_model_nuclear)


getwd()
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)
write_parquet(my_test, "compare_creneaux.parquet")
write_parquet(my_test_nuclear, "compare_creneaux_nuclear.parquet")



