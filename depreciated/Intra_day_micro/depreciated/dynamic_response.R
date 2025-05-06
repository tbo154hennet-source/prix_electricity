library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

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

trigger_data_final$product_term <- trigger_data_final$unavailability_trigger * trigger_data_final$Published.Unavailable.Capacity

trigger_model <- lm(data = trigger_data_final,
                    log_price_response ~ product_term)
summary(trigger_model)





log_price_data <- my_test %>% 
  filter(creneau_unavailability > 0) %>%
  group_by(electricity_delivery_date, creneau) %>%
  
  # Définir l'enchère de référence
  mutate(reference_auction = min(auction_before[unavailability_trigger == 1], na.rm = TRUE),
         unavail = min(Published.Unavailable.Capacity[unavailability_trigger == 1], na.rm = TRUE)) %>%
  
  # Calculer la distance de l'enchère
  mutate(distance_auction = as.numeric(auction_before) - as.numeric(reference_auction)) %>%
  
  # Filtrer pour ne garder que les distances entre -4 et 4
  filter(distance_auction >= -4 & distance_auction <= 4) %>%
  
  # Agréger les valeurs si plusieurs existent par (date, creneau, distance)
  group_by(electricity_delivery_date, creneau, distance_auction, unavail) %>%
  summarise(log_price_diff = mean(log_price_diff, na.rm = TRUE), .groups = "drop") %>%
  
  # Transformer en format large par distance_auction
  pivot_wider(names_from = distance_auction, 
              names_prefix = "log_price_", 
              values_from = log_price_diff)


#enlever les inf
log_price_data_final <- log_price_data[sapply(log_price_data,
                      function(col) !(is.infinite(col) | is.nan(col))) |> rowSums() == ncol(log_price_data), ]


table_coefficients <- data.frame()
for (col in colnames(log_price_data)[4:length(log_price_data)]){
  # Créer la formule pour la régression
  
  # Créer la formule en utilisant backticks pour le nom de colonne
  formula <- as.formula(paste("`" , col, "`" , " ~ unavail" , sep = ""))
  
  # Ajuster le modèle
  model <- lm(formula, data = log_price_data_final)
  
  coefficients <- summary(model)$coefficients
  
  # Ajouter les coefficients au dataframe
  temp_df <- data.frame(variable = col,
                        estimate = coefficients[2, 1],  # Estimation du coefficient
                        std_error = coefficients[2, 2],  # Erreur standard
                        t_value = coefficients[2, 3],  # Valeur t
                        p_value = coefficients[2, 4])  # Valeur p
  
  # Ajouter les résultats à table_coefficients
  table_coefficients <- rbind(table_coefficients, temp_df)
}


ggplot(data = table_coefficients, aes( x = c(-4:4),y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*std_error,
                  ymax = estimate + 1.96*std_error), width = 0.2, alpha= 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #coord_flip() +
  labs(title = "Dynamic response to plan availability shock",
       x = "Auction",
       y = "EUR/MWh")






