

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








library(dplyr)
library(tidyr)

log_price_data_nuclear <- my_test_nuclear  %>% 
  filter(creneau_unavailability > 0) %>%
  group_by(electricity_delivery_date, creneau) %>%
  
  # Définir l'enchère de référence
  mutate(reference_auction = min(auction_before[unavailability_trigger == 1], na.rm = TRUE),
         unavail_level = min(Published.Unavailable.Nuclear.Capacity[unavailability_trigger == 1], na.rm = TRUE),
         unavail = unavail_level* unavailability_trigger) %>%
  
  # Calculer la distance de l'enchère
  mutate(distance_auction = as.numeric(auction_before) - as.numeric(reference_auction)) %>%
  
  # Filtrer pour ne garder que les distances entre -4 et 4
  filter(distance_auction >= -4 & distance_auction <= 4) %>%
  
  # Agréger les valeurs si plusieurs existent par (date, creneau, distance)
  group_by(electricity_delivery_date, creneau, distance_auction, unavail) %>%
  summarise(log_price_diff = mean(log_price_diff, na.rm = TRUE), 
            Published.Unavailable.Nuclear.Share = mean(Published.Unavailable.Nuclear.Share, na.rm = TRUE),.groups = "drop") %>%
  
  # Transformer en format large par distance_auction
  pivot_wider(names_from = distance_auction, 
              names_prefix = "log_price_", 
              values_from = log_price_diff)

table_coefficients_nuclear  <- data.frame()
for (col in colnames(log_price_data)[4:length(log_price_data)]){
  # Créer la formule pour la régression
  
  # Créer la formule en utilisant backticks pour le nom de colonne
  formula <- as.formula(paste("`" , col, "`" , " ~ unavail" , sep = ""))
  
  # Ajuster le modèle
  model <- lm(formula, data = log_price_data_nuclear)
  
  coefficients <- summary(model)$coefficients
  
  # Ajouter les coefficients au dataframe
  temp_df <- data.frame(variable = col,
                        estimate = coefficients[2, 1],  # Estimation du coefficient
                        std_error = coefficients[2, 2],  # Erreur standard
                        t_value = coefficients[2, 3],  # Valeur t
                        p_value = coefficients[2, 4])  # Valeur p
  
  # Ajouter les résultats à table_coefficients
  table_coefficients_nuclear  <- rbind(table_coefficients_nuclear , temp_df)
}


ggplot(data = table_coefficients_nuclear, aes( x = c(-4:4),y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*std_error,
                  ymax = estimate + 1.96*std_error), width = 0.2, alpha= 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #coord_flip() +
  labs(title = "Dynamic response to plan availability shock",
       x = "Auction",
       y = "EUR/MWh")
