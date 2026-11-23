





save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
first_stage_data <- read_parquet('first_stage_data.parquet') 


# on va pondérer par une mesure multiplicative n_creneau_unavail * n_creneau_trend
#pour savoir si c'est très représentatif ou pas





first_stage_test_bis_nuclear <- first_stage_data %>%
  mutate(nuclear_unavailability_creneau = (Published.Unavailable.Nuclear.Share >0)) %>% 
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


colnames(my_test_nuclear)
log_price_data_nuclear_grouped_gauche <- my_test_nuclear  %>% 
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
            reference_auction = first(reference_auction),
            Published.Unavailable.Nuclear.Capacity = mean(Published.Unavailable.Nuclear.Capacity, na.rm = TRUE),.groups = "drop") %>%
  
  # Transformer en format large par distance_auction
  pivot_wider(names_from = distance_auction, 
              names_prefix = "log_price_", 
              values_from = log_price_diff)


#il y a souvent annonce, puis désannonce (à savoir fin de l'indisponibilité à un créneau suivant)
#attention je perds de la variation




log_price_data_nuclear_grouped <- log_price_data_nuclear %>%  
  arrange(electricity_delivery_date, creneau, unavail) %>% 
  group_by(electricity_delivery_date, creneau) %>%
  summarize(across(starts_with("log_price"), ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),
            creneau = first(creneau),
            reference_auction = first(reference_auction),
            unavail = last(unavail),
            electricity_delivery_date = first(electricity_delivery_date),
            .groups = "drop")



table_coefficients_nuclear  <- data.frame()

for (col in colnames(log_price_data_nuclear_grouped)[3:10]){
  # Créer la formule pour la régression
  
  # Créer la formule en utilisant backticks pour le nom de colonne
  formula <- as.formula(paste("`" , col, "`" , " ~ unavail" , sep = ""))
  
  # Ajuster le modèle
  model <- lm(formula, data = log_price_data_nuclear_grouped)
  
  vcov_cluster <- vcovCL(model, type = "HC0", cluster = ~ electricity_delivery_date)
  # Erreurs standard clusterisées
  cl_se<- sqrt(diag(vcov_cluster))
  
  
  coefficients <- summary(model)$coefficients
  n_ddl <- summary(model)$df[2]
  # Créer un dataframe temporaire avec les variables et les coefficients
  temp_df <- data.frame(variable = col,
                        estimate = coefficients[2, 1],  # Estimation du coefficient
                        std_error = cl_se[2],  # Erreur standard modifiée
                        t_value = coefficients[2, 1] / cl_se[2],  # Recalcul de la valeur t
                        p_value = 2 * (1 - pt(abs(coefficients[2, 1] / cl_se[2]), df = n_ddl)))  # Recalcul de la valeur p
  
  
  # Ajouter les résultats à table_coefficients
  table_coefficients_nuclear  <- rbind(table_coefficients_nuclear , temp_df)
}



ggplot(data = table_coefficients_nuclear, aes( x = c(-4:3),y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = estimate - 1.96*std_error,
                  ymax = estimate + 1.96*std_error), width = 0.2, alpha= 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #coord_flip() +
  labs(title = "Dynamic response to plan availability shock",
       x = "Auction",
       y = "EUR/MWh")



