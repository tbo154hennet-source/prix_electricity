

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

non_duplicated_indispo <- read_parquet('non_duplicated_indispo_credible.parquet') %>% 
  filter(as.Date(electricity_delivery_date) >= as.Date('2018-01-01')) %>% 
  filter(plant_type == 'Nuclear')


class(non_duplicated_indispo$electricity_delivery_date)
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)


first_stage_data <- read_parquet('first_stage_data.parquet') %>% 
  filter(auction_after != 7)

#double diff in diff method
colnames(first_stage_data)


#diff in the creneaux class concernées vs non concernées
#diff in auction before information shock and after

first_stage_test <- first_stage_data %>%
  mutate(published_nuclear_unavailability_creneau = (Published.Unavailable.Nuclear.Share >0)) %>% 
  group_by(delivery_date) %>%
  mutate(day_unavailability = sum(published_nuclear_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  filter(day_unavailability > 0)


first_stage_test_2 <- first_stage_data %>%
  # Créer une variable logique indiquant si la disponibilité nucléaire a été publiée
  mutate(published_nuclear_unavailability_creneau = (Published.Unavailable.Nuclear.Share > 0)) %>% 
  
  # Grouper par la date de livraison
  group_by(delivery_date) %>%
  
  # Calculer day_unavailability pour les heures antérieures
  mutate(
    day_unavailability = sapply(
      seq_along(published_nuclear_unavailability_creneau),
      function(i) sum(published_nuclear_unavailability_creneau[1:i], na.rm = TRUE)
    )
  ) %>%
  
  # Dégrouper
  ungroup() %>% 
  
  # Filtrer uniquement les observations avec des indisponibilités publiées le jour même avant l'heure actuelle
  filter(day_unavailability > 0)






first_stage_test %>% 
  distinct(published_nuclear_unavailability_creneau, delivery_date, auction_before) %>% 
  nrow()

first_stage_reg_data <- first_stage_test %>% 
  group_by(delivery_date, published_nuclear_unavailability_creneau, auction_before) %>%
  summarise(price_avg = mean(as.numeric(price), na.rm = TRUE),
         log_price_change = mean(log_price_diff, na.rm = TRUE))



first_stage_diff_data <- first_stage_reg_data %>%
  group_by(delivery_date, auction_before) %>%
  summarise(
    count_true = sum(published_nuclear_unavailability_creneau == TRUE, na.rm = TRUE),
    count_false = sum(published_nuclear_unavailability_creneau == FALSE, na.rm = TRUE),
    difference = 
      mean(log_price_change[published_nuclear_unavailability_creneau == TRUE], na.rm = TRUE) - 
      mean(log_price_change[published_nuclear_unavailability_creneau == FALSE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(count_true > 0 & count_false > 0)  # Keep only rows where both TRUE and FALSE are present


unavailabilities <- first_stage_data %>%
  filter(Published.Unavailable.Nuclear.Share >0) %>% 
  distinct(delivery_date, Published.Unavailable.Nuclear.Capacity, .keep_all = TRUE) %>% 
  select(auction_before, delivery_date, Published.Unavailable.Nuclear.Capacity,
         Published.Unavailable.Nuclear.Share)


Did_data <- first_stage_diff_data %>%
  left_join(unavailabilities)
colnames(Did_data)

Did_data_pre_2021 <- Did_data %>% 
  filter(delivery_date < as.Date('2021-01-01'))
Did_data_post_2021 <- Did_data %>% 
  filter(delivery_date  >= as.Date('2021-01-01'))



m_did_pre <- lm(
  difference ~  Published.Unavailable.Nuclear.Share,
  data= Did_data_pre_2021)

m_did_post <- lm(
  difference ~  Published.Unavailable.Nuclear.Share,
  data= Did_data_post_2021)
summary(m_did_pre)
summary(m_did_post)
