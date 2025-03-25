


first_stage_test_ter <- first_stage_data %>%
  group_by(delivery_date, creneau) %>%
  mutate(
    published_unavailability_change = (Published.Unavailable.Capacity != lag(Published.Unavailable.Capacity,
                                         default = first(Published.Unavailable.Capacity))),
    published_unavailability_change_capacity = Published.Unavailable.Capacity - 
      lag(Published.Unavailable.Capacity, default = first(Published.Unavailable.Capacity))
  )


trigger_data_ter <- first_stage_test_ter %>% 
  #je veux comparer lorsqu'il y a à la fois un trigger et pas de trigger 
  #pour la même séance d'enchères
  group_by(delivery_date, auction_before) %>% 
  filter(n_distinct(published_unavailability_change) == 2) %>%  # Garde seulement les groupes avec trigger 0 et 1
  group_by(delivery_date, auction_before, 
           published_unavailability_change, published_unavailability_change_capacity) %>% 
  summarise(
    log_price_response = mean(log_price_diff, na.rm = TRUE),
  ) %>% 
  ungroup()
trigger_data_ter_final <- trigger_data_ter[sapply(trigger_data_ter, function(col) !(is.infinite(col) | is.nan(col))) |> rowSums() == ncol(trigger_data_ter), ]
trigger_data_ter_final <- trigger_data_ter_final[complete.cases(trigger_data_ter_final), ]

trigger_model_ter <- lm(data = trigger_data_ter_final,
                    log_price_response ~ published_unavailability_change_capacity)
summary(trigger_model_ter)



colnames(first_stage_data)




first_stage_test_ter_predicted <- first_stage_data %>%
  group_by(delivery_date, creneau) %>%
  mutate(
    published_unavailability_change = (Published.Unavailable.Nuclear.Capacity != lag(Published.Unavailable.Nuclear.Capacity ,
                                                                             default = first(Published.Unavailable.Nuclear.Capacity ))),
    published_unavailability_change_capacity = Published.Unavailable.Nuclear.Capacity - 
      lag(Published.Unavailable.Nuclear.Capacity , default = first(Published.Unavailable.Nuclear.Capacity ))
  )


trigger_data_ter_predicted <- first_stage_test_ter_predicted %>% 
  #je veux comparer lorsqu'il y a à la fois un trigger et pas de trigger 
  #pour la même séance d'enchères
  group_by(delivery_date, auction_before) %>% 
  filter(n_distinct(published_unavailability_change) == 2) %>%  # Garde seulement les groupes avec trigger 0 et 1
  group_by(delivery_date, auction_before, 
           published_unavailability_change, published_unavailability_change_capacity) %>% 
  summarise(
    log_price_response = mean(log_price_diff, na.rm = TRUE),
  ) %>% 
  ungroup()
trigger_data_ter_predicted_final <- trigger_data_ter_predicted[sapply(trigger_data_ter_predicted, function(col) !(is.infinite(col) | is.nan(col))) |> rowSums() == ncol(trigger_data_ter_predicted), ]
trigger_data_ter_predicted_final <- trigger_data_ter_predicted_final[complete.cases(trigger_data_ter_predicted_final), ]

trigger_model_ter_predicted <- lm(data = trigger_data_ter_predicted_final,
                                  log_price_response ~ published_unavailability_change_capacity)
summary(trigger_model_ter_predicted)













