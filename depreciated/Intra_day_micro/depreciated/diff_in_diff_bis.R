
save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

non_duplicated_indispo <- read_parquet('non_duplicated_indispo.parquet') %>% 
  filter(as.Date(electricity_delivery_date) >= as.Date('2018-01-01')) %>% 
  filter(plant_type == 'Nuclear')


non_duplicated_indispo <- unique(non_duplicated_indispo)


setwd("Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format")

long_format_nuclear_indispo <- read_parquet('long_format_nuclear_indispo.parquet') 



save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
first_stage_data <- read_parquet('first_stage_data.parquet') 




first_stage_test_bis <- first_stage_data %>%
  mutate(published_nuclear_unavailability_creneau = (Published.Unavailable.Nuclear.Share >0)) %>% 
  group_by(delivery_date, creneau) %>%
  mutate(creneau_unavailability = sum(published_nuclear_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  group_by(delivery_date) %>%
  mutate(day_unavailability = sum(published_nuclear_unavailability_creneau, na.rm = TRUE))%>%
  ungroup() %>% 
  arrange(datetime)

my_test <- first_stage_test_bis %>% 
  filter(day_unavailability > 0) %>% 
  arrange(datetime)



#il faut regarder si l'on est un seuil
# à savoir si le même creneau  de l'auction suivante 
# a une indisponibilité différente

my_test <- my_test %>% 
  group_by(electricity_delivery_date, creneau) %>%
  mutate(unavailability_trigger = 
           ifelse(auction_before ==0, published_nuclear_unavailability_creneau, published_nuclear_unavailability_creneau - 
              lag(published_nuclear_unavailability_creneau))) %>% 
  ungroup()




#même créneau des avec et sans trigger

trigger_data <- my_test %>% 
  #je veux comparer lorsqu'il y a à la fois un trigger et pas de trigger 
  #pour la même séance d'enchères
  group_by(delivery_date, auction_before) %>% 
  filter(n_distinct(unavailability_trigger) == 2) %>%  # Garde seulement les groupes avec trigger 0 et 1
  group_by(delivery_date, auction_before, unavailability_trigger) %>% 
  summarise(
    log_price_response = mean(log_price_diff, na.rm = TRUE),
    count = n()
  ) %>% 
  ungroup()


# le poblème de trigger data c'est que ce sontsouvent
# un créneau contre 10 autres
#mauvaise comparabilité des deux groupes
#avec du bruit 




trigger_data_analysis <- trigger_data %>% 
  group_by(delivery_date, auction_before) %>% 
  mutate(
    log_price_response_diff = log_price_response[unavailability_trigger == 1] - log_price_response[unavailability_trigger == 0]
  ) %>% 
  ungroup() %>%
  select(delivery_date, auction_before, log_price_response_diff) %>% 
  distinct()


trigger_model <- lm(data = trigger_data,
   log_price_response ~ unavailability_trigger)
summary(trigger_model)


effect <- my_test %>% 
  filter(delivery_date =="2023-04-28")

View(my_test %>% 
       filter(delivery_date =="2023-04-28") %>% 
       filter(unavailability_trigger == 0 & auction_before == 3))
View(my_test %>% 
       filter(delivery_date =="2023-04-28") %>% 
       filter(unavailability_trigger == 1))


library(ggplot2)
effect$unavailability_trigger <- factor(effect$unavailability_trigger, levels = c(-1, 0, 1))

ggplot(effect, aes(x = auction_before, y = log_price_diff, color = unavailability_trigger, group = creneau)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Impact of nuclear unavailability on prices",
       x = "Auction before",
       y = "Log price difference") +
  scale_color_manual(values = c("red", "blue", "green"))


# long_format_nuclear_indispo %>% 
#   filter(electricity_delivery_date == "2020-12-02") 
# View(first_stage_data %>% 
#   filter(electricity_delivery_date == "2020-12-02" &
#            Published.Unavailable.Nuclear.Capacity >0) )
# 
# long_format_nuclear_indispo %>% 
#   filter(electricity_delivery_date == "2018-11-09") 


