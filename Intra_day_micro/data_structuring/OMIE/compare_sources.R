
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/IIP_indispo/formatted'
setwd(save_path_esp)
final_indispo_IIP_data <- read_parquet("indispo_IIP_data.parquet")


save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE_unavailabilities/ES/formatted'
setwd(save_path_esp)
bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet")
View(bout_indispo_ENTSOE_data %>% 
  mutate(date = as.Date(datetime_debut)) %>%
  filter(production_resource_name == "ASCO 1" ) %>% 
    arrange(datetime_debut))

View(final_indispo_IIP_data %>% 
       mutate(date = as.Date(datetime_debut)) %>%
       filter(production_resource_name == "ASCO2") %>% 
       arrange(datetime_debut))



View(generation_per_plant %>% 
  mutate(date = as.Date(`__index_level_0__`)) %>%
  filter(date =="2022-07-27" | date =="2022-07-28" ) %>% 
  select(`ASCO 1`,`ASCO 2`, `__index_level_0__`))


View(price_diff_data %>% 
  mutate(date = as.Date(datetime)) %>%
  filter(date == "2021-04-24" )
     )

my_data <- price_diff_data %>% 
  mutate(date = as.Date(datetime)) %>%
  filter(date =="2022-07-28"  )
ggplot(my_data)+
  geom_line(aes(x = auction, y = as.numeric(price), color = creneau, group = creneau))+
  theme_minimal()+
  labs(title = "Plot Auction",
       x = "Auction",
       y = "Price")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

