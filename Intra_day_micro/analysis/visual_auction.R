
# I want to visualize auction price movements 
#when they are big unavailabilities
#take a few examples
library(ggplot2)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
final_publi_nuclear_ENTSOE_indispo <- read_parquet('final_publi_nuclear_ENTSOE_indispo.parquet') %>% 
  arrange(electricity_delivery_date, auction_before, creneau)

colnames(final_publi_nuclear_ENTSOE_indispo)

top_unavailabilities <- final_publi_nuclear_ENTSOE_indispo %>% 
  distinct(Unavailable.Capacity, electricity_delivery_date, .keep_all = TRUE ) %>% 
  mutate(electricity_delivery_date = as.Date(electricity_delivery_date, format = "%Y-%m-%d")) %>%
  filter(electricity_delivery_date < as.Date("2023-02-01") & electricity_delivery_date > as.Date("2021-01-01")) %>%
  filter(auction_before!= 6 | auction_after!= 2) %>% 
  arrange(desc(Unavailable.Capacity)) %>% # Sort in descending order
  slice_max(Unavailable.Capacity, n = 20) #

my_unavailability <- top_unavailabilities[1,]

my_date <- my_unavailability$electricity_delivery_date
my_date <- "2021-05-18" #"2022-05-31"   #"2021-12-08" #"2021-05-19"  #"2021-10-16"


colnames(final_publi_nuclear_ENTSOE_indispo)

unavail_data <- final_publi_nuclear_ENTSOE_indispo %>%
  filter(electricity_delivery_date == my_date)
creneaux_concernes <- unique(unavail_data$creneau)

my_test <- price_data  %>% 
  filter(delivery_date == my_date)

plot_data_group <- price_data  %>% 
  filter(delivery_date == my_date)%>% 
  mutate(price = as.numeric(price),
         #price = price - mean(price , na.rm = TRUE),
         Legend = ifelse(creneau %in% creneaux_concernes, "Unavailability", "Trend") ) %>% 
  group_by(Legend, auction) %>% 
  summarise(avg_price_change = mean(price, na.rm = TRUE))


combined_plot <- ggplot() +
  # Prices on the positive axis
  geom_line(data =plot_data_group , aes(x = auction, y = avg_price_change, color= Legend, group = Legend), size = 1) +
  # Unavailabilities on the negative axis
  geom_col(data = unavail_data, aes(x = as.numeric(auction_before) + 0.5, y = Unavailable.Capacity/1000), 
           fill = "red", alpha = 0.7, width = 0.5) +
  # Labels and themes
  labs(title = "Prices and Announced Unavailabilities",
       x = "Auction",
       y = "Values") +
  theme_minimal() +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 1000, name = "Unavailable Capacity (MWh)"),
    name = "Price Change"
  ) +
  theme(legend.position = "top")

combined_plot


#On veut comparer les créneaux affectés 
#avec les créneaux non-affectés par l'annonce

#on le fait donc par annonce




plot_data <- price_data  %>% 
  filter(delivery_date == my_date)%>% 
  mutate(price = as.numeric(price),
         price = price - mean(price , na.rm = TRUE),
         Legend = creneau %in% creneaux_concernes)

combined_plot <- ggplot() +
  # Prices on the positive axis
  geom_line(data = plot_data, aes(x = auction, y = price, color=  creneau, group =  creneau), size = 1) +
  # Unavailabilities on the negative axis
  geom_col(data = unavail_data, aes(x = as.numeric(auction_before) + 0.5, y = -Unavailable.Capacity/1000), 
           fill = "red", alpha = 0.7, width = 0.5) +
  # Labels and themes
  labs(title = "Prices and Announced Unavailabilities",
       x = "Auction",
       y = "Values") +
  theme_minimal() +
  scale_y_continuous(sec.axis = dup_axis(), name = "Unavailable Capacity GWh ") +
  theme(legend.position = "top")

combined_plot

# save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
# setwd(save_path_entsoe_output)
# 
# actual_ENTSOE_indispo <- read_parquet('actual_ENTSOE_indispo.parquet')
# 
# colnames(actual_ENTSOE_indispo)
# my_test <- actual_ENTSOE_indispo %>% 
#   filter(electricity_delivery_date == my_date)





