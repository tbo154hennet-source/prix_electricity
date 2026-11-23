
# I want to visualize auction price movements 
#when they are big unavailabilities
#take a few examples
library(ggplot2)
auction_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/intra_day_prices/OMIE-auction-market-data'

setwd(auction_path)
price_data <- read_parquet('auction_price_data_all.parquet')

# NB

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
final_publi_nuclear_ENTSOE_indispo <- read_parquet('final_publi_nuclear_ENTSOE_indispo.parquet') %>% 
  arrange(electricity_delivery_date, auction_before, creneau)

colnames(final_publi_nuclear_ENTSOE_indispo)

top_unavailabilities <- final_publi_nuclear_ENTSOE_indispo %>% 
  distinct(Unavailable.Capacity, electricity_delivery_date, .keep_all = TRUE ) %>% 
  mutate(electricity_delivery_date = as.Date(electricity_delivery_date, format = "%Y-%m-%d")) %>%
  filter(electricity_delivery_date < as.Date("2023-02-01") & electricity_delivery_date > as.Date("2021-01-01")) %>%
  arrange(desc(Unavailable.Capacity)) %>% # Sort in descending order
  slice_max(Unavailable.Capacity, n = 20) #

my_unavailability <- top_unavailabilities[1,]
View(my_unavailability)
my_date <- my_unavailability$electricity_delivery_date
my_date <- "2021-10-16" #"2021-05-19"  #"2021-10-16"




unavail_data <- final_publi_nuclear_ENTSOE_indispo %>%
  filter(electricity_delivery_date == my_date)
creneaux_concernes <- unique(unavail_data$creneau)

my_test <- first_stage_data  %>% 
  filter(delivery_date == my_date)

plot_data_group <- first_stage_data  %>% 
  filter(delivery_date == my_date)%>% 
  mutate(price = as.numeric(price),
         price = price - mean(price , na.rm = TRUE),
         Legend = ifelse(creneau %in% creneaux_concernes, "Unavailability", "Trend") ) %>% 
  group_by(Legend, auction) %>% 
  summarise(price_change = mean(price_diff, na.rm = TRUE),
            price = mean(price, na.rm = TRUE))


combined_plot <- ggplot() +
  # Prices on the positive axis
  geom_point(data =plot_data_group , aes(x = auction, y = price_change, color= Legend, group = Legend), size = 1) +
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
    name = "Price Change ($) "
  ) +
  theme(legend.position = "top")

combined_plot


#On veut comparer les créneaux affectés 
#avec les créneaux non-affectés par l'annonce

#on prend maintenant la différence entre les deux
library(dplyr)
df_wide <- plot_data_group %>%
  select(-price) %>% 
  pivot_wider(names_from = Legend, values_from = price_change)

# Compute the difference (Trend - Unavailability)
df_wide <- df_wide %>%
  mutate(Difference = Unavailability - Trend) %>% 
  ungroup()

df_wide$auction <- as.numeric(df_wide$auction)


diff_plot <- ggplot() +
  # Prices on the positive axis
  geom_point(data =df_wide , aes(x = auction, y = Difference), size = 1) +
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
    name = "Price Change ($) "
  ) +
  theme(legend.position = "top")

diff_plot


#with published indispo

plot_data_group <- first_stage_data  %>% 
  mutate(price = as.numeric(price),
         Legend = ifelse(creneau %in% creneaux_concernes, "Unavailability", "Trend") ) %>% 
  group_by(delivery_date, creneau) %>% 
  mutate(
    #prix moyen sur toutes les encheres pour ce creneau
    day_price = mean(price, na.rm = TRUE),
    price_demeaned = price - day_price) %>%
  ungroup() %>% 
  group_by(Published.Unavailable.Capacity, auction, delivery_date) %>% 
  summarise(price = mean(price, na.rm = TRUE),
            price_change = mean(price_diff, na.rm = TRUE),
            price_demeaned = mean(price_demeaned, na.rm = TRUE) ,
            log_diff = mean(log_price_diff, na.rm = TRUE),
            n_creneau = n()
  ) %>% 
  arrange(delivery_date)

estimate_data <- plot_data_group %>% 
  ungroup() %>% 
  group_by(delivery_date, auction) %>%
  filter(n_distinct(Published.Unavailable.Capacity) > 1) %>%
  ungroup()

#with unpublished indispo


