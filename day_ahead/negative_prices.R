


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet( "price_indispo_merge_esp.parquet")

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
day_ahead_prices <- read_parquet( "day_ahead_prices.parquet")

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/fr")
price_indispo_merge_fr <- read_parquet( "price_indispo_merge_fr.parquet")

library(ggplot2)

ggplot(data = day_ahead_prices, aes(x = price)) +   # replace Price with your column name
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram of Prices",
    x = "Price",
    y = "Count"
  ) +
  theme_minimal()

price_histo_fr <- ggplot(data = price_indispo_merge_fr , aes(x = price)) +   # replace Price with your column name
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram of Prices",
    x = "Price",
    y = "Count"
  ) +
  theme_minimal()

price_histo_fr

colnames(price_indispo_merge)
#get the share of negative prices

price_indispo_merge_esp %>% 
  group_by(price < 0) %>% 
  summarise(n = n())

price_indispo_merge_fr %>% 
  group_by(price < 0) %>% 
  summarise(n = n())

price_indispo_merge_fr %>%
  summarise(
    total = n(),
    negative = sum(price < 0),
    share_negative = negative / total
  )

price_indispo_merge_fr %>%
  filter(year == 2022) %>% 
  summarise(
    total = n(),
    negative = sum(price < 0),
    share_negative = negative / total
  )

#max negative prices share is 1.7% for 2023
#very recent phenomenon 0.04 % in 2022
#for france

#for spain the first negative prices are in April 2024 
#that is after the beginning of my study period
