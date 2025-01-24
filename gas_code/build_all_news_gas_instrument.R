library(dplyr)
library(arrow)
library(tidyr)
library(ggplot2)
library(zoo)

# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
# Set locale to English (ensure month names are understood)
Sys.setlocale("LC_TIME", "C")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
gas_data_shock <- read_parquet("gas_data_shock.parquet")


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/all_news_approach')


news_data_all <- read_parquet("store_full_news_data.parquet")


news_data_all$datetime <- as.Date(format(as.Date(news_data_all$date, "%b %d, %Y"), "%Y-%m-%d"))

news_data_all$datetime <- ifelse( is.na(news_data_all$datetime), as.Date(format(as.Date(news_data_all$date, "%d %b %Y"), "%Y-%m-%d")), news_data_all$datetime)

news_data_all <- news_data_all %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime) %>% 
  select(title, datetime, event_type)

full_news_count_all <- news_data_all %>% 
  select(media, datetime, title) %>%
  distinct() %>% 
  group_by(datetime) %>% 
  summarise(news_per_day = n())

event_news_count_all <- news_data_all %>% 
  select(media, datetime, event_type, title) %>%
  distinct() %>% 
  group_by(datetime, event_type) %>% 
  summarise(news_per_day = n())

supply_news_count_all <- event_news_count_all %>% 
  pivot_wider(names_from = event_type, values_from = news_per_day) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  mutate(day_type = ifelse(supply < 3*demand, "ambiguous", "supply")) %>% 
  select(supply, demand, day_type)

# Fusionner le DataFrame original avec la séquence complète de dates
complete_data_all <- full_dates %>%
  left_join(supply_news_count_all , by = "datetime") %>%
  rename(supply_news = supply, demand_news = demand) %>%
  replace_na(list(supply_news = 0, demand_news = 0))


shock_data_supply_all <- complete_data_all %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(supply_news, width = 30, FUN = mean, fill = NA),
         moving_std_news = rollapply(supply_news, width = 30, FUN = sd, fill = NA),
         binary_shock= (supply_news >  moving_avg_news + 1.9*moving_std_news),
         moving_avg_news_demand = rollapply(demand_news, width = 30, FUN = mean, fill = NA),
         moving_std_news_demand = rollapply(demand_news, width = 30, FUN = sd, fill = NA),
         demand_shock= (demand_news >  moving_avg_news_demand + 1.9*moving_std_news_demand)
  )


info_supply_all <- merge(gas_data_shock, shock_data_supply_all, 
                         by.x = "Date", by.y = "datetime" ) 


save_compare_info <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/info_files"
setwd(save_compare_info)

write_parquet(info_supply_all, "info_supply_all.parquet")

save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)
demand_news_days_all <- info_all %>% 
  filter(demand_shock == TRUE & demand_news >2 ) %>% 
  pull(Date)

write.csv(demand_news_days_all, "demand_news_days_all.csv")

instrument_days_supply_all <- info_supply_all %>% 
  filter(binary_shock ==TRUE) %>% 
  filter(supply_news > 2) %>% 
  filter(day_type == "supply")


nrow(instrument_days_supply_all)

sd(instrument_days_supply_all$PC1, na.rm=TRUE)


