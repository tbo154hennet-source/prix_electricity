library(readxl)
library(tidyr)
library(dplyr)
library(arrow)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")

news_data_clustered <- read.csv("full_news_double_clustering_gas_restrict.csv")

news_data_clustered$datetime <- as.Date(format(as.Date(news_data_clustered$date, "%b %d, %Y"), "%Y-%m-%d"))

news_data_clustered$datetime <- ifelse( is.na(news_data_clustered$datetime), as.Date(format(as.Date(news_data_clustered$date, "%d %b %Y"), "%Y-%m-%d")), news_data_clustered$datetime)

news_data_clustered <- news_data_clustered %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime) %>% 
  select(-article)

colnames(news_data_clustered)
class(news_data_clustered)
nrow(news_data_clustered)
# duplicates <- news_data_clustered[duplicated(news_data_clustered), ]
# nrow(duplicates)


small_cluster_news_count <- news_data_clustered %>% 
  select(media, datetime, small_cluster, small_cluster_label) %>%
  distinct() %>% 
  group_by(datetime, small_cluster, small_cluster_label) %>% 
  summarise(news_per_day = n())

small_cluster_labels <- unique(news_data_clustered[, c("small_cluster_label", "small_cluster")])

full_dates <- tibble(datetime = seq(min(news_data_clustered$datetime, na.rm=TRUE),
                                    max(news_data_clustered$datetime, na.rm=TRUE), by = "day"))
# Créer toutes les combinaisons de dates et d'événements
full_small_cluster_dates <- full_dates %>%
  crossing(small_cluster_labels)

# Joindre avec les données de nouvelles
small_cluster_complete_data <- full_small_cluster_dates %>%
  left_join(small_cluster_news_count, by = c("datetime", "small_cluster_label", "small_cluster")) %>%
  replace_na(list(news_per_day = 0))


shock_data_small_cluster <- small_cluster_complete_data %>%
  group_by(small_cluster) %>% 
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news),
                        ptstat = (news_per_day -moving_avg_news)/moving_std_news
         
  ) %>% 
  ungroup()



small_info_cluster <- left_join(gas_data_shock, shock_data_small_cluster,
                          by = c("Date" = "datetime"))

small_info_cluster <- small_info_cluster %>%
  group_by(Date) %>%
  slice_max(news_per_day, with_ties = FALSE) %>%
  ungroup()

save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

write_parquet(info_cluster, "info_cluster.parquet")



# on s'interroge sur à quel point 
#il ne faut pas aussi prendre en compte les news du lendemain

#unification des sujets
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
options(warn = -1)
gas_data_shock <- read_parquet("gas_data_shock.parquet")


info_small_cluster <- left_join(gas_data_shock, shock_data_small_cluster,
                  by = c("Date" = "datetime"))




colnames(news_data_clustered)

high_cluster_news_count <- news_data_clustered %>% 
  select(media, datetime, high_cluster, high_cluster_label) %>%
  distinct() %>% 
  group_by(datetime, high_cluster, high_cluster_label) %>% 
  summarise(news_per_day = n())

high_cluster_labels <- unique(news_data_clustered[, c("high_cluster_label", "high_cluster")])

full_dates <- tibble(datetime = seq(min(news_data_clustered$datetime, na.rm=TRUE),
                                    max(news_data_clustered$datetime, na.rm=TRUE), by = "day"))
# Créer toutes les combinaisons de dates et d'événements
full_high_cluster_dates <- full_dates %>%
  crossing(high_cluster_labels)

# Joindre avec les données de nouvelles
high_cluster_complete_data <- full_high_cluster_dates %>%
  left_join(high_cluster_news_count, by = c("datetime", "high_cluster_label", "high_cluster")) %>%
  replace_na(list(news_per_day = 0))


shock_data_high_cluster <- high_cluster_complete_data %>%
  group_by(high_cluster) %>% 
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news),
         ptstat = (news_per_day -moving_avg_news)/moving_std_news
         
  ) %>% 
  ungroup()

info_high_cluster <- left_join(gas_data_shock, shock_data_high_cluster,
                               by = c("Date" = "datetime"))

info_high_cluster  <- info_high_cluster %>%
  group_by(Date) %>%
  slice_max(news_per_day, with_ties = FALSE) %>%
  ungroup()

save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

write_parquet(info_high_cluster, "info_high_cluster.parquet")

# je specifie les clusters à considérer
# à partir du label

high_cluster_selection <- c(2,3,9)

selected_cluster_news_count <- news_data_clustered %>% 
  select(media, datetime, high_cluster, high_cluster_label) %>%
  distinct() %>% 
  filter(high_cluster %in% high_cluster_selection)%>% 
  group_by(datetime) %>% 
  summarise(news_per_day = n())


full_dates <- tibble(datetime = seq(min(news_data_clustered$datetime, na.rm=TRUE),
                                    max(news_data_clustered$datetime, na.rm=TRUE), by = "day"))

# Joindre avec les données de nouvelles
selected_cluster_complete_data <- full_dates %>%
  left_join(selected_cluster_news_count, by = c("datetime")) %>%
  replace_na(list(news_per_day = 0))


shock_data_selected_cluster <- selected_cluster_complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news),
         ptstat = (news_per_day -moving_avg_news)/moving_std_news
         
  ) %>% 
  ungroup()

info_selected <- merge(gas_data_shock, shock_data_selected_cluster, 
                  by.x = "Date", by.y = "datetime" ) 

save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

write_parquet(info_selected, "info_selected.parquet")



