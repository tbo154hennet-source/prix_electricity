library(readxl)
library(tidyr)
library(dplyr)
library(arrow)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)

# Function to find the closest posterior date
find_closest_posterior <- function(input_date, date_list) {
  # Ensure the input date is in Date format
  input_date <- as.Date(input_date)
  
  # Check if the input date is in the list
  if (input_date %in% date_list) {
    return(input_date)  # Return the input date if it's in the list
  } else {
    # Find the closest posterior date (greater than the input date)
    posterior_dates <- date_list[date_list > input_date]
    
    if (length(posterior_dates) > 0) {
      return(min(posterior_dates))  # Return the smallest (next) posterior date
    } else {
      return(NA)  # Return NA if no posterior date exists
    }
  }
}


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
gas_tot_futures <- read_parquet("gas_data_shock.parquet")
gas_tot_futures$datetime <- as.Date(gas_tot_futures$time)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news")
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")

news_data_clustered <- read.csv("labeled_news_double_clustering.csv")
#news_data_clustered$datetime <- as.Date(format(as.Date(news_data_clustered$date, "%b %d, %Y"), "%Y-%m-%d"))
#news_data_clustered$datetime <- ifelse( is.na(news_data_clustered$datetime), as.Date(format(as.Date(news_data_clustered$date, "%d %b %Y"), "%Y-%m-%d")), news_data_clustered$datetime)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/outputs')
instrument_days_2std <- read.csv("instrument_news_events_2std_bis.csv")
instrument_days_2std$Trading.day <- as.Date(instrument_days_2std$Trading.day, format = "%Y-%m-%d")

demand_news_days_2std <- instrument_days_2std %>% 
  filter(demand_binary_shock==1 & demand_news > 2 & supply_news < 2*demand_news) 


news_data_clustered_type <- news_data_clustered %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime) %>% 
  select(-c("article", "date")) %>% 
  distinct(title, media, .keep_all = TRUE)

# Define the keywords you want to exclude
exclude_keywords <- c("nut", "olive", "palm", "soy", "vegetable",
                      "seed", "edible", "coco", "tea","cooking",
                      "sunflower",  "food", "biofuel", "\\?",
                      "was", "were", "did", "would", "heating")
# Create a single pattern from the keywords
exclude_pattern <- paste(exclude_keywords, collapse = "|")
# Filter out rows where 'title' contains any of the exclude keywords
news_data_clustered_type <- news_data_clustered_type[!grepl(exclude_pattern, news_data_clustered_type$title, ignore.case = TRUE), ]


demand_keywords <- c("demand ", "inventory", "stocks", 
                     "slowdown", "recession", "consum", "covid", "coronavirus",
                     "economic growth", "economic recovery")
demand_pattern <- paste(demand_keywords, collapse = "|")

supply_keywords <- c("supply", "production", "Iraq","Iran", "Russia", "Ukraine","Israel",
                     "Libya", "crisis", "attack", "Qatar",
                     "violence", "conflict", "war ","reserves", "refinery", 
                     "pipeline","facility", "ban", "shale")
supply_pattern <- paste(supply_keywords, collapse = "|")



news_data_clustered_type <- news_data_clustered_type %>%
  mutate(
    supply_match = grepl(supply_pattern, title, ignore.case = TRUE),
    demand_match = grepl(demand_pattern, title, ignore.case = TRUE),
    day_type = case_when(
      supply_match & demand_match ~ "ambiguous_news",
      supply_match ~ "supply_news",
      demand_match ~ "demand_news",
      TRUE ~ "ambiguous_news"
    )
  ) %>%
  select(-supply_match, -demand_match)  # Supprime les colonnes temporaires


news_data_clustered_type %>% 
  group_by(day_type) %>% 
  summarise(news_per_day = n(), .groups = "drop")

  
colnames(news_data_clustered)
nrow(news_data_clustered)
# duplicates <- news_data_clustered[duplicated(news_data_clustered), ]
# nrow(duplicates)


small_cluster_news_count_type <- news_data_clustered_type %>% 
  select(media, datetime, small_cluster, small_cluster_label, day_type) %>%
  distinct() %>% 
  group_by(datetime, small_cluster, small_cluster_label, day_type) %>% 
  summarise(news_per_day = n(), .groups = "drop") %>%
  pivot_wider(names_from = day_type, values_from = news_per_day, values_fill = list(news_per_day = 0))  


small_cluster_labels <- unique(news_data_clustered_type[, c("small_cluster_label", "small_cluster")])

full_dates <- tibble(datetime = seq(min(news_data_clustered_type$datetime, na.rm=TRUE),
                                    max(news_data_clustered_type$datetime, na.rm=TRUE), by = "day"))
# Créer toutes les combinaisons de dates et d'événements
full_small_cluster_dates <- full_dates %>%
  crossing(small_cluster_labels)

# Joindre avec les données de nouvelles
small_cluster_complete_data_type <- full_small_cluster_dates %>%
  left_join(small_cluster_news_count_type, by = c("datetime", "small_cluster_label", "small_cluster")) %>%
  replace_na(list(supply_news = 0, demand_news = 0, ambiguous_news = 0))


shock_data_small_cluster_type <- small_cluster_complete_data_type %>%
  group_by(small_cluster) %>% 
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(
    news_per_day = supply_news + demand_news + ambiguous_news,
    moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
    moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
    binary_shock= (news_per_day >  moving_avg_news + 3*moving_std_news),
    ptstat = (news_per_day -moving_avg_news)/moving_std_news,
    day_type = case_when(
      supply_news >= 3*demand_news ~ "supply",
      demand_news > supply_news ~ "demand",
      TRUE ~ "ambiguous"
    )

  ) %>% 
  ungroup()


small_info_cluster_type <- shock_data_small_cluster_type%>%
  group_by(datetime) %>%
  slice_max(news_per_day, with_ties = FALSE) %>%
  ungroup()

small_info_days_type <-small_info_cluster_type %>% 
  filter(binary_shock==TRUE & news_per_day > 2) %>% 
  #exclude demand news that I have identified à la relecture
  mutate(day_type = ifelse(datetime %in% demand_news_days_2std$datetime, "demand", day_type)) 

small_info_days_type %>% 
  group_by(day_type) %>% 
  summarise(news_per_day = n(), .groups = "drop")

# Convertir en format Date si ce n'est pas déjà fait
date_list <- as.Date(gas_tot_futures$datetime)
shock_dates <- as.Date(small_info_days_type$datetime)
# Appliquer la fonction à chaque date de shock_data_small_cluster_type$datetime
closest_dates <- sapply(shock_dates, find_closest_posterior, date_list = date_list)

# Ajouter les dates trouvées au dataframe
small_info_days_type$Trading.day <- as.Date(closest_dates, origin = "1970-01-01")

getwd()

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news")
write.csv(small_info_cluster_type, "small_info_cluster_type.csv")
write.csv(small_info_days_type, "small_info_days_type.csv")




# on s'interroge sur à quel point 
#il ne faut pas aussi prendre en compte les news du lendemain

#unification des sujets


high_cluster_news_count_type <- news_data_clustered_type %>% 
  select(media, datetime, high_cluster, high_cluster_label, day_type) %>%
  distinct() %>% 
  group_by(datetime, high_cluster, high_cluster_label, day_type) %>% 
  summarise(news_per_day = n(), .groups = "drop") %>%
  pivot_wider(names_from = day_type, values_from = news_per_day, values_fill = list(news_per_day = 0))  


high_cluster_labels <- unique(news_data_clustered_type[, c("high_cluster_label", "high_cluster")])

full_dates <- tibble(datetime = seq(min(news_data_clustered_type$datetime, na.rm=TRUE),
                                    max(news_data_clustered_type$datetime, na.rm=TRUE), by = "day"))
# Créer toutes les combinaisons de dates et d'événements
full_high_cluster_dates <- full_dates %>%
  crossing(high_cluster_labels)

# Joindre avec les données de nouvelles
high_cluster_complete_data_type <- full_high_cluster_dates %>%
  left_join(high_cluster_news_count_type, by = c("datetime", "high_cluster_label", "high_cluster")) %>%
  replace_na(list(supply_news = 0, demand_news = 0, ambiguous_news = 0))


shock_data_high_cluster_type <- high_cluster_complete_data_type %>%
  group_by(high_cluster) %>% 
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(
    news_per_day = supply_news + demand_news + ambiguous_news,
    moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
    moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
    binary_shock= (news_per_day >  moving_avg_news + 3*moving_std_news),
    ptstat = (news_per_day -moving_avg_news)/moving_std_news,
    day_type = case_when(
      supply_news >= 3*demand_news ~ "supply",
      demand_news >= supply_news ~ "demand",
      TRUE ~ "ambiguous"
    )
    
  ) %>% 
  ungroup()



high_info_cluster_type <- shock_data_high_cluster_type%>%
  group_by(datetime) %>%
  slice_max(news_per_day, with_ties = FALSE) %>%
  ungroup()

high_info_days_type <-high_info_cluster_type %>% 
  filter(binary_shock==TRUE & news_per_day > 2) %>% 
  #exclude demand news that I have identified à la relecture
  mutate(day_type = ifelse(datetime %in% demand_news_days_2std$datetime, "demand", day_type)) 

high_info_days_type %>% 
  group_by(day_type) %>% 
  summarise(news_per_day = n(), .groups = "drop")

# Convertir en format Date si ce n'est pas déjà fait
date_list <- as.Date(gas_tot_futures$datetime)
shock_dates <- as.Date(high_info_days_type$datetime)
# Appliquer la fonction à chaque date de shock_data_small_cluster_type$datetime
closest_dates <- sapply(shock_dates, find_closest_posterior, date_list = date_list)

# Ajouter les dates trouvées au dataframe
high_info_days_type$Trading.day <- as.Date(closest_dates, origin = "1970-01-01")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news")
write.csv(high_info_cluster_type, "high_info_cluster_type.csv")
write.csv(high_info_days_type, "high_info_days_type.csv")


#comparing them
all_info_days_type <- full_join(small_info_days_type, high_info_days_type) %>%
  group_by(datetime) %>% 
  mutate(small_cluster = ifelse(is.na(first(small_cluster)),last(small_cluster),first(small_cluster)),
         small_cluster_label = ifelse(is.na(first(small_cluster_label)),last(small_cluster_label),first(small_cluster_label)),
         high_cluster = ifelse(is.na(first(high_cluster)),last(high_cluster),first(high_cluster)),
         high_cluster_label = ifelse(is.na(first(high_cluster_label)),last(high_cluster_label),first(high_cluster_label))
         ) %>% 
  distinct(datetime, .keep_all = TRUE) %>%   # Keep only unique datetime values 
  ungroup() %>% 
  arrange(datetime)
nrow(all_info_days_type)
write.csv(all_info_days_type, "all_info_days_type.csv")


# je specifie les clusters à considérer
# à partir du label

high_cluster_selection <- c(12, 1, 5, 6, 8, 11, 4, 10)


selected_cluster_news_count_type <- news_data_clustered_type %>% 
  select(media, datetime, high_cluster, high_cluster_label, day_type) %>%
  distinct() %>% 
  filter(high_cluster %in% high_cluster_selection)%>% 
  group_by(datetime, high_cluster, high_cluster_label, day_type) %>% 
  summarise(news_per_day = n(), .groups = "drop") %>%
  pivot_wider(names_from = day_type, values_from = news_per_day, values_fill = list(news_per_day = 0))  


selected_cluster_labels <- unique(news_data_clustered_type[, c("high_cluster_label", "high_cluster")])

full_dates <- tibble(datetime = seq(min(news_data_clustered_type$datetime, na.rm=TRUE),
                                    max(news_data_clustered_type$datetime, na.rm=TRUE), by = "day"))
# Créer toutes les combinaisons de dates et d'événements
full_selected_cluster_dates <- full_dates %>%
  crossing(selected_cluster_labels)

# Joindre avec les données de nouvelles
selected_cluster_complete_data_type <- full_selected_cluster_dates %>%
  left_join(selected_cluster_news_count_type, by = c("datetime", "high_cluster_label", "high_cluster")) %>%
  replace_na(list(supply_news = 0, demand_news = 0, ambiguous_news = 0))


shock_data_selected_cluster_type <- selected_cluster_complete_data_type %>%
  group_by(high_cluster) %>% 
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(
    news_per_day = supply_news + demand_news + ambiguous_news,
    moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
    moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
    binary_shock= (news_per_day >  moving_avg_news + 3*moving_std_news),
    ptstat = (news_per_day -moving_avg_news)/moving_std_news,
    day_type = case_when(
      supply_news >= 3*demand_news ~ "supply",
      demand_news >= supply_news ~ "demand",
      TRUE ~ "ambiguous"
    )
    
  ) %>% 
  ungroup()


selected_info_cluster_type <- shock_data_selected_cluster_type%>%
  group_by(datetime) %>%
  slice_max(news_per_day, with_ties = FALSE) %>%
  ungroup()

selected_info_days_type <-selected_info_cluster_type %>% 
  filter(binary_shock==TRUE & news_per_day > 2) %>% 
  #exclude demand news that I have identified à la relecture
  mutate(day_type = ifelse(datetime %in% demand_news_days_2std$datetime, "demand", day_type)) 



# Convertir en format Date si ce n'est pas déjà fait
date_list <- as.Date(gas_tot_futures$datetime)
shock_dates <- as.Date(selected_info_days_type$datetime)
# Appliquer la fonction à chaque date de shock_data_selected_cluster_type$datetime
closest_dates <- sapply(shock_dates, find_closest_posterior, date_list = date_list)

# Ajouter les dates trouvées au dataframe
selected_info_days_type$Trading.day <- closest_dates


write.csv(selected_info_cluster_type, "selected_info_cluster_type.csv")
write.csv(selected_info_days_type, "selected_info_days_type.csv")






