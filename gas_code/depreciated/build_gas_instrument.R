library(readxl)
library(tidyr)
library(dplyr)
library(arrow)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")

news_data <- read_parquet("store_full_news_data.parquet")

#english reading system
Sys.setlocale("LC_TIME", "English")

news_data$datetime <- as.Date(format(as.Date(news_data$date, "%b %d, %Y"), "%Y-%m-%d"))

news_data$datetime <- ifelse( is.na(news_data$datetime), as.Date(format(as.Date(news_data$date, "%d %b %Y"), "%Y-%m-%d")), news_data$datetime)

news_data <- news_data %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime)

# Define keywords as a list
gas_keywords <- c("gas", "Gazprom", "LNG", "TTF", "GAZP")
# Create a regular expression pattern from the list
gas_pattern <- paste(gas_keywords, collapse = "|")
# Filter rows in news_data based on the title containing any keyword
filtered_news <- news_data[grepl(gas_pattern, news_data$title, ignore.case = TRUE), ]

#  Look for Typical Sentence Structures
# Affirmative sentences often follow a [subject-verb-object (SVO)] structure, like "The cat sits on the mat."

# Define the keywords you want to exclude
exclude_keywords <- c( "edible",  "food", "biofuel",
                       "\\?", "station", "gasoline", "diesel")
# ? most often are title of opinion pieces
#that don't bring new informations

# Create a single pattern from the keywords
exclude_pattern <- paste(exclude_keywords, collapse = "|")
# Filter out rows where 'title' contains any of the exclude keywords
filtered_news <- filtered_news[!grepl(exclude_pattern, filtered_news$title, ignore.case = TRUE), ]


filtered_news_distinct <- filtered_news %>% 
  select(-event_type) %>% 
  distinct() %>% 
  arrange(desc(datetime))
# news_data <- news_data %>% 
#   select(-datetime)

demand_keywords <- c("demand ", "inventory", "stocks")
demand_pattern <- paste(demand_keywords, collapse = "|")
demand_filtered_news  <- filtered_news_distinct[grepl(demand_pattern, filtered_news_distinct$title, ignore.case = TRUE), ]%>% 
  mutate(event_type = "demand")

supply_keywords <- c("supply", "production", "Iraq","Iran", "Russia", "Ukraine","Israel", "Libya", "crisis", "attack", "Qatar",
                     "violence", "conflict", "war ","reserves", "refinery", "pipeline","facility", "ban", "shale")
supply_pattern <- paste(supply_keywords, collapse = "|")
supply_filtered_news <- filtered_news_distinct[grepl(supply_pattern, filtered_news_distinct$title, ignore.case = TRUE), ]%>% 
  mutate(event_type = "supply")

uncertainty_keywords <- c("uncertainty", "fear", "concern",
                          "threaten", "risk", "turmoil",
                          "tension", "worr", "turmoil",
                          "surprise", "shock")
uncertainty_pattern <- paste(uncertainty_keywords, collapse = "|")
uncertainty_filtered_news <- filtered_news_distinct[grepl(uncertainty_pattern, filtered_news_distinct$title, ignore.case = TRUE), ]%>% 
  mutate(event_type = "uncertainty")


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')

labeled_news <- rbind(demand_filtered_news, supply_filtered_news, uncertainty_filtered_news)%>% arrange(datetime)

labeled_news_distinct <- labeled_news %>% 
  select(-event_type) %>% 
  distinct()

write_parquet(labeled_news_distinct , "labeled_news_distinct.parquet")

write_parquet(labeled_news , "labeled_news_data.parquet")

# setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/')
# write.csv(labeled_news, "labeled_news_data.csv")
# write.csv(labeled_news_distinct, "labeled_news_distinct.csv")

full_news_count <- labeled_news%>% 
  select(media, datetime,title) %>%
  distinct() %>% 
  group_by(datetime) %>% 
  summarise(news_per_day = n())

event_news_count <- labeled_news %>% 
  select(media, datetime, event_type) %>%
  distinct() %>% 
  group_by(datetime, event_type) %>% 
  summarise(news_per_day = n())


full_dates <- tibble(datetime = seq(min(news_data$datetime, na.rm=TRUE), max(news_data$datetime, na.rm=TRUE), by = "day"))


# Créer un dataframe avec les types d'événements
event_types <- data.frame(event_type = c("supply", "demand", "uncertainty"))

# Créer toutes les combinaisons de dates et d'événements
full_event_dates <- full_dates %>%
  crossing(event_types)

# Joindre avec les données de nouvelles
event_complete_data <- full_event_dates %>%
  left_join(event_news_count, by = c("datetime", "event_type")) %>%
  replace_na(list(news_per_day = 0))

full_dates <- tibble(datetime = seq(min(news_data$datetime, na.rm=TRUE), max(news_data$datetime, na.rm=TRUE), by = "day"))

#on veut selectionner les jours d'annonce qui sont à dominante un supply shock
# on demande 4 fois plus de supply news que demand news


supply_news_count <- event_news_count %>% 
  pivot_wider(names_from = event_type, values_from = news_per_day) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  mutate(day_type = ifelse(supply < 3*demand, "ambiguous", "supply")) %>% 
  select(supply, demand, day_type)

# Fusionner le DataFrame original avec la séquence complète de dates
complete_data <- full_dates %>%
  left_join(supply_news_count , by = "datetime") %>%
  rename(supply_news = supply, demand_news = demand) %>%
  replace_na(list(supply_news = 0, demand_news=0))


shock_data_supply <- complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(supply_news, width = 30, FUN = mean, fill = NA),
         moving_std_news = rollapply(supply_news, width = 30, FUN = sd, fill = NA),
         binary_shock= (supply_news >  moving_avg_news + 3*moving_std_news),
         intensity_shock = ifelse(binary_shock, supply_news/moving_avg_news, 0),
         
         demand_moving_avg_news = rollapply(demand_news, width = 30, FUN = mean, fill = NA),
         demand_moving_std_news = rollapply(demand_news, width = 30, FUN = sd, fill = NA),
         demand_binary_shock= (demand_news >  demand_moving_avg_news + 3*demand_moving_std_news),
         demand_intensity_shock = ifelse(demand_binary_shock, demand_news/demand_moving_avg_news, 0)
         
  )


#on veut exclure les jours où il y a eu des annonces de demande
demand_news_days <- shock_data_supply %>% 
  filter(demand_binary_shock==1 & demand_news > 2) %>% 
  pull(datetime) %>% 
  as.Date()

getwd()
write.csv(demand_news_days, "demand_news_days.csv")


all_news_count <- news_data %>% 
  select(media, datetime,title) %>%
  distinct() %>% 
  group_by(datetime) %>% 
  summarise(news_per_day = n())

# Fusionner le DataFrame original avec la séquence complète de dates
complete_data_bis <- full_dates %>%
  left_join(all_news_count , by = "datetime") %>%
  replace_na(list(news_per_day = 0))

shock_data_full <- complete_data_bis %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news),
         intensity_shock = ifelse(binary_shock, news_per_day/moving_avg_news, 0)
  )
# on s'interroge sur à quel point 
#il ne faut pas aussi prendre en compte les news du lendemain

#unification des sujets
#est ce que cela traite de la même chose
#on envoie le corpus du jour à chatgpt et on lui demande de faire le job

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
options(warn = -1)
gas_data_tot <- read_excel("TTF_futures.xlsx") %>% 
  na.omit() %>% 
  arrange(desc(time))
#gas_data <- read.csv2("gas_data.csv")
class(gas_data_tot$time)

log_daily_market_surprises <- as.data.frame(gas_data_tot[, c(-1)]) %>%
  mutate(across(everything(), ~ log(.) - log(lead(.)), .names = "dms_{col}"))%>%
  select(starts_with("dms"))

# Step 2: Perform PCA
pca_result <- prcomp(na.omit(log_daily_market_surprises), center = TRUE)


# a titre informatif
#combien y a t-il de PCs

# Get the explained variance
explained_variance <- summary(pca_result)$importance[2, 1:4]  # Proportion of variance

# plein de petites PCs qui décroisent faiblement vers 0
#il faut aller jusqu'à PC6 pour expliquer 90 % de la variance


gas_data_shock <- pca_result$x %>% 
  as.data.frame() %>% 
  select(PC1) %>% 
  mutate(time = gas_data_tot$time[-1],
         Date = as.Date(time, format = "%Y-%m-%d"),
                        TTF = gas_data_tot$DTFE1C.D7[-1])

write_parquet(gas_data_shock, "gas_data_shock.parquet")



info_supply <- merge(gas_data_shock, shock_data_supply, 
              by.x = "Date", by.y = "datetime" ) 

info_full<- merge(gas_data_shock, shock_data_full, 
                     by.x = "Date", by.y = "datetime" ) 

instrument_days_full <- info_full %>% 
  filter(binary_shock ==TRUE) %>% 
  filter(news_per_day > 3)
# %>% 
#   select(Date, shock_serie, supply_news, 
#          moving_avg_news, moving_std_news, day_type, Gas_TTF)

instrument_days_supply <- info_supply %>% 
  filter(supply_binary_shock ==TRUE) %>% 
  filter(supply_news > 3)

nrow(instrument_days_full)
nrow(instrument_days_supply)

save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

write_parquet(info_supply, "info_supply.parquet")
write_parquet(info_full, "info_full.parquet")

#mesure la borne de pseudo t-stat pour en faire un shock
# (news_per_day - moving avg) / moving_std


