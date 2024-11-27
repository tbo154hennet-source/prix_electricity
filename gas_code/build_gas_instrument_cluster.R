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

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/')
write.csv(labeled_news, "labeled_news_data.csv")
write.csv(labeled_news_distinct, "labeled_news_distinct.csv")

full_news_count <- labeled_news %>% 
  select(media, datetime) %>%
  distinct() %>% 
  group_by(datetime) %>% 
  summarise(news_per_day = n())

event_news_count <- labeled_news %>% 
  select(media, datetime, event_type) %>%
  distinct() %>% 
  group_by(datetime, event_type) %>% 
  summarise(news_per_day = n())


full_dates <- tibble(datetime = seq(min(news_data$datetime, na.rm=TRUE), max(news_data$datetime, na.rm=TRUE), by = "day"))

# Fusionner le DataFrame original avec la séquence complète de dates
complete_data <- full_dates %>%
  left_join(full_news_count , by = "datetime") %>%
  replace_na(list(news_per_day = 0))


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
  select(supply, day_type)

# Fusionner le DataFrame original avec la séquence complète de dates
complete_data <- full_dates %>%
  left_join(supply_news_count , by = "datetime") %>%
  rename(supply_news = supply) %>%
  replace_na(list(supply_news = 0))




shock_data <- cluster_complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news),
         intensity_shock = ifelse(binary_shock, news_per_day/moving_avg_news, 0)
  )

instrument_days <- shock_data %>% 
  filter(binary_shock == TRUE & news_per_day>=3)

nrow(instrument_days %>% 
       filter(binary_shock))

# on s'interroge sur à quel point 
#il ne faut pas aussi prendre en compte les news du lendemain

#unification des sujets
#est ce que cela traite de la même chose
#on envoie le corpus du jour à chatgpt et on lui demande de faire le job
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
options(warn = -1)
gas_data_tot <- read_excel("gas.xlsx")
#gas_data <- read.csv2("gas_data.csv")

gas_data_tot <- gas_data_tot[,1:2]
colnames(gas_data_tot) <- c('time', 'Gas_TTF')

gas_data_shock <- gas_data_tot[order(gas_data_tot$time), ]
colnames(gas_data_shock)
gas_data_shock$Date <- as.Date(gas_data_shock$time)
gas_data_shock$Gas_TTF_previous_day <- lag(gas_data_shock$Gas_TTF)
gas_data_shock$Gas_TTF_next_day<- lead(gas_data_shock$Gas_TTF)
gas_data_shock$shock_serie <-  gas_data_shock$Gas_TTF - gas_data_shock$Gas_TTF_previous_day



info <- left_join(gas_data_shock, instrument_days,
                  by = c("Date" = "datetime"))

nrow(info %>% 
  filter(binary_shock))

nrow(info)

View(info)

instrument_results <- left_join(full_dates, instrument_days, 
                                   by = c("datetime")) %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  replace_na(list(binary_shock=FALSE))



# save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
# setwd(save_var_path)
# write_parquet(instrument_results, "instrument_news_shock.parquet")



     
                                                  
save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
gas_instrument_data <- merge(gas_data_shock, instrument_results, by.x= c('Date'), by.y= c('datetime'))

write_parquet(gas_instrument_data, "gas_instrument_data.parquet")

surprise_serie <- gas_instrument_data %>% 
  mutate(month = month(Date), year= year(Date)) %>% 
  group_by(month, year) %>% 
  summarize(shock = sum(shock_serie*binary_shock), avg_price= mean(Gas_TTF), Date = mean(Date)) 
surprise_serie$Delta <- 100*surprise_serie$shock /surprise_serie$avg_price

write_parquet(surprise_serie, "surprise_serie.parquet")




gas_instrument_data %>% 
  filter(binary_shock == TRUE) %>% 
  select(Date, shock_serie) %>% 
  #arrange(desc(abs(Date))) %>% 
  slice(1:20)



#on voudrait checker si les grosses variations de prix ne sont pas des shocks
#on regarde les 20 plus grosses variations de prix
#et les news associées











