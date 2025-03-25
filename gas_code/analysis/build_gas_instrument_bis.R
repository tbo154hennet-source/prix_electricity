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
gas_keywords <- c("gas", "Gazprom", "LNG", "TTF", "GAZP", "Ukraine")
# Create a regular expression pattern from the list
gas_pattern <- paste(gas_keywords, collapse = "|")
# Filter rows in news_data based on the title containing any keyword
filtered_news <- news_data[grepl(gas_pattern, news_data$title, ignore.case = TRUE), ]

#  Look for Typical Sentence Structures
# Affirmative sentences often follow a [subject-verb-object (SVO)] structure, like "The cat sits on the mat."

# Define the keywords you want to exclude
exclude_keywords <- c( "edible",  "food", "biofuel", "bio",
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

demand_keywords <- c("demand ", "inventory", "stocks", "consum", "covid", "coronavirus")
demand_pattern <- paste(demand_keywords, collapse = "|")
demand_filtered_news  <- filtered_news_distinct[grepl(demand_pattern, filtered_news_distinct$title, ignore.case = TRUE), ]%>% 
  mutate(event_type = "demand")

supply_keywords <- c("supply", "production", "Iraq","Iran", "Russia", "Ukraine","Israel",
                     "Libya", "crisis", "attack", "Qatar",
                     "violence", "conflict", "war ","reserves", "refinery", 
                     "pipeline","facility", "ban", "shale")
supply_pattern <- paste(supply_keywords, collapse = "|")
supply_filtered_news <- filtered_news_distinct[grepl(supply_pattern, filtered_news_distinct$title, ignore.case = TRUE), ]%>% 
  mutate(event_type = "supply")

uncertainty_keywords <- c("uncertainty", "fear", "concern",
                          "threaten", "risk",
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

full_news_count <- labeled_news %>% 
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


shock_data_3std <- complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(supply_moving_avg_news = rollapply(supply_news, width = 30, FUN = mean, fill = NA),
         supply_moving_std_news = rollapply(supply_news, width = 30, FUN = sd, fill = NA),
         supply_binary_shock= (supply_news >  supply_moving_avg_news + 3*supply_moving_std_news),
         supply_intensity_shock = ifelse(supply_binary_shock, supply_news/supply_moving_avg_news, 0),
         
         demand_moving_avg_news = rollapply(demand_news, width = 30, FUN = mean, fill = NA),
         demand_moving_std_news = rollapply(demand_news, width = 30, FUN = sd, fill = NA),
         demand_binary_shock= (demand_news >  demand_moving_avg_news + 3*demand_moving_std_news),
         demand_intensity_shock = ifelse(demand_binary_shock, demand_news/demand_moving_avg_news, 0),
         
  )


shock_data_2std <- complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(supply_moving_avg_news = rollapply(supply_news, width = 20, FUN = mean, fill = NA),
         supply_moving_std_news = rollapply(supply_news, width = 20, FUN = sd, fill = NA),
         supply_binary_shock= (supply_news >  supply_moving_avg_news + 2*supply_moving_std_news),
         supply_intensity_shock = ifelse(supply_binary_shock, supply_news/supply_moving_avg_news, 0),
         
         demand_moving_avg_news = rollapply(demand_news, width = 20, FUN = mean, fill = NA),
         demand_moving_std_news = rollapply(demand_news, width = 20, FUN = sd, fill = NA),
         demand_binary_shock= (demand_news >  demand_moving_avg_news + 2*demand_moving_std_news),
         demand_intensity_shock = ifelse(demand_binary_shock, demand_news/demand_moving_avg_news, 0)
         
  )


View(shock_data_2std %>% 
       filter(datetime >= "2022-02-01" & datetime <= "2022-04-01"))

summary(shock_data_2std %>% select(supply_news,demand_news))

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')

gas_tot_futures <- read_parquet("gas_data_shock.parquet")

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

date_list <- gas_tot_futures$time


# Apply the function row-wise using apply
shock_data_3std$Trading.day <- apply(shock_data_3std, 1, function(row) {
  trading_day <- row['datetime']
  # Check if trading_day is NA, and if so, find the closest posterior date
  if (is.na(trading_day)) {
    return(find_closest_posterior(announcement_day = row['announcement_day'], date_list = date_list))
  } else {
    return(trading_day)
  }
})

# Convert numeric to Date format (days since 1970-01-01)
shock_data_3std$Trading.day <- format(as.Date(shock_data_3std$Trading.day, origin = "1970-01-01"),"%Y-%m-%d")


# Apply the function row-wise using apply
shock_data_2std$Trading.day <- apply(shock_data_2std, 1, function(row) {
  trading_day <- row['datetime']
  # Check if trading_day is NA, and if so, find the closest posterior date
  if (is.na(trading_day)) {
    return(find_closest_posterior(announcement_day = row['announcement_day'], date_list = date_list))
  } else {
    return(trading_day)
  }
})

# Convert numeric to Date format (days since 1970-01-01)
shock_data_2std$Trading.day <- format(as.Date(shock_data_2std$Trading.day, origin = "1970-01-01"),"%Y-%m-%d")

colnames(shock_data_3std)

instrument_days_3std_bis <- shock_data_3std %>% 
  filter((supply_binary_shock ==TRUE & supply_news > 3) |
           (demand_binary_shock==1 & demand_news > 2))


instrument_days_2std_bis <- shock_data_2std %>% 
  filter((supply_binary_shock ==TRUE & supply_news > 3) |
           (demand_binary_shock==1 & demand_news > 2))


instrument_days_2std_bis %>%
  select(supply_binary_shock,  demand_binary_shock) %>%
  count(supply_binary_shock, demand_binary_shock)


instrument_days_3std_bis %>%
  select(supply_binary_shock, demand_binary_shock) %>%
  count(supply_binary_shock, demand_binary_shock)


# #on veut exclure les jours où il y a eu des annonces de demande

demand_news_days <- instrument_days_2std_bis %>%
  filter(demand_binary_shock==1 & demand_news > 2) %>%
  pull(datetime) %>%
  as.Date()

getwd()
write.csv(demand_news_days, "demand_news_days_bis.csv")


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/outputs')
write.csv(instrument_days_2std_bis, "instrument_news_events_2std_bis.csv")
write.csv(instrument_days_3std_bis, "instrument_news_events_3std_bis.csv")



#stat des
#I take only supply or binary well-defined_shocks

instru_stat_2std <- instrument_days_2std_bis %>% 
  filter( (supply_binary_shock==1 & demand_binary_shock==0) |
            (supply_binary_shock==0 & demand_binary_shock==1) ) %>%
  group_by(supply_binary_shock,  demand_binary_shock) %>%
  summarise(n=n())



instru_stat_3std <- instrument_days_3std_bis %>% 
  filter( (supply_binary_shock==1 & demand_binary_shock==0) |
            (supply_binary_shock==0 & demand_binary_shock==1) ) %>%
  group_by(supply_binary_shock, demand_binary_shock) %>%
  summarise(n=n())

# Sample data
library(gt)
library(webshot2)
data <- data.frame(
  Category = c( "Demand Shocks", "Supply shocks"),
  Value1 = c(instru_stat_2std$n[1:2], instru_stat_3std$n[1:2])
)

# Create a gt table with styling
gt_table <- data %>%
  gt()%>%
  cols_label(
    Category = "",
    Value1 = "n"
  ) %>%
  tab_row_group(
    group = "+2std",
    rows = 1:2 # Rows 1 to 3 for the first section
  ) %>%
  tab_row_group(
    group = "+3std",
    rows = 3:4# Rows 4 to 6 for the second section
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "gray", weight = px(2)),
    locations = cells_row_groups(groups = everything())
  ) %>%
  tab_options(
    table.width = pct(100)
  )%>%
  gtsave("stat_des_gas_instrument_days.png")
getwd()

