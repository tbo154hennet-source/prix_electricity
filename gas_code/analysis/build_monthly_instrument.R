library(readxl)
library(tidyr)
library(dplyr)
library(arrow)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
gas_tot_futures <- read_parquet("gas_data_shock.parquet")
gas_tot_futures$datetime <- as.Date(gas_tot_futures$time)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news")
all_info_days_type <- read.csv("all_info_days_type.csv")

my_LLM_titles <- read.csv('dominating_news_data.csv')

final_info_days <- merge(all_info_days_type, my_LLM_titles, by = "datetime")%>% 
  arrange(datetime) 



correct_instrument_dates <- final_info_days %>%
  mutate(Trading.day = as.Date(Trading.day),
         # Nouveau groupe si l'écart > 3 jours
         group = cumsum(c(1, diff(Trading.day) > 2)
         )) %>% 
  group_by(group) %>%
  mutate(
    diff_days = c(NA, diff(Trading.day)),  # Calcul de la différence en jours entre chaque date
    consecutive = ifelse(all(diff_days == 1, na.rm = TRUE), "Consécutives", "Manquantes")
  ) %>%
  ungroup()

# Trouver les dates manquantes

correct_instrument_dates %>%
  filter(consecutive == "Manquantes") %>%
  select(Trading.day, group, consecutive) %>%
  arrange(Trading.day) %>%
  print(n = Inf)

# Trouver les dates manquantes consécutives pour chaque groupe
missing_dates_by_group <- correct_instrument_dates %>%
  filter(consecutive == "Manquantes") %>%
  group_by(group) %>%
  arrange(Trading.day) %>%
  summarise(
    missing_date = first(Trading.day) + 1
  )

# Afficher les dates manquantes par groupe
missing_dates_by_group


# Identifier les champs communs (valeurs identiques sur tout le groupe)
common_fields <- correct_instrument_dates %>%
  group_by(group) %>%
  summarise(across(everything(), ~ifelse(n_distinct(.) == 1, first(.), NA)),
            .groups = "drop") %>%
  select(-Trading.day)  # Exclure Trading.day car il varie


final_info_days_complete <- correct_instrument_dates %>%
  group_by(group) %>%
  complete(Trading.day = seq(min(Trading.day), max(Trading.day), by = "1 day"))

extra_dates <- merge(common_fields,
                     final_info_days_complete %>%
                       filter(is.na(consecutive)) %>% 
                       select(group, Trading.day),
                     by = "group")
final_info_days_complete <- rbind(correct_instrument_dates, extra_dates) 


my_instrumented_days <- merge(final_info_days_complete %>% 
                                mutate(Trading.day = as.Date(Trading.day)), 
                              gas_tot_futures,  
                              by.x = "Trading.day", by.y = "datetime")%>%
  distinct(Trading.day, .keep_all = TRUE)

getwd()
write.csv(my_instrumented_days, "final_clustered_instrumented_days.csv")

my_instrumented_days %>% 
  group_by(day_type) %>% 
  summarize(n = n())




