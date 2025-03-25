library(zoo)
library(tidyr)
library(lubridate)
library(dplyr)
library(arrow)
library(readxl)
library(ggplot2)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news')
my_instrumented_days <- read.csv("final_instrumented_days.csv")


demand_keywords <- c("demand ", "inventory", "stocks",
                     "consum", "covid", "coronavirus")
# Vérifier si 'summary' contient au moins un mot-clé
contains_demand_keyword <- sapply(demand_keywords, function(k) grepl(k, my_instrumented_days$summary))

# Garder seulement les lignes où au moins un mot-clé est présent
test <- my_instrumented_days[rowSums(contains_demand_keyword) > 0, ]


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
gas_tot_futures <- read_parquet("gas_data_shock.parquet")
gas_tot_futures <- gas_tot_futures %>% 
  arrange(time) %>% 
  mutate(gas_shock = TTF - lag(TTF),
         datetime = as.Date(time, format = "%Y-%m-%d")
  )


my_instrumented_days <- my_instrumented_days %>% 
  filter(day_type!="ambiguous")%>% 
  mutate(gas_shock = TTF - lag(TTF),
         datetime = as.Date(time, format = "%Y-%m-%d")
  )

fundamental_shock_serie <- my_instrumented_days %>% 
  mutate(month = month(Trading.day), year = year(Trading.day)) %>%
  group_by(month, year, day_type) %>% 
  summarise(
    PC1 = sum(PC1),
    gas_shock = sum(gas_shock), 
    Trading.day = min(Trading.day)   # Extract the earliest date
    , .groups = "drop") %>%   # Drop extra grouping to avoid warnings
  arrange(Trading.day)       # Sort by the earliest date in each group




wider_fundamental_shock_serie <- fundamental_shock_serie %>%
  pivot_wider(names_from = day_type, 
              values_from = c(PC1, gas_shock), 
              names_glue = "{.value}_{day_type}") %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
  group_by(year, month) %>% 
  summarise(across(where(is.numeric), sum),
            last_Trading.day = last(Trading.day))

gas_price_data <- gas_tot_futures %>%
  arrange(time) %>%
  mutate(month = month(time), year = year(time)) %>% 
  group_by(month, year) %>% 
  summarise(
    avg_spot_price = mean(TTF, na.rm = TRUE),
    last_spot_price = last(na.omit(TTF)),  # Prendre la dernière valeur non NA
    #last_Date = as.Date(median(time, na.rm = TRUE)), # Convertir median en date
    last_Date = as.Date(last(na.omit(time)))
  ) %>%   
  ungroup() %>%  # Supprime le groupement pour éviter des erreurs
  arrange(last_Date)  %>% 
  mutate(avg_spot_diff = avg_spot_price - lag(avg_spot_price),
         last_spot_diff = last_spot_price - lag(last_spot_price)
  )

# Création d'une séquence complète de mois et années
date_range <- seq(from = as.Date(format(min(gas_price_data$last_Date, na.rm = TRUE), "%Y-%m-01")), 
                  to = as.Date(format(max(gas_price_data$last_Date, na.rm = TRUE), "%Y-%m-01")), 
                  by = "month")


full_grid <- expand.grid(
  year = unique(year(date_range)),
  month = unique(month(date_range))
)

# Jointure avec les données existantes

gas_fundamental_data <- full_grid %>%
  left_join(wider_fundamental_shock_serie, by = c("year", "month")) %>%
  left_join(gas_price_data, by = c("year", "month")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>% 
  arrange(last_Date)





setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data')
write_parquet(gas_fundamental_data, 
              "gas_fundamental_data.parquet")


# gas_fundamental_data <- full_join(fundamental_shock_serie, gas_price_data,
#                                  by = c("month", "year")) %>% 
#   arrange(year, as.numeric(month)) %>%
#   filter(last_Date >= min(as.Date(fundamental_shock_serie$observation_date), na.rm = TRUE) %m-% years(1)) %>% 
#   replace_na(list(fundamental_shock = 0, uncertainty_shock = 0, PC1 =0))

# Liste des day_types uniques présents dans vos données
day_types <- unique(fundamental_shock_serie$day_type)

full_grid_type <- expand.grid(
  year = unique(year(date_range)),
  month = unique(month(date_range)),
  day_type = day_types
)

plot_data <- full_grid_type %>%
  left_join(fundamental_shock_serie, by = c("year", "month", "day_type")) %>%
  left_join(gas_price_data, by = c("year", "month")) %>%
  arrange(year, month) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

ggplot(data = plot_data)+
  geom_line(aes(x = last_Date, y = gas_shock, color = day_type, group = day_type))+
  geom_line(aes(x = last_Date, y = PC1, color = day_type, group = day_type), linetype = "dashed")+
  theme_minimal()



supply_model <- lm(avg_spot_diff  ~ PC1, 
                  #last_spot_diff  ~ Fund,
                   data = plot_data %>% 
                     filter(day_type == "supply"))

summary(supply_model)


demand_model <- lm(avg_spot_diff  ~ PC1, 
                   data = plot_data %>% 
                     filter(day_type == "demand"))
summary(demand_model)









