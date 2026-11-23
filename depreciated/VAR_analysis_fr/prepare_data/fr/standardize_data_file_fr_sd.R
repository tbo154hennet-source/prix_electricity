
library(readxl)
library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
library(purrr)

var_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output"
setwd(var_path)

VAR_series <- read_parquet("VAR_series_fr_2026.parquet")

colnames(VAR_series)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr")
price_indispo_merge <- read_parquet("price_indispo_merge_fr.parquet")

colnames(price_indispo_merge)

monthly_VAR_data <- price_indispo_merge %>%
  mutate(month = month(datetime),
         year = year(datetime)) %>%
  group_by(month, year) %>%
  summarise(across(c(price, load_forecast, load_tot, gen_forecast,
                     Fore_Solar, Fore_Wind,
                     temperature_2m, heat_excess, cold_excess, temperature_discomfort,
                     spot_gas), mean, na.rm = TRUE),
            last_Date = max(datetime)
  ) %>%
  rename(spot_price = price) %>%
  ungroup() %>%
  arrange(year, month)


# View(price_indispo_merge %>% 
#        select(datetime, Fore_Wind))

IV_data <- merge(VAR_series, monthly_VAR_data , by= c('month', 'year'))%>%
  arrange(last_Date)
tail(IV_data)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
write_parquet(IV_data, 'IV_data_fr_new.parquet')



