
library(tidyr)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")

first_hour <- as.POSIXct("01-01-2018 00:00:00", 
                           format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")

#class(indispo_on_the_day$indispo_day)
indispo_on_the_day <-  read_parquet("indispo_day_ahead_hourly_bis.parquet")%>% 
  arrange(indispo_day) %>% 
  filter(indispo_day >= first_hour) %>% 
  replace_na(list(Tot_Unplanned_Unavailability = 0, Tot_Planned_Unavailability = 0)) 


#on essaye avec la série lissée désaisonnalisée en niveau
#
library(forecast)
library(pracma)

# Initialize a list to store the seasonally adjusted series
adjusted_data <- list()
start_date = c('2018', '01', '01')
frequency = 365
vars_list <- setdiff(colnames(indispo_on_the_day), "indispo_day")
# Loop over each variable and perform STL decomposition
for (var in vars_list) {
  ts_data <- ts(indispo_on_the_day[[var]], start = start_date, frequency = frequency)
  # Perform STL decomposition
  stl_fit <- stl(ts_data, s.window = "periodic")
  adjusted_data[[var]] <- seasadj(stl_fit)

}


# Combine the seasonally adjusted data into a dataframe
seasonally_adjusted_df <- as.data.frame(adjusted_data)

daily_indispo_data <- cbind(indispo_on_the_day, seasonally_adjusted_df %>% 
                              rename(Tot_Unplanned_Unavailability_sadj = Tot_Unplanned_Unavailability,
                                     Tot_Planned_Unavailability_sadj = Tot_Planned_Unavailability)) %>% 
  mutate(Tot_Unplanned_Unavailability_diff = Tot_Unplanned_Unavailability - lag(Tot_Unplanned_Unavailability),
         Tot_Planned_Unavailability_diff = Tot_Planned_Unavailability - lag(Tot_Planned_Unavailability),
         #pour corriger la limite horaire
         Tot_Unplanned_Unavailability_diff2 = Tot_Unplanned_Unavailability - lag(Tot_Unplanned_Unavailability, 2),
         Tot_Planned_Unavailability_diff2 = Tot_Planned_Unavailability - lag(Tot_Planned_Unavailability, 2),
         )

library(ggplot2)

# résolution graphique souffre avec une telle fréquence de plot
# gros pâté

# Plot the results
ggplot(daily_indispo_data ) +
  geom_line( aes(x=indispo_day, y=Tot_Planned_Unavailability, color = 'Planned Maintenance', linetype = 'Observed')) +
  geom_line( aes(x=indispo_day, y=Tot_Unplanned_Unavailability, color = 'Unplanned outage', linetype = 'Observed')) +
  geom_line( aes(x=indispo_day, y=Tot_Planned_Unavailability_sadj, color = 'Planned Maintenance', linetype = 'Seasonally Adj')) +
  geom_line( aes(x=indispo_day, y=Tot_Unplanned_Unavailability_sadj, color = 'Unplanned outage', linetype = 'Seasonally Adj')) +
  labs(title = "Total Unavailability Capacity by Day", x = "Day", y = "Total Unavailability Capacity") +
  theme_minimal()

colnames(day_ahead_prices)




load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/esp"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_prices <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`) 
day_ahead_prices$`__index_level_0__`

day_ahead_prices<- day_ahead_prices[,1:2]
colnames(day_ahead_prices) <- c("price", "datetime")
day_ahead_prices<- day_ahead_prices %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         delivery_date = as.Date(datetime),
         price = as.numeric(price),
         price_diff = price - lag(price),
         log_price_diff = log(price) - lag(log(price))
         ) %>% 
  filter(delivery_date >= as.Date('2018-01-01')) %>% 
  arrange(datetime)



price_indispo_merge <- left_join(day_ahead_prices,
      daily_indispo_data,
      by = c("datetime" = "indispo_day"))

#getting outages share
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts.parquet')
colnames(renewable_forecasts)
gen_tot <- renewable_forecasts %>%
  select(`__index_level_0__`, gen_tot, renewable_forecast,
         unpredicted_renewables, intraday_update_renewables)

#nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
#sample start

price_indispo_merge <- price_indispo_merge %>%
  left_join(gen_tot, by=c('datetime' = '__index_level_0__'))

# pour l'estimation en niveau,
#on rajoute le spot gas price
library(readxl)
gas_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(gas_path)

gas_prices <- read_excel("gas.xlsx") %>% 
  mutate(time = as.Date(time),
         spot_gas = as.numeric(paste0(
           `Gaz naturel, contrats à terme aux Pays-Bas (TTF), euros par megawatt-heure`
                                      ,".",virgule ))) %>% 
  select(time, spot_gas)


price_indispo_merge <- price_indispo_merge %>%
  left_join(gas_prices, by=c('delivery_date' = 'time'))

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/day_ahead/outputs"
setwd(save_path_data)
write_parquet(price_indispo_merge, "price_indispo_merge.parquet")




