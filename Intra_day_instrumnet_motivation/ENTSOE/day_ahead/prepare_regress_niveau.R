
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")

#class(indispo_on_the_day$indispo_day)
indispo_on_the_day <-  read_parquet("indispo_day_ahead_bis.parquet") %>% 
  filter(indispo_day >= as.Date('2018-01-01'))

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
         Tot_Planned_Unavailability_diff = Tot_Planned_Unavailability - lag(Tot_Planned_Unavailability)
         )


# Plot the results
ggplot(daily_indispo_data ) +
  #geom_line( aes(x=indispo_day, y=Tot_Planned_Unavailability, color = 'Planned Maintenance', linetype = 'Observed')) +
  geom_line( aes(x=indispo_day, y=Tot_Unplanned_Unavailability, color = 'Unplanned outage', linetype = 'Observed')) +
  #geom_line( aes(x=indispo_day, y=Tot_Planned_Unavailability_sadj, color = 'Planned Maintenance', linetype = 'Seasonally Adj')) +
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




