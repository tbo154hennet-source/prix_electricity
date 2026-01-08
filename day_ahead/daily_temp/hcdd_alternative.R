

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
tempall_data_data <- read_parquet( "tempall_data_data.parquet")

colnames(tempall_data_data)

T_base <- 18

tempall_data_data <- tempall_data_data %>%
  mutate(
    across(
      starts_with("temperature_"),
      list(
        HDD = ~pmax(0,. - T_base),
        CDD = ~pmax(0, T_base - .)
      ),
      .names = "{.fn}_{sub('^temperature_', '', .col)}"
    )
  )

HCDD_cols <- colnames(tempall_data_data)[grep("^HDD_|^CDD_", colnames(tempall_data_data))]

temp_monthly <- tempall_data_data %>%
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, year) %>% 
  summarise(
    across(all_of(HCDD_cols), mean, na.rm = TRUE),
    date = first(date),
    .groups = "drop"
  )
colnames(temp_monthly)

temp_monthly_excess <- temp_monthly %>%
  group_by(month) %>%
  mutate(
    across(
      all_of(HCDD_cols),
      ~ . - mean(., na.rm = TRUE),
      .names = "{.col}_excess"
    )
  ) %>%
  ungroup()


#méthode de David qui a l'air de changer de manière dramatique les résultats trouver
#à voir si tête du proxy se confirme par la suite

library(ggplot2)
ggplot()+
  geom_line(data = monthly_weather_data, aes( x = datetime, y = cold_excess, color = "Cold Excess"))+
  geom_line(data = monthly_weather_data, aes( x = datetime, y = heat_excess, color = "Heat Excess"))+
  geom_line(data = temp_monthly_excess, aes( x= date, y = HDD_sth_excess, color = "HDD" ))+
  geom_line(data = temp_monthly_excess, aes( x= date, y = CDD_sth_excess, color = "CDD" ))
  
#### compare with monthly weather data
cor(monthly_weather_data$cold_excess, temp_monthly_excess$CDD_sth_excess)
cor(monthly_weather_data$heat_excess, temp_monthly_excess$HDD_sth_excess)
colnames(monthly_weather_data)

