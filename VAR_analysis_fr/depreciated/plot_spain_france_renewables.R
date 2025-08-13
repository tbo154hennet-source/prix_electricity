library(ggplot2)

save_path <- "Z:/prix_electricite_gitlab/Data_structuring/output"
setwd(save_path)

my_generation_data <- read_parquet('generation_data.parquet')
colnames(my_generation_data)

ggplot(my_generation_data, aes(x = datetime, y = generation)) +
  geom_line(aes(y = solar, color = "Solar")) +
  geom_line(aes(y = wind, color = "Wind")) +
  labs(title = "Electricity generation in France",
       x = "Date",
       y = "Generation (MWh)")

monthly_generation_data <- my_generation_data %>% 
  mutate(month= month(datetime),
         year = year(datetime)) %>% 
  # select(datetime, month, year, solar, wind_actual) %>% 
  group_by( year, month) %>% 
  summarise(solar = mean(solar, na.rm = TRUE),
            wind = mean(wind, na.rm = TRUE)) %>% 
  mutate(datetime = as.Date(paste0(year, "-", month, "-01")))

price_renew <- read_parquet( 'price_renew.parquet')

ggplot()+
  geom_line(data = price_renew, aes(x = as.Date(datetime), y = solar, color = "Solar bis")) +
  geom_line(data = monthly_generation_data,  aes(x = datetime, y = solar, color = "Solar")) +
  geom_line(data = price_renew, aes(x = as.Date(datetime), y = wind, color = "Solar bis")) +
  geom_line(data = monthly_generation_data,  aes(x = datetime, y = wind, color = "Solar")) +
  labs(title = "Electricity generation and price in France",
       x = "Date",
       y = "Generation (MWh)")



save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/macro_exercise'
setwd(save_path_esp)
price_renew_final <- read_parquet( 'price_renew.parquet') %>% 
  mutate(datetime = as.Date(paste0(year, "-", month, "-01")))

ggplot()+
  geom_line(data = price_renew, aes(x = as.Date(datetime), y = solar, color = "France")) +
  geom_line(data = price_renew_final,  aes(x = datetime, y = solar, color = "Espagne")) +
  geom_line(data = price_renew, aes(x = as.Date(datetime), y = wind, color = "France")) +
  geom_line(data = price_renew_final,  aes(x = datetime, y = wind, color = "Espagne")) +
  labs(title = "Electricity generation and price in France",
       x = "Date",
       y = "Generation (MWh)")

ggplot()+
  geom_line(data = price_renew, aes(x = as.Date(datetime), y = renewable_generation, color = "France")) +
  geom_line(data = price_renew_final,  aes(x = datetime, y = renewable_generation, color = "Espagne")) +
  labs(title = "Electricity generation and price in France",
       x = "Date",
       y = "Generation (MWh)")

model_fr = lm(log_price ~ solar + wind +
                as.factor(year) + as.factor(month), data = price_renew)

summary(model_fr)

model_esp = lm(log_price ~ solar + wind+
                 as.factor(year) + as.factor(month), data = price_renew_final)

summary(model_esp)

model_fr = lm(log_price ~ renewable_generation +
                as.factor(year) + as.factor(month), data = price_renew)

summary(model_fr)

model_esp = lm(log_price ~  renewable_generation +
                 as.factor(year) + as.factor(month), data = price_renew_final)

summary(model_esp)

