
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
gas_data_shock$log_shock_serie <-  log(gas_data_shock$Gas_TTF) - log(gas_data_shock$Gas_TTF_previous_day)

gas_data_shock <- gas_data_shock %>% 
  arrange(Date) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_gasi = rollapply(log_shock_serie, width = 30, FUN = mean, fill = NA),
         moving_std_gasi = rollapply(log_shock_serie, width = 30, FUN = sd, fill = NA),
         log_shock_serie_relative = log_shock_serie/moving_std_gasi,
  )




good_news_count <- news_data_bert %>% 
  select(media, datetime,title) %>%
  distinct() %>% 
  group_by(datetime) %>% 
  summarise(news_per_day = n())


full_dates <- tibble(datetime = seq(min(news_data$datetime, na.rm=TRUE), max(news_data$datetime, na.rm=TRUE), by = "day"))


# Joindre avec les données de nouvelles
good_complete_data <- full_dates %>%
  left_join(good_news_count, by = c("datetime")) %>%
  replace_na(list(news_per_day = 0))

#on veut selectionner les jours d'annonce qui sont à dominante un supply shock
# on demande 4 fois plus de supply news que demand news
library(lubridate)
shock_data_good <- good_complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news),
         month = month(datetime),
         year = year(datetime)
  )



info_good <- left_join(gas_data_shock, shock_data_good,
                               by = c("Date" = "datetime"))

instrument_days_good <- shock_data_good %>% 
  filter(binary_shock == TRUE & news_per_day>=3)

shock_info_good <- info_good %>% 
  mutate(order = abs(log_shock_serie),
         ptstat = (news_per_day -moving_avg_news)/moving_std_news) %>% 
  arrange(desc(order)) %>% 
  slice(1:30) %>% 
  select(Date, order, news_per_day, 
         ptstat, binary_shock)





#small regression assesment of predictability


model1 <- lm(abs(log_shock_serie) ~ binary_shock + factor(month) + factor(year),
             data = info_good)
model2 <- lm(abs(log_shock_serie) ~ news_per_day+ factor(month) + factor(year), data = info_good)

print_no_factors <- function(model){
  # Extraction des coefficients du résumé de la régression
  coef_matrix <- coef(summary(model))
  
  # Suppression des coefficients contenant "month" ou "year" dans leurs noms
  coef_no_factors <- coef_matrix[!grepl("month|year", rownames(coef_matrix)), ]
  
  # Affichage des coefficients filtrés
  return(coef_no_factors)
}
# Extraire les coefficients qui ne sont pas des facteurs

print_no_factors(model1)
print_no_factors(model2)

model3 <- lm(abs(log_shock_serie_relative) ~ binary_shock, data = info_good)
model4 <- lm(abs(log_shock_serie_relative) ~ news_per_day, data = info_good)

print_no_factors(model3)
print_no_factors(model4)


