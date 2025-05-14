
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

temp_instrument_data <- t(read_excel('temperature_data.xlsx'))
temp_instrument_data <- as.data.frame(temp_instrument_data[c(-1,-2),])
colnames(temp_instrument_data) <- c("T_avg", "T_min", "T_max")
temp_instrument_data$Date <- rownames(temp_instrument_data)
temp_instrument_data <- temp_instrument_data %>% 
  mutate(Date = as.Date(paste0(Date,"-01"), format = "%Y-%m-%d"),
         month = month(Date),
         year = year(Date)) %>% 
  filter(year >= 2014)
library(lubridate)

# test <- sample_n(temp_instrument_data, 100)
# 
# colnames(test)

#quantitative instrument for temperature

cold_months <- c(1, 2, 11, 12)
hot_months <- c(6, 7, 8, 9)

#class(temp_instrument_data$T_avg)

mean_temperatures <- temp_instrument_data %>% 
  group_by(month) %>% 
  summarise(mean_temp = mean(as.numeric(T_avg), na.rm = TRUE))

month=1

mean_temp_instrument <- temp_instrument_data %>% 
  mutate(cold_excess_instrument = -ifelse(month %in% cold_months, 
                                         as.numeric(T_avg) - mean_temperatures[month, 'mean_temp'][[1]], 0),
  heat_excess_instrument = ifelse(month %in% hot_months,
                                  as.numeric(T_avg) - mean_temperatures[month, 'mean_temp'][[1]], 0))


getwd()
library(openxlsx)
#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data/esp")
write.xlsx(mean_temp_instrument, 'temperature_instrument.xlsx')


