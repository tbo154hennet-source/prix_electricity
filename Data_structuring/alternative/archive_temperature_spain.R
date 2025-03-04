save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)
library(readxl)

temp_instrument_data <- read.csv("monthly_temperature_spain (1996-2023).csv") %>% 
  filter(place != "Melilla" &place != "Ceuta" )

# Create a data frame with the province names and population data
population_data <- data.frame(
  Province = c("Murcia", "Lugo", "Pontevedra", "Ourense", "Cáceres", 
               "Badajoz", "Álava", "Valencia", "Alicante", "Madrid", 
               "Melilla", "Ceuta", "Lleida", "Girona", "Tarragona", 
               "Barcelona", "Guadalajara", "Cuenca", "Toledo", "Albacete", 
               "León", "Zamora", "Valladolid", "Soria", "Burgos", 
               "Segovia", "Ávila", "Salamanca", "Palencia", "Huesca", 
               "Zaragoza", "Teruel", "Jaén", "Huelva", "Granada", 
               "Córdoba", "Cádiz", "Sevilla", "Almería", "Málaga"),
  Population = c(1474000, 331000, 942000, 307000, 413000, 
                 672000, 333000, 2600000, 1900000, 6700000, 
                 87000, 85000, 439000, 781000, 810000, 
                 5600000, 258000, 198000, 711000, 388000, 
                 462000, 174000, 520000, 88000, 356000, 
                 156000, 159000, 347000, 160000, 221000, 
                 973000, 134000, 631000, 523000, 919000, 
                 786000, 1240000, 1900000, 727000, 1700000)
)

colnames(temp_instrument_data)
temp_instrument_data <- temp_instrument_data %>% 
  left_join(population_data, by = c("place" = "Province")) %>% 
  group_by(place) %>%
  summarise()

population_data %>% 
  arrange(desc(Population))

unique(temp_instrument_data$place)


