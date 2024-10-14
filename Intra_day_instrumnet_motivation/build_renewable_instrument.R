
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices')

price_data <- NULL
my_files <- list.files()
# Obtenez la liste des fichiers .parquet
#my_files <- my_files[grep("\\.csv$", my_files)]
# Supprimez les fichiers .parquet de la liste des fichiers
# my_files <- setdiff(my_files, parquet_files)

for (file in my_files){
  parquet_file <- read_parquet(file)
  print(file)
  if (is.null(price_data)){
    price_data<- parquet_file
  }
  else{
    price_data<- rbind(price_data, parquet_file)
  }
}  

price_data <-unique(price_data)
colnames(price_data) <- c('spot_price', 'datetime')


#setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot')
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Intraday_wind_and_solar_forecasts')
generation_data <- NULL
my_files <- list.files()
headers <- c("('Wind Onshore', 'Actual Aggregated')",
             "('Solar', 'Actual Aggregated')",
             'gen_tot',"datetime",
             "('Hydro Pumped Storage', 'Actual Aggregated')",
             "('Hydro Pumped Storage', 'Actual Consumption')",
             "('Hydro Run-of-river and poundage', 'Actual Aggregated')",
             "('Hydro Water Reservoir', 'Actual Aggregated')"
             )
hydro_headers <- c("('Hydro Pumped Storage', 'Actual Aggregated')",
                   "('Hydro Pumped Storage', 'Actual Consumption')",
                   "('Hydro Run-of-river and poundage', 'Actual Aggregated')",
                   "('Hydro Water Reservoir', 'Actual Aggregated')")

for (file in my_files){
  parquet_file <- read_parquet(file)
  colnames(parquet_file)[ncol(parquet_file)] <- 'datetime'
  parquet_file <- parquet_file  %>%
    rowwise() %>%
    mutate(across(-datetime, as.numeric))%>%
    mutate(gen_tot = sum(c_across(-datetime), na.rm = TRUE))
  
  generation_file <- parquet_file[, headers]
  
  colnames(generation_file)[1:2] <- c('wind' , 'solar')
  generation_file$hydro_production <- generation_file %>%
    select(all_of(hydro_headers )) %>%
    rowSums(na.rm = TRUE)
  
  if (is.null(generation_data)){
    generation_data<- generation_file
  }
  else{
    generation_data<- rbind(generation_data, generation_file)
  }
}  
  
  

  # Fonction pour calculer la moyenne pondérée
weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

print(class(generation_data))
nrow(generation_data)

my_generation_data <- generation_data %>% 
  ungroup() %>%  # Ungroup the row-wise data frame
  as.data.frame()  # Convert to standard data frame
# Sample the data

renewable_data <- my_generation_data %>% 
  mutate(month= month(datetime),
         year = year(datetime))

renewable_test <- renewable_data %>% 
  # select(datetime, month, year, solar, wind_actual) %>% 
  group_by(month, year) %>% 
  summarise_all( mean, na.rm = TRUE) %>% 
  ungroup()


# adding demand growth from month to month
renewable_test$renewable_generation <- renewable_test$solar +
  renewable_test$wind

renewable_test <- renewable_test %>% 
  arrange(year, month) %>% 
  mutate(demand_growth_period = log(gen_tot) - log(lag(gen_tot)),
         renewable_growth = log(renewable_generation) - log(lag(renewable_generation)))


# write_parquet(renewable_test, 'renewable_test.parquet')
# renewable_test <- read_parquet('renewable_test.parquet')


price_data <- merge(price_data, my_generation_data[, c("gen_tot", "datetime")], by='datetime')

price_data <- price_data %>% 
  mutate(month= month(datetime),
         year = year(datetime)) %>% 
  # select(datetime, month, year, solar, wind_actual) %>% 
  group_by( year, month) %>% 
  summarise(spot_price = weighted_mean(spot_price, gen_tot)) %>% 
  ungroup() %>% 
  arrange( year, month) %>% 
  mutate(log_price = log(spot_price)) %>% 
  mutate(log_diff = log_price - lag(log_price))



price_renew <- merge(price_data,
               renewable_test, by= c('month', 'year')) 

price_renew_hors_crise <- price_renew %>% 
  filter(year!='2022')

setwd(save_path)

write_parquet(price_renew, 'price_renew.parquet')

write_parquet(price_renew_hors_crise, 'price_renew_hors_crise.parquet')


