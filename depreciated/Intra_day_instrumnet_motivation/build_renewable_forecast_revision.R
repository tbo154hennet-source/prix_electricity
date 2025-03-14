
load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast/day_ahead"
setwd(load_path_data)

wind_solar_forecast <- read_parquet('wind_solar_forecast.parquet')

load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast/intradaily"
setwd(load_path_data)
Intraday_wind_solar_forecast <- read_parquet('Intraday_wind_solar_forecast.parquet')

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/outputs'
setwd(save_path_esp)
generation_by_fuel <- read_parquet('generation_by_fuel_format.parquet')

colnames(generation_by_fuel)[4:7] <- c('Solar', 'Wind Onshore', 
           'Wind Offshore', 'Hydro Run-of-river and poundage')
colnames(wind_solar_forecast)[1:2] <- c('Fore_Wind', 'Fore_Solar')
colnames(Intraday_wind_solar_forecast)[1:2] <- paste0('Intraday_', 
                               colnames(wind_solar_forecast)[1:2])

renewable_forecasts <- merge(wind_solar_forecast, Intraday_wind_solar_forecast)
renewable_forecasts <- merge(renewable_forecasts, generation_by_fuel)


renewable_forecasts <- renewable_forecasts %>% 
  mutate(renewable_forecast = Fore_Wind + Fore_Solar + `Hydro Run-of-river and poundage`,
         unexpected_renewables = gen_uncontrolled - renewable_forecast,

         intraday_update_renewables = Intraday_Fore_Wind + Intraday_Fore_Solar - renewable_forecast)


write_parquet(renewable_forecasts, 'renewable_forecasts.parquet')

