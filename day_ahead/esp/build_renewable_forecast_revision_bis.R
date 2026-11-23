
load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES/day_ahead"
setwd(load_path_data)


# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_forecast <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES/intradaily"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
intraday_wind_solar_forecast  <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


#save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel'
save_path_esp <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/esp/by_fuel"
setwd(save_path_esp)

# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
generation_by_fuel  <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


# Sélectionner les colonnes spécifiques pour calculer la somme
wind_solar_headers <- c(
  "('Solar', 'Actual Aggregated')",
  "('Wind Offshore', 'Actual Aggregated')",
  "('Wind Onshore', 'Actual Aggregated')"
)

thermal_headers <- c(
  "('Biomass', 'Actual Aggregated')",                        
  "('Fossil Brown coal/Lignite', 'Actual Aggregated')",      
  "('Fossil Coal-derived gas', 'Actual Aggregated')",        
  "('Fossil Gas', 'Actual Aggregated')"  ,                   
  "('Fossil Hard coal', 'Actual Aggregated')",               
  "('Fossil Oil', 'Actual Aggregated')"   ,                  
  "('Fossil Oil shale', 'Actual Aggregated')" ,              
  "('Fossil Peat', 'Actual Aggregated')"
)

hydro_headers <- c(
  "('Hydro Pumped Storage', 'Actual Aggregated')",           
  "('Hydro Pumped Storage', 'Actual Consumption')"  ,        
  "('Hydro Run-of-river and poundage', 'Actual Aggregated')",
  "('Hydro Water Reservoir', 'Actual Aggregated')",          
  "('Marine', 'Actual Aggregated')"
)

# Calculer la somme des colonnes spécifiées
generation_by_fuel_final <- generation_by_fuel %>%
  rowwise() %>%
  mutate(across(-`__index_level_0__`, as.numeric)) %>%
  mutate(
    gen_tot = sum(c_across(-`__index_level_0__`), na.rm = TRUE),# Nouvelle colonne pour la somme
    wind_solar_tot = sum(c_across(all_of(wind_solar_headers)), na.rm = TRUE),
    hydro_tot = sum(c_across(all_of(hydro_headers)), na.rm = TRUE),
    thermal_tot = sum(c_across(all_of(thermal_headers)), na.rm = TRUE)
  ) %>%
  ungroup()

# generation_by_fuel %>%
#   #slice(1:10) %>%
#   slice((nrow(generation_by_fuel)-10):nrow(generation_by_fuel)) %>%
#   rowwise() %>%
#   summarise(row_sum = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
#   ungroup()

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot")
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/formatted_OMIE")
write_parquet(generation_by_fuel_final, 'generation_by_fuel_format_esp.parquet')

# setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot")
generation_by_fuel_final <- read_parquet('generation_by_fuel_format_esp.parquet')

colnames(generation_by_fuel_final)

wind_solar_actual <- generation_by_fuel_final[,c("('Solar', 'Actual Aggregated')",
                                                 "('Wind Offshore', 'Actual Aggregated')",
                                                 "('Wind Onshore', 'Actual Aggregated')",
                                                 "('Nuclear', 'Actual Aggregated')" ,
                                                 'gen_tot', 'wind_solar_tot', 
                                                 "thermal_tot", "hydro_tot", "__index_level_0__")] %>% 
  rename(nuclear = "('Nuclear', 'Actual Aggregated')")


colnames(wind_solar_actual)[1:3] <- c('Solar', 'Wind Offshore','Wind Onshore')
colnames(day_ahead_forecast)[1:3] <- c( 'Fore_Solar','Fore_Wind','__index_level_0__')
colnames(intraday_wind_solar_forecast)[1:3] <- c('intraday_Solar','intraday_Wind', "__index_level_0__")

renewable_forecasts <- left_join(day_ahead_forecast, intraday_wind_solar_forecast)
renewable_forecasts <- left_join(renewable_forecasts, wind_solar_actual)

colnames(renewable_forecasts)
renewable_forecasts <- renewable_forecasts %>%
  mutate(across(c('Wind Offshore' ), ~ replace(., is.na(.), 0))) %>% 
  mutate(
          day_ahead_forecast = Fore_Wind + Fore_Solar ,
         intraday_forecast = intraday_Wind + intraday_Solar  ,
         unpredicted_renewables = wind_solar_tot - intraday_forecast ,
         intraday_update_renewables = intraday_forecast - day_ahead_forecast)

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
setwd(save_path_data)
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
setwd(save_path_data)
write_parquet(renewable_forecasts, 'renewable_forecasts_esp.parquet')


