
load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast/day_ahead"
setwd(load_path_data)


# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_forecast <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast/intradaily"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
intraday_wind_solar_forecast  <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel'
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

# Calculer la somme des colonnes spécifiées
generation_by_fuel_final <- generation_by_fuel %>%
  rowwise() %>%
  mutate(across(-`__index_level_0__`, as.numeric)) %>%
  mutate(
    gen_tot = sum(c_across(-`__index_level_0__`), na.rm = TRUE),
    wind_solar_tot = sum(c_across(all_of(wind_solar_headers)), na.rm = TRUE) # Nouvelle colonne pour la somme
  ) %>%
  ungroup()


save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel'
setwd(save_path_esp)
write_parquet(generation_by_fuel_final, 'generation_by_fuel_format.parquet')

generation_by_fuel_final <- read_parquet('generation_by_fuel_format.parquet')

colnames(generation_by_fuel_final)


wind_solar_actual <- generation_by_fuel_final[,c("('Solar', 'Actual Aggregated')",
                                                 "('Wind Offshore', 'Actual Aggregated')",
                                                 "('Wind Onshore', 'Actual Aggregated')",
                                                 'gen_tot', 'wind_solar_tot', "__index_level_0__")]


colnames(wind_solar_actual)[1:3] <- c('Solar', 'Wind Offshore','Wind Onshore')
colnames(day_ahead_forecast)[1:4] <- c( 'Fore_Solar','Fore_Wind','__index_level_0__')
colnames(intraday_wind_solar_forecast)[1:4] <- c('intraday_Solar','intraday_Wind', "__index_level_0__")

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

load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/renewable_forecast"
setwd(load_path_data)

write_parquet(renewable_forecasts, 'renewable_forecasts_esp.parquet')


