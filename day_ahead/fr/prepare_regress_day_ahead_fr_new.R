library(data.table)
library(dplyr)
library(tidyr)  
library(arrow)
library(lubridate)



paths <- c(
  "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/fr/Generation_forecast",
  "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/fr/Load_forecast",
  "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/fr/Load_final"
)
# path <- paths[1]
# file <- files[1]
# test2 <- read_parquet(file)
# head(test2)
read_parquet_folder <- function(path) {
  files <- list.files(path, pattern = "\\.parquet$", full.names = TRUE)
  
  df_list <- lapply(files, function(file) {
    message(basename(file))
    read_parquet(file)
  })
  
  unique(do.call(rbind, df_list))
}
test <- unique(do.call(rbind, df_list))
data_list <- lapply(paths, read_parquet_folder)

gen_data <- Reduce(function(x, y) {
  inner_join(x, y, by = "__index_level_0__")
}, data_list)

colnames(gen_data) <- c('gen_forecast',
 'datetime', "load_forecast", "load_tot")
#gen_data$gen_forecast_net <- gen_data$gen_forecast -  gen_data$gen_forecast_conso 
gen_data <- gen_data %>%
  mutate(date = as.Date(datetime))
tail(gen_data)
# gen_data <- gen_data %>% 
#   filter(datetime <= threshold_date)
threshold_date_gen <- gen_data %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(n_obs = n_distinct(datetime), .groups = "drop") %>%
  filter(n_obs > 30) %>%          # quart-horaire (ou plus fin)
  summarise(threshold_date = min(date,na.rm = TRUE)) %>%
  pull(threshold_date)

class(threshold_date_gen)
class(dt$datetime)
#attention gen_data is wrong for the last 3 months
#not provided

library(data.table)
dt <- as.data.table(gen_data)
dt_hourly_after_threshold <- dt[
  date >= threshold_date_gen,
  .(
    gen_forecast    = mean(gen_forecast, na.rm = TRUE),
    #gen_forecast_conso = mean(gen_forecast_conso, na.rm = TRUE),
    #gen_forecast_net = mean(gen_forecast_net, na.rm = TRUE),
    load_forecast   = mean(load_forecast, na.rm = TRUE),
    load_tot        = mean(load_tot, na.rm = TRUE)

  ),
  by = .(
    datetime = as.POSIXct(
      paste(as.Date(datetime),
            sprintf("%02d:00:00", hour(datetime))),
      tz = "Europe/Madrid"
    )
  )
]
dt_hourly <- dt[ date <= threshold_date_gen,]

gen_data <- full_join(dt_hourly,
        dt_hourly_after_threshold
        )

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(gen_data, "fr_generation_day_ahead.parquet")




setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/fr")
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
# Liste des noms de colonnes souhaités
colnames_target <- c( "price", "datetime")

# Lecture + renommage dans lapply
day_ahead_prices <- parquet_files %>%
  lapply(function(file) {
    df <- read_parquet(file)
    names(df) <- colnames_target
    return(df)
  }) %>%
  bind_rows() %>%
  arrange(datetime)


day_ahead_prices <- day_ahead_prices %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         delivery_date = as.Date(datetime),
         price = as.numeric(price)
         ) %>% 
  #filter(delivery_date >= as.Date('2018-01-01')) %>% 
  arrange(datetime)

load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/FR/day_ahead"
setwd(load_path_data)

# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_forecast <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/FR/intraday"
setwd(load_path_data)
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
intraday_wind_solar_forecast  <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


day_ahead_forecast$Wind <- rowSums(day_ahead_forecast[, c("Wind Offshore","Wind Onshore")], na.rm = TRUE)
intraday_wind_solar_forecast$Wind <- rowSums(intraday_wind_solar_forecast[, c("Wind Offshore","Wind Onshore")], na.rm = TRUE)

colnames(day_ahead_forecast) <- c( 'Fore_Solar','Fore_Wind_On','__index_level_0__','Fore_Wind_Off', 'Fore_Wind')
colnames(intraday_wind_solar_forecast) <- c( 'intraday_Solar','intraday_Wind_On','__index_level_0__','intraday_Wind_Off', 'intraday_Wind')

renewable_forecasts <- left_join(day_ahead_forecast, intraday_wind_solar_forecast)
head(renewable_forecasts)


#add temperature  and other weather conditions

setwd("C:\\Users\\DBT3KD\\Desktop\\Etude_macro_elec\\prix_electricite_gitlab\\day_ahead\\daily_temp\\outputs")
list.files()
# Charger les données
temperature_demand_data <- as.data.table(read_parquet("temperature_pop_fr.parquet"))
temperature_demand_data_2026 <- as.data.table(read_parquet("temperature_pop_fr_2026.parquet"))

tempall_data_data <- rbind(temperature_demand_data,
							temperature_demand_data_2026)

colnames(tempall_data_data)
T_base <- 18
temperature_openmeteo_data_test <- tempall_data_data %>%
  mutate(
    heat_excess   = pmax(temperature_2m - T_base, 0),
    cold_excess      = pmax(T_base - temperature_2m, 0),
    #distnce à la tempérautre de confort 
    #18°C dans la littérature
    temperature_discomfort = abs(temperature_2m)
  )



setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
gas_prices <- read_parquet( "gas_price_formatted.parquet")

price_indispo_merge_fr <- left_join(gen_data,
                                     day_ahead_prices,
                                     by = c("datetime" ))

price_indispo_merge_fr  <- price_indispo_merge_fr %>%
  left_join(renewable_forecasts, by=c('datetime' = '__index_level_0__'))

price_indispo_merge_fr <- price_indispo_merge_fr  %>%
  left_join(temperature_openmeteo_data_test , by=c('datetime' = 'date'))

# pour l'estimation en niveau,
#on rajoute le spot gas price
library(readxl)

price_indispo_merge_fr  <- price_indispo_merge_fr  %>%
  left_join(gas_prices, by=c('delivery_date' = 'time'))


head(price_indispo_merge_fr  %>%
 filter(datetime >= as.POSIXct('2025-01-01 00:00:00', tz='Europe/Madrid'))
)
head(price_indispo_merge_fr  %>%
 filter(datetime >= as.POSIXct('2025-10-01 00:00:00', tz='Europe/Madrid'))
)
price_indispo_merge_fr <- price_indispo_merge_fr  %>% distinct(datetime, .keep_all = TRUE)

save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
write_parquet(price_indispo_merge_fr, "price_indispo_merge_fr.parquet")


# Winsoriser au 95e centile
p95 <- quantile(price_indispo_merge_fr$price, 0.95, na.rm = TRUE)

price_indispo_merge_fr <- price_indispo_merge_fr %>%
  mutate(price_wins = pmin(price, p95))

# Cr?er l'histogramme
price_histo <- ggplot(price_indispo_merge_fr, aes(x = price_wins)) +
  geom_histogram(aes(y = ..density..), binwidth = 1,
                 fill = "steelblue", color = "black", alpha = 0.5) +
  #geom_density(fill = "steelblue", alpha = 0.6) +
  labs(x = "Price (euros/MWh)", y = "Density") +
  theme_minimal()

price_histo
# Sauvegarder dans le dossier voulu
# ggsave(
#   filename = "Z:/Overleaf_elec/images/data_description/price_histo.png",
#   plot = price_histo,
#   width = 8, height = 5, dpi = 300
# )
