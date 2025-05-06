library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)

# Fonction pour appeler l’API pour un intervalle de dates
fetch_batch <- function(start_date, end_date) {
  base_url <- "https://data.elexon.co.uk/bmrs/api/v1/forecast/generation/wind-and-solar/day-ahead"
  
  url <- paste0(
    base_url,
    "?from=", start_date,
    "&to=", end_date,
    "&settlementPeriodFrom=36&settlementPeriodTo=12&processType=all"
  )
  
  response <- GET(url)
  if (status_code(response) != 200) {
    warning(paste("Erreur pour:", start_date, "-", end_date))
    return(NULL)
  }
  
  data_parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))
  df <- data_parsed$data
  
  return(df)
}

# Liste des dates par pas de 6 jours
start <- as.Date("2020-01-01")
end <- as.Date("2024-12-31")
date_batches <- seq(start, end, by = 6)

# Télécharger les données par lot
all_data <- map2_dfr(
  date_batches,
  lead(date_batches, default = end),
  ~ fetch_batch(as.character(.x), as.character(.y))
)

# Nettoyage final
all_data_clean <- all_data %>%
  mutate(
    publishTime = ymd_hms(publishTime),
    startTime = ymd_hms(startTime),
    settlementDate = ymd(settlementDate),
    businessType = factor(businessType),
    processType = factor(processType),
    psrType = factor(psrType)
  ) %>%
  arrange(settlementDate, settlementPeriod)




# Aperçu
print(head(all_data_clean))


#i compare the data for the year 2020 where I still have ENTSOE
load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/other_EU"
setwd(load_path_data)
write_parquet(all_data_clean, "GB_2020_2024")
all_data_clean <- read_parquet( "GB_2020_2024")



load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/other_EU/GB"
setwd(load_path_data)

list.files()

# Lister tous les fichiers avec l'extension .parquet dans le répertoire courant
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_forecast <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)


df_clean_wide <- pivot_wider(all_data_clean %>% 
                               select(-businessType)
                               ,
                             names_from = "psrType",
                             values_from = "quantity")
#attention frequency mismatch
#hourly versus bihourly
colnames(df_clean_compare)
#on prend les premiers jours de juin pour exemple

df_clean_compare <- df_clean_wide %>%
  filter(settlementDate >= "2020-01-05" & settlementDate <= "2020-01-06") 
day_ahead_forecast_compare <- day_ahead_forecast %>%
  filter(`__index_level_0__` >= as.POSIXct("2020-01-05") & `__index_level_0__` <= as.POSIXct("2020-01-06"))

library(ggplot2)
ggplot()+
  geom_line(data = df_clean_compare,
            aes(x = startTime, y = Solar, color = "GB API")) +
  geom_line(data = day_ahead_forecast_compare,
            aes(x = `__index_level_0__`, y = Solar, color = "ENTSOE")) +
  theme_minimal()

#ce sont exactement les mêmes données
colnames(df_clean_wide)
df_clean_compare_monthly <- df_clean_wide %>%
  mutate(year = year(startTime), 
         month = month(startTime)) %>%
  group_by(year, month) %>% 
  summarise(Renewable  = mean(Solar, na.rm = TRUE) +
              mean(`Wind Onshore`, na.rm = TRUE) + mean(`Wind Offshore`, na.rm = TRUE) ) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-01")))


day_ahead_forecast_compare_monthly <- day_ahead_forecast %>%
  mutate(year = year(`__index_level_0__`), 
         month = month(`__index_level_0__`)) %>%
  group_by(year, month) %>% 
  summarise(Renewable  = mean(Solar, na.rm = TRUE) +
              mean(`Wind Onshore`, na.rm = TRUE) + mean(`Wind Offshore`, na.rm = TRUE) ) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-01")))
  


ggplot()+
  geom_line(data = df_clean_compare_monthly,
            aes(x = Date, y = Renewable, color = "GB API")) +
  geom_line(data = day_ahead_forecast_compare_monthly,
            aes(x = Date, y = Renewable, color = "ENTSOE")) +
  theme_minimal()


final_GB_data <- df_clean_wide %>% 
  rename(`__index_level_0__` = startTime) %>% 
  select(colnames(day_ahead_forecast)) %>% 
  filter(`__index_level_0__` >= as.POSIXct("2020-06-01")) %>% 
  rbind(day_ahead_forecast%>%
              filter(`__index_level_0__` < as.POSIXct("2020-06-01")
              )) %>% 
  arrange(`__index_level_0__`) %>% 
  group_by(`__index_level_0__`) %>%
  slice_head(n = 1) %>% 
  ungroup()



# GB_forecast <- GB_forecast %>%
#   mutate(`__index_level_0__` = with_tz(as.POSIXct(`__index_level_0__`), tzone = "Europe/Brussels"))


getwd()
write_parquet(final_GB_data, "GB_data.parquet")







