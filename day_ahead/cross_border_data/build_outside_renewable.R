library(arrow)
library(purrr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(data.table)
library(tibble)
library(readxl)


#I will put all these forecasts in the interconnexion border file
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

price_merge_CBF_tot <- read_parquet( "price_merge_CBF_tot.parquet")

#for spain first
#so I need fr and pt

load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/PT/day_ahead"
setwd(load_path_data)

list.files()
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_forecast <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)

#correct l'absence de tout un mois en prenant à la place la intradaily 
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/PT")
additional_forecast <- read_parquet("2018-07PT.parquet")

colnames(combined_forecast)

combined_forecast <- full_join(day_ahead_forecast, additional_forecast, by = c("__index_level_0__")) %>%
  mutate(
    `Wind Onshore` = coalesce(`Wind Onshore.y`, `Wind Onshore.x`),
    Solar = coalesce(Solar.y, Solar.x)
  ) %>%
  select(-`Wind Onshore.x`, -`Wind Onshore.y`, -Solar.x, -Solar.y) %>% 
  arrange(`__index_level_0__`)

day_ahead_forecast_pt <- combined_forecast %>%
  transmute(
    `__index_level_0__`,
    renewable_day_ahead_forecast_pt = rowSums(across(!`__index_level_0__`), na.rm = TRUE)
  )




load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/other_EU"
setwd(load_path_data)
colnames(price_merge_CBF_tot)
new_price_merge_CBF_tot <- price_merge_CBF_tot %>%
  left_join(day_ahead_forecast_pt, by = c("datetime"  = "__index_level_0__") )



#maintenant la partie pour la FR


load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/other_EU"
setwd(load_path_data)

final_GB_data <- read_parquet( "GB_data.parquet")

GB_forecast <- final_GB_data %>%
  transmute(
    `__index_level_0__`,
    renewable_day_ahead_forecast_gb = rowSums(select(., -`__index_level_0__`), na.rm = TRUE)
  )

new_price_merge_CBF_tot <- new_price_merge_CBF_tot %>%
  left_join(GB_forecast, by = c("datetime"  = "__index_level_0__") )



load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/other_EU"
setwd(load_path_data)

country <- "BE"
for (country in c("BE", "CH" , "DE" ,"IT_NORD")){
  setwd(country)
  # Lister tous les fichiers dans le dossier avec l'extension .parquet
  parquet_files <- list.files(pattern = "\\.parquet$")
  
  # Lire tous les fichiers parquet et les fusionner dans un seul dataframe
  day_ahead_forecast <- parquet_files %>%
    lapply(read_parquet) %>% # Lire chaque fichier parquet
    bind_rows() %>%  # Fusionner tous les dataframes
    arrange(`__index_level_0__`)
  
  
  day_ahead_forecast <- day_ahead_forecast %>%
    transmute(
      `__index_level_0__`,
      !!paste0("renewable_day_ahead_forecast_", tolower(country)) := rowSums(across(!`__index_level_0__`), na.rm = TRUE)
    )
  
  new_price_merge_CBF_tot <- new_price_merge_CBF_tot %>%
    left_join(day_ahead_forecast, by = c("datetime"  = "__index_level_0__") )
  setwd(load_path_data)
  
}

colnames(new_price_merge_CBF_tot)



#do the same for generation forecast

load_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/generation_tot/eu"
setwd(load_path_data)


for (country in list.files()){
  setwd(country)
  # Lister tous les fichiers dans le dossier avec l'extension .parquet
  parquet_files <- list.files(pattern = "\\.parquet$")
  
  # Lire tous les fichiers parquet et les fusionner dans un seul dataframe
  gen_day_ahead_forecast <- parquet_files %>%
    lapply(read_parquet) %>% # Lire chaque fichier parquet
    bind_rows() %>%  # Fusionner tous les dataframes
    arrange(`__index_level_0__`)
  
  gen_day_ahead_forecast <- gen_day_ahead_forecast %>%
    transmute(
      `__index_level_0__`,
      !!paste0("gen_tot_da_", tolower(country)) := rowSums(across(!`__index_level_0__`), na.rm = TRUE)
    )
  
  new_price_merge_CBF_tot <- new_price_merge_CBF_tot %>%
    left_join(gen_day_ahead_forecast, by = c("datetime"  = "__index_level_0__") )
  setwd(load_path_data)
  
}

colnames(new_price_merge_CBF_tot)

new_price_merge_CBF_tot <- new_price_merge_CBF_tot %>% 
  rename(renewable_day_ahead_forecast_fr = day_ahead_forecast_fr,
         renewable_day_ahead_forecast_esp = day_ahead_forecast_esp)

#save a final price_merge file
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")
write_parquet(new_price_merge_CBF_tot, "final_price_merge_CBF_tot.parquet")

colSums(is.na(new_price_merge_CBF_tot %>% 
                select(ntc_col, renew_col, gen_col)))

View( new_price_merge_CBF_tot %>% 
        mutate(is.na(renewable_day_ahead_forecast_gb)))
colnames(new_price_merge_CBF_tot)


attr(GB_forecast$`__index_level_0__`, "tzone")













