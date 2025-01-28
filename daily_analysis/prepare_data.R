


#get indispos
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)

final_publi_ENTSOE_indispo <- read_parquet('final_publi_ENTSOE_indispo.parquet') %>% 
  arrange(electricity_delivery_date, auction_before, creneau)

final_publi_nuclear_ENTSOE_indispo <- read_parquet('final_publi_nuclear_ENTSOE_indispo.parquet') %>% 
  arrange(electricity_delivery_date, auction_before, creneau)

tot_publi_ENTSOE_indispo <- final_publi_ENTSOE_indispo %>% 
  rename(Published.Unavailable.Capacity = Unavailable.Capacity)  %>%
  left_join(final_publi_nuclear_ENTSOE_indispo %>% 
              rename(Published.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity'))) %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1)
                               , format="%Y-%m-%d %H", tz = 'UTC'),
         creneau= as.factor(creneau))



#get renewable average

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel'
setwd(save_path_esp)
generation_by_fuel_final <- read_parquet('generation_by_fuel_format.parquet')


wind_solar_actual <- generation_by_fuel_final[,c("('Solar', 'Actual Aggregated')",
                                                 "('Wind Offshore', 'Actual Aggregated')",
                                                 "('Wind Onshore', 'Actual Aggregated')",
                                                 'gen_tot', 'wind_solar_tot', "__index_level_0__")]


colnames(wind_solar_actual)[1:3] <- c('Solar','Wind Offshore', 'Wind Onshore' 
                                      )

wind_solar_actual$date <- as.Date(wind_solar_actual$`__index_level_0__`)

# renewable_daily <- wind_solar_actual %>% 
#   group_by(date) %>% 
#   summarise_all(mean, na.rm = TRUE) 
# 
# daily_ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>%
#   mutate(electricity_delivery_date = as.Date(electricity_delivery_date, format = "%Y-%m-%d")) %>% 
#   group_by(electricity_delivery_date) %>% 
#   summarise(Published.Unavailable.Nuclear.Capacity = sum(Published.Unavailable.Nuclear.Capacity, na.rm = TRUE),
#             Published.Unavailable.Capacity = sum(Published.Unavailable.Capacity, na.rm = TRUE)) %>%
#   ungroup()

ENTSOE_indispo <- tot_publi_ENTSOE_indispo %>% 
  group_by(datetime) %>% 
  summarise(Published.Unavailable.Capacity = sum(Published.Unavailable.Capacity, na.rm = TRUE),
            Published.Unavailable.Nuclear.Capacity = sum(Published.Unavailable.Nuclear.Capacity, na.rm = TRUE)) 

instrument_data <- wind_solar_actual %>% 
  left_join(ENTSOE_indispo , by = c("__index_level_0__" = 'datetime')) 


#day-ahead prices

day_ahead_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/esp'
setwd(day_ahead_path)

# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
day_ahead_prices <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)

colnames(day_ahead_prices)[1] <- c('spot_price')

reg_data <- day_ahead_prices %>% 
  left_join(instrument_data) %>% 
  mutate(log_price = log(spot_price))
  
reg_data <- reg_data %>% 
  replace_na(
    list(
      Published.Unavailable.Capacity = 0,
      Published.Unavailable.Nuclear.Capacity = 0
    )
  )%>%
  filter(!is.infinite(log_price))



reg_data <- reg_data[complete.cases(reg_data), ]

# perform first stage regression
m_basic <- lm(
  log(spot_price+1) ~ log(wind_solar_tot+1)+ log(Published.Unavailable.Capacity+1) + 
    log(Published.Unavailable.Nuclear.Capacity+1) + log(gen_tot+1),
  data= reg_data)

# dummy for creneau

m_time_FE <- lm(
  log(spot_price+1) ~ log(wind_solar_tot+1)+ log(Published.Unavailable.Capacity+1) + 
    log(Published.Unavailable.Nuclear.Capacity+1) + log(gen_tot+1) + as.factor(date) ,
  data= reg_data)

# Create LaTeX code with regression results
stargazer(m_basic, m_time_FE,
                       type = "text",
                       title = "Regression Results",
                       column.labels = c("Basic", "Timeslot FE", 'Timeslot + Auction FE')
)
