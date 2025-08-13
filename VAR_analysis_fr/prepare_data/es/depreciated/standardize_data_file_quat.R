
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data/esp")

my_files <- list.files()

my_files_xls <-  my_files[grep("\\.xls$$", my_files)]

my_files_xlsx <-  my_files[grep("\\.xlsx$", my_files)]

# Create a data frame with specified column names
dates <- data.frame(serie = character(),
                    start_date = as.Date(character()),
                    end_date = as.Date(character()),
                    stringsAsFactors = FALSE)

VAR_series <- NULL

for (file in my_files_xls){
  print(file)
  # Split the file name by underscore
  split_file <-  strsplit(file, split = "[_.]")
  # Extract the parts (strsplit returns a list)
  split_parts <- unlist(split_file)
  split_parts <- split_parts[3:length(split_parts)-1]
  name <- paste(split_parts, collapse = " ")
  # Concatenate the split parts with automatic line breaks
  title_text <- paste(strwrap(name, width = 30), collapse = "\n")
  excel_file <- read_excel(file, skip=10)
  
  excel_file$observation_date <- as.Date(excel_file$observation_date)
  # print(class(excel_file %>% head(1)))
  # print(colnames(excel_file %>% head(1)))
  
  start_date <- excel_file %>% head(1) %>% dplyr::select(observation_date)
  end_date <- excel_file %>% tail(1) %>% dplyr::select(observation_date)
  dates <<- rbind(dates, c(file, start_date[[1]], end_date[[1]]))
  
  if(is.null(VAR_series)){
    VAR_series <- excel_file
  }else{
    VAR_series <- merge( VAR_series ,excel_file, by= 'observation_date')
  }

}

for (file in my_files_xlsx){
  #file <- my_files_xlsx[1]
  print(file)
  # Split the file name by underscore
  split_file <-  strsplit(file, split = "[_.]")
  # Extract the parts (strsplit returns a list)
  split_parts <- unlist(split_file)
  split_parts <- split_parts[3:length(split_parts)-1]
  name <- paste(split_parts, collapse = " ")
  # Concatenate the split parts with automatic line breaks
  title_text <- paste(strwrap(name, width = 30), collapse = "\n")
  excel_file <- read_excel(file, sheet = "Monthly")
  
  excel_file$observation_date <- as.Date(excel_file$observation_date)
  # print(class(excel_file %>% head(1)))
  # print(colnames(excel_file %>% head(1)))
  
  start_date <- excel_file %>% head(1) %>% dplyr::select(observation_date)
  end_date <- excel_file %>% tail(1) %>% dplyr::select(observation_date)
  dates <<- rbind(dates, c(file, start_date[[1]], end_date[[1]]))
  
  if(is.null(VAR_series)){
    VAR_series <- excel_file
  }else{
    VAR_series <- merge( VAR_series ,excel_file, by= 'observation_date')
  }
  
}


colnames(VAR_series)
VAR_series <- VAR_series %>% 
  mutate(year = year(observation_date),
         month = month(observation_date))%>% 
  rename(ffr = ECBMRRFR,
         IPI = ESPPROINDMISMEI,
         NE_Index= `00XE00ESM086NEST`,
         E_Index = ENRGY0ESM086NEST,
         UnE = LRHUTTTTESM156S,
         Bond_rate = IR3TIB01ESM156N,
         Spot_gas =PNGASEUUSDM,
         HICP = CP0000ESM086NEST,
         CPElec = CP0451ESM086NEST
  )
VAR_series$Spot_gas
colnames(VAR_series)

save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge <- read_parquet("price_indispo_merge_esp_NTC.parquet")


#adjust for iberian exception gas cap

iberian_cap <- data.frame(
  Month = c("2022-06", "2022-07", "2022-08", "2022-09", "2022-10", "2022-11",
            "2022-12", "2023-01", "2023-02", "2023-03",
            "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09", 
            "2023-10", "2023-11", "2023-12"),
  Cap_EUR_MWh = c(40, 40, 40, 40, 40, 40,
                  45, 50, 55, 60,
                  55, 56.1, 57.2, 58.3, 59.4, 60.5,
                  61.6, 62.7, 63.8)
) %>% 
  mutate(date = as.Date(paste0(Month, "-01")),
        year = as.character(year(date)),
         month = month(date))


price_indispo_merge <- price_indispo_merge %>% 
  left_join(iberian_cap, by = c("year", "month")) %>%
  fill(spot_gas, .direction = "up")%>% # Backfill (bfill) 
  fill(spot_gas, .direction = "down") %>%    # Backfill (bfill)%>%
  mutate(spot_gas_capped = ifelse(spot_gas < Cap_EUR_MWh | is.na(Cap_EUR_MWh), spot_gas, Cap_EUR_MWh)) 


weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}


monthly_energy_data <- price_indispo_merge %>%
  arrange(datetime) %>% 
  mutate(
    Nuclear.Unavailabilities =
      #coalesce(Planned.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Published.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Uneffective.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Unpublished.Unavailable.Nuclear.Capacity, 0),
    
    Thermal.Unavailabilities =
      #coalesce(Planned.Unavailable.Capacity, 0) +
      coalesce(Published.Unavailable.Capacity, 0) +
      coalesce(Uneffective.Unavailable.Capacity, 0) +
      coalesce(Unpublished.Unavailable.Capacity, 0) -
      coalesce(Nuclear.Unavailabilities, 0),
    
    Predicted.Unavailabilities =
      #coalesce(Planned.Predicted.Unavailable.Capacity, 0) +
      coalesce(Published.Predicted.Unavailable.Capacity, 0) +
      coalesce(Uneffective.Predicted.Unavailable.Capacity, 0) +
      coalesce(Unpublished.Predicted.Unavailable.Capacity, 0)
  )%>% 
  group_by(month, year) %>%
  summarise(
    spot_price = weighted_mean(price, gen_tot),
    renewable_forecast = weighted_mean(day_ahead_forecast, gen_tot),
    wind_forecast = weighted_mean(Fore_Wind, gen_tot),
    solar_forecast = weighted_mean(Fore_Solar, gen_tot),
    nuclear_unavailabilities = weighted_mean(Nuclear.Unavailabilities, gen_tot),
    thermal_unavailabilities = weighted_mean(Thermal.Unavailabilities, gen_tot),
    predicted_unavailabilities = weighted_mean(Predicted.Unavailabilities, gen_tot),
    # NTC_ESP_FR = mean(NTC_ESP_FR, na.rm = TRUE),
    # NTC_FR_ESP = mean(NTC_FR_ESP, na.rm = TRUE),
    gen_tot = mean(gen_tot, na.rm = TRUE),
    gen_tot_da = mean(gen_tot_da, na.rm = TRUE),
    spot_gas_capped = mean(spot_gas_capped, na.rm = TRUE),
    spot_gas= mean(spot_gas, na.rm = TRUE),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()



# View(monthly_energy_data %>%
#        dplyr::select(datetime, spot_gas, spot_gas_capped))
# 

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/cross_border")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

price_merge_CBF <- read_parquet("price_merge_CBF_max.parquet")
colnames(price_merge_CBF)
monthly_crossborder_data <- price_merge_CBF %>% 
  filter(gen_tot_fr > 0 & gen_tot_esp > 0) %>%
  group_by(month, year) %>%
  summarise(
    renewable_day_ahead_forecast_ex_es = weighted_mean(renewable_day_ahead_forecast_pt +
                                                         renewable_day_ahead_forecast_fr , 
                                                       gen_tot_esp),
    cbf_es = weighted_mean(CBF_ES_pred, gen_tot_esp),
    
    renewable_day_ahead_forecast_pt = weighted_mean(renewable_day_ahead_forecast_pt, gen_tot_esp),
    renewable_day_ahead_forecast_fr = weighted_mean(renewable_day_ahead_forecast_fr, 
                                                    gen_tot_esp),
    
    
    renewable_day_ahead_forecast_ex_fr = weighted_mean(renewable_day_ahead_forecast_esp+
                                                         renewable_day_ahead_forecast_it_nord+
                                                         renewable_day_ahead_forecast_de+
                                                         renewable_day_ahead_forecast_be+
                                                         renewable_day_ahead_forecast_ch+
                                                         renewable_day_ahead_forecast_gb, 
                                                       gen_tot_fr),
    cbf_fr = weighted_mean(CBF_FR_pred, gen_tot_fr),
    renewable_day_ahead_forecast_esp = weighted_mean(renewable_day_ahead_forecast_esp, gen_tot_fr),
    renewable_day_ahead_forecast_it_nord = weighted_mean(renewable_day_ahead_forecast_it_nord, gen_tot_fr),
    renewable_day_ahead_forecast_de = weighted_mean(renewable_day_ahead_forecast_de, gen_tot_fr),
    renewable_day_ahead_forecast_be = weighted_mean(renewable_day_ahead_forecast_be, gen_tot_fr),
    renewable_day_ahead_forecast_ch = weighted_mean(renewable_day_ahead_forecast_ch, gen_tot_fr),
    renewable_day_ahead_forecast_gb = weighted_mean(renewable_day_ahead_forecast_gb, gen_tot_fr),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()

#monthly_crossborder_data$renewable_forecast_hors_esp


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data')
gas_fundamental_data <- read_parquet("gas_fundamental_data.parquet")
# setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Temperature_instrument/esp')
# temperature_instrument <- read_excel("temperature_instrument.xlsx")


#design partie temperature

cold_months <- c(1, 2, 11, 12)
hot_months <- c(6, 7, 8, 9)

price_indispo_merge <- price_indispo_merge %>% 
  mutate(precipitation_mismatch = precipitation_demand - precipitation_supply,
         relative_humidity_mismatch = relative_humidity_demand - relative_humidity_supply,
         temperature_mismatch = temperature_demand - temperature_supply,
         cold_excess_mismatch = cold_excess_demand - cold_excess_supply,
         heat_excess_mismatch = heat_excess_demand - heat_excess_supply
  )

colnames(price_indispo_merge)
#backfill spot gas

monthly_weather_data <- price_indispo_merge %>%
  arrange(datetime) %>% 
  group_by(month, year) %>%
  summarise(
    temperature_demand = mean(temperature_demand, na.rm = TRUE),
    temperature_supply = mean(temperature_supply, na.rm = TRUE),
    temperature_mismatch = mean(temperature_mismatch, na.rm = TRUE),
    cold_excess_demand = mean(cold_excess_demand, na.rm = TRUE),
    heat_excess_demand = mean(heat_excess_demand, na.rm = TRUE),
    cold_excess_supply = mean(cold_excess_supply, na.rm = TRUE),
    heat_excess_supply = mean(heat_excess_supply, na.rm = TRUE),
    heat_excess_mismatch = mean(heat_excess_mismatch, na.rm = TRUE),
    cold_excess_mismatch = mean(cold_excess_mismatch, na.rm = TRUE),
    temp_excess_demand = heat_excess_demand + cold_excess_demand,
    temp_excess_supply = heat_excess_supply + cold_excess_supply,
    temp_excess_mismatch = heat_excess_mismatch + cold_excess_mismatch,
    precipitation_demand = mean(precipitation_demand, na.rm = TRUE),
    relative_humidity_demand = mean(relative_humidity_demand, na.rm = TRUE),
    precipitation_mismatch = mean(precipitation_mismatch, na.rm = TRUE),
    relative_humidity_mismatch = mean(relative_humidity_mismatch, na.rm = TRUE),
    wind_speed_100m = mean(wind_speed_100m, na.rm = TRUE),
    cloud_cover = mean(cloud_cover, na.rm = TRUE),
    global_tilted_irradiance = mean(global_tilted_irradiance, na.rm = TRUE),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()


colnames(VAR_series)
colnames(monthly_energy_data)
colnames(gas_fundamental_data)
#colnames(temperature_instrument)
colnames(monthly_crossborder_data)
colnames(monthly_weather_data)

IV_data <- merge(VAR_series, monthly_energy_data, by= c('month', 'year'))
IV_data <- merge(IV_data, monthly_crossborder_data %>% 
                   dplyr::select(contains("es"), month, year),
                 by= c('month', 'year'))
IV_data <- merge(IV_data, gas_fundamental_data,
                 by= c('month', 'year'))
IV_data <- merge(IV_data, monthly_weather_data %>% 
                   select(-datetime),
                 by= c('month', 'year'))%>%
  arrange(datetime)


save_var_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
write_parquet(IV_data, 'IV_data_esp.parquet')

colnames(IV_data)


library(ggplot2)
plot_data <- monthly_crossborder_data %>%
  select(datetime, FR = renewable_day_ahead_forecast_fr, 
         PT = renewable_day_ahead_forecast_pt) %>%
  pivot_longer(cols = -datetime, names_to = "zone", values_to = "forecast") 

ggplot(data = plot_data) +
  geom_area( aes(x = datetime, y = forecast, fill = zone),position = "stack", alpha = 0.6) +
  geom_line(data = monthly_crossborder_data, 
            aes(x= datetime, y = renewable_day_ahead_forecast_ex_es ,linetype = "dashed")) +
  labs(title = "Contributions des prévisions renouvelables FR et PT",
       y = "Prévision (MW)", x = "Date", fill = "Zone") +
  theme_minimal()

plot_data <- monthly_crossborder_data %>%
  select(datetime, ES = renewable_day_ahead_forecast_fr, 
         CH = renewable_day_ahead_forecast_ch,
         DE = renewable_day_ahead_forecast_de,
         BE = renewable_day_ahead_forecast_be,
         IT = renewable_day_ahead_forecast_it_nord,
         GB = renewable_day_ahead_forecast_gb
         
         ) %>%
  pivot_longer(cols = -datetime, names_to = "zone", values_to = "forecast") 

ggplot(data = plot_data) +
  geom_area( aes(x = datetime, y = forecast, fill = zone),position = "stack", alpha = 0.6) +
  geom_line(data = monthly_crossborder_data, 
            aes(x= datetime, y = renewable_day_ahead_forecast_ex_fr ,linetype = "dashed")) +
  labs(title = "Contributions des prévisions renouvelables FR et PT",
       y = "Prévision (MW)", x = "Date", fill = "Zone") +
  theme_minimal()

colnames(IV_data)

plot_data_elec <- IV_data %>%
  select(datetime, 
         wind_forecast, solar_forecast
         
  )  %>%
  pivot_longer(cols = -datetime, names_to = "energy", values_to = "forecast") 


ggplot(data = plot_data_elec) +
  geom_area( aes(x = datetime, y = forecast, fill = energy),position = "stack", alpha = 0.6) +
  geom_line(data = IV_data, 
            aes(x= datetime, y = renewable_forecast ,linetype = "dashed")) +
  geom_line(data = IV_data, aes(x= datetime, y = wind_speed_100m*100, color = "Wind speed")) +
  geom_line(data = IV_data, aes(x= datetime, y = global_tilted_irradiance*10 , color = "Solar radiance")) +
  labs(title = "Renewable energy decomposition",
       y = "Prévision (MW)", x = "Date", fill = "Zone") +
  theme_minimal()






