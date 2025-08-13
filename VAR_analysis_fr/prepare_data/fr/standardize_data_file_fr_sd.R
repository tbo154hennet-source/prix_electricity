
library(readxl)
library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
library(purrr)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data/fr")

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


VAR_series <- VAR_series %>% 
  mutate(year = year(observation_date),
         month = month(observation_date))%>% 
  rename(ffr = ECBMRRFR,
         IPI = FRAPROINDMISMEI,
         NE_Index= `00XE00FRM086NEST`,
         E_Index = LRHUTTTTFRM156S,
         Exp_pi = CSINFT02FRM460S,
         P_man = FRAPROMANMISMEI,
         Spot_gas =PNGASEUUSDM,
         HICP = CP0000FRM086NEST,
         CPElec = CP0451FRM086NEST
  )

colnames(VAR_series)


instruments <- c("renewable_day_ahead_forecast",
                 #"Published.Unavailable.Nuclear.Capacity"
                 "cbf_fr",
                 "cold_excess_instrument",
                 "heat_excess_instrument",
                 "Published.Predicted.Unavailable.Capacity")

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/fr")
price_indispo_merge <- read_parquet("price_indispo_merge_delta_supply_demand_fr.parquet")

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts_fr.parquet')
col_com <- intersect(colnames(renewable_forecasts), colnames(price_indispo_merge))

# price_indispo_merge_bronte <- left_join(price_indispo_merge_delta, renewable_forecasts %>% select(-col_com),
#                                     by = c("datetime" = "__index_level_0__"))
price_indispo_merge <- merge(price_indispo_merge, renewable_forecasts %>% dplyr::select(-col_com),
                             by.x = "datetime" , by.y= "__index_level_0__")
# Calculer la somme ligne par ligne des colonnes sélectionnées


instruments_supply <- paste0("delta_s_", instruments)
instruments_demand <- paste0("delta_d_", instruments)

# Construire les noms de colonnes
cols_supply<- paste0("delta_s_", instruments)
cols_demand<- paste0("delta_d_", instruments)

# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge$delta_s_total <- rowSums(price_indispo_merge[, cols_supply], na.rm = TRUE)
price_indispo_merge$delta_d_total <- rowSums(price_indispo_merge[, cols_demand], na.rm = TRUE)

#adjust for iberian exception gas cap


price_indispo_merge <- price_indispo_merge %>% 
  fill(spot_gas, .direction = "up")%>% # Backfill (bfill) 
  fill(spot_gas, .direction = "down") #%>%    # Backfill (bfill)%>%
  #mutate(spot_gas_capped = ifelse(spot_gas < Cap_EUR_MWh | is.na(Cap_EUR_MWh), spot_gas, Cap_EUR_MWh)) 



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
    renewable_forecast = weighted_mean(renewable_day_ahead_forecast, gen_tot),
    wind_forecast = weighted_mean(Fore_Wind, gen_tot),
    solar_forecast = weighted_mean(Fore_Solar, gen_tot),
    nuclear_unavailabilities = weighted_mean(Nuclear.Unavailabilities, gen_tot),
    thermal_unavailabilities = weighted_mean(Thermal.Unavailabilities, gen_tot),
    predicted_unavailabilities = weighted_mean(Predicted.Unavailabilities, gen_tot),
    # NTC_ESP_FR = mean(NTC_ESP_FR, na.rm = TRUE),
    # NTC_FR_ESP = mean(NTC_FR_ESP, na.rm = TRUE),
    gen_tot = mean(gen_tot, na.rm = TRUE),
    #gen_tot_da = mean(gen_tot_da, na.rm = TRUE),
    #spot_gas_capped = mean(spot_gas_capped, na.rm = TRUE),
    spot_gas= mean(spot_gas, na.rm = TRUE),
    delta_s_total = weighted_mean(delta_s_total, gen_tot),
    delta_d_total = weighted_mean(delta_d_total, gen_tot),
    delta_s_renewable_day_ahead_forecast = weighted_mean(delta_s_renewable_day_ahead_forecast, gen_tot),
    delta_d_renewable_day_ahead_forecast = weighted_mean(delta_d_renewable_day_ahead_forecast, gen_tot),
    delta_s_cold_excess_instrument = weighted_mean(delta_s_cold_excess_instrument, gen_tot),
    delta_d_cold_excess_instrument = weighted_mean(delta_d_cold_excess_instrument, gen_tot),
    delta_s_heat_excess_instrument = weighted_mean(delta_s_heat_excess_instrument, gen_tot),
    delta_d_heat_excess_instrument = weighted_mean(delta_d_heat_excess_instrument, gen_tot),
    delta_s_Published.Predicted.Unavailable.Capacity = weighted_mean(delta_s_Published.Predicted.Unavailable.Capacity, gen_tot),
    delta_d_Published.Predicted.Unavailable.Capacity = weighted_mean(delta_d_Published.Predicted.Unavailable.Capacity, gen_tot),
    delta_s_cbf_fr = weighted_mean(delta_s_cbf_fr, gen_tot),
    delta_d_cbf_fr = weighted_mean(delta_d_cbf_fr, gen_tot),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()


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


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
price_indispo_merge_weather <- read_parquet( "price_indispo_merge_fr.parquet")

cold_months <- c(1, 2, 11, 12)
hot_months <- c(6, 7, 8, 9)

#backfill spot gas
colnames(price_indispo_merge)
monthly_weather_data <- price_indispo_merge_weather %>%
  arrange(datetime) %>% 
  group_by(month, year) %>%
  summarise(
    temperature_2m = mean(temperature_2m, na.rm = TRUE),
    cold_excess_instrument = mean(cold_excess_instrument, na.rm = TRUE),
    heat_excess_instrument = mean(heat_excess_instrument, na.rm = TRUE),
    temp_excess_instrument = heat_excess_instrument + cold_excess_instrument,
    precipitation = mean(precipitation, na.rm = TRUE),
    relative_humidity_2m = mean(relative_humidity_2m, na.rm = TRUE),
    # wind_speed_100m = mean(wind_speed_100m, na.rm = TRUE),
    # cloud_cover = mean(cloud_cover, na.rm = TRUE),
    # global_tilted_irradiance = mean(global_tilted_irradiance, na.rm = TRUE),
    datetime = first(datetime)
  ) %>%
  arrange(datetime) %>% 
  ungroup()


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data')
gas_fundamental_data <- read_parquet("gas_fundamental_data.parquet")
# save_path_fr <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
# setwd(save_path_fr)
# temperature_data <- read.csv("temperature_instrument.csv") %>% 
#   dplyr::select(month, year, T_avg, deviation,
#          cold_excess_instrument, heat_excess_instrument,
#          CDD_excess_instrument, HDD_excess_instrument,
#          CDD_intensity_instrument, HDD_intensity_instrument
#          )



colnames(VAR_series)
colnames(monthly_energy_data)
colnames(gas_fundamental_data)
colnames(monthly_weather_data)
dim(VAR_series)
dim(monthly_energy_data)
dim(monthly_weather_data)
dim(monthly_crossborder_data)

IV_data <- merge(VAR_series, monthly_energy_data , by= c('month', 'year'))
IV_data <- merge(IV_data, gas_fundamental_data,
                 by= c('month', 'year'))
IV_data <- merge(IV_data, monthly_crossborder_data%>% 
                   dplyr::select(-datetime),
                 by= c('month', 'year'))
# IV_data <- merge(IV_data, temperature_data,
#                  by= c('month', 'year'))%>%
#   arrange(last_Date)
IV_data <- merge(IV_data, monthly_weather_data %>% 
                   dplyr::select(-datetime),
                 by= c('month', 'year'))%>%
  arrange(datetime)


save_var_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
write_parquet(IV_data, 'IV_data_fr_elas.parquet')

colnames(IV_data)

