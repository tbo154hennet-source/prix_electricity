
library(readxl)
library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
library(purrr)
library(fredr)
library(rsdmx)

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
colnames(VAR_series)

setwd("C:/Users/DBT3KD/Desktop/clés")
# Read the key from the file (assuming only one line, no header)
fred_key <- readLines("fred.txt", warn = FALSE)

# Set the API key
fredr_set_key(fred_key)


VAR_series_headers <- c(
  "ECBMRRFR", "ENRGY0FRM086NEST", "FRAPROINDMISMEI",
  "CSINFT02FRM460S", "FRAPROMANMISMEI", "00XE00FRM086NEST",
  "PNGASEUUSDM", "LRHUTTTTFRM156S", "CP0000FRM086NEST",
  "CP0451FRM086NEST"
)


# Téléchargement groupé
fred_data <- map_dfr(VAR_series_headers, function(series_id) {
  fredr(series_id = series_id, observation_start = as.Date("2000-01-01")) %>%
    dplyr::select(date, value) %>%
    mutate(series = series_id)
})


fred_wide <- fred_data %>%
  pivot_wider(names_from = series, values_from = value) %>%
  rename(observation_date = date) %>% 
  filter(day(as.Date(observation_date)) == 1)




# Affiche les 10 premières lignes
head(fr_prod , 10)
unique(fr_prod$unit)
colnames(fr_prod)
ggplot()+
  geom_line(data = fr_prod, aes(x = TIME_PERIOD, y = values, color = "Production Index")) +
  geom_line(data = fred_wide, aes(x = observation_date, y = FRAPROINDMISMEI, color = "OECD Production Index"))

# Step 1: Build OECD SDMX URL for French Industrial Production Index (IPI)
# Dataset: MEI (Monthly Economic Indicators)
# Subject: Production (PRD), France (FRA), Measure: Index (IXOB), Adjusted: Seasonally Adjusted (SA)
# Install if needed


colnames(VAR_series)



save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
price_indispo_merge <- read_parquet("price_indispo_merge_fr_NTC.parquet")

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}


monthly_energy_data <- price_indispo_merge %>%
  arrange(datetime) %>% 
  mutate(
    Nuclear.Unavailabilities =
      coalesce(Planned.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Published.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Uneffective.Unavailable.Nuclear.Capacity, 0) +
      coalesce(Unpublished.Unavailable.Nuclear.Capacity, 0),
    
    Thermal.Unavailabilities =
      coalesce(Planned.Unavailable.Capacity, 0) +
      coalesce(Published.Unavailable.Capacity, 0) +
      coalesce(Uneffective.Unavailable.Capacity, 0) +
      coalesce(Unpublished.Unavailable.Capacity, 0) -
      coalesce(Nuclear.Unavailabilities, 0),
    
    Predicted.Unavailabilities =
      coalesce(Planned.Predicted.Unavailable.Capacity, 0) +
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
    NTC_ESP_FR = mean(NTC_ESP_FR, na.rm = TRUE),
    NTC_FR_ESP = mean(NTC_FR_ESP, na.rm = TRUE),
    gen_tot = mean(gen_tot, na.rm = TRUE),
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


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data')
gas_fundamental_data <- read_parquet("gas_fundamental_data.parquet")
save_path_fr <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_path_fr)
temperature_data <- read.csv("temperature_instrument.csv") %>% 
  dplyr::select(month, year, T_avg, deviation,
         cold_excess_instrument, heat_excess_instrument,
         CDD_excess_instrument, HDD_excess_instrument,
         CDD_intensity_instrument, HDD_intensity_instrument
         )



colnames(VAR_series)
colnames(monthly_energy_data)
colnames(gas_fundamental_data)
colnames(temperature_data)

IV_data <- merge(VAR_series, monthly_energy_data , by= c('month', 'year'))
IV_data <- merge(IV_data, gas_fundamental_data,
                 by= c('month', 'year'))
IV_data <- merge(IV_data, monthly_crossborder_data%>% 
                   dplyr::select(-datetime),
                 by= c('month', 'year'))
IV_data <- merge(IV_data, temperature_data,
                 by= c('month', 'year'))%>%
  arrange(last_Date)



save_var_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
write_parquet(IV_data, 'IV_data_fr.parquet')

colnames(IV_data)

