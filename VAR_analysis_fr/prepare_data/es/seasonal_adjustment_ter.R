library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(forecast)
library(pracma)
library(arrow)
Sys.setenv(lang = "en_US")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data <- read_parquet('IV_data_esp.parquet') 

# colnames(IV_data)
# dim(IV_data)
# IV_data$spot_price
# IV_data <- IV_data[1:108,]
periodicity_test_col <- setdiff(colnames(IV_data),
                                c("month" , "year" ,                     
                                  "datetime", "last_Date", "last_Trading.day",
                                  "X" ,"Date", "observation_date", "T_min", "T_max"))


remove_trend <- function(series) {
  time_index <- 1:length(series)  # Créer un vecteur de temps
  model <- lm(series ~ time_index, na.action = na.omit)  # Ajuster une régression linéaire
  detrended_series <- resid(model)  # Récupérer les résidus (série sans tendance)
  return(detrended_series)
}

IV_data_detrended <- IV_data %>%
  dplyr::select(periodicity_test_col) %>%
  mutate(across(everything(), remove_trend))


#determine which variables need to be desasonnalized


detect_general_periodicity <- function(series) {
  spec <- spectrum(series, plot = FALSE)  # Compute the periodogram
  dominant_freq <- spec$freq[which.max(spec$spec)]  # Get the most dominant frequency
  estimated_period <- 1 / dominant_freq  # Convert frequency to period
  return(estimated_period)
}

# if the dominant periodicity is infra-annual
#dplyr::select as needed to be deseasonnalized
# series <- IV_data_detrended$ECB_rate
# detect_general_periodicity(series)
# Apply to your data
# Define the keywords you want to exclude
exclude_keywords <- c("instrument", "gas_shock", "PC1")
# Create a single pattern from the keywords
exclude_pattern <- paste(exclude_keywords, collapse = "|")
# Filter out rows where 'title' contains any of the exclude keywords
periodicity_test_col <- periodicity_test_col[!grepl(periodicity_test_col, pattern = exclude_pattern)]


dominant_periodicity <- IV_data_detrended %>%
  dplyr::select(periodicity_test_col) %>%
  summarise(across(everything(), detect_general_periodicity)) %>% 
  pivot_longer(everything(), names_to = "series", values_to = "dominant_period") %>%
  filter(as.numeric(dominant_period) <= 13)



dominant_periodicity <- IV_data_detrended %>%
  dplyr::select(periodicity_test_col) %>%
  #dplyr::select(-c("PC1_supply","PC1_demand", "gas_shock_supply", "gas_shock_demand")) %>%
  summarise(across(everything(), detect_general_periodicity)) %>% 
  pivot_longer(everything(), names_to = "series", values_to = "dominant_period") %>%
  filter(as.numeric(dominant_period) <= 13)

#after plotting I choose to add NE Index that exhibits obvious periodicity

var_season <- c(dominant_periodicity$series, "NE_Index")


# Initialize a list to store the seasonally adjusted series
adjusted_data <- list()
start_date = c('2015', '01')
frequency = 12

# Loop over each variable and perform STL decomposition
for (var in colnames(IV_data)) {
  ts_data <- ts(IV_data[[var]], start = start_date, frequency = frequency)
  print(var)
  if (var %in% var_season){
    # Perform STL decomposition
    stl_fit <- stl(ts_data, s.window = "periodic")
    # Extract the seasonally adjusted component
    ts_data <- seasadj(stl_fit)
    
  }
  adjusted_data[[var]] <- ts_data
}


getwd()
seasonally_adjusted_df <- as.data.frame(adjusted_data)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output")
write.csv(seasonally_adjusted_df, "DATASET_levels_sa.csv")



non_log_diff_var <- c("month", "year", "datetime", "last_Date", "last_Trading.day",
                      "cold_excess_instrument" ,"heat_excess_instrument",
                      "Date", "ffr", "Bond_rate",
                      "PC1_supply","PC1_demand", "gas_shock_supply", "gas_shock_demand", "cbf_es")

non_log_col_var <- c(non_log_diff_var)

#attention pour abs shock and relative shock possible valeurs négatives

# Apply log transformation to all columns not in non_log_diff_var
IV_data_log <- seasonally_adjusted_df
#IV_data_log[setdiff(names(IV_data_log), non_log_col_var)] <- 
# log(IV_data_log[setdiff(names(IV_data_log), non_log_col_var)] +1)
cols_to_transform <- setdiff(periodicity_test_col, non_log_col_var)
# 1. Calculate log transformation and store with suffix "_log"
for (col in cols_to_transform) {
  numeric_col <- as.numeric(IV_data_log[[col]])  # Ensure numeric
  #detrended_col <- detrend(numeric_col)          # Detrend
  IV_data_log[[paste0(col, "_log")]] <- log(numeric_col + 1)  # Log transform
}

# 3. Remove the original columns (except the non-transformed ones)
#IV_data_log <- IV_data_log[, !names(IV_data_log) %in% cols_to_transform]


seasonally_adjusted_df[is.na(seasonally_adjusted_df)] <- 0
IV_data_log[is.na(IV_data_log)] <- 0




IV_data_log <- IV_data_log %>% 
  mutate(
    temp_excess_instrument = cold_excess_instrument + heat_excess_instrument,
    log_diff_gas = avg_spot_price_log - lag(as.numeric(avg_spot_price_log)),
    log_diff_spot = spot_price_log - lag(as.numeric(spot_price_log)),
    diff_cbf = cbf_es - lag(as.numeric(cbf_es)),
    log_diff_renewable = renewable_day_ahead_forecast_esp_log - lag(as.numeric(renewable_day_ahead_forecast_esp_log)),
    log_diff_ex_renewable = renewable_day_ahead_forecast_ex_es_log - lag(as.numeric(renewable_day_ahead_forecast_ex_es_log)),
    log_diff_pred_unavail = predicted_unavailabilities_log - lag(as.numeric(predicted_unavailabilities_log))
    ) %>%
  arrange(datetime)

getwd()
#write.csv(detrended_data, "DATASET_sa_detrended.csv")
write_parquet(IV_data_log, 'IV_data_esp_log.parquet')


#write_parquet(IV_data_log, 'IV_data_esp_log_detrended.parquet')



#petit plot pour voir l'effet associé


par(mar = c(2, 2, 2, 2))
plot.new()
ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$NE_Index , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$NE_Index, color = "Detrended")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$wind_forecast , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$wind_forecast, color = "S ADj")) +
  geom_line(aes(x = datetime, y = detrended_data$wind_forecast, color = "Detrended"))+
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red", "S ADj" = "green")) +
  theme_minimal()

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y = log(IV_data$spot_price) -mean(log(IV_data$spot_price)) , color = "Original")) +
  geom_line(aes(y = IV_data_log_detrended$spot_price_log, color = "Detrended")) +
  labs(title = "Log Spot price - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()


ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y = log(IV_data$NTC_FR_ESP) -mean(log(IV_data$NTC_FR_ESP)) , color = "Original")) +
  geom_line(aes(y = IV_data_log$NTC_FR_ESP_log, color = "Detrended")) +
  labs(title = "NTC_FR_ESP",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

ggplot() +
  geom_line(data = IV_data_log_detrended, aes(x = c(1:108), y = renewable_day_ahead_forecast_esp_log, color ="ES", linetype = "Baseline" )) +
  geom_line(data = IV_data_log_detrended, aes(x = c(1:108), y = renewable_day_ahead_forecast_ex_es_log - mean(renewable_day_ahead_forecast_ex_es_log), color ="ex-ES", linetype = "Baseline" )) +
  geom_line(data = IV_data_log_detrended, aes(x = c(1:108), y = detrended_renewable_day_ahead_forecast_esp_log, color ="ES", linetype = "Detrended" )) +
  geom_line(data = IV_data_log_detrended, aes(x = c(1:108), y = detrended_renewable_day_ahead_forecast_ex_es_log, color ="ex-ES", linetype = "Detrended" )) +
  labs(title = "Should I detrend instruments ?",
       x = "Time",
       y = "GWh") +
  theme_minimal()

