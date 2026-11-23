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
IV_data <- read_parquet('IV_data_fr_new.parquet') 


#normalize prices with price index
IV_data <- IV_data %>%
  mutate(across(contains("spot"), ~ . / HICP*100 )) %>% 
  #pour l'instant IPI Esp va jusqu'à 2025-10-01  
  filter(observation_date <= as.Date("2025-09-01") )
periodicity_test_col <- setdiff(colnames(IV_data),
                                c("month" , "year" ,      "ffr",                
                                  "datetime", "last_Date", "last_Trading.day",
                                  "X" ,"Date", "observation_date", "T_min", "T_max"))


remove_trend <- function(series) {
  time_index <- seq_along(series)
  valid <- !is.na(series)
  model <- lm(series[valid] ~ time_index[valid])
  detrended <- rep(NA, length(series))
  detrended[valid] <- resid(model)
  detrended[is.na(detrended)] <- 0  # Remplace les NA par 0
  return(detrended)
}



IV_data_detrended <- IV_data %>%
  dplyr::select(periodicity_test_col) %>%
  mutate(across(everything(), remove_trend))


#determine which variables need to be desasonnalized

#series <- IV_data_detrended$wind_forecast
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

print(dominant_periodicity, n= 30)

#after plotting I choose to add NE Index that exhibits obvious periodicity

var_season <- unique(c(
  dominant_periodicity$series,
  "NE_Index", "HICP", "spot_price", "spot_gas"
))


# Initialize a list to store the seasonally adjusted series
adjusted_data <- list()
start_date = c('2015', '01')
frequency = 12


# Loop over each variable and perform STL decomposition
for (var in colnames(IV_data)) {
  if(var %in% var_season){
    ts_data <- ts(IV_data[[var]], start = start_date, frequency = frequency)
    print(var)
    stl_fit <- stl(ts_data, s.window = "periodic")
    # Extract the seasonally adjusted component
    ts_data <- seasadj(stl_fit)
  }else{
    ts_data <- ts(IV_data[[var]], start = start_date, frequency = frequency)
  }
  adjusted_data[[var]] <- ts_data
}


getwd()
seasonally_adjusted_df <- as.data.frame(adjusted_data)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(seasonally_adjusted_df, "DATASET_levels_sa_fr.csv")



non_log_diff_var <- c("month", "year", "datetime", "last_Date", "last_Trading.day",
                      "cold_excess_instrument" ,"heat_excess_instrument",
                      "Date", "ffr", "Bond_rate",
                      "cold_excess" , "heat_excess", "temp_discomfort",
                      periodicity_test_col[grepl("delta_", periodicity_test_col)]
)

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


colnames(IV_data_log)

#write.csv(detrended_data, "DATASET_sa_detrended.csv")
write_parquet(IV_data_log, 'IV_data_fr_log_elas_sd_2026.parquet')


gc()  # force le garbage collection



#petit plot pour voir l'effet associé


par(mar = c(2, 2, 2, 2))
plot.new()
ggplot(IV_data, aes(x = observation_date)) +
  geom_line(aes(y =IV_data$NE_Index , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$NE_Index, color = "Detrended")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()


ggplot(IV_data, aes(x = observation_date)) +
  geom_line(aes(y =IV_data$IPI , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$IPI, color = "Detrended")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()






