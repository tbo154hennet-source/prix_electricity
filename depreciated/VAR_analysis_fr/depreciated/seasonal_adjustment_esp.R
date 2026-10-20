library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(forecast)
library(pracma)
Sys.setenv(lang = "en_US")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/macro_exercise'
setwd(save_path_esp)

IV_data <- read_parquet('low_freq_data.parquet') %>% 
  rename(Date = Date.x) %>% 
  dplyr::select(-c("Date.y", "T_min", "T_max"))

# Identify columns with NA values
columns_with_na <- names(IV_data)[colSums(is.na(IV_data)) > 0]

# Print the result
print(columns_with_na)

#parfois aucune indispo nucleaire imprevue
IV_data <- IV_data %>% 
  replace_na(list(unexpected_nuclear_indispo = 0, nuclear_indispo= 0,
                  gas_supply_shock = 0))




periodicity_test_col <- setdiff(colnames(IV_data),
                                c("month" , "year" ,                     
                                  "datetime", "plot_date",
                                  "X" ,"Date"))


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
#select as needed to be deseasonnalized
# series <- IV_data_detrended$ECB_rate
# detect_general_periodicity(series)
# Apply to your data

dominant_periodicity <- IV_data_detrended %>%
  dplyr::select(periodicity_test_col) %>%
  dplyr::select(-c("gas_supply_shock")) %>%
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


# Combine the seasonally adjusted data into a dataframe
seasonally_adjusted_df <- as.data.frame(adjusted_data)
getwd()
write.csv(seasonally_adjusted_df, "DATASET_levels_sa.csv")


non_log_diff_var <- c("month", "year","datetime",
                      "X", "T_year_month", 
                      "cold_excess_instrument" ,"heat_excess_instrument",
                      "gas_supply_shock","Date", "ECB_rate", "plot_date" )

non_log_col_var <- c(non_log_diff_var)

#attention pour abs shock and relative shock possible valeurs négatives

# Apply log transformation to all columns not in non_log_diff_var
IV_data_log <- seasonally_adjusted_df
#IV_data_log[setdiff(names(IV_data_log), non_log_col_var)] <- 
# log(IV_data_log[setdiff(names(IV_data_log), non_log_col_var)] +1)
cols_to_transform <- setdiff(names(IV_data_log), non_log_col_var)
# 1. Calculate log transformation and store with suffix "_log"
for (col in cols_to_transform) {
  IV_data_log[[paste0(col, "_log")]] <- log(IV_data_log[[col]]+1)
}

# 3. Remove the original columns (except the non-transformed ones)
IV_data_log <- IV_data_log[, !names(IV_data_log) %in% cols_to_transform]


# Create a copy of the original dataframe to store log differences
IV_data_log_diff <- seasonally_adjusted_df
# Apply log difference transformation to columns not in non_log_diff_var
cols_to_diff <- setdiff(names(IV_data_log_diff), non_log_diff_var)
# IV_data_log_diff[cols_to_diff] <- lapply(IV_data_log_diff[cols_to_diff], function(column) {
#   log(column+1) - log(dplyr::lag(column)+1)
# })
# 2. Calculate log difference and store with suffix "_log_diff"
for (col in cols_to_diff) {
  IV_data_log_diff[[paste0(col, "_log_diff")]] <- log(as.vector(IV_data_log_diff[[col]])+1) -
    log(dplyr::lag(as.vector(IV_data_log_diff[[col]]))+1)
}
# 3. Remove the original columns (except the non-transformed ones)
IV_data_log_diff <- IV_data_log_diff[, !names(IV_data_log_diff) %in% cols_to_diff]


seasonally_adjusted_df[is.na(seasonally_adjusted_df)] <- 0
IV_data_log[is.na(IV_data_log)] <- 0
IV_data_log_diff[is.na(IV_data_log_diff)] <- 0


# Detrend the data
detrended_data <- as.data.frame(detrend(data.matrix(seasonally_adjusted_df)))
IV_data_log <- as.data.frame(detrend(data.matrix(IV_data_log)))
IV_data_log_diff <- as.data.frame(data.matrix(IV_data_log_diff))

getwd()
write.csv(detrended_data, "DATASET_sa_detrended.csv")
write_parquet(IV_data_log, 'low_freq_data_log_bis.parquet')
write_parquet(IV_data_log_diff, 'low_freq_data_log_diff_bis.parquet')


colnames(seasonally_adjusted_df)
#petit plot pour voir l'effet associé

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$renewable_generation , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$renewable_generation, color = "Seasonally Adjusted")) +
  labs(title = "renewable_generation - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Seasonally Adjusted" = "red")) +
  theme_minimal()


ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$T_avg -mean(IV_data$T_avg) , color = "Original")) +
  geom_line(aes(y = detrended_data$T_avg, color = "Detrended")) +
  labs(title = "Temperature - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()


ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$NE_Index , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$NE_Index, color = "Seasonally Adjusted")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Seasonally Adjusted" = "red")) +
  theme_minimal()



ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$unexpected_tot_indispo -mean(IV_data$unexpected_tot_indispo) , color = "Original")) +
  geom_line(aes(y = detrended_data$unexpected_tot_indispo, color = "Detrended")) +
  labs(title = "Temperature - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y = log(IV_data$unexpected_tot_indispo +1) -mean(log(IV_data$unexpected_tot_indispo+1)) , color = "Original")) +
  geom_line(aes(y = IV_data_log$unexpected_tot_indispo_log, color = "Detrended")) +
  labs(title = "Log Demand - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()



ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y = log(IV_data$gen_tot) - log(lag(IV_data$gen_tot)) , color = "Original")) +
  geom_line(aes(y = IV_data_log_diff$gen_tot_log_diff, color = "Detrended")) +
  labs(title = "Demand Growth rate - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

cor(detrended_data$T_avg, detrended_data$gen_tot)
cor(detrended_data$renewable_generation, detrended_data$spot_price)


#cor(IV_data_log_diff$T_avg_log_diff, IV_data_log_diff$gen_tot_log_diff)
cor(IV_data_log_diff$cold_excess_instrument, IV_data_log_diff$gen_tot_log_diff)
cor(IV_data_log_diff$heat_excess_instrument, IV_data_log_diff$gen_tot_log_diff)
cor(IV_data_log_diff$renewable_generation_log_diff, 
    IV_data_log_diff$spot_price_log_diff)

