library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(forecast)
library(pracma)
Sys.setenv(lang = "en_US")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp')

IV_data <- read_parquet('low_freq_data.parquet')

# Identify columns with NA values
columns_with_na <- names(IV_data)[colSums(is.na(IV_data)) > 0]

# Print the result
print(columns_with_na)

#parfois aucune indispo nucleaire imprevue
IV_data <- IV_data %>% 
  replace_na(list(unexpected_nuclear_indispo = 0, nuclear_indispo= 0))



# Example variables in the data frame
df_vars <- colnames(IV_data)

non_variables <- c("month", "year", "datetime",
                   "X", "T_year_month", "Date.x", "Date.y")

# Find variables in the data frame that are not in var_list
vars_list <- setdiff(df_vars, non_variables)


# Initialize a list to store the seasonally adjusted series
adjusted_data <- list()
start_date = c('2015', '01')
frequency = 12

# Loop over each variable and perform STL decomposition
for (var in vars_list) {
  ts_data <- ts(IV_data[[var]], start = start_date, frequency = frequency)
  print(var)
  # Perform STL decomposition
  stl_fit <- stl(ts_data, s.window = "periodic")
  
  # Extract the seasonally adjusted component
  adjusted_data[[var]] <- seasadj(stl_fit)
}


# Combine the seasonally adjusted data into a dataframe
seasonally_adjusted_df <- as.data.frame(adjusted_data)

getwd()
write.csv(seasonally_adjusted_df, "DATASET_levels_sa.csv")


non_log_diff_var <- c("month", "year","datetime",
                      "X", "T_year_month", 
                      "cold_excess_instrument" ,"heat_excess_instrument",
                      "abs_gas_shock", "rel_gas_shock","Date")

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


head(IV_data_log_diff)


seasonally_adjusted_df[is.na(seasonally_adjusted_df)] <- 0
IV_data_log[is.na(IV_data_log)] <- 0
IV_data_log_diff[is.na(IV_data_log_diff)] <- 0


# Detrend the data
detrended_data <- as.data.frame(detrend(as.matrix(seasonally_adjusted_df)))
IV_data_log <- as.data.frame(detrend(as.matrix(IV_data_log)))
IV_data_log_diff <- as.data.frame(detrend(as.matrix(IV_data_log_diff)))


write.csv(detrended_data, "DATASET_sa_detrended.csv")
write_parquet(IV_data_log, 'low_freq_data_log.parquet')
write_parquet(IV_data_log_diff, 'low_freq_data_log_diff.parquet')


colnames(IV_data_log_diff)
#petit plot pour voir l'effet associé

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$NE_Index , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$NE_Index, color = "Seasonally Adjusted")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
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


colnames(IV_data_log)
ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y = log(IV_data$gen_tot) -mean(log(IV_data$gen_tot)) , color = "Original")) +
  geom_line(aes(y = IV_data_log$gen_tot_log, color = "Detrended")) +
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


cor(IV_data_log_diff$T_avg_log_diff, IV_data_log_diff$gen_tot_log_diff)
cor(IV_data_log_diff$renewable_generation_log_diff, 
    IV_data_log_diff$spot_price_log_diff)

