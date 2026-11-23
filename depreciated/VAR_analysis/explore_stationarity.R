library(tseries)
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(estimatr)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
detrended_data <- read.csv("DATASET_sa_detrended.csv")
IV_data_log <- read_parquet('low_freq_data_log.parquet')
IV_data_log_diff <- read_parquet('low_freq_data_log_diff.parquet')

# Apply ADF test to all numeric columns and store the results
adf_results <- lapply(detrended_data , function(column) {
  if (is.numeric(column)) {
    adf_test <- adf.test(column)
    return(adf_test$p.value) # Extract the p-value for stationarity check
  } else {
    return(NA) # Skip non-numeric columns
  }
})

# Convert the list of results to a named vector for easier interpretation
adf_results <- unlist(adf_results)
names(adf_results) <- names(detrended_data)
print(adf_results)
# Filter column names with p-value > 0.05
non_stationary_columns <- adf_results[adf_results > 0.05]
print(non_stationary_columns)


adf_results_log <- lapply(IV_data_log , function(column) {
  if (is.numeric(column)) {
    adf_test <- adf.test(column)
    return(adf_test$p.value) # Extract the p-value for stationarity check
  } else {
    return(NA) # Skip non-numeric columns
  }
})
# Convert the list of results to a named vector for easier interpretation
adf_results_log<- unlist(adf_results_log)
names(adf_results_log) <- names(IV_data_log)
print(adf_results_log)
# Filter column names with p-value > 0.05
non_stationary_columns <- adf_results_log[adf_results_log> 0.05]
print(non_stationary_columns)


adf_results_log_diff <- lapply(IV_data_log_diff , function(column) {
  if (is.numeric(column)) {
    adf_test <- adf.test(column)
    return(adf_test$p.value) # Extract the p-value for stationarity check
  } else {
    return(NA) # Skip non-numeric columns
  }
})
# Convert the list of results to a named vector for easier interpretation
adf_results_log_diff<- unlist(adf_results_log_diff)
names(adf_results_log_diff) <- names(IV_data_log_diff)
print(adf_results_log_diff)
# Filter column names with p-value > 0.05
non_stationary_columns <- adf_results_log_diff[adf_results_log_diff> 0.05]
print(non_stationary_columns)



library(ggplot2)
my_plot <- ggplot()+
  ggtitle("Non-stationary columns")+
  theme_minimal()+
  labs(x = "Date", y = "Normalized Value")
non_stationary_columns_names <- names(non_stationary_columns)

#colnames(IV_data_log_diff)
# Plot each non-stationary column
for (col in non_stationary_columns_names) {
  #col="gas_price"
  # col = non_stationary_columns[i]
  # i= i+1
  if (!is.na(col)){
    my_plot <- my_plot +
      geom_line(aes_string(x = IV_data[["datetime"]], 
                           y = IV_data_log_diff[[col]],
                           color = shQuote(col))) 
  }
}
my_plot

