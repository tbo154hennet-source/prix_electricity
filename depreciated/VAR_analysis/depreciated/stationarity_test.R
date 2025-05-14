

colnames(IV_data)

library(tseries)

# Perform ADF test
# adf_result <- adf.test(your_time_series)
# print(adf_result)

#for demand growth, log_diff, renewable_growth
#I would need 2014612 value
IV_data[is.na(IV_data)] <- 0

# Identify columns with NA values
columns_with_na <- names(IV_data)[colSums(is.na(IV_data)) > 0]

# Print the column names with NAs
print(columns_with_na)


# Replace 'column_name' with the actual name of your column
column_name <- "log_diff"
na_percentage <- sum(is.na(IV_data[[column_name]])) / nrow(IV_data) * 100

# Print the percentage of NAs
cat("Percentage of NAs in", column_name, ":", na_percentage, "%\n")


print(class(IV_data$month))
print(class(IV_data$datetime))





# Apply ADF test to all numeric columns and store the results
adf_results <- lapply(IV_data, function(column) {
  if (is.numeric(column)) {
    adf_test <- adf.test(column)
    return(adf_test$p.value) # Extract the p-value for stationarity check
  } else {
    return(NA) # Skip non-numeric columns
  }
})

# Convert the list of results to a named vector for easier interpretation
adf_results <- unlist(adf_results)
names(adf_results) <- names(IV_data)
print(adf_results)


# Filter column names with p-value > 0.05
non_stationary_columns <- names(adf_results[adf_results > 0.05])
# Define the columns to exclude
index_variables <- c("month", "year", "datetime")

# Exclude these variables from the non_stationary_columns list
non_stationary_columns_filtered <- setdiff(non_stationary_columns, index_variables)


library(ggplot2)
my_plot <- ggplot()+
  ggtitle("Non-stationary columns")+
  theme_minimal()+
  labs(x = "Date", y = "Normalized Value")

# Plot each non-stationary column
for (col in non_stationary_columns_filtered) {
  #col="gas_price"
  # col = non_stationary_columns[i]
  # i= i+1
  if (!is.na(col)){
    my_plot <- my_plot +
      geom_line(aes_string(x = IV_data[["datetime"]], 
                           y = IV_data[[col]]/mean(IV_data[[col]]),
                           color = shQuote(col))) 
  }
}
my_plot


# variables can all be detrended
#that are not growth rates

#ipi_growth rate is not stationary
subset = c("ipi_growth_rate",                 
          "ne_growth_rate",
          "Exp_pi")
subset_bis = c("IPI","Exp_pi","NE_Index" )

colnames(IV_data)

library(ggplot2)
my_plot <- ggplot()+
  ggtitle("Non-stationary columns")+
  theme_minimal()+
  labs(x = "Date", y = "Normalized Value")

# Plot each non-stationary column
for (col in subset_bis) {
  #col="gas_price"
  # col = non_stationary_columns[i]
  # i= i+1
  if (!is.na(col)){
    my_plot <- my_plot +
      geom_line(aes_string(x = IV_data[["datetime"]], 
                           y = IV_data[[col]], #/mean(IV_data[[col]]),
                           color = shQuote(col))) 
  }
}
my_plot








