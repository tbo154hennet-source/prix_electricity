
#on se fait des focus
demand_variables <- c("log_diff", "demand_growth_period", "renewable_growth")# ,"gen_tot",demand_growth_period" ,"log_demand")

library(ggplot2)
my_plot <- ggplot()+
  ggtitle("Non-stationary columns")+
  theme_minimal()+
  labs(x = "Date", y = "Normalized Value")

# Plot each non-stationary column
for (col in demand_variables) {
  #col="gas_price"
  # col = non_stationary_columns[i]
  # i= i+1
  if (!is.na(col)){
    my_plot <- my_plot +
      geom_line(aes_string(x = IV_data[["datetime"]], 
                    y = IV_data[[col]],
                    color = shQuote(col))) 
  }
}
my_plot


adf_test <- adf.test(IV_data$log_diff)


"tot_indispo" 

my_cols <- c("heat_excess_instrument", "cold_excess_instrument", 
             "CDD_excess_instrument" , "HDD_excess_instrument")
my_plot <- ggplot()+
  ggtitle("Non-stationary columns")+
  theme_minimal()+
  labs(x = "Date", y = "Normalized Value")

# Plot each non-stationary column
for (col in my_cols) {
  #col="gas_price"
  # col = non_stationary_columns[i]
  # i= i+1
  if (!is.na(col)){
    my_plot <- my_plot +
      geom_line(aes_string(x = IV_data[["datetime"]], 
                           y = IV_data[[col]],
                           color = shQuote(col))) 
  }
}
my_plot



