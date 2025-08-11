data_IV_with_gas <- as.data.frame(t(structural_shocks_IV))
colnames(data_IV_with_gas ) <- c( "quantity", "price")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV_with_gas$price, color = "price"))+
  geom_line(aes(x = dates_formatted, y = data_IV_with_gas$quantity, color = "quantity")) +
  theme_minimal()


# Compute standard deviation
normalization_factor <- sd(t(structural_shocks_IV))

# Print results
cat("Mean of structural shocks:", mean_shock, "\n")
cat("Standard deviation of structural shocks:", sd_shock, "\n")





data_IV <- as.data.frame(t(structural_shocks_IV))
colnames(data_IV) <- c( "quantity", "price")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$price, color = "price"))+
  geom_line(aes(x = dates_formatted, y = data_IV$quantity, color = "quantity")) +
  theme_minimal()

data <- as.data.frame(structural_shocks)


ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$price, color = "without gas"))+
  geom_line(aes(x = dates_formatted, y = data_IV_with_gas$price, color = "with gas")) +
  geom_line(aes(x = dates_formatted, y = IV_data_log_diff_fr$Spot_gas[(p+1):(p+length(dates_formatted))]/100, color = "gas price")) +
  theme_minimal()


ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$price/normalization_factor, color = "without gas", linetype ="IV"))+
  geom_line(aes(x = dates_formatted, y = data_IV_with_gas$price/normalization_factor, color = "with gas", linetype ="IV")) +
  geom_line(aes(x = dates_formatted, y = data$spot_price_log_diff, color = "without gas", linetype ="Cholesky")) +
  #geom_line(aes(x = dates_formatted, y = data_with_gas$spot_price_log_diff, color = "without gas", linetype ="Cholesky")) +
  theme_minimal()

length(dates_formatted)
length(IV_data_log_diff_fr$Spot_gas)
summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE),
  mean_gas = colMeans(data_IV_with_gas, na.rm = TRUE),
  sd_gas = apply(data_IV_with_gas, 2, sd, na.rm = TRUE)
)



gamma_no_gas <- gamma



