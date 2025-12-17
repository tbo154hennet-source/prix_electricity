
VAR_data_log_sov_proxies[424,]

my_var_IV_short =
  sovereign::VAR(
    data = VAR_data_log_sov_proxies[424:528,],
    horizon = 10,
    freq = 'month',
    p = 4,
    # lag.ic = 'BIC',
    # lag.max = 4,
    structure = "IV",
    instrument = "proxies_kanzig",
    instrumented = "X1"
    
  )
# impulse response functions
var.irf.iv = sovereign::var_irf(my_var_IV_short)

plot_irf(var.irf.iv,  shocks = "X1", responses = NULL, verticle = FALSE)



#install.packages("seasonal")  # Install the package if not already installed
library(seasonal)

# Load and transform the data
set.seed(123)
data <- ts(rnorm(120, mean = 100, sd = 10), start = c(2010, 1), frequency = 12)

# Apply X-13ARIMA-SEATS seasonal adjustment
sa_result <- seas(data)

# Extract the seasonally adjusted series and plot
sa_series <- final(sa_result)
plot(sa_series, main = "Seasonally Adjusted Series")

# Optional: Export results
write.csv(data.frame(Date = time(sa_series), Adjusted = sa_series), "seasonally_adjusted.csv")
