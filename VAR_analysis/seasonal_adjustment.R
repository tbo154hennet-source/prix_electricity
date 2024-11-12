library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(forecast)
Sys.setenv(lang = "en_US")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data <- read_parquet('IV_data.parquet')
# meme chose pour IV demand
colnames(IV_data)
index_variables <- c("month","year", "datetime")

elec_variables <- c("spot_price",
                    "gen_tot",
                    "log_elec_price",
                    "log_demand")

instruments <- c("cold_excess_instrument",
                 "heat_excess_instrument",                   
                "CDD_excess_instrument" ,
                  "HDD_excess_instrument",
                  "tot_indispo",
                  "nuclear_indispo",
                "gas_price",
                "renewable_generation",
                "log_gas_price"
                )

VAR_levels_variables <- c("ECBMRRFR_interest_rate.xls",
                          "Industrial_prod_FRAPROINDMISMEI.xls",      
                          "Inflation_expectation_CSINFT02FRM460S.xls",
                          "non_energy_index_00XE00FRM086NEST.xls",    
                          "unemployment_LRHUTTTTFRM156S.xls",
                          )

# Initialize a list to store the seasonally adjusted series
adjusted_data <- list()
start_date = c('2015', '01')
frequency = 12

# Loop over each variable and perform STL decomposition
for (var in VAR_levels_variables) {
  ts_data <- ts(IV_data[[var]], start = start_date, frequency = frequency)
  
  # Perform STL decomposition
  stl_fit <- stl(ts_data, s.window = "periodic")
  
  # Extract the seasonally adjusted component
  adjusted_data[[var]] <- seasadj(stl_fit)
}


library(pracma)

print(class(adjusted_data))

# Combine the seasonally adjusted data into a dataframe
seasonally_adjusted_df <- as.data.frame(adjusted_data)

write.csv(seasonally_adjusted_df, "DATASET_levels_sa.csv")
#petit plot pour voir l'effet associé

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$non_energy_index_00XE00FRM086NEST.xls , color = "Original")) +
  geom_line(aes(y = seasonally_adjusted_df$non_energy_index_00XE00FRM086NEST.xls, color = "Seasonally Adjusted")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Seasonally Adjusted" = "red")) +
  theme_minimal()


# Detrend the data
detrended_data <- detrend(as.matrix(seasonally_adjusted_df))
detrended_data <- as.data.frame(detrended_data)

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$non_energy_index_00XE00FRM086NEST.xls -mean(IV_data$non_energy_index_00XE00FRM086NEST.xls) , color = "Original")) +
  geom_line(aes(y = detrended_data$non_energy_index_00XE00FRM086NEST.xls, color = "Detrended")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()


write.csv(detrended_data, "DATASET_levels_detrended.csv")


ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$log_demand -mean(IV_data$log_demand) , color = "Original")) +
  geom_line(aes(y = detrended_data$log_demand, color = "Detrended")) +
  labs(title = "Log Demand - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$log_elec_price -mean(IV_data$log_elec_price) , color = "Original")) +
  geom_line(aes(y = detrended_data$log_elec_price, color = "Detrended")) +
  labs(title = "Log Elec Price - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()


ggplot(IV_data, aes(x = datetime)) +
  geom_line(aes(y =IV_data$renewable_growth -mean(IV_data$renewable_growth) , color = "Original")) +
  geom_line(aes(y = detrended_data$renewable_growth, color = "Detrended")) +
  labs(title = "Renewable Growth rate - Original and Detrended",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()

