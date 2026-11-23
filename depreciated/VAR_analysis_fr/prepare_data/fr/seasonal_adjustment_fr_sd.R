library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(seasonal)
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
transformable_cols <- setdiff(
  colnames(IV_data),
  c(
    "month", "year", "ffr", "datetime", "last_Date", "last_Trading.day",
    "X", "Date", "observation_date", "T_min", "T_max"
  )
)
transformable_cols <- transformable_cols[
  !grepl("instrument|gas_shock|PC1", transformable_cols)
]
x13_vars <- intersect(
  c(
    "HICP", "NE_Index", "spot_price", "spot_gas", "spot_gas_capped",
    "load_tot", "load_forecast", "gen_tot", "gen_forecast"
  ),
  colnames(IV_data)
)
level_proxy_vars <- intersect(
  c("delta_s_tot", "delta_d_tot"),
  colnames(IV_data)
)
log_proxy_vars <- intersect(
  c("wind_forecast", "Fore_Wind", "temperature_discomfort"),
  colnames(IV_data)
)

monthly_anomaly <- function(series, months, series_name) {
  if (any(!is.finite(series))) {
    stop("Proxy contains non-finite values: ", series_name)
  }
  if (any(!months %in% 1:12)) {
    stop("Invalid month values while adjusting proxy: ", series_name)
  }
  series - ave(series, months, FUN = mean)
}


# Initialize a list to store the seasonally adjusted series
adjusted_data <- list()
x13_models <- list()
start_date = c('2015', '01')
frequency = 12


# Apply X-13 to observed monthly series and monthly anomalies to proxies
for (var in colnames(IV_data)) {
  ts_data <- ts(IV_data[[var]], start = start_date, frequency = frequency)
  if (var %in% x13_vars) {
    if (any(!is.finite(ts_data))) {
      stop("X-13 input contains non-finite values: ", var)
    }
    x13_fit <- seasonal::seas(
      ts_data,
      transform.function = "auto",
      regression.aictest = c("td", "easter")
    )
    ts_data <- seasonal::final(x13_fit)
    x13_models[[var]] <- x13_fit
  } else if (var %in% level_proxy_vars) {
    ts_data <- ts(
      monthly_anomaly(as.numeric(ts_data), IV_data$month, var),
      start = start_date,
      frequency = frequency
    )
  }
  adjusted_data[[var]] <- ts_data
}


getwd()
seasonally_adjusted_df <- as.data.frame(adjusted_data)
seasonally_adjusted_df$observation_date <- as.Date(IV_data$observation_date)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
write.csv(seasonally_adjusted_df, "DATASET_levels_sa_fr.csv")



non_log_diff_var <- c("month", "year", "datetime", "last_Date", "last_Trading.day",
                      "cold_excess_instrument" ,"heat_excess_instrument",
                      "Date", "ffr", "Bond_rate",
                      "cold_excess" , "heat_excess", "temp_discomfort",
                      transformable_cols[grepl("delta_", transformable_cols)]
)

non_log_col_var <- c(non_log_diff_var)

#attention pour abs shock and relative shock possible valeurs négatives

# Apply log transformation to all columns not in non_log_diff_var
IV_data_log <- seasonally_adjusted_df
#IV_data_log[setdiff(names(IV_data_log), non_log_col_var)] <- 
# log(IV_data_log[setdiff(names(IV_data_log), non_log_col_var)] +1)
cols_to_transform <- setdiff(transformable_cols, non_log_col_var)
# 1. Calculate log transformation and store with suffix "_log"
for (col in cols_to_transform) {
  numeric_col <- as.numeric(IV_data_log[[col]])  # Ensure numeric
  #detrended_col <- detrend(numeric_col)          # Detrend
  log_values <- log(numeric_col + 1)
  if (col %in% log_proxy_vars) {
    log_values <- monthly_anomaly(log_values, IV_data$month, col)
  }
  IV_data_log[[paste0(col, "_log")]] <- log_values
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






