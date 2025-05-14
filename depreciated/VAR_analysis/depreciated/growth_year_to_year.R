#instead or seasonnally adjust and take growth rates,
#I take year over year growth rate

non_log_diff_var <- c("month", "year","datetime",
                      "X", "T_year_month", 
                      "cold_excess_instrument" ,"heat_excess_instrument",
                      "abs_gas_shock", "rel_gas_shock","Date")
# Create a copy of the original dataframe to store log differences
IV_data_log_diff_yty <- IV_data


# Apply log difference transformation to columns not in non_log_diff_var
cols_to_diff <- setdiff(names(IV_data_log_diff_yty), non_log_diff_var)
# IV_data_log_diff[cols_to_diff] <- lapply(IV_data_log_diff[cols_to_diff], function(column) {
#   log(column+1) - log(dplyr::lag(column)+1)
# })
# 2. Calculate log difference and store with suffix "_log_diff"
for (col in cols_to_diff) {
  IV_data_log_diff_yty[[paste0(col, "_log_diff")]] <- log(as.vector(IV_data_log_diff_yty[[col]])+1) -
    log(dplyr::lag(as.vector(IV_data_log_diff_yty[[col]]), 12)+1)
}
# 3. Remove the original columns (except the non-transformed ones)
IV_data_log_diff_yty <- IV_data_log_diff_yty[, !names(IV_data_log_diff_yty) %in% cols_to_diff]

View(IV_data_log_diff_yty)

#attention pour ECB rate par exemple ne fait aps de sens
# même augmentation de 50bips qui apparaît un an
#sur douze mesures de suite
#uniquement pour les réalisations independantes
#à savoir les instruments random


