#stationanry tests
library(tseries)  # for adf.test
library(urca)     # for ur.kpss, ur.df etc.


setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output")
IV_data_diff <- read_parquet( 'IV_data_esp_diff.parquet')
IV_data_log <- read_parquet( 'IV_data_esp_log.parquet')


var_name <- headers_var_gen_first[1]
adf_results <- lapply(headers_var_gen_first, function(var_name) {
  series <- IV_data_log[[var_name]]
  result <- tryCatch({
    adf.test(series)
  }, error = function(e) {
    NA  # Handle errors (e.g., too short or missing values)
  })
  return(result)
})

names(adf_results) <- headers_var_gen_first
sapply(adf_results, function(res) if (is.list(res)) res$p.value else NA)


colnames(IV_data_log_diff)
headers_var_gen_first_diff <- sub("_log$", "_diff", headers_var_gen_first)

var_name <- headers_var_gen_first_diff[1]
adf_results <- lapply(headers_var_gen_first_diff, function(var_name) {
  series <- IV_data_diff[[var_name]]
  result <- tryCatch({
    adf.test(series)
  }, error = function(e) {
    NA  # Handle errors (e.g., too short or missing values)
  })
  return(result)
})

names(adf_results) <- headers_var_gen_first
sapply(adf_results, function(res) if (is.list(res)) res$p.value else NA)







