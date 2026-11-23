source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("reliability_auxiliary.R")
source("auxiliary_control.R")
library(MASS)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)

IV_data_log <- read_parquet('low_freq_data_log.parquet')
IV_data_log_diff <- read_parquet('low_freq_data_log_diff.parquet')

# IV_data_log$unexpected_nuclear_indispo_log <- IV_data_log$unexpected_nuclear_indispo.x_log 
# IV_data_log_diff$unexpected_nuclear_indispo_log_diff <- IV_data_log_diff$unexpected_nuclear_indispo.x_log_diff


#in growth rates

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)

headers_proxies = c(
  # "CDD_excess_instrument_log_diff",
  # "HDD_excess_instrument_log_diff", 
  # "abs_gas_shock",                 
  # "rel_gas_shock" ,
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  "gas_supply_shock",
  "T_avg_log_diff",
  "wind_log_diff", 
  "solar_log_diff",
  "unexpected_tot_indispo_log_diff",          
  "unexpected_nuclear_indispo_log_diff" 
)
colnames(IV_data_log_diff)


proxies <- ts(IV_data_log_diff[, c(headers_proxies)], frequency=12)

headers_var = c("gen_tot_log_diff",
                "spot_price_log_diff",
                "IPI_log_diff",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                "NE_Index_log_diff"
)
vars<- ts(IV_data_log_diff[, c(headers_var)], frequency=12)
save_reliable_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output/reliability"
setwd(save_reliable_path)
get_table(vars,p, proxies, "reliability_table_gr_wind_solar.png", headers_proxies)




#in log levels

headers_proxies = c(
  #"CDD_excess_instrument_log",
  #"HDD_excess_instrument_log",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  # "abs_gas_shock",                 
  # "rel_gas_shock" ,
  "gas_supply_shock",
  "T_avg_log",
  "wind_log", 
  "solar_log",
  "unexpected_tot_indispo_log",          
  "unexpected_nuclear_indispo_log" 
)

proxies <- ts(IV_data_log[, c(headers_proxies)], frequency=12)

headers_var = c("gen_tot_log",
                "spot_price_log",
                "IPI_log",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                "NE_Index_log"
)
vars<- ts(IV_data_log[, c(headers_var)], frequency=12)


getwd()

# most complete specification FE + controls for the whole period
#for title don't forget .png
setwd(save_reliable_path)
get_table(vars,p, proxies, "reliability_table_levels_solar_wind.png", headers_proxies)
  

  
  
  
  
  
  
  
  
