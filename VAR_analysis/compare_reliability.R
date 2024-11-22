save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)

IV_data_log <- read_parquet('low_freq_data_log.parquet')
IV_data_log_diff <- read_parquet('low_freq_data_log_diff.parquet')

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
  "T_avg_log_diff",
  "renewable_generation_log_diff", 
  "tot_indispo_log_diff",          
  "nuclear_indispo_log_diff" 
)

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
get_table(vars,p, proxies, "reliability_table_gr.png", headers_proxies)






#in log levels

headers_proxies = c(
  #"CDD_excess_instrument_log",
  #"HDD_excess_instrument_log",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  "T_avg_log",
  # "abs_gas_shock",                 
  # "rel_gas_shock" ,
  "renewable_generation_log", 
  "tot_indispo_log",          
  "nuclear_indispo_log" 
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
get_table(vars,p, proxies, "reliability_table_levels.png", headers_proxies)
  

  
  
  
  
  
  
  
  