

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data_fr <- read_parquet('IV_data_fr2.parquet') 


log_model <- lm(data = IV_data_fr, log(spot_price) ~ cbf_fr+ 
                  renewable_day_ahead_forecast_fr + Spot_gas+ log(gen_tot)+
                  as.factor(year) + as.factor(month))

summary(log_model)


gas_elec_data <- IV_data_fr %>% 
  arrange(datetime)%>% 
  mutate(log_diff_gas = log(Spot_gas) - log(lag(Spot_gas)),
         log_diff_spot = log(spot_price) - log(lag(spot_price)),
         log_diff_cbf = cbf_fr - lag(cbf_fr),
         log_diff_renewable = log(renewable_day_ahead_forecast_fr) - log(lag(renewable_day_ahead_forecast_fr)),
         log_diff_gen = log(gen_tot) - log(lag(gen_tot))
  ) %>% 
  dplyr::select(datetime, log_diff_spot, log_diff_gas, log_diff_cbf, log_diff_renewable,log_diff_gen,
                PC1_supply, year, month) 

colnames(gas_elec_data)
# Régression linéaire
log_diff_model <- lm(data = gas_elec_data, log_diff_spot ~
                       log_diff_renewable +PC1_supply + log_diff_gen
                     #+as.factor(year) + as.factor(month)
                       )

summary(log_diff_model)


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data_es <- read_parquet('IV_data_esp.parquet') 


log_model <- lm(data = IV_data_es, log(spot_price) ~ cbf_es+ 
                  renewable_day_ahead_forecast_esp + Spot_gas+ log(gen_tot)+
                  as.factor(year) + as.factor(month))

summary(log_model)


gas_elec_data <- IV_data_es %>% 
  arrange(datetime)%>% 
  mutate(log_diff_gas = log(Spot_gas) - log(lag(Spot_gas)),
         log_diff_spot = log(spot_price) - log(lag(spot_price)),
         log_diff_cbf = cbf_es - lag(cbf_es),
         log_diff_renewable = log(renewable_day_ahead_forecast_esp) - log(lag(renewable_day_ahead_forecast_esp)),
         log_diff_gen = log(gen_tot) - log(lag(gen_tot))
  ) %>% 
  dplyr::select(datetime, log_diff_spot, log_diff_gas, log_diff_cbf, log_diff_renewable,log_diff_gen,
                PC1_supply, year, month) 

colnames(gas_elec_data)
# Régression linéaire
log_diff_model <- lm(data = gas_elec_data, log_diff_spot ~
                       log_diff_renewable +PC1_supply + log_diff_gen
                     #+as.factor(year) + as.factor(month)
                     )

summary(log_diff_model)






