library(arrow)
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(estimatr)
# get IV_data part


Sys.setenv(lang = "en_US")
save_path_fr <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_path_fr)

monthly_indispo <- read_parquet('monthly_indispo.parquet')
nuclear_monthly_indispo <- read_parquet('nuclear_monthly_indispo.parquet')
price_renew_final <- read_parquet( 'price_renew.parquet')
macro_data <- read_parquet( 'macro_data_monthly_index.parquet')
temperature_data <- read.csv("temperature_instrument.csv")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
surprise_serie<- read_parquet("surprise_serie.parquet") %>% 
  rename(abs_shock = shock, relative_shock = Delta)


macro_data <- macro_data %>%
  mutate(month= month(datetime),
         year= year(datetime)) 

colnames(temperature_data)
colnames(macro_data)
colnames(price_renew_final)
colnames(monthly_indispo)
colnames(nuclear_monthly_indispo)
colnames(surprise_serie)

#merge indispos
monthly_indispo <- monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(tot_indispo = Unavailable.Capacity)
nuclear_monthly_indispo <- nuclear_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(nuclear_indispo = Unavailable.Capacity)
plant_indispos <- merge(monthly_indispo, nuclear_monthly_indispo, 
                        by= c('month', 'year'))



IV_data <- merge(macro_data, price_renew_final, by= c('month', 'year')) %>% 
  select("datetime.x", "ECBMRRFR_interest_rate.xls",                              
"energy_index_ENRGY0FRM086NEST.xls","Industrial_prod_FRAPROINDMISMEI.xls",                     
"Inflation_expectation_CSINFT02FRM460S.xls", "non-energy_index_00XE00FRM086NEST.xls",                   
"unemployment_LRHUTTTTFRM156S.xls", "log_price",
"log_diff", "spot_price", "renewable_generation", "gen_tot",
"demand_growth_period", "renewable_growth", 'month', 'year') %>% 
  rename("datetime" = "datetime.x")

IV_data <- merge(IV_data, plant_indispos,
                 by= c('month', 'year'))

IV_data <- merge(IV_data, temperature_data,
                 by= c('month', 'year'))

IV_data <- merge(IV_data, surprise_serie,
                 by= c('month', 'year'))
IV_data <- IV_data %>% 
  rename(gas_price = avg_price,
         log_elec_price = log_price,
         elec_growth_rate = log_diff,
         ECB_rate= ECBMRRFR_interest_rate.xls,
         IPI = Industrial_prod_FRAPROINDMISMEI.xls,
         NE_Index= `non-energy_index_00XE00FRM086NEST.xls`,
         E_Index = energy_index_ENRGY0FRM086NEST.xls,
         UnE = unemployment_LRHUTTTTFRM156S.xls,
         Exp_pi = Inflation_expectation_CSINFT02FRM460S.xls, 
         demand_growth_rate = demand_growth_period) %>% 
  select(-c("log_elec_price", "elec_growth_rate", 
            "demand_growth_rate", "renewable_growth")) %>% 
  arrange(Date)

colnames(IV_data)
write_parquet(IV_data, 'low_freq_data.parquet')


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data <- read_parquet('low_freq_data.parquet')
# meme chose pour IV demand

# on prend le niveau en pourcentage au dessus ou au dessous de la moyenne
# comme dans kilian





