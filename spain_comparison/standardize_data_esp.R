library(arrow)
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(estimatr)
# get IV_data part


Sys.setenv(lang = "en_US")
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

monthly_indispo <- read_parquet('spain_monthly_indispo.parquet')
nuclear_monthly_indispo <- read_parquet('spain_nuclear_monthly_indispo.parquet')
unexpected_monthly_indispo <- read_parquet('unexpected_monthly_indispo.parquet')
unexpected_nuclear_monthly_indispo <- read_parquet('unexpected_nuclear_monthly_indispo.parquet')
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
colnames(unexpected_monthly_indispo)
colnames(unexpected_nuclear_monthly_indispo)
colnames(surprise_serie)

#merge indispos
monthly_indispo <- monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(tot_indispo = Unavailable.Capacity)
nuclear_monthly_indispo <- nuclear_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(nuclear_indispo = Unavailable.Capacity)
unexpected_monthly_indispo <- unexpected_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(unexpected_tot_indispo = Unavailable.Capacity)
unexpected_nuclear_monthly_indispo <- unexpected_nuclear_monthly_indispo %>% 
  select(-Installed.Capacity) %>% 
  rename(unexpected_nuclear_indispo = Unavailable.Capacity)



plant_indispos <- left_join(monthly_indispo, nuclear_monthly_indispo, 
                        by= c('month', 'year'))
plant_indispos <- left_join(plant_indispos, unexpected_monthly_indispo, 
                            by= c('month', 'year'))
plant_indispos <- left_join(plant_indispos, unexpected_nuclear_monthly_indispo, 
                        by= c('month', 'year'))

colnames(macro_data)

IV_data <- merge(macro_data, price_renew_final, by= c('month', 'year')) %>% 
  select("datetime", "ECBMRRFR_interest_rate.xls",                              
         "ENRGY0ESM086NEST_energy_index.xls" ,"ESPPROINDMISMEI_production_sales_monthly.xls",                     
         "00XE00ESM086NEST_non_energy_index.xls" , "LRHUTTTTESM156S_monthly_unemployment.xls", "log_price",
"log_diff", "spot_price", "renewable_generation", "gen_tot",
"demand_growth_period", "renewable_growth", 'month', 'year')

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
         IPI = ESPPROINDMISMEI_production_sales_monthly.xls,
         NE_Index= `00XE00ESM086NEST_non_energy_index.xls`,
         E_Index = ENRGY0ESM086NEST_energy_index.xls,
         UnE = LRHUTTTTESM156S_monthly_unemployment.xls,
         #Exp_pi = Inflation_expectation_CSINFT02FRM460S.xls, 
         demand_growth_rate = demand_growth_period,
         abs_gas_shock = abs_shock,
         rel_gas_shock = relative_shock) %>%
  select(-c("log_elec_price", "elec_growth_rate", 
            "demand_growth_rate", "renewable_growth")) %>% 
  arrange(datetime)

colnames(IV_data)
write_parquet(IV_data, 'low_freq_data.parquet')

getwd()
save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data <- read_parquet('low_freq_data.parquet')
# meme chose pour IV demand

# on prend le niveau en pourcentage au dessus ou au dessous de la moyenne
# comme dans kilian





