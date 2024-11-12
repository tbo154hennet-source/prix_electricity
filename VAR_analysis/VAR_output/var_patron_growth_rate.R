library(vars)
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(estimatr)
library(stargazer)
library(zoo)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
DATASET <- read.csv("DATASET_sa_detrended.csv") %>% 
  rename(ECB_rate= ECBMRRFR_interest_rate.xls,
         IPI = Industrial_prod_FRAPROINDMISMEI.xls,
         NE_Index= non_energy_index_00XE00FRM086NEST.xls,
         UnE = unemployment_LRHUTTTTFRM156S.xls
         #Exp_pi = Inflation_expectation_CSINFT02FRM460S.xls)
  )
colnames(DATASET)

DATASET = zoo(DATASET, frequency=12)
colnames(var_data)

var_data = DATASET["ECBMRRFR_interest_rate.xls",
                   "Industrial_prod_FRAPROINDMISMEI.xls"      
                   [4] "Inflation_expectation_CSINFT02FRM460S.xls"
                   [5] "non_energy_index_00XE00FRM086NEST.xls"    
                   [6] "unemployment_LRHUTTTTFRM156S.xls"]
 


proxies = DATASET["rel_gas_shock",
                   "renewable_growth",
                   "abs_gas_shock", 
                   "tot_indispo",
                   "nuclear_indispo",
                   "cold_excess_instrument",
                   "heat_excess_instrument",
                   "CDD_excess_instrument",
                   "HDD_excess_instrument"]

# year fixed effects
DATASET[, 1]  <- (DATASET[, 1]-1) %/% 12
# monthly fixed effects
DATASET[, 2]<- (DATASET[, 2]-1) %% 12
DATASET[, 17] <- log(DATASET[, 17]+1)

