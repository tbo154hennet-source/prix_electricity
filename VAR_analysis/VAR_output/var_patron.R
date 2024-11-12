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
DATASET <- read.csv("DATASET_sa_detrended.csv") 

colnames(DATASET)

DATASET = zoo(DATASET, frequency=12)

var_data = DATASET[, c("demand_growth_rate",
                   "elec_growth_rate",
                   "ne_growth_rate",
                   "ipi_growth_rate",
                   "ECB_rate")]
 


proxies = DATASET[,c("rel_gas_shock",
                   "renewable_growth",
                   "abs_gas_shock", 
                   "tot_indispo",
                   "nuclear_indispo",
                   "cold_excess_instrument",
                   "heat_excess_instrument",
                   "CDD_excess_instrument",
                   "HDD_excess_instrument")]

# year fixed effects
DATASET[, 1]  <- (DATASET[, 1]-1) %/% 12
# monthly fixed effects
DATASET[, 2]<- (DATASET[, 2]-1) %% 12
DATASET[, 17] <- log(DATASET[, 17]+1)

