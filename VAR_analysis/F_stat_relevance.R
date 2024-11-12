library(vars)
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(estimatr)
library(stargazer)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
DATASET <- read.csv("DATASET_levels_sa.csv")

colnames(DATASET)
var_data <- DATASET %>% 
  select(log_demand, log_elec_price,
          ))









