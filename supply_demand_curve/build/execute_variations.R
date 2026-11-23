supply_factors_temp <- c(
  general_factors,
  #"temperature_demand", "precipitation_demand", "relative_humidity_demand",
  "temperature_supply", "precipitation_supply", "relative_humidity_supply",
  "temperature_mismatch", "precipitation_mismatch", "relative_humidity_mismatch"
)

supply_factors_temp_demand <- c(
  general_factors,
  "temperature_demand", "precipitation_demand", "relative_humidity_demand"
)

weather_vars <- price_indispo_merge_esp %>%
  select(precipitation_demand, relative_humidity_demand, 
         cold_excess_demand, heat_excess_demand) %>%
  scale()  # Standardize

pca_result <- prcomp(weather_vars, center = TRUE, scale. = TRUE)
price_indispo_merge_esp$weather_condition_index_demand <- pca_result$x[,1]


weather_vars <- price_indispo_merge_esp %>%
  select(precipitation_mismatch, relative_humidity_mismatch, 
         cold_excess_mismatch, heat_excess_mismatch) %>%
  scale()  # Standardize

pca_result <- prcomp(weather_vars, center = TRUE, scale. = TRUE)
price_indispo_merge_esp$weather_condition_index_mismatch <- pca_result$x[,1]



condensed_factors <-  c("renewable_day_ahead_forecast", "unpredicted_renewables", 
                               "intraday_update_renewables", "spot_gas",
                               "Published.Unavailable.Capacity", 
                               "Unpublished.Unavailable.Capacity",
                        "renewable_day_ahead_forecast_ex_es",
                        "weather_condition_index_demand",
                        "weather_condition_index_mismatch"
                        )
                        
                        


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = condensed_factors 
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_condensed_variation.rds")




general_factors_nuclear <- c("renewable_day_ahead_forecast", "unpredicted_renewables", 
                     "intraday_update_renewables", "spot_gas",
                     "Published.Unavailable.Nuclear.Capacity", 
                     "Uneffective.Unavailable.Nuclear.Capacity", 
                     "Unpublished.Unavailable.Nuclear.Capacity",
                     "NTC_ES_FR" ,"NTC_FR_ES",                                 
                     "NTC_ES_PT", "NTC_PT_ES",
                     "renewable_day_ahead_forecast_ex_es")

supply_factors_excess_nuclear <- c(general_factors_nuclear,
                           "cold_excess_supply", "heat_excess_supply",
                           #"cold_excess_demand", "heat_excess_demand",
                           "cold_excess_mismatch", "heat_excess_mismatch",
                           "precipitation_supply", "relative_humidity_supply",
                           "precipitation_mismatch", "relative_humidity_mismatch" 
)

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_excess_nuclear
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_nuclear_variation.rds")


#variation using directly temperature


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_temp
  )
})

saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_temp_variation.rds")


demand_factors_temp <- c(general_factors,
                         "temperature_demand", "precipitation_demand", "relative_humidity_demand",
                         #temperature_supply", "precipitation_supply", "relative_humidity_supply",
                         "temperature_mismatch", "precipitation_mismatch", "relative_humidity_mismatch",
                         "cold_excess_demand", "heat_excess_demand",
                         "cold_excess_mismatch", "heat_excess_mismatch"
)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_temp
  )
})

saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_temp_bis_variation.rds")







#wind speed and solar variance variation
#using day ahead forecast

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]


fore_wind_sun_factors <-  c("Fore_Wind", "Fore_Solar" , 
                       "spot_gas",
                        "Published.Unavailable.Capacity", 
                        "Unpublished.Unavailable.Capacity",
                         "NTC_ES_FR" ,"NTC_FR_ES",                                 
                         "NTC_ES_PT", "NTC_PT_ES"
                       
)
demand_factors_fore_wind_sun <- c(fore_wind_sun_factors,
                         "cold_excess_demand", "heat_excess_demand",
                         "cold_excess_mismatch", "heat_excess_mismatch"
)

  
# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_fore_wind_sun
  )
})

saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_fore_wind_sun_variation.rds")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

supply_factors_excess <- c(fore_wind_sun_factors,
                           "cold_excess_supply", "heat_excess_supply",
                           #"cold_excess_demand", "heat_excess_demand",
                           "cold_excess_mismatch", "heat_excess_mismatch",
                           "precipitation_supply", "relative_humidity_supply",
                           "precipitation_mismatch", "relative_humidity_mismatch" 
)

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_excess
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_fore_wind_sun_variation.rds")




#wind speed and solar variance variation


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]


wind_sun_factors <-  c("wind_speed_100m", "cloud_cover" , "global_tilted_irradiance" , 
                       "spot_gas",
                       "Published.Unavailable.Capacity", 
                       "Unpublished.Unavailable.Capacity",
                       "NTC_ES_FR" ,"NTC_FR_ES",                                 
                       "NTC_ES_PT", "NTC_PT_ES"
                       
)
demand_factors_wind_sun <- c(wind_sun_factors,
                             "cold_excess_demand", "heat_excess_demand",
                             "cold_excess_mismatch", "heat_excess_mismatch"
)


# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_wind_sun
  )
})

saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_wind_sun_variation.rds")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

supply_factors_excess <- c(wind_sun_factors,
                           "cold_excess_supply", "heat_excess_supply",
                           #"cold_excess_demand", "heat_excess_demand",
                           "cold_excess_mismatch", "heat_excess_mismatch",
                           "precipitation_supply", "relative_humidity_supply",
                           "precipitation_mismatch", "relative_humidity_mismatch" 
)

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_excess
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_wind_sun_variation.rds")










#variation with predicted unavailabilities


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]


factors_indispo_predicted <-  c("renewable_day_ahead_forecast", "unpredicted_renewables", 
                                "intraday_update_renewables", "spot_gas", "cbf_es",
                       "Published.Unavailable.Capacity", 
                       "Unpublished.Unavailable.Capacity",
                       "Published.Predicted.Unavailable.Capacity",
                       "Unpublished.Predicted.Unavailable.Capacity"
                       
)
#colnames(price_indispo_merge_esp)
demand_factors_indispo_predicted <- c(factors_indispo_predicted,
                             "cold_excess_demand", "heat_excess_demand"
)


# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_indispo_predicted
  )
})

saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_predicted_indispo_variation.rds")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

supply_factors_indispo_predicted <- c(factors_indispo_predicted,
                                      "cold_excess_supply", "heat_excess_supply"
)

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_indispo_predicted
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_predicted_indispo_variation.rds")

















