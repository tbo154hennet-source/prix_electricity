options(future.globals.maxSize = 5 * 1024^2 * 1024)  # 10 GiB


################### NEW Temperature plots
# Variables à transformer en facteurs
factor_vars <- c("year","month", "creneau", "weekday")

#factor_vars <- c("year_month", "creneau","weekday")
# colnames(price_indispo_merge_esp)
# "intraday_update_wind" %in% colnames(price_indispo_merge_esp)
# colnames(renewable_forecasts)
general_factors <- c(
  "Fore_Wind","intraday_update_wind", "unpredicted_wind",
  "Fore_Solar", "unpredicted_sun","intraday_update_sun",
  "spot_gas",
  "Published.Predicted.Unavailable.Capacity",
  "Unpublished.Predicted.Unavailable.Capacity",
  "cbf_es"
)

general_factors_indispo <- c(
  setdiff(general_factors, c("Published.Predicted.Unavailable.Capacity",
                             "Unpublished.Predicted.Unavailable.Capacity")),
  "Published.Unavailable.Capacity",
  "Unpublished.Unavailable.Capacity",
  "Published.Unavailable.Nuclear.Capacity",
  "Unpublished.Unavailable.Nuclear.Capacity"
)
control_factors <- c("relative_humidity_2m_ss", "precipitation_ss",
                     "relative_humidity_2m_demand", "precipitation_demand",
                     "relative_humidity_2m_sth", "precipitation_sth"
)

colnames(price_indispo_merge_esp)


                          
price_indispo_merge_esp <- price_indispo_merge_esp %>%

  # # Renommer ss → supply
  rename(
    cold_excess_supply = cold_excess_ss,
    heat_excess_supply = heat_excess_ss,
    temperature_discomfort_supply = temperature_discomfort_ss
  ) %>%

  # Créer le mismatch froid
  mutate(
    cold_spatial_mismatch_supply =
      residuals(lm(cold_excess_supply ~ cold_excess_demand, data = .)),
    cold_spatial_mismatch_demand =
      residuals(lm(cold_excess_demand ~ cold_excess_supply, data = .)),
    # Créer le mismatch chaud
    heat_spatial_mismatch_demand =
      residuals(lm(heat_excess_demand ~ heat_excess_supply, data = .)),
    heat_spatial_mismatch_supply=
      residuals(lm(heat_excess_supply ~ heat_excess_demand, data = .)),
    # Créer le mismatch chaud
    temp_spatial_mismatch_supply=
      residuals(lm(temperature_discomfort_supply ~ temperature_discomfort_demand, data = .)),
    temp_spatial_mismatch_demand=
      residuals(lm(temperature_discomfort_demand ~ temperature_discomfort_supply, data = .))
  )



temp_factors <- c( "temperature_discomfort_ss"  , "temperature_discomfort_sth" ,
                   "temperature_discomfort_demand")


temp_factors_detailed <- c("cold_excess_ss", "heat_excess_ss",
                           "cold_excess_sth", "heat_excess_sth",
                           "cold_excess_demand", "heat_excess_demand")

temp_mismatches <- c("cold_spatial_mismatch_supply",
                     "heat_spatial_mismatch_supply",
                     "cold_spatial_mismatch_demand",
                     "heat_spatial_mismatch_demand",
                     "temp_spatial_mismatch_supply"
                     )

temp_main_effects <- c("temp_spatial_mismatch_supply","temperature_discomfort_demand" )

temp_detailed <-  c("cold_spatial_mismatch_supply", "heat_spatial_mismatch_supply",
                  "cold_excess_demand", "heat_excess_demand")
#one with the main effects 
#using only temperature demand
#and temperature mistmatch

my_factors <- c(general_factors, control_factors, temp_main_effects)

# my_factors_supply <- c(general_factors, control_factors, 
#                         c("cold_excess_supply", "heat_excess_supply",
#                           "cold_spatial_mismatch", "heat_spatial_mismatch") )
# my_factors_demand <- c(general_factors, control_factors, 
#                         c("cold_excess_demand", "heat_excess_demand") )
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_lv.rds")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

coef_demand_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors
  )
})
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_lv.rds")




my_factors_detailed <- c(general_factors, control_factors, temp_detailed)

# my_factors_supply <- c(general_factors, control_factors, 
#                         c("cold_excess_supply", "heat_excess_supply",
#                           "cold_spatial_mismatch", "heat_spatial_mismatch") )
# my_factors_demand <- c(general_factors, control_factors, 
#                         c("cold_excess_demand", "heat_excess_demand") )
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_detailed
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_lv_detailed.rds")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_detailed
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_lv_detailed.rds")



############# Wind and solar directly



general_factors_natural <- c("spot_gas", "Published.Predicted.Unavailable.Capacity"  ,
                             "Unpublished.Predicted.Unavailable.Capacity", "cbf_es", 
                             "wind_speed_100m_supply" , "global_tilted_irradiance_supply")
my_factors_natural <- c(general_factors_natural, control_factors, temp_detailed)

# my_factors_supply <- c(general_factors, control_factors, 
#                         c("cold_excess_supply", "heat_excess_supply",
#                           "cold_spatial_mismatch", "heat_spatial_mismatch") )
# my_factors_demand <- c(general_factors, control_factors, 
#                         c("cold_excess_demand", "heat_excess_demand") )
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_natural
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_lv_natural.rds")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_natural
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_lv_natural.rds")





#############indispo



my_factors_indispo<- c(general_factors_indispo, control_factors, temp_main_effects)

# my_factors_supply <- c(general_factors, control_factors, 
#                         c("cold_excess_supply", "heat_excess_supply",
#                           "cold_spatial_mismatch", "heat_spatial_mismatch") )
# my_factors_demand <- c(general_factors, control_factors, 
#                         c("cold_excess_demand", "heat_excess_demand") )
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_indispo
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_lv_indispo.rds")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_indispo
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_lv_indispo.rds")




############## Temperature mismatch evidence


colnames(price_indispo_merge_esp)
temp_mismatch_effects <- c("temp_spatial_mismatch_demand","temperature_discomfort_supply" )

my_factors_mismatch <- c(general_factors, control_factors, temp_mismatch_effects)

# my_factors_supply <- c(general_factors, control_factors, 
#                         c("cold_excess_supply", "heat_excess_supply",
#                           "cold_spatial_mismatch", "heat_spatial_mismatch") )
# my_factors_demand <- c(general_factors, control_factors, 
#                         c("cold_excess_demand", "heat_excess_demand") )
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_mismatch
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_lv_mismatch.rds")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

coef_demand_unmatched <- lapply(price_grid_sparse, function(g) {
  print(g)
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = my_factors_mismatch
  )
})
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_lv_mismatch.rds")











################ OLD STUFF


# 
# setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
# reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]
# 
# # Ex?cution sans barre de progression
# coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
#   estimate_coeff_shift_qty(
#     price_test = g,
#     reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#     factors = supply_factors_excess
#   )
# })
# 
# #setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
# #write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
# saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse.rds")
# 
# setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')
# reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]
# 
# coef_supply_matched <- future_lapply(price_grid_sparse, function(g) {
#   estimate_coeff_shift_qty(
#     price_test = g,
#     reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#     factors = supply_factors_excess
#   )
# })
# saveRDS(coef_supply_matched, "reg_shift_result_qty_supply_matched_sparse.rds")
# 
# 
# setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
# reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]
# 
# # Ex?cution sans barre de progression
# coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
#   estimate_coeff_shift_qty(
#     price_test = g,
#     reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#     factors = demand_factors_excess
#   )
# })
# saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse.rds")
# 
# getwd()
# 
# setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')
# reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]
# 
# # Ex?cution sans barre de progression
# coef_demand_matched <- future_lapply(price_grid_sparse, function(g) {
#   estimate_coeff_shift_qty(
#     price_test = g,
#     reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#     factors = demand_factors_excess
#   )
# })
# saveRDS(coef_demand_matched, "reg_shift_result_qty_demand_matched_sparse.rds")
# 
# 
# 
# 
# #all exogens
# #belle decomposition
# 
# 
# save_path_data <- "Z:/prix_electricite_gitlab"
# setwd(save_path_data)
# 
# renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')
# col_com <- intersect(colnames(renewable_forecasts), colnames(price_indispo_merge_esp))
# 
# # price_indispo_merge_Bonte <- left_join(price_indispo_merge_delta, renewable_forecasts %>% select(-col_com),
# #                                     by = c("datetime" = "__index_level_0__"))
# price_indispo_merge_esp <- merge(price_indispo_merge_esp, renewable_forecasts %>% dplyr::select(-col_com),
#                                    by.x = "datetime" , by.y= "__index_level_0__")
# 
# price_indispo_merge_esp$Wind_Total <- 
#   rowSums(price_indispo_merge_esp[, c("Wind Offshore", "Wind Onshore")], na.rm = TRUE)
# 
# 
# price_indispo_merge_esp <- price_indispo_merge_esp %>% 
#   mutate(intraday_update_wind = intraday_Wind - Fore_Wind,
#   unpredicted_wind = intraday_Solar - Fore_Solar,
#   intraday_update_sun = Wind_Total - intraday_Wind,
#   unpredicted_sun = Solar -intraday_Solar
#   )%>% 
#   mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
#          Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
#          Uneffective.Unavailable.Capacity = Uneffective.Unavailable.Capacity - Uneffective.Unavailable.Nuclear.Capacity
#   ) 
# 
# 
# 
# general_factors <- c(
#    "Fore_Wind","intraday_update_wind", "unpredicted_wind",
#    "Fore_Solar", "unpredicted_sun","intraday_update_sun",
#   "spot_gas",
#   # "Published.Unavailable.Nuclear.Capacity",
#   # "Unpublished.Unavailable.Nuclear.Capacity",
#   "cbf_es"
#   )
# 
# 
# 
# my_factors <- c(general_factors,
#                            "cold_excess_supply", "heat_excess_supply",
#                            "precipitation_supply", "relative_humidity_supply",
#                        "cold_excess_mismatch", "heat_excess_mismatch",
#                        "precipitation_mismatch","relative_humidity_mismatch"
#                        
# )
# 
# 
# #options(future.globals.maxSize = 5 * 1024^2 * 1024)  # 10 GiB
# 
# setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
# reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]
# 
# 
# # Ex?cution sans barre de progression
# coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
#     estimate_coeff_shift_qty(
#       price_test = g,
#       reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#       factors = my_factors
#     )
# })
# #which(sapply(coef_supply_unmatched, inherits, "error"))
# getwd()
# saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_exo_no_unavail.rds")
# 
# 
# setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
# reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]
# 
# # Ex?cution sans barre de progression
# 
# coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
#     estimate_coeff_shift_qty(
#       price_test = g,
#       reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#       factors = my_factors)
# })
# #which(sapply(coef_supply_unmatched, inherits, "error"))
# getwd()
# saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_exo.rds")
# 
# 



























