options(future.globals.maxSize = 5 * 1024^2 * 1024)  # 10 GiB

setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

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
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse.rds")

setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

coef_supply_matched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_excess
  )
})
saveRDS(coef_supply_matched, "reg_shift_result_qty_supply_matched_sparse.rds")


setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_excess
  )
})
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse.rds")

getwd()

setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_demand_matched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_excess
  )
})
saveRDS(coef_demand_matched, "reg_shift_result_qty_demand_matched_sparse.rds")




#all exogens
#belle decomposition


save_path_data <- "Z:/prix_electricite_gitlab"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')
col_com <- intersect(colnames(renewable_forecasts), colnames(price_indispo_merge_esp))

# price_indispo_merge_Bonte <- left_join(price_indispo_merge_delta, renewable_forecasts %>% select(-col_com),
#                                     by = c("datetime" = "__index_level_0__"))
price_indispo_merge_esp <- merge(price_indispo_merge_esp, renewable_forecasts %>% dplyr::select(-col_com),
                                   by.x = "datetime" , by.y= "__index_level_0__")

price_indispo_merge_esp$Wind_Total <- 
  rowSums(price_indispo_merge_esp[, c("Wind Offshore", "Wind Onshore")], na.rm = TRUE)


price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(intraday_update_wind = intraday_Wind - Fore_Wind,
  unpredicted_wind = intraday_Solar - Fore_Solar,
  intraday_update_sun = Wind_Total - intraday_Wind,
  unpredicted_sun = Solar -intraday_Solar
  )%>% 
  mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
         Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
         Uneffective.Unavailable.Capacity = Uneffective.Unavailable.Capacity - Uneffective.Unavailable.Nuclear.Capacity
  ) 



general_factors <- c(
   "Fore_Wind","intraday_update_wind", "unpredicted_wind",
   "Fore_Solar", "unpredicted_sun","intraday_update_sun",
  "spot_gas",
  # "Published.Unavailable.Nuclear.Capacity",
  # "Unpublished.Unavailable.Nuclear.Capacity",
  "cbf_es"
  )



my_factors <- c(general_factors,
                           "cold_excess_supply", "heat_excess_supply",
                           "precipitation_supply", "relative_humidity_supply",
                       "cold_excess_mismatch", "heat_excess_mismatch",
                       "precipitation_mismatch","relative_humidity_mismatch"
                       
)


#options(future.globals.maxSize = 5 * 1024^2 * 1024)  # 10 GiB

setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
    estimate_coeff_shift_qty(
      price_test = g,
      reconstruct_full_curve_shift = reconstruct_full_curve_shift,
      factors = my_factors
    )
})
#which(sapply(coef_supply_unmatched, inherits, "error"))
getwd()
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_sparse_exo_no_unavail.rds")


setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
reconstruct_full_curve_shift <- reconstruct_full_curve_shift[PRICE %in% price_grid_sparse]

# Ex?cution sans barre de progression

coef_demand_unmatched <- future_lapply(price_grid_sparse, function(g) {
    estimate_coeff_shift_qty(
      price_test = g,
      reconstruct_full_curve_shift = reconstruct_full_curve_shift,
      factors = my_factors)
})
#which(sapply(coef_supply_unmatched, inherits, "error"))
getwd()
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_sparse_exo.rds")





























