library(future.apply)
library(progress)
library(future)
library(progressr)
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)
library(sandwich)
library(lmtest)
library(fixest)

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")


# Liste des colonnes à sélectionner
time_factors <- c("year", "month", "delivery_date", "creneau", "weekday", "datetime")


# Lire le fichier .parquet et convertir en data.table
# save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
# setwd(save_path_data)
output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge<- read_parquet( "price_indispo_merge_esp_npc_all.parquet")  %>% as.data.table()
#setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp $delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)

price_indispo_merge_esp$global_tilted_irradiance <- price_indispo_merge_esp$global_tilted_irradiance/1000
# Prétraitement sur l'autre table
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(Hour = creneau + 1)

# Remplir les NAs dans la colonne 'spot_gas' avec la derni?re valeur non manquante pr?c?dente
price_indispo_merge_esp[, spot_gas := nafill(spot_gas, "locf")]


colnames_with_capacity <- colnames(price_indispo_merge_esp)[grepl("Capacity", colnames(price_indispo_merge_esp))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables",
    "renewable_day_ahead_forecast_ex_es", "cbf_es",
    "NTC_ES_FR" ,"NTC_FR_ES",                                 
    "NTC_ES_PT", "NTC_PT_ES"
  )
price_indispo_merge_esp[, (colnames_with_capacity) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = colnames_with_capacity]

price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(across(all_of(capacity_fields), 
                ~ . / 1000))


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
# price_indispo_merge_esp_subset <- price_indispo_merge_esp[, ..selected_cols]
# price_indispo_merge_esp_subset[, Hour := creneau + 1]

#mismatch whatever rsidual in the rgression of one against the other
# price_indispo_merge_esp <- price_indispo_merge_esp %>% 
#   mutate(cold_excess_mismatch = cold_excess_demand - cold_excess_supply,
#          heat_excess_mismatch = heat_excess_demand - heat_excess_supply,
#          precipitation_mismatch = precipitation_demand - precipitation_supply,
#          relative_humidity_mismatch = relative_humidity_demand - relative_humidity_supply,
#          temperature_mismatch = temperature_demand - temperature_supply
#          )


colnames(price_indispo_merge_esp)

#factors = my_factors_detailed
#price_test = price_grid[200]
#price_test = price_grid[1]
estimate_coeff_shift_qty <- function(price_test, reconstruct_full_curve_shift,
                                 factors){
  
  # Étape 1 : filtrage et prétraitement
  reg_data_test <- reconstruct_full_curve_shift[PRICE == price_test]
  # Fusion finale
  reg_data_test <- merge(
    reg_data_test,
    price_indispo_merge_esp,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  )
  reg_data_test <- reg_data_test[!duplicated(reg_data_test$datetime), ]
  
  # setdiff(c("PRICE",supply_factors), colnames(reg_data_test))
  #reg_data_test <- reg_data_test[complete.cases(.SD), .SD, .SDcols = c("PRICE",supply_factors)]
  # Définir les colonnes utiles AVANT le data.table
  
  # Variables à transformer en facteurs
  factor_vars <- c("year", "month", "creneau", "weekday")
  # Créer les termes factorisés : as.factor(var)
  factor_terms <- paste0("as.factor(", factor_vars, ")")
  
  # Combiner tous les termes
  all_rhs_terms <- c(factor_terms, factors )
  #colnames(reg_data_test)
  # Filtrer en gardant les lignes complètes uniquement pour PRICE + variables sélectionnées
  reg_data_test <- reg_data_test[
    complete.cases(reg_data_test[, .SD, .SDcols = c("QTY", factor_vars, factors)])
  ]

  #colSums(is.na(reg_data_test))
  # View(reg_data_test[!complete.cases(reg_data_test)])
  # Crée la formule dynamiquement
  formula <- reformulate(
    all_rhs_terms,  # droite de la formule
    response = "QTY"      # variable à gauche
  )
  # Applique la formule à lm()
  # lm_grid_point <- lm(
  #   formula = formula,
  #   data = reg_data_test)
  # # Compute Newey-West standard errors with lag = 24
  # nw_se_24 <- NeweyWest(lm_grid_point, lag = 24, prewhite = FALSE)
  # 
  # # Extract coefficient test results with Newey-West SEs
  # nw_coefs <- coeftest(lm_grid_point, vcov. = nw_se_24)
  # 
  # # Create the table with NW std errors
  # coef_table <- data.table(
  #   variable = rownames(nw_coefs),
  #   estimate = nw_coefs[, "Estimate"],
  #   std_error = nw_coefs[, "Std. Error"],
  #   price_test = price_test
  # )

  m_grid <- feols(fml = formula,
                  data = reg_data_test,
                  NW(24) ~ datetime)
  reg_results <- m_grid$coeftable
  
  reg_results <- as.data.table(reg_results)
  reg_results$variable <- names(m_grid$coefficients)
  reg_results <- reg_results %>%
    mutate(price_test = price_test) %>% 
    rename(estimate = Estimate,
           std_error = `Std. Error`) %>% 
    dplyr::select(variable, estimate, std_error, price_test)
  

  
  
  return(  reg_results)
  
}


#  OFFER_TYPE == 'C' demand(compra) & OFFER_TYPE == 'V' supply(venta)
#  MATCHED == 'O' unmatched (oferta) & MATCHED == 'C' matched (casada)


general_factors <- c(
  "renewable_day_ahead_forecast", "unpredicted_renewables", 
  "intraday_update_renewables", "spot_gas",
  "Published.Unavailable.Capacity", 
  "Unpublished.Unavailable.Capacity",
  "Published.Unavailable.Nuclear.Capacity",
  "Unpublished.Unavailable.Nuclear.Capacity",
  #"renewable_day_ahead_forecast_ex_es",
      "cbf_es")



supply_factors_excess <- c(general_factors,
                           "cold_excess_supply", "heat_excess_supply",
                           "precipitation_supply", "relative_humidity_supply"
)

supply_factors_excess_demand <- c(general_factors,
                                  "cold_excess_demand", "heat_excess_demand"
                                  , "precipitation_demand", "relative_humidity_demand"
                                  
)

demand_factors_excess <- c(general_factors,
                           #"cold_excess_supply", "heat_excess_supply",
                           "cold_excess_demand", "heat_excess_demand",
                           "cold_excess_mismatch", "heat_excess_mismatch",
                           "precipitation_demand","precipitation_mismatch",
                           "relative_humidity_demand","relative_humidity_mismatch"
)



# Boucle sur les valeurs de cum_energy_test
# coef_supply_unmatched <- lapply(
#   cum_grid[200:210], 
#   estimate_coeff_shift, 
#   reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#   factors = supply_factors_temp
# )

# Utilise tous les cours disponibles
future::plan(multisession)

#change max export value for future
options(future.globals.maxSize = 6 * 1024^3)  # ou une valeur plus pr?cise comme 5 * 1024^3 pour 5 Go

# Activer le gestionnaire de progression
handlers(global = TRUE)
handlers("progress")

# Active le suivi avec `with_progress()`

lag_nw <- floor(0.75*(50000)^(1/3))
print(lag_nw)
#reconstruct_full_curve_shift <- reconstruct_full_curve_shift[DATE >= as.Date("2018-03-02") & DATE <= as.Date("2023-12-31")]

price_grid <- seq(-200, 1000, length.out = 1001)
# Get every 10th value
# 1. Zone dense : entre -10 et 200
dense_zone <- price_grid[price_grid >= -10 & price_grid <= 200]
dense_points <- dense_zone[round(seq(1, length(dense_zone), length.out = 80))]

# 2. Zone moins dense : hors de -10 ? 200
sparse_zone <- price_grid[price_grid < -10 | (price_grid > 200 & price_grid <500) ]
sparse_points <- sparse_zone[round(seq(1, length(sparse_zone), length.out = 20))]

# 3. Combinaison et tri
price_grid_sparse <- sort(unique(c(dense_points, sparse_points)))

price_test <- price_grid_sparse[20]

# Utiliser `lapply` avec barre de progression
# coef_supply_unmatched <- with_progress({
#   p <- progressor(along = price_grid)
#   
#   lapply(price_grid, function(g) {
#     p(sprintf("price = %.2f", g))
#     estimate_coeff_shift_qty(
#       price_test = g,
#       reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#       factors = supply_factors_excess
#     )
#   })
# })
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')

# Ex?cution sans barre de progression
coef_supply_unmatched <- future_lapply(price_grid, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_excess
  )
})

#setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
#write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched.rds")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')

coef_supply_matched <- future_lapply(price_grid, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = supply_factors_excess
  )
})
saveRDS(coef_supply_matched, "reg_shift_result_qty_supply_matched.rds")


demand_factors_excess <- c(general_factors,
                           #"cold_excess_supply", "heat_excess_supply",
                           "cold_excess_demand", "heat_excess_demand",
                           "cold_excess_mismatch", "heat_excess_mismatch",
                           "precipitation_demand","precipitation_mismatch",
                           "relative_humidity_demand","relative_humidity_mismatch"
)
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')

# Ex?cution sans barre de progression
coef_demand_unmatched <- future_lapply(price_grid, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_excess
  )
})
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched.rds")

getwd()

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')

# Ex?cution sans barre de progression
coef_demand_matched <- future_lapply(price_grid, function(g) {
  estimate_coeff_shift_qty(
    price_test = g,
    reconstruct_full_curve_shift = reconstruct_full_curve_shift,
    factors = demand_factors_excess
  )
})
saveRDS(coef_demand_matched, "reg_shift_result_qty_demand_matched.rds")

