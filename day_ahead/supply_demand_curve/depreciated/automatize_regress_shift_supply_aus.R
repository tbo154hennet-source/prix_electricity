
#setwd("Z:/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_supply_unmatched.parquet')

# Liste des colonnes à sélectionner
time_factors <- c("year", "month", "delivery_date", "creneau", "weekday", "datetime")


# Lire le fichier .parquet et convertir en data.table
# save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
# setwd(save_path_data)

price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp_NTC.parquet") %>% as.data.table()
setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp $delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp $datetime)

price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(temperature_mismatch = temperature_demand - temperature_supply,
         precipitation_mismatch = precipitation_demand - precipitation_supply,
         relative_humidity_mismatch =  relative_humidity_demand - relative_humidity_supply
  )

# Prétraitement sur l'autre table
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(Hour = creneau + 1)
# price_indispo_merge_esp_subset <- price_indispo_merge_esp[, ..selected_cols]
# price_indispo_merge_esp_subset[, Hour := creneau + 1]

cum_grid <- seq(0, 60000,length.out = 1000)
cum_energy_test = cum_grid[1]

estimate_coeff_shift <- function(cum_energy_test, reconstruct_full_curve_shift,
                                 factors){
  
  # Étape 1 : filtrage et prétraitement
  reg_data_test <- reconstruct_full_curve_shift[grid_point == cum_energy_test]
  # Fusion finale
  reg_data_test <- merge(
    reg_data_test,
    price_indispo_merge_esp,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  )
  # colnames(reg_data_test)
  # setdiff(c("PRICE",supply_factors), colnames(reg_data_test))
  #reg_data_test <- reg_data_test[complete.cases(.SD), .SD, .SDcols = c("PRICE",supply_factors)]
  # Définir les colonnes utiles AVANT le data.table
  supply_factors_subset <- setdiff(factors, c("unpredicted_renewables", "intraday_update_renewables"))
  
  
  # Variables à transformer en facteurs
  factor_vars <- c("year", "month", "creneau", "weekday")
  factor_vars_subset <- c( "creneau", "weekday")
  # Créer les termes factorisés : as.factor(var)
  factor_terms <- paste0("as.factor(", factor_vars_subset, ")")
  
  # Combiner tous les termes
  all_rhs_terms <- c(factor_terms, supply_factors_subset )
  
  # Filtrer en gardant les lignes complètes uniquement pour PRICE + variables sélectionnées
  reg_data_test <- reg_data_test[
    complete.cases(reg_data_test[, .SD, .SDcols = c("PRICE", factor_vars_subset, supply_factors_subset)])
  ]
  
  # Crée la formule dynamiquement
  formula_supply <- reformulate(
    all_rhs_terms,  # droite de la formule
    response = "PRICE"      # variable à gauche
  )
  
  # Applique la formule à lm()
  lm_grid_point_supply <- lm(
    formula = formula_supply,
    data = reg_data_test
  )

  coefs <- summary(lm_grid_point_supply)$coefficients
  
  coef_table <- data.table::data.table(
    variable = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    cum_energy_test = cum_energy_test
  )
  
  return(coef_table)
  
}

#  OFFER_TYPE == 'C' demand(compra) & OFFER_TYPE == 'V' supply(venta)
#  MATCHED == 'O' unmatched (oferta) & MATCHED == 'C' matched (casada)

supply_factors_temp <- c(
                    "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables", "spot_gas",
                    "Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
                    "Unpublished.Predicted.Unavailable.Capacity",
                    #"temperature_demand", "precipitation_demand", "relative_humidity_demand",
                    "temperature_supply", "precipitation_supply", "relative_humidity_supply",
                    "temperature_mismatch", "precipitation_mismatch", "relative_humidity_mismatch"
                    )

supply_factors_temp_demand <- c(
  "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables", "spot_gas",
  "Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
  "Unpublished.Predicted.Unavailable.Capacity",
  "temperature_demand", "precipitation_demand", "relative_humidity_demand"
)

supply_factors_excess <- c("precipitation_supply", "relative_humidity_supply",
                             "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables",
                           "spot_gas",
                           "Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
                           "Unpublished.Predicted.Unavailable.Capacity",
                           "cold_excess_supply", "heat_excess_supply",
                           #"cold_excess_demand", "heat_excess_demand",
                           "cold_excess_mismatch", "heat_excess_mismatch"
                           )

supply_factors_excess_demand <- c("precipitation_supply", "relative_humidity_supply",
                           "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables",
                           "spot_gas",
                           "Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
                           "Unpublished.Predicted.Unavailable.Capacity",
                           "cold_excess_demand", "heat_excess_demand"
                           
)

# demand_factors_temp <- c("temperature_demand", "precipitation_demand", 
#                          "relative_humidity_demand", "renewable_day_ahead_forecast", "spot_gas")
# 
# demand_factors_excess <- c( "precipitation_demand",
#                             "relative_humidity_demand", "renewable_day_ahead_forecast", "spot_gas",
#                             "cold_excess_demand", "heat_excess_demand")


# Boucle sur les valeurs de cum_energy_test
# coef_supply_unmatched <- lapply(
#   cum_grid[200:210], 
#   estimate_coeff_shift, 
#   reconstruct_full_curve_shift = reconstruct_full_curve_shift,
#   factors = supply_factors_temp
# )


library(future)
library(future.apply)
library(progressr)

# Utilise tous les cours disponibles
future::plan(multisession)

#change max export value for future
options(future.globals.maxSize = 5 * 1024^3)  # ou une valeur plus pr�cise comme 5 * 1024^3 pour 5 Go

# Activer le gestionnaire de progression
handlers(global = TRUE)
handlers("progress")

# Active le suivi avec `with_progress()`
coef_supply_unmatched <- with_progress({
  p <- progressor(steps = length(cum_grid))
  
  future_lapply(cum_grid, function(g) {
    res <- estimate_coeff_shift(
      cum_energy_test = g,
      reconstruct_full_curve_shift = reconstruct_full_curve_shift,
      factors = supply_factors_temp
    )
    p()  # Incr�mente la barre
    res
  })
})

#setwd("Z:/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
write_parquet(coef_supply_unmatched, "reg_shift_result_supply_unmatched.parquet")



