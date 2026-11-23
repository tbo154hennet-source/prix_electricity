#price_test = price_grid[200]

# })
library(limSolve)
library(MASS)
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


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_qty_supply_unmatched <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_qty_demand_unmatched <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')



estimate_coeff_shift_qty_controled <- function(price_test, reconstruct_full_curve_shift_qty_supply_unmatched,
                                               reconstruct_full_curve_shift_qty_demand_unmatched,
                                     factors){
  
  # Étape 1 : filtrage et prétraitement
  reg_data_test_demand <- reconstruct_full_curve_shift_qty_demand_unmatched[PRICE == price_test]
  
  reg_data_test_supply <- reconstruct_full_curve_shift_qty_supply_unmatched[PRICE == price_test]
  # Renommer QTY dans chaque dataframe
  # reg_data_test_demand <- reg_data_test_demand %>% rename(QTY_D = QTY) %>%  dplyr::select(-OFFER_TYPE)
  # reg_data_test_supply <- reg_data_test_supply %>% rename(QTY_S = QTY) %>%  dplyr::select(-OFFER_TYPE)
  # demand
  reg_data_test_demand[, QTY_D := QTY][, QTY := NULL][, MATCHED := NULL]
  
  # supply
  reg_data_test_supply[, QTY_S := QTY][, QTY := NULL][, OFFER_TYPE := NULL]
  
  # Fusion sur toutes les colonnes communes automatiquement
  reg_data_test <- merge(reg_data_test_demand,
                         reg_data_test_supply,
                         by = intersect(names(reg_data_test_demand), names(reg_data_test_supply)))
  
  
  # Fusion finale
  reg_data_test <- merge(
    reg_data_test,
    price_indispo_merge_esp,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  )
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
    complete.cases(reg_data_test[, .SD, .SDcols = c("QTY_S", "QTY_D",  factor_vars, factors)])
  ]
  
  
  # Suppose: reg_data_test contient "QTY" et des facteurs X1,...,Xn
  # Assure-toi que X et Y sont num�riques
  X <- reg_data_test %>% dplyr::select(factors) %>%  dplyr::mutate(across(everything(), as.numeric)) 
  Y_D <- as.numeric(reg_data_test$QTY_D)
  Y_S <- as.numeric(reg_data_test$QTY_S)
  # corr�lation de chaque colonne avec Y
  c_i <- cor(X, Y_S, use = "complete.obs")
  
  
  X_mat <- as.matrix(cbind(1, X))  # matrice des r�gressions
  y_vec <- as.numeric(Y_D)
  
  # Contraintes: somme l_i c_i = 0
  # (ici uniquement sur les colonnes des facteurs, pas sur l'intercept)
  #A <- matrix(c(0, c_i), nrow = 1)  # 0 pour l'intercept
  A <- matrix(c(0, c_i), nrow = 1, ncol = ncol(X_mat), byrow = TRUE)
  b <- 0
  
  #fit_beta <- lsei(A = X_mat, B = y_vec, E = A, F = b)
  
  n_idx <- length(A)
  A_n <- A[n_idx]
  rest_idx <- setdiff(1:n_idx, n_idx)
  
  # build reduced X
  X_reduced <- X_mat[, rest_idx, drop=FALSE] - (matrix(A[rest_idx]/A_n, nrow=nrow(X_mat), ncol=length(rest_idx), byrow=TRUE) * X_mat[, n_idx])
  
  df <- as.data.frame(X_reduced)
  df$y <- y_vec
  
  lm_reduced <- lm(y ~ . - 1, data = df)  # -1 to avoid intercept if X already includes it
  
  # get reduced coefficients
  beta_reduced <- coef(lm_reduced)
  # Lag = 24, adjust if needed
  nw_cov <- NeweyWest(lm_reduced, lag = 24, prewhite = FALSE)
  nw_coefs <- coeftest(lm_reduced, vcov = nw_cov)
  
  # reconstruct full beta
  beta_full <- numeric(n_idx)
  beta_full[rest_idx] <- beta_reduced
  beta_full[n_idx] <- - sum(A[rest_idx] * beta_reduced) / A_n
  
  
  # map covariance to full coefficients
  C <- matrix(0, nrow=n_idx, ncol=length(rest_idx))
  C[rest_idx, ] <- diag(length(rest_idx))
  C[n_idx, ] <- -A[rest_idx]/A_n
  
  Vnw_full <- C %*% nw_cov %*% t(C)
  se_full <- sqrt(diag(Vnw_full))
  
  # create result table
  coef_table <- data.table(
    variable = colnames(X_mat),
    estimate = beta_full,
    std_error = se_full,
    price_test = price_test
  )
  
  return(coef_table)
  
}


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp_NTC.parquet") %>% as.data.table()
setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

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
# price_indispo_merge_esp_subset <- price_indispo_merge_esp[, ..selected_cols]
# price_indispo_merge_esp_subset[, Hour := creneau + 1]

#add mismatch
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(cold_excess_mismatch = cold_excess_demand - cold_excess_supply,
         heat_excess_mismatch = heat_excess_demand - heat_excess_supply,
         precipitation_mismatch = precipitation_demand - precipitation_supply,
         relative_humidity_mismatch = relative_humidity_demand - relative_humidity_supply,
         temperature_mismatch = temperature_demand - temperature_supply
  )
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
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
  "Published.Predicted.Unavailable.Capacity",
  "Unpublished.Predicted.Unavailable.Capacity",
  "cbf_es"
)



my_factors <- c(general_factors,
                "cold_excess_supply", "heat_excess_supply",
                "precipitation_supply", "relative_humidity_supply",
                "cold_excess_mismatch", "heat_excess_mismatch",
                "precipitation_mismatch","relative_humidity_mismatch"
                
)


#reconstruct_full_curve_shift <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
options(future.globals.maxSize = 20 * 1024^3)  # 10 Go

# Ex?cution sans barre de progression
coef_demand_unmatched<- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty_controled(
    price_test = g,
    reconstruct_full_curve_shift_qty_supply_unmatched = reconstruct_full_curve_shift_qty_supply_unmatched,
    reconstruct_full_curve_shift_qty_demand_unmatched = reconstruct_full_curve_shift_qty_demand_unmatched,
    factors = my_factors
  )
})

# coef_demand_unmatched <-  with_progress({
#     p <- progressor(along = price_grid_sparse)
# 
#     lapply(price_grid, function(g) {
#       estimate_coeff_shift_qty_controled(
#         price_test = g,
#         reconstruct_full_curve_shift_qty_supply_unmatched = reconstruct_full_curve_shift_qty_supply_unmatched,
#         reconstruct_full_curve_shift_qty_demand_unmatched = reconstruct_full_curve_shift_qty_demand_unmatched,
#         factors = my_factors
#       )
#     })
#   })


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
saveRDS(coef_demand_unmatched, "reg_shift_result_qty_demand_unmatched_ortho_mat.rds")

coef_supply_unmatched <- future_lapply(price_grid_sparse, function(g) {
  estimate_coeff_shift_qty_controled(
    price_test = g,
    reconstruct_full_curve_shift_qty_supply_unmatched = reconstruct_full_curve_shift_qty_demand_unmatched,
    reconstruct_full_curve_shift_qty_demand_unmatched = reconstruct_full_curve_shift_qty_supply_unmatched,
    factors = my_factors
  )
})


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
saveRDS(coef_supply_unmatched, "reg_shift_result_qty_supply_unmatched_ortho_mat.rds")


