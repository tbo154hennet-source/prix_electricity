library(ggplot2)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
curve_shift_data <- read_parquet( 'curve_shift_data_supply.parquet')
setDT(curve_shift_data)

curve_shift_data <- read_parquet( 'curve_shift_data_all.parquet')

#limiter entre 2018-02-13 et 2023-12-31
curve_shift_data <- curve_shift_data[DATE >= "02/03/2018" & DATE <= "31/12/2023"]

cum_grid <- seq(0, 60000,length.out = 1000)
# Table avec la grille pour tous les groupes
grid_dt <- unique(curve_shift_data[, .(DATE, Hour, OFFER_TYPE, MATCHED)])[
  , .(grid_point_test = cum_grid), 
  by = .(DATE, Hour, OFFER_TYPE, MATCHED)
]
# Faire un join non-équi : on cherche le dernier grid_point <= grid_point_test
reconstruct_full_curve_shift <- curve_shift_data[grid_dt, 
                                                 on = .(DATE, Hour, OFFER_TYPE, MATCHED, grid_point <= grid_point_test),
                                                 mult = "last",
                                                 allow.cartesian = TRUE][
             , Hour := as.numeric(Hour)][, DATE := as.Date(DATE, format = "%d/%m/%Y")][ order(DATE, Hour)]

rm(grid_dt)
gc()
colnames(reconstruct_full_curve_shift)
max(reconstruct_full_curve_shift$DATE)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
reconstruct_full_curve_shift <-read_parquet("reconstruct_full_curve_shift_supply_unmatched.parquet")


# Lire le fichier .parquet et convertir en data.table
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)

price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp.parquet") %>% as.data.table()
setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp $delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp $datetime)

#design partie temperature

cold_months <- c(1, 2, 11, 12)
hot_months <- c(6, 7, 8, 9)

price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(cold_excess_demand = -ifelse(temperature_demand <= 10, 
                                      temperature_demand, 0),
         heat_excess_demand = ifelse(temperature_demand >= 20,
                                     temperature_demand, 0),
         cold_excess_supply = ifelse(temperature_supply <= 10, 
                                     temperature_supply, 0),
         heat_excess_supply = ifelse(temperature_supply >= 20,
                                     temperature_supply, 0),
         heat_excess_mismatch = ifelse(month %in% hot_months,
                                       temperature_demand - temperature_supply, 0),
         cold_excess_mismatch = ifelse(month %in% cold_months,
                                       temperature_supply - temperature_demand, 0)
  )


setDT(price_indispo_merge_esp)


# Utilisation de set pour modifier 'Hour' et 'temperature_mismatch' sans générer de copie interne
set(price_indispo_merge_esp, j = "Hour", value = price_indispo_merge_esp$creneau + 1)
set(price_indispo_merge_esp, j = "temperature_mismatch", value = price_indispo_merge_esp$temperature_demand - price_indispo_merge_esp$temperature_supply)
set(price_indispo_merge_esp, j = "precipitation_mismatch", value = price_indispo_merge_esp$precipitation_demand - price_indispo_merge_esp$precipitation_supply)
set(price_indispo_merge_esp, j = "relative_humidity_mismatch", value = price_indispo_merge_esp$relative_humidity_demand - price_indispo_merge_esp$relative_humidity_supply)
colnames_with_capacity <- colnames(price_indispo_merge_esp)[grepl("Capacity", colnames(price_indispo_merge_esp))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables"
    
  )
price_indispo_merge_esp[, (colnames_with_capacity) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = colnames_with_capacity]

price_indispo_merge_esp[, (capacity_fields) := lapply(.SD, function(x) x / 1000), .SDcols = capacity_fields]

# Remplir les NAs dans la colonne 'spot_gas' avec la dernière valeur non manquante précédente
price_indispo_merge_esp[, spot_gas := nafill(spot_gas, "locf")]


colnames(price_indispo_merge_esp)

result_coeff <- list()

cum_energy_test = cum_grid[1]
# Liste des colonnes à sélectionner
time_factors <- c("year", "month", "delivery_date", "creneau", "weekday", "datetime", "Hour")


estimate_coeff_shift <- function(cum_energy_test, reconstruct_full_curve_shift,
                                 factors){
  
  # Étape 1 : filtrage et prétraitement
  reg_data_test <- reconstruct_full_curve_shift[grid_point == cum_energy_test]
  
  selected_cols <- unique(c(time_factors, factors))

  
  # PrÃ©traitement sur l'autre table
  price_indispo_merge_esp_subset <- price_indispo_merge_esp[, ..selected_cols]

  # Fusion finale
  reg_data_test <- merge(
    reg_data_test,
    price_indispo_merge_esp_subset,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  )
  # colnames(reg_data_test)
  # setdiff(c("PRICE",supply_factors), colnames(reg_data_test))
  #reg_data_test <- reg_data_test[complete.cases(.SD), .SD, .SDcols = c("PRICE",supply_factors)]
  # Définir les colonnes utiles AVANT le data.table

  # Variables à transformer en facteurs
  factor_vars <- c("year", "month", "creneau", "weekday")

  # Créer les termes factorisés : as.factor(var)
  factor_terms <- paste0("as.factor(", factor_vars, ")")
  
  # Combiner tous les termes
  all_rhs_terms <- c(factor_terms, factors )
  
  #colSums(is.na(reg_data_test))
  
  # Filtrer en gardant les lignes complètes uniquement pour PRICE + variables sélectionnées
  reg_data_test <- reg_data_test[
    complete.cases(reg_data_test[, .SD, .SDcols = c("PRICE", factor_vars, factors)])
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
                    "Unpublished.Predicted.Unavailable.Capacity"
                    #,"temperature_supply", "precipitation_supply", "relative_humidity_supply",
                    ,"temperature_demand", "precipitation_demand", "relative_humidity_demand"
                    ,"temperature_mismatch", "precipitation_mismatch", "relative_humidity_mismatch"
                    )

demand_factors_temp <- c("temperature_demand", "precipitation_demand", 
                    "relative_humidity_demand", "renewable_day_ahead_forecast", "spot_gas"
                    ,"temperature_mismatch", "precipitation_mismatch", "relative_humidity_mismatch"
                    )





supply_factors_excess <- c("precipitation_supply", "relative_humidity_supply",
                             "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables",
                           "spot_gas",
                             "cold_excess_supply", "heat_excess_supply", 
                           "Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
                           "Unpublished.Predicted.Unavailable.Capacity")

demand_factors_excess <- c( "precipitation_demand",
                            "relative_humidity_demand", "renewable_day_ahead_forecast", "spot_gas",
                             "cold_excess_demand", "heat_excess_demand")


# Boucle sur les valeurs de cum_energy_test
coef_supply_unmatched <- lapply(
  cum_grid[200:210],
  estimate_coeff_shift,
  reconstruct_full_curve_shift = reconstruct_full_curve_shift,
  factors = supply_factors_temp
)


library(progress)


estimate_curve_shift <- function(reconstruct_full_curve_shift, supply_factors_temp){
  # Initialiser la barre de progression
  pb <- progress_bar$new(
    format = "  Traitement [:bar] :percent | Temps écoulé :elapsed",
    total = length(cum_grid), clear = FALSE, width = 60
  )
  
  # Liste pour stocker les résultats
  coef_supply_unmatched <- vector("list", length(cum_grid))
  
  # Boucle avec progression
  for (i in seq_along(cum_grid)) {
    coef_supply_unmatched[[i]] <- estimate_coeff_shift(
      cum_energy_test = cum_grid[i],
      reconstruct_full_curve_shift = reconstruct_full_curve_shift,
      factors = supply_factors_temp
    )
    pb$tick()  # Met à jour la barre
  }
  
  return(coef_supply_unmatched)
}


coef_supply_unmatched <- estimate_curve_shift(reconstruct_full_curve_shift, supply_factors_temp)
names(coef_supply_unmatched) <- cum_grid


names(coef_supply_unmatched) <- cum_grid[200:210]

variables <- c("temperature_demand", "temperature_mismatch", "precipitation_supply", "relative_humidity_supply",
                    "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables",
                    "Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity")


variables_temperatures <- c("temperature_demand", "temperature_mismatch")
variables_renewables <- c("renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables")
variables_indispo <- c("Published.Predicted.Unavailable.Capacity", "Uneffective.Predicted.Unavailable.Capacity", 
                            "Unpublished.Predicted.Unavailable.Capacity")

plot_shift_curve <- function(coef_supply_unmatched, variables) {
  # Convertir la liste en data.table
  coef_dt <- rbindlist(coef_supply_unmatched)
  
  # Filtrer pour le cum_energy_test spécifique
  plot_data <- coef_dt %>% 
    filter(variable %in% variables)
  # Tracer
  ggplot(plot_data) +
    geom_line(aes(x = cum_energy_test, y = estimate, color = variable)) +
    geom_ribbon(aes(x = cum_energy_test, ymin = estimate - std_error, ymax = estimate + std_error, fill = variable), alpha = 0.2)+
    labs(title = paste("Shift in Supply Curve"),
         x = "Variable",
         y = "Estimate") +
    coord_flip() +
    theme_minimal()
}


plot_shift_curve(coef_supply_unmatched, variables_temperatures)
plot_shift_curve(coef_supply_unmatched, variables_renewables)
plot_shift_curve(coef_supply_unmatched, variables_indispo)




