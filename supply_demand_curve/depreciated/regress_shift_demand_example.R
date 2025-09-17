
library(tidyr)
library(dplyr)
library(arrow)
library(stargazer)
library(lmtest)
library(data.table)
library(sandwich)

setwd("Z:/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
curve_shift_data <- read_parquet( 'curve_shift_data_all.parquet')
colnames(curve_shift_data)



curve_shift_data[2, "grid_point"][[1]]
cum_energy_test = curve_shift_data[2, "grid_point"][[1]]




View(curve_shift_data[1:100,])


cum_grid <- seq(0, 60000,length.out = 1000)


# Table avec la grille pour tous les groupes
grid_dt <- unique(curve_shift_data[, .(DATE, Hour, OFFER_TYPE, MATCHED)])[
  , .(grid_point_test = cum_grid), 
  by = .(DATE, Hour, OFFER_TYPE, MATCHED)
]

setDT(curve_shift_data)
setkey(curve_shift_data, DATE, Hour, OFFER_TYPE, MATCHED, grid_point)

# Faire un join non-équi : on cherche le dernier grid_point <= grid_point_test
reconstruct_full_curve_shift <- curve_shift_data[grid_dt, 
                           on = .(DATE, Hour, OFFER_TYPE, MATCHED, grid_point <= grid_point_test),
                           mult = "last",
                           allow.cartesian = TRUE]


# Étape 1 : filtrage et prétraitement
reg_data_test2 <- reconstruct_full_curve_shift[grid_point == cum_energy_test & OFFER_TYPE == "V" & MATCHED == "O"][
  , Hour := as.numeric(Hour)][, DATE := as.Date(DATE, format = "%d/%m/%Y")][ order(DATE, Hour)]

# Lire le fichier .parquet et convertir en data.table
save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)

price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp.parquet") %>% as.data.table()
setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

price_indispo_merge_esp $weekday <- weekdays(price_indispo_merge_esp $delivery_date)
price_indispo_merge_esp $creneau <- hour(price_indispo_merge_esp $datetime)


# Liste des colonnes à sélectionner
time_factors <- c("year", "month", "delivery_date", "creneau", "weekday", "datetime")
supply_factors <- c("temperature_supply", "precipitation_supply", "relative_humidity_supply",
                    "renewable_day_ahead_forecast", "unpredicted_renewables", "intraday_update_renewables", "spot_gas")
demand_factors <- c("temperature_demand", "precipitation_demand", 
                    "relative_humidity_demand", "renewable_day_ahead_forecast", "spot_gas")

selected_cols <- unique(c(time_factors, demand_factors, supply_factors, "creneau"))
colnames(price_indispo_merge_esp)

# Prétraitement sur l'autre table
price_indispo_merge_esp_subset <- price_indispo_merge_esp[, ..selected_cols]
price_indispo_merge_esp_subset[, Hour := creneau + 1]

# Fusion finale
reg_data_test3 <- merge(
  reg_data_test2,
  price_indispo_merge_esp_subset,
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "Hour"),
  all.x = TRUE
)
reg_data_test4 <- reg_data_test3[complete.cases(.SD), .SD, .SDcols = c("PRICE",supply_factors)]
# Définir les colonnes utiles AVANT le data.table
supply_factors_subset <- setdiff(supply_factors, c("unpredicted_renewables", "intraday_update_renewables"))

# Filtrer en gardant les lignes complètes uniquement pour PRICE + variables sélectionnées
reg_data_test4 <- reg_data_test3[
  complete.cases(reg_data_test3[, .SD, .SDcols = c("PRICE", supply_factors_subset)])
]

lm_grid_point_supply <- lm(
  PRICE ~ temperature_supply +  precipitation_supply + 
    relative_humidity_supply + renewable_day_ahead_forecast  + spot_gas
  #+ unpredicted_renewables +intraday_update_renewables
  ,
  data = reg_data_test4
)

coefs <- summary(lm_grid_point_supply)$coefficients

# Pour les noms, estimateurs et erreurs standards
coef_table <- data.table::data.table(
  variable = rownames(coefs),
  estimate = coefs[, "Estimate"],
  std_error = coefs[, "Std. Error"],
  p_value = coefs[, "Pr(>|t|)"]
)

print(coef_table)


