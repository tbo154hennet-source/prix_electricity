

# Lire le fichier .parquet et convertir en data.table
# save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
# setwd(save_path_data)
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




instruments_tot <- c("Fore_Wind",
                     "Fore_Solar",
                     #"Published.Predicted.Unavailable.Capacity",
                     "cbf_es",
                     "cold_excess",
                     "heat_excess")

output_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
# Liste tous les fichiers RDS
coef_files <- list.files(pattern = "\\.rds$")
coef_files_filtered <- coef_files[setdiff(seq_along(1:length(coef_files)),grep("no_price", coef_files))]
#grep("nfyear", coef_files, invert = TRUE, value = TRUE)


# Fonction pour cr?er un nom court ? partir du fichier
short_name <- function(f){
  # Supprime "coef_" et ".rds", puis remplace les "_" par des points ou rien
  name <- gsub("^coef_", "", f)
  name <- gsub("\\.rds$", "", name)
  name <- gsub("_", ".", name)  # ou gsub("_", "", name) si tu veux sans s?parateur
  paste0("fit_", name)
}

# Exemple
sapply(coef_files_filtered, short_name)
# [1] "fitted.no.price.demand.matched" ...
coef_file <- coef_files_filtered[[5]]


# Fonction utilitaire pour g?n?rer les fitted values
compute_fitted_values <- function(coef_file, data=price_indispo_merge_esp_sd){
  
  # Charger les coefficients
  coef_table <- readRDS(coef_file)
  
  # Cr?er la matrice design ? partir des facteurs
  X <- model.matrix(~ month + weekday + creneau, data = data)
  X <- cbind("(Intercept)" = 1, X[, -1])  # forcer intercept
  

  
  # Renommer pour matcher les noms des coefs
  #colnames(X) <- gsub("^year",    "as.factor(year)",    colnames(X))
  colnames(X) <- gsub("^month",   "as.factor(month)",   colnames(X))
  colnames(X) <- gsub("^weekday", "as.factor(weekday)", colnames(X))
  colnames(X) <- gsub("^creneau", "as.factor(creneau)", colnames(X))
  vars_in_model <- unique(coef_table$variable)
  numeric_vars <- setdiff(vars_in_model[!grepl("^as\\.factor\\(", vars_in_model)],
                          c("(Intercept)", "PRICE"))        

  
  X <- cbind(X, data[, ..numeric_vars])
  # Aligner les coefficients
  coefs <- setNames(coef_table$estimate, coef_table$variable)
  #coefs <- coefs[colnames(X)]  # r?ordonner
  
  # V?rifier qu'il n'y a pas de NA (colonne absente ou coef manquant)
  missing <- is.na(coefs)
  
  print(coef_file)
  if(any(missing)){
    missing_names <- names(coefs)[missing]
    
    warning(paste("Attention : coefficients manquants pour", coef_file, ":", 
                  paste(names(coefs)[missing], collapse = ", ")))
    warning(missing_names)
    coefs[missing] <- 0
  }
  if ("PRICE" %in% names(coefs)){
    coefs <- coefs[-length(coef_table)]
  }
  X_num <- as.matrix(sapply(X, function(col) as.numeric(col)))

  # Calculer fitted values
  fitted_vals <- as.vector(X_num %*% coefs)
  
  return(fitted_vals)
}


# Fonction utilitaire pour g?n?rer les fitted values
compute_fitted_values <- function(coef_file, data=price_indispo_merge_esp_sd){
  
  # Charger les coefficients
  coef_table <- readRDS(coef_file)
  
  # Cr?er la matrice design ? partir des facteurs
  X <- model.matrix(~ month + weekday + creneau, data = data)
  X <- cbind("(Intercept)" = 1, X[, -1])  # forcer intercept
  
  
  
  # Renommer pour matcher les noms des coefs
  #colnames(X) <- gsub("^year",    "as.factor(year)",    colnames(X))
  colnames(X) <- gsub("^month",   "as.factor(month)",   colnames(X))
  colnames(X) <- gsub("^weekday", "as.factor(weekday)", colnames(X))
  colnames(X) <- gsub("^creneau", "as.factor(creneau)", colnames(X))
  vars_in_model <- unique(coef_table$variable)
  numeric_vars <- setdiff(vars_in_model[!grepl("^as\\.factor\\(", vars_in_model)],
                          c("(Intercept)", "PRICE"))        
  
  
  X <- cbind(X, data[, ..numeric_vars])
  # Aligner les coefficients
  coefs <- setNames(coef_table$estimate, coef_table$variable)
  #coefs <- coefs[colnames(X)]  # r?ordonner
  
  # V?rifier qu'il n'y a pas de NA (colonne absente ou coef manquant)
  missing <- is.na(coefs)
  
  print(coef_file)
  if(any(missing)){
    missing_names <- names(coefs)[missing]
    
    warning(paste("Attention : coefficients manquants pour", coef_file, ":", 
                  paste(names(coefs)[missing], collapse = ", ")))
    warning(missing_names)
    coefs[missing] <- 0
  }
  if ("PRICE" %in% names(coefs)){
    coefs <- coefs[-length(coef_table)]
  }
  X_num <- as.matrix(sapply(X, function(col) as.numeric(col)))
  
  # Calculer fitted values
  fitted_vals <- as.vector(X_num %*% coefs)
  
  return(fitted_vals)
}

# Donn?es de base : assurer que les facteurs sont bien d?finis
price_indispo_merge_esp_sd <- price_indispo_merge_esp %>%
  mutate(
    month   = factor(month),
    year    = as.numeric(year),
    creneau = factor(creneau),
    weekday = factor(weekday)
  )

# Appliquer la fonction ? tous les fichiers de coefs
all_fitted <- lapply(coef_files_filtered, function(f){
  compute_fitted_values(f, price_indispo_merge_esp_sd)
})

# Strip ".rds", remove "coef_", replace "_" by ".", and prepend "fitted_"
short_names <- short_name(gsub("\\.rds$", "", coef_files_filtered))
# [1] "fitted.no.price.demand.matched" ...

# Sauvegarder les fitted values dans le dataframe avec noms explicites
for(i in seq_along(coef_files_filtered)){
  name <- gsub("\\.rds$", "", coef_files[i])
  price_indispo_merge_esp_sd[[paste0(short_name(name) )]] <- all_fitted[[i]]
}
colnames(price_indispo_merge_esp_sd)



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")

write_parquet(price_indispo_merge_esp_sd, "price_indispo_merge_esp_delta_supply_demand_npc.parquet")




