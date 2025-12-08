library(data.table)
library(arrow)
library(fixest)


# Lire le fichier .parquet et convertir en data.table
# save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
# setwd(save_path_data)
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
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

# Dossiers
input_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs"
output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"

# Fichiers à traiter
files_to_load <- c(
  "reconstruct_full_curve_shift_qty_supply_unmatched.parquet",
  "reconstruct_full_curve_shift_qty_supply_matched.parquet",
  "reconstruct_full_curve_shift_qty_demand_unmatched.parquet",
  "reconstruct_full_curve_shift_qty_demand_matched.parquet"
)

# Variables à transformer en facteurs
factor_vars <- c("year","month", "creneau", "weekday")

#factor_vars <- c("year_month", "creneau","weekday")

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


temp_factors <- c( "cold_excess_demand", "heat_excess_demand",
              "precipitation_demand", "relative_humidity_demand",
              "cold_excess_mismatch", "heat_excess_mismatch",
              "precipitation_mismatch","relative_humidity_mismatch")
factors <- c(general_factors,temp_factors)
factors_indispo <- c(general_factors_indispo,temp_factors)

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

price_indispo_merge_esp_npc <- price_indispo_merge_esp %>%
  mutate(
    year_month = factor(paste0(year, '_', month)),
    month   = factor(month),
    year    = factor(year),
    creneau = factor(creneau),
    weekday = factor(weekday)
  )


instruments_tot <- c("Fore_Wind",
                     "Fore_Solar",
                     "Published.Predicted.Unavailable.Capacity",
                     #"cbf_es",
                     "cold_excess_demand",
                     "heat_excess_demand")

instruments_indispo <-  c("Fore_Wind",
                          "Fore_Solar",
                          "Published.Unavailable.Capacity",
                          "Published.Unavailable.Nuclear.Capacity",
                          #"cbf_es",
                          "cold_excess_demand",
                          "heat_excess_demand")

instruments_min <- c("Fore_Wind",
                     "Fore_Solar",
                     "cold_excess_demand",
                     "heat_excess_demand")



#instruments = instruments_tot
# Function to compute fitted values for a model without PRICE
get_fitted_no_price <- function(data_price, data_reg, factors = NULL, instruments=NULL, FE =TRUE){
  
  # Build RHS formula
  all_rhs_terms <- if(!FE) instruments else c(factor_vars, factors)
  rhs_formula <- reformulate(c(all_rhs_terms, "PRICE"), response = "QTY") #reformulate(all_rhs_terms, response = "QTY")
  #rhs_formula <- reformulate(c(all_rhs_terms), response = "QTY")
  vars <- all.vars(rhs_formula)
  rhs_formula_build <- as.formula(paste("~", paste(instruments, collapse = " + ")))
  
  feols_model <- feols(fml = rhs_formula, data = data_reg)
  
  # Remove PRICE if it exists
  coefs_no_price <- coef(feols_model)[names(coef(feols_model)) %in% instruments]
  
  #coefs_no_price*diag(sqrt(cov(data_reg[, .SD, .SDcols = instruments])))
  
  # Replace NAs with column means for specific columns
  na_to_mean <- function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  cols_to_fix <- c("cbf_es", grep("Unavailable", names(data_price), value = TRUE))
  data_price[, (cols_to_fix) := lapply(.SD, na_to_mean), .SDcols = cols_to_fix]
  
  # Complete cases
  complete_idx <- complete.cases(data_price[, all.vars(rhs_formula_build), with = FALSE])
  X_complete <- model.matrix(rhs_formula_build, data = data_price[complete_idx, ])
  #coefs_vec <- coefs_no_price[colnames(X_complete)]
  
  # Supprimer l'intercept de la matrice
  X_no_intercept <- X_complete[, colnames(X_complete) != "(Intercept)", drop = FALSE]
  # Supprimer aussi le coefficient d'intercept
  coefs_no_intercept <- coefs_no_price[colnames(X_no_intercept)] #coefs_vec[names(coefs_vec) != "(Intercept)"]
  
  # Fitted values sans intercept
  fitted_vals <- as.vector(X_no_intercept %*% coefs_no_intercept)
  # if(!is.null(instruments)){
  #   # Supprimer l'intercept de la matrice
  #   X_no_intercept <- X_complete[, colnames(X_complete) != "(Intercept)", drop = FALSE]
  #   # Supprimer aussi le coefficient d'intercept
  #   coefs_no_intercept <- coefs_no_price[colnames(X_no_intercept)] #coefs_vec[names(coefs_vec) != "(Intercept)"]
  #   
  #   # Fitted values sans intercept
  #   fitted_vals <- as.vector(X_no_intercept %*% coefs_no_intercept)
  #   
  # }else{
  #   fitted_vals <- as.vector(X_complete %*% coefs_vec)
  # }
  
  
  # Contribution à la variance
  var_total <- var(fitted_vals)
  effects <- sweep(X_no_intercept, 2, coefs_no_intercept, `*`)
  contrib <- apply(effects, 2, var) / var_total
  
  # Retourner fitted + contributions
  out <- rep(NA_real_, nrow(data_price))
  out[complete_idx] <- fitted_vals
  list(fitted = out, contrib = contrib, coef = coefs_no_price)
}

file_name <- files_to_load[1]
process_file <- function(file_name) {
  cat("Processing:", file_name, "\n")
  
  # Charger le fichier
  df <- read_parquet(file.path(input_dir, file_name))
  df <-  df[PRICE %in% price_grid_sparse[13:(length(price_grid_sparse)-10)]]
  
  reg_data <- merge(
    df,
    price_indispo_merge_esp_npc,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  ) %>% 
    mutate(DATE = factor(DATE))
  
  all_rhs_terms <- c(factor_vars, factors)
  all_rhs_terms_indispo <- c(factor_vars, factors_indispo)
  
  reg_data <- reg_data[
    complete.cases(reg_data[, .SD, .SDcols = c("QTY", "PRICE", factor_vars, factors)])
  ]
  
  clean_name <- sub("^re.*qty_", "", file_name)   
  clean_name <- sub("\\.parquet$", "", clean_name) 
  
  new_col             <- paste0("fit_", clean_name)
  new_col_no_FE       <- paste0("fit_nfe_", clean_name)
  new_col_indispo     <- paste0("fit_indispo_", clean_name)
  new_col_no_FE_indispo <- paste0("fit_indispo_nfe_", clean_name)
  
  res_FE <- get_fitted_no_price(
    data_price   = price_indispo_merge_esp_npc,
    data_reg     = reg_data,
    factors      = factors, 
    instruments  = instruments_tot,
    FE = TRUE
  )
  
  price_indispo_merge_esp_npc[, (new_col) := res_FE$fitted]
  
  res <- get_fitted_no_price(
    data_price   = price_indispo_merge_esp_npc,
    data_reg     = reg_data,
    factors      = factors, 
    instruments  = instruments_tot,
    FE = FALSE
  )
  price_indispo_merge_esp_npc[, (new_col_no_FE) := res$fitted]
  
  # res_indispo_FE <- get_fitted_no_price(
  #   data_price   = price_indispo_merge_esp_npc,
  #   data_reg     = reg_data,
  #   factors      = factors_indispo, 
  #   instruments  = instruments_indispo,
  #   FE = TRUE
  # )
  # price_indispo_merge_esp_npc[, (new_col_indispo) := res_indispo_FE$fitted]
  # 
  # res_indispo <- get_fitted_no_price(
  #   data_price   = price_indispo_merge_esp_npc,
  #   data_reg     = reg_data,
  #   factors      = factors_indispo, 
  #   instruments  = instruments_indispo,
  #   FE = FALSE
  # )
  # price_indispo_merge_esp_npc[, (new_col_no_FE_indispo) := res_indispo$fitted]
  # 
  # -----------------------------
  # Tables de contributions
  contrib_table <- rbind(
    data.table(Name = clean_name, Type = "FE",    Variable = names(res_FE$contrib), Contribution = res_FE$contrib),
    data.table(Name = clean_name, Type = "No FE", Variable = names(res$contrib),    Contribution = res$contrib),
    fill = TRUE
  )
  
  # Tables de coefficients
  coef_table <- rbind(
    data.table(Name = clean_name, Type = "FE",    Variable = names(res_FE$coef), Coef = res_FE$coef),
    data.table(Name = clean_name, Type = "No FE", Variable = names(res$coef),    Coef = res$coef),
    fill = TRUE
  )
  
  rm(df); gc()
  
  # Retour propre
  list(
    contrib_table = contrib_table,
    coef_table    = coef_table,
    price_indispo = price_indispo_merge_esp_npc
  )
}

# results_1 <- lapply(files_to_load[1], process_file)
# rbindlist(lapply(results_1, `[[`, "coef_table"), fill = TRUE)

results <- lapply(files_to_load[c(1,3)], process_file)

contrib_table_tot <- rbindlist(lapply(results, `[[`, "contrib_table"), fill = TRUE)
coef_table_tot    <- rbindlist(lapply(results, `[[`, "coef_table"), fill = TRUE)

# si tu veux r�cup�rer le dernier price_indispo_merge_esp_npc modifi�
price_indispo_merge_esp_npc = results[[length(results)]]$price_indispo


# price_indispo_merge_esp_npc %>%
#   dplyr::select(fitted_select_supply_unmatched,fitted_select_supply_matched,
#   fitted_select_demand_unmatched,fitted_select_demand_matched) %>%
#   as.data.frame() %>%
#   cor(use = "complete.obs")

# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_supply <- grep("fit.*supply", names(price_indispo_merge_esp_npc), value = TRUE)

# Calculer la corrélation
table_cor_supply <- price_indispo_merge_esp_npc[, ..cols_fitted_supply] %>%
  as.data.frame() %>%
  cor(use = "complete.obs")


# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_demand <- grep("fit.*demand", names(price_indispo_merge_esp_npc), value = TRUE)

# Calculer la corrélation
table_cor_demand <- price_indispo_merge_esp_npc[, ..cols_fitted_demand] %>%
  as.data.frame() %>%
  cor(use = "complete.obs")


# Construire le chemin complet
file_out <- file.path(input_dir, "price_indispo_merge_esp_npc_no_cbf.parquet")

#file_out <- file.path(input_dir, "price_indispo_merge_esp_npc_no_cbf_YM.parquet")

# Sauvegarder
write_parquet(price_indispo_merge_esp_npc, file_out)
setwd(input_dir)
write.csv(contrib_table_tot, "contrib_table_tot.csv")
write.csv(coef_table_tot, "coef_table_tot.csv")


# library(data.table)
# 
# # Copier la table totale pour éviter de modifier l'originale
# dt <- copy(contrib_table_tot)
# 
# # Identifier les fixed effects
# fe_pattern <- "month|creneau|weekday|year"
# 
# # Somme des contributions des FE par Name et Type
# fe_summary <- dt[grep(fe_pattern, Variable), 
#                  .(Contribution = sum(Contribution)), 
#                  by = .(Name, Type)]
# fe_summary[, Variable := "FE"]  # renommer la variable en "FE"
# 
# # Contributions des autres variables (non-FE)
# other_vars <- dt[!grep(fe_pattern, Variable)]
# 
# # Ajouter les FE regroupés
# contrib_table_tot_grouped <- rbind(fe_summary, other_vars, fill = TRUE)
# 
# # Pour les types "no_FE" qui n'ont pas de FE, ajouter Contribution = 0
# # Identifier les combinaisons Name/Type qui n'ont pas de FE
# missing_FE <- setdiff(unique(dt[, .(Name, Type)]), fe_summary[, .(Name, Type)])
# if (nrow(missing_FE) > 0) {
#   contrib_table_tot_grouped <- rbind(
#     contrib_table_tot_grouped,
#     data.table(Name = missing_FE$Name, Type = missing_FE$Type, Variable = "FE", Contribution = 0),
#     fill = TRUE
#   )
# }
# 
# # Optionnel : trier
# setorder(contrib_table_tot_grouped, Name, Type, -Contribution)
# 
# colnames(contrib_table_tot_grouped)
# unique(contrib_table_tot_grouped %>% 
#          select(Name, Type))

ggplot(
  contrib_table_tot %>% 
    filter(Type == "No FE", Name == "supply_unmatched") %>% 
    mutate(Contribution = Contribution / sum(Contribution)),
  aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)
) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution (normalized)", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


ggplot(contrib_table_tot %>% filter(Type == "FE" & Name == "supply_unmatched"), aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution", title = paste("")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(contrib_table_tot %>% filter(Type == "No FE" & Name == "demand_unmatched"), aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution", title = paste("")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(contrib_table_tot %>% filter(Type == "FE" & Name == "demand_unmatched"), aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution", title = paste("")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


