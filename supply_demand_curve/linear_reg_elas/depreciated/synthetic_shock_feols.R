

price_indispo_merge_esp_npc <- price_indispo_merge_esp %>%
  mutate(
    month   = factor(month),
    year    = factor(year),
    creneau = factor(creneau),
    weekday = factor(weekday)
  ) %>% 
  filter(year <=2023 & year >= 2018)


controls_vec <- c("DATE", "Hour")
factors_feols <- setdiff(factors,  "year" )
factors_feols_indispo <-setdiff(factors_indispo,  "year" )
#instruments = instruments_tot
# Function to compute fitted values for a model without PRICE
get_fitted_no_price_feols <- function(data_price, data_reg, factors_feols = NULL, instruments=NULL, FE =TRUE){
  
  # Build RHS formula
  all_rhs_terms <- if(!FE) instruments else c(controls_vec , factors_feols)
  rhs_formula <- reformulate(c(all_rhs_terms, "PRICE"), response = "QTY") #reformulate(all_rhs_terms, response = "QTY")
  vars <- all.vars(rhs_formula)
  rhs_formula_build <- as.formula(paste("~", paste(instruments, collapse = " + ")))
  
  feols_model <- feols(fml = rhs_formula, data = data_reg)
  
  # Remove PRICE if it exists
  coefs_no_price <- coef(feols_model)[names(coef(feols_model)) %in% instruments]
  
  
  
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
  list(fitted = out, contrib = contrib)
}


contrib_table_tot_feols <<- NULL

# Variables supplémentaires (factors_feols) à inclure dans la régression
# Remplace 'factors_feols' par ton vecteur réel
file_name <-  "reconstruct_full_curve_shift_qty_supply_unmatched.parquet"
# Fonction pour traiter chaque fichier
process_file_feols <- function(file_name) {
  cat("Processing:", file_name, "\n")
  
  # Charger le fichier
  df <- read_parquet(file.path(input_dir, file_name))
  df <-  df[PRICE %in% price_grid_sparse]
  # Merge avec le prix
  reg_data <- merge(
    df,
    price_indispo_merge_esp_npc,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  )
  
  # Créer les termes factorisés
  #factor_terms <- paste0("as.factor(", controls_vec , ")")
  all_rhs_terms <- c(controls_vec , factors_feols)
  all_rhs_terms_indispo <- c(controls_vec , factors_feols_indispo)
  
  # Filtrer lignes complètes
  reg_data <- reg_data[complete.cases(reg_data[, .SD, .SDcols = c("QTY", "PRICE", controls_vec , factors_feols)])]
  # Supposons que la colonne s'appelle "year"
  reg_data[, year := as.numeric(year)]
  
  
  # # Régressions avec fixest
  # lm_price <- feols(fml = formula_price, data = reg_data)
  # lm_indispo <- feols(fml = formula_indispo, data = reg_data)
  # 
  # #Clustered errors
  # coefs_price <- summary(lm_price, vcov = ~DATE + creneau)
  # coefs_indispo <- summary(lm_indispo, vcov = ~DATE + creneau)
  
  # # Créer tables
  # coef_table_price <- data.table(
  #   variable = names(coefs_price$coefficients),
  #   estimate = coefs_price$coefficients,
  #   std_error = coefs_price$se
  # )
  # 
  # coef_table_indispo <- data.table(
  #   variable = names(coefs_indispo$coefficients),
  #   estimate = coefs_indispo$coefficients,
  #   std_error = coefs_indispo$se
  # )
  clean_name <- sub("^re.*qty_", "", file_name)   # enlève le début jusqu’à qty_
  clean_name <- sub("\\.parquet$", "", clean_name) # enlève l’extension
  
  # [1] "supply_matched"
  
  # # Sauvegarder
  # saveRDS(coef_table_price, file.path(output_dir, paste0("coef_with_price_", gsub(".parquet","", clean_name), ".rds")))
  # saveRDS(coef_table_indispo, file.path(output_dir, paste0("coef_nfyear_price_", gsub(".parquet","", clean_name), ".rds")))
  new_col <- paste0("fit_", clean_name)
  new_col_no_FE <- paste0("fit_nfe_", clean_name)
  new_col_indispo <- paste0("fit_indispo_", clean_name)
  new_col_no_FE_indispo <- paste0("fit_indispo_nfe_", clean_name)
  
  res_FE <- get_fitted_no_price_feols(
    data_price        = price_indispo_merge_esp_npc,
    data_reg = reg_data,
    factors_feols     = factors_feols, 
    instruments = instruments_tot,
    FE= TRUE)
  price_indispo_merge_esp_npc[, (new_col) := res_FE$fitted]
  
  res <- get_fitted_no_price_feols(
    data_price =  price_indispo_merge_esp_npc,
    data_reg = reg_data,
    factors_feols     = factors_feols, 
    instruments = instruments_tot,
    FE= FALSE)
  price_indispo_merge_esp_npc[, (new_col_no_FE) := res$fitted]
  
  
  
  # Résultat avec instruments
  res_indispo_FE <- get_fitted_no_price_feols(
    data_price =  price_indispo_merge_esp_npc,
    data_reg = reg_data,
    factors_feols     = factors_feols_indispo, 
    instruments = instruments_indispo,
    FE= TRUE
  )
  price_indispo_merge_esp_npc[, (new_col_indispo) := res_indispo_FE$fitted]
  
  # Résultat sans instruments
  res_indispo <- get_fitted_no_price_feols(
    data_price =  price_indispo_merge_esp_npc,
    data_reg = reg_data,
    factors_feols     = factors_feols_indispo, 
    instruments = instruments_indispo,
    FE= FALSE
  )
  price_indispo_merge_esp_npc[, (new_col_no_FE_indispo) := res_indispo$fitted]
  
  # Construire les tables de contributions
  contrib_table_select <- data.table(
    Name = clean_name,
    Type = "FE",
    Variable = names( res_FE$contrib),
    Contribution =  res_FE$contrib
  )
  
  contrib_table_noFE <- data.table(
    Name = clean_name,
    Type = "No FE",
    Variable = names(res$contrib),
    Contribution = res$contrib
  )
  
  # Combiner les deux
  # Combiner les deux et assigner globalement
  contrib_table_tot <<- rbind(contrib_table_tot, contrib_table_select, fill = TRUE)
  contrib_table_tot <<- rbind(contrib_table_tot, contrib_table_noFE, fill = TRUE)
  
  
  
  # colnames(price_indispo_merge_esp_npc)
  # price_indispo_merge_esp_npc %>%
  #   dplyr::select(fitted, fitted_select) %>% #, fitted_indispo, fitted_indispo_select) %>%
  #   as.data.frame() %>%
  #   cor(use = "complete.obs")
  # price_indispo_merge_esp_npc %>%
  #   dplyr::select(fitted, fitted_select_supply_matched, fitted_indispo, fitted_indispo_select) %>%
  #   as.data.frame() %>%
  #   cor(use = "complete.obs")
  # 
  #les 4 sont corrélés à 98 % au minimum,
  #on garde le truc de base
  
  
  rm(df)
  gc()
}

# Boucle sur tous les fichiers
lapply(files_to_load, process_file_feols)




# Construire le chemin complet
file_out <- file.path(input_dir, "price_indispo_merge_esp_npc_date_FE.parquet")

# Sauvegarder
write_parquet(price_indispo_merge_esp_npc, file_out)



setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# Sauvegarder
price_indispo_merge_esp_npc_date_FE <- read_parquet("price_indispo_merge_esp_npc_date_FE.parquet")

price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_nfe.parquet")


# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_supply <- grep("fit.*supply", names(price_indispo_merge_esp_npc_date_FE), value = TRUE)
cols_fitted_demand <- grep("fit.*demand", names(price_indispo_merge_esp_npc_date_FE), value = TRUE)

# keep only "unmatched"
cols_fitted_supply <- grep("unmatched", cols_fitted_supply, value = TRUE)
cols_fitted_demand <- grep("unmatched", cols_fitted_demand, value = TRUE)

# drop "indispo"
cols_fitted_supply <- grep("indispo", cols_fitted_supply, invert = TRUE, value = TRUE)
cols_fitted_demand <- grep("indispo", cols_fitted_demand, invert = TRUE, value = TRUE)


price_data <- merge(price_indispo_merge_esp_npc_date_FE %>%
        select(datetime, all_of(c(cols_fitted_supply, cols_fitted_demand))) %>%
        rename_with(~ paste0(.x, "_date_FE"), all_of(c(cols_fitted_supply, cols_fitted_demand)))
      ,
      price_indispo_merge_esp_npc%>% 
        select(datetime,
               cols_fitted_supply , cols_fitted_demand   ),
      by = "datetime")
colnames(price_data)


price_data %>% 
  dplyr::select(-datetime) %>% 
  select(c(paste0(cols_fitted_supply, "_date_FE"),cols_fitted_supply)) %>% 
  cor(use = "complete.obs")
price_data %>% 
  dplyr::select(-datetime) %>% 
  select(c(paste0(cols_fitted_demand, "_date_FE"),cols_fitted_demand)) %>% 
  cor(use = "complete.obs")
View(price_data %>% 
  dplyr::select(-datetime))










price_indispo_merge_compare <- merge(price_indispo_merge_esp_npc_date_FE %>% 
                                       select(datetime,
                                              cols_fitted_supply , cols_fitted_demand   ),
                                     price_indispo_merge_esp_delta
)



price_indispo_merge_delta_clean <- price_indispo_merge_compare %>%
  dplyr::select(-log_price_diff, -price_diff) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.)) %>%           # garde seulement les lignes sans NA dans ces colonnes
  distinct()

dep_var <- "log(price+1)"
endog_vars <- c("log(gen_tot_da)")
#endog_vars <- c("log(gen_tot_da)")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
extended_controls <- c(controls_vars, "unpredicted_renewables",  "intraday_update_renewables" )


# Orthogonaliser delta_s_total par rapport ? delta_d_total
price_indispo_merge_delta_clean$delta_s_orth <- resid(
  lm(delta_s_total ~ delta_d_total, data = price_indispo_merge_delta_clean)
)

# Orthogonaliser delta_d_total par rapport ? delta_s_total
price_indispo_merge_delta_clean$delta_d_orth <- resid(
  lm(delta_d_total ~ delta_s_total, data = price_indispo_merge_delta_clean)
)


# Sélectionner toutes les colonnes fit
cols_fitted <- grep("^fit_", names(price_indispo_merge_delta_clean), value = TRUE)

for(col in cols_fitted){
  if(grepl("supply", col)){
    # Pour les colonnes supply, régresser sur la colonne demand correspondante
    target_col <- sub("supply", "demand", col)
  } else if(grepl("demand", col)){
    # Pour les colonnes demand, régresser sur la colonne supply correspondante
    target_col <- sub("demand", "supply", col)
  } else {
    next  # ignorer si ce n’est ni supply ni demand
  }
  
  # Nouveau nom pour la colonne orthogonalisée
  new_col <- paste0(col, "_orth")
  
  # Vérifier que la colonne cible existe pour éviter les erreurs
  if(target_col %in% names(price_indispo_merge_delta_clean)){
    price_indispo_merge_delta_clean[, (new_col) := resid(
      lm(as.formula(paste(col, "~", target_col)), data = price_indispo_merge_delta_clean)
    )]
  }
}

cols_fitted_bis <- setdiff(grep("^fit_", names(price_indispo_merge_delta_clean), value = TRUE), cols_fitted)



library(AER)
#controls_vec <- c("year", "month", "weekday", "creneau")
controls_vec <- c("delivery_date", "creneau")
controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))

ols_formula <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_vec), collapse = " + "))))  

m_ols <- ivreg(ols_formula , data = price_indispo_merge_delta_clean)
summary(m_ols)
# nw_se_ols  <- NeweyWest(m_ols , lag = 24, prewhite = FALSE, adjust = TRUE)
# # Extraire les r?sultats des coefficients avec SE NW
# nw_ols  <- coeftest(m_ols , vcov. = nw_se_ols )
# 
# 


# Colonnes orthogonalisées de supply et demand
cols_fitted_supply_orth <- grep("fit.*supply.*_orth$", names(price_indispo_merge_delta_clean), value = TRUE)
cols_fitted_demand_orth <- grep("fit.*demand.*_orth$", names(price_indispo_merge_delta_clean), value = TRUE)



price_indispo_merge_delta_clean %>% 
  select(c("delta_supply_orth",cols_fitted_supply_orth)) %>% 
  cor(use = "complete.obs")

price_indispo_merge_delta_clean %>% 
  select(c("delta_demand_orth",cols_fitted_demand_orth)) %>% 
  cor(use = "complete.obs")


price_indispo_merge_delta_clean <- price_indispo_merge_delta_clean %>% 
  rename(delta_supply_orth = delta_s_orth,
         delta_demand_orth= delta_d_orth)


price_indispo_merge_feols<- price_indispo_merge_delta_clean %>% 
  mutate(creneau = factor(creneau),
         delivery_date = factor(delivery_date)
  )
# Créer une liste pour stocker les résultats
iv_results_feols <- list()
s_col  <- "delta_supply_orth"
d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c("delta_supply_orth",cols_fitted_supply_orth)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  if(!d_col %in% names(price_indispo_merge_delta_clean)) next

  
  
  rhs_vars <- paste( paste(controls_vec, collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "log(gen_tot_da) ~ ", rhs_vars, " | log(price + 1) ~ ", instr_vars_s
    )
  )
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "log(gen_tot_da) ~ ", rhs_vars, " | log(price + 1) ~ ", instr_vars_d
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_feols)
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_feols)
  

  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "wald")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "wald")
  
  
  # Stocker dans la liste
  iv_results_feols[[suffix]] <- list(
    supply = list(model = iv_supply, #nw = nw_s, 
                  fstat = fstat_s$wald$stat),
    demand = list(model =iv_demand, #nw = nw_d, 
                  fstat = fstat_d$wald$stat)
  )
}



# Préparer les modèles à mettre dans le tableau
all_models_supply_feols <- list()
all_models_demand_feols <- list()

F_stat_supply_feols <- c()
F_stat_demand_feols <- c()
for(suffix in names(iv_results_feols)){
  
  # Supply
  all_models_supply_feols <- c(all_models_supply_feols, list(iv_results_feols[[suffix]]$supply$model))
  #column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply_feols  <- c(F_stat_supply_feols , round(iv_results_feols[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand_feols <- c(all_models_demand_feols, list(iv_results_feols[[suffix]]$demand$model))
  #column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand_feols <- c(F_stat_demand_feols, round(iv_results_feols[[suffix]]$demand$fstat, 2))
}



# Extraire le coefficient spécifique de chaque modèle
# coef_price_supply_feols <- sapply(all_models_supply_feols, function(mod) {
#   summary(mod)$se["fit_log(price + 1)", "Std. Error"]
# })

# Supply
coef_supply_feols <- sapply(all_models_supply_feols, function(mod) {
c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})

# Demand
coef_demand_feols <- sapply(all_models_demand_feols, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})


coef_supply_feols
coef_demand_feols





iv_results_feols_lag_12 <- list()
s_col  <- "fitted_select_nfesupply_matched_orth"
d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c("delta_supply_orth",cols_fitted_supply_orth)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  
  #controls_vec <- c("year", "month", "weekday", "creneau")
  controls_vec <- c("delivery_date", "creneau")
  controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))
  
  lag_exog_s <- paste0("lag(", s_col, ", ", 1:12, ")")
  lag_exog_d <- paste0("lag(", d_col, ", ", 1:12, ")")
  
  
  rhs_vars_s <- paste( paste(c(controls_vec,lag_exog_s), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec,lag_exog_d), collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "log(gen_tot_da) ~ ", rhs_vars_s, " | log(price + 1) ~ ", instr_vars_s
    )
  )
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "log(gen_tot_da) ~ ", rhs_vars_d, " | log(price + 1) ~ ", instr_vars_d
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_feols)
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_feols)
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "wald")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "wald")
  
  
  # Stocker dans la liste
  iv_results_feols_lag_12 [[suffix]] <- list(
    supply = list(model = iv_supply, #nw = nw_s, 
                  fstat = fstat_s$wald$stat),
    demand = list(model =iv_demand, #nw = nw_d, 
                  fstat = fstat_d$wald$stat)
  )
}



# Préparer les modèles à mettre dans le tableau
all_models_supply_feols_lag_12 <- list()
all_models_demand_feols_lag_12 <- list()

F_stat_supply_feols_lag_12 <- c()
F_stat_demand_feols_lag_12 <- c()
for(suffix in names(iv_results_feols_lag_12)){
  
  # Supply
  all_models_supply_feols_lag_12 <- c(all_models_supply_feols_lag_12, list(iv_results_feols_lag_12[[suffix]]$supply$model))
  #column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply_feols_lag_12  <- c(F_stat_supply_feols_lag_12 , round(iv_results_feols_lag_12[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand_feols_lag_12 <- c(all_models_demand_feols_lag_12, list(iv_results_feols_lag_12[[suffix]]$demand$model))
  #column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand_feols_lag_12 <- c(F_stat_demand_feols_lag_12, round(iv_results_feols_lag_12[[suffix]]$demand$fstat, 2))
}




# Extraire le coefficient spécifique de chaque modèle


coef_supply_feols_lag_12 <- sapply(all_models_supply_feols_lag_12, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})

# Demand
coef_demand_feols_lag_12 <- sapply(all_models_demand_feols, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})
coef_supply_feols_lag_12
coef_demand_feols_lag_12

coef_table_feols <- data.frame(
  Model = rep(c("Delta","Unmatched_FE", "Unmatched"), 4),
  Coef_log_price = c(coef_supply_feols[1,], coef_demand_feols[1,], coef_supply_feols_lag_12[1,], coef_demand_feols_lag_12[1,]),
  Std_Error      = c(coef_supply_feols[2,],   coef_demand_feols[2,],   coef_supply_feols_lag_12[2,],   coef_demand_feols_lag_12[2,]),
  Type = rep(c("Supply","Demand","Supply","Demand"), each = 3),
  FE   = "DH",
  Lag  = rep(c("0","0","12","12"), each = 3)
)


getwd()
write.csv(coef_table_feols, "coef_table_feols.csv")

coef_table_wide_feols <- coef_table_feols %>%
  filter(Model != "Unmatched") %>% 
  mutate(
    coef_se = sprintf("%.3f (%.3f)", Coef_log_price, Std_Error),
    Lag = paste0("Lag", Lag)  # pour nommer clairement les colonnes
  ) %>%
  select(Type, Lag, Model, coef_se) %>%
  pivot_wider(
    names_from = c(Lag, Model),
    values_from = coef_se
  )


gt_table_feols <- coef_table_wide_feols %>%
  gt(rowname_col = "Type") %>%
  tab_header(
    title = "Coefficient Estimates",
    subtitle = "Delta, Unmatched FE, and Unmatched for Lag 0 and Lag 12"
  ) %>%
  # Labels propres
  cols_label(
    Lag0_Delta = "Delta",
    Lag0_Unmatched_FE = "Unmatched FE",
    #Lag0_Unmatched = "Unmatched",
    Lag12_Delta = "Delta",
    Lag12_Unmatched_FE = "Unmatched FE",
    #Lag12_Unmatched = "Unmatched"
  ) %>%
  # Spanners (groupes de colonnes)
  tab_spanner(
    label = "Lag 0",
    columns = c(Lag0_Delta, Lag0_Unmatched_FE)#, Lag0_Unmatched)
  ) %>%
  tab_spanner(
    label = "Lag 12",
    columns = c(Lag12_Delta, Lag12_Unmatched_FE)#, Lag12_Unmatched)
  ) %>%
  # Mise en forme
  opt_table_outline() %>%
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = cells_column_labels(columns = everything())
  )
gt_table_feols
