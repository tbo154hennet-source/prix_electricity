


setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# Sauvegarder
price_indispo_merge_esp_npc_YM <- read_parquet("price_indispo_merge_esp_npc_no_cbf_YM.parquet")

price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_no_CBF.parquet")


# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_supply <- grep("fit.*supply", names(price_indispo_merge_esp_npc_YM), value = TRUE)
cols_fitted_demand <- grep("fit.*demand", names(price_indispo_merge_esp_npc_YM), value = TRUE)

# keep only "unmatched"
cols_fitted_supply <- grep("unmatched", cols_fitted_supply, value = TRUE)
cols_fitted_demand <- grep("unmatched", cols_fitted_demand, value = TRUE)

# drop "indispo"
cols_fitted_supply <- grep("indispo", cols_fitted_supply, invert = TRUE, value = TRUE)
cols_fitted_demand <- grep("indispo", cols_fitted_demand, invert = TRUE, value = TRUE)


price_data <- merge(price_indispo_merge_esp_npc_YM %>%
                      select(datetime, all_of(c(cols_fitted_supply, cols_fitted_demand))) %>%
                      rename_with(~ paste0(.x, "_YM"), all_of(c(cols_fitted_supply, cols_fitted_demand)))
                    ,
                    price_indispo_merge_esp_npc%>% 
                      select(datetime,
                             cols_fitted_supply , cols_fitted_demand   ),
                    by = "datetime")
colnames(price_data)


price_data %>% 
  dplyr::select(-datetime) %>% 
  select(c(paste0(cols_fitted_supply, "_YM"),cols_fitted_supply)) %>% 
  cor(use = "complete.obs")
price_data %>% 
  dplyr::select(-datetime) %>% 
  select(c(paste0(cols_fitted_demand, "_YM"),cols_fitted_demand)) %>% 
  cor(use = "complete.obs")




price_indispo_merge_compare <- merge(price_indispo_merge_esp_npc_YM %>% 
                                       select(datetime, year_month,
                                              cols_fitted_supply , cols_fitted_demand   ),
                                     price_indispo_merge_esp_delta
)


# Orthogonaliser delta_s_total par rapport ? delta_d_total
price_indispo_merge_compare$delta_s_orth <- resid(
  lm(delta_s_total ~ delta_d_total, data = price_indispo_merge_compare)
)

# Orthogonaliser delta_d_total par rapport ? delta_s_total
price_indispo_merge_compare$delta_d_orth <- resid(
  lm(delta_d_total ~ delta_s_total, data = price_indispo_merge_compare)
)

price_indispo_merge_feols_YM <- price_indispo_merge_compare %>%
  dplyr::select(-log_price_diff, -price_diff) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.)) %>%           # garde seulement les lignes sans NA dans ces colonnes
  distinct()%>% 
  mutate(
    year = factor(year),
    month = factor(month),
    creneau = factor(creneau)
  ) %>% 
  rename(delta_supply_orth = delta_s_orth,
         delta_demand_orth =delta_d_orth)


# Sélectionner toutes les colonnes fit
cols_fitted <- grep("^fit_", names(price_indispo_merge_feols_YM), value = TRUE)

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
  if(target_col %in% names(price_indispo_merge_feols_YM)){
    price_indispo_merge_feols_YM[, (new_col) := resid(
      lm(as.formula(paste(col, "~", target_col)), data = price_indispo_merge_feols_YM)
    )]
  }
}

cols_fitted_bis <- setdiff(grep("^fit_", names(price_indispo_merge_feols_YM), value = TRUE), cols_fitted)



controls_vec <- c("year_month", "creneau", "spot_gas")


# Créer une liste pour stocker les résultats
iv_results_feols_YM <- list()
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
  if(!d_col %in% names(price_indispo_merge_feols_YM)) next
  
  
  
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
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_feols_YM)
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_feols_YM)
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  
  # Stocker dans la liste
  iv_results_feols_YM[[suffix]] <- list(
    supply = list(model = iv_supply, #nw = nw_s, 
                  fstat = fstat_s$ivf1$stat),
    demand = list(model =iv_demand, #nw = nw_d, 
                  fstat = fstat_d$ivf1$stat)
  )
}


# Préparer les modèles à mettre dans le tableau
all_models_supply_feols_YM <- list()
all_models_demand_feols_YM <- list()

F_stat_supply_feols_YM <- c()
F_stat_demand_feols_YM <- c()
suffix <- "fit_supply_unmatched"
for(suffix in names(iv_results_feols_YM)){
  
  # Supply
  all_models_supply_feols_YM <- c(all_models_supply_feols_YM, list(iv_results_feols_YM[[suffix]]$supply$model))
  #column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply_feols_YM  <- c(F_stat_supply_feols_YM , round(iv_results_feols_YM[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand_feols_YM <- c(all_models_demand_feols_YM, list(iv_results_feols_YM[[suffix]]$demand$model))
  #column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand_feols_YM <- c(F_stat_demand_feols_YM, round(iv_results_feols_YM[[suffix]]$demand$fstat, 2))
}



# Extraire le coefficient spécifique de chaque modèle
# coef_price_supply_feols_YM <- sapply(all_models_supply_feols_YM, function(mod) {
#   summary(mod)$se["fit_log(price + 1)", "Std. Error"]
# })

# Supply
coef_supply_feols_YM <- sapply(all_models_supply_feols_YM, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})

# Demand
coef_demand_feols_YM <- sapply(all_models_demand_feols_YM, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})


coef_supply_feols_YM
coef_demand_feols_YM

# List of columns you want to lag
cols_to_lag <- c("delta_supply_orth", cols_fitted_supply_orth,
                 "delta_demand_orth", cols_fitted_demand_orth
)

# price_indispo_merge_feols_YM <- price_indispo_merge_feols_YM %>%
#   group_by(unit_id) %>%  # or your panel id
#   mutate(instr_lag1 = dplyr::lag(!!sym(instr_vars_s), 1)) %>%
#   ungroup()



price_indispo_merge_feols_YM <- price_indispo_merge_feols_YM %>%
  mutate(across(all_of(cols_to_lag), ~ lag(.), .names = "{.col}_lag"))

price_indispo_merge_feols_YM %>% 
  select(fit_supply_unmatched_orth, fit_supply_unmatched_orth_lag,
         fit_demand_unmatched_orth, fit_demand_unmatched_orth_lag) %>% 
  cor(use = "complete.obs")


iv_results_feols_YM_lag <- list()
s_col  <- "fit_supply_unmatched_orth"
# d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c("delta_supply_orth",cols_fitted_supply_orth)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  if(!d_col %in% names(price_indispo_merge_feols_YM)) next
  
  
  #controls_vec <- c("year", "month", "weekday", "creneau")
  # controls_vec <- c("delivery_date", "creneau")
  # controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))
  # 
  # lag_exog_s <- paste0( s_col, "_lag")
  # lag_exog_d <- paste0(d_col, "_lag")
  rhs_vars_s <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  # Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
  inst_name <- instr_vars_s
  
  # Generate 12 lags
  price_indispo_merge_feols_YM <- price_indispo_merge_feols_YM %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1),
        lag2 = ~dplyr::lag(.x, 2),
        lag3 = ~dplyr::lag(.x, 3),
        lag4 = ~dplyr::lag(.x, 4),
        lag5 = ~dplyr::lag(.x, 5),
        lag6 = ~dplyr::lag(.x, 6),
        lag7 = ~dplyr::lag(.x, 7),
        lag8 = ~dplyr::lag(.x, 8),
        lag9 = ~dplyr::lag(.x, 9),
        lag10 = ~dplyr::lag(.x, 10),
        lag11 = ~dplyr::lag(.x, 11),
        lag12 = ~dplyr::lag(.x, 12)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(inst_name, "_lag", 1, collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "log(gen_tot_da) ~ ", rhs_vars_s, " + ", lag_controls,
      " | log(price + 1) ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_feols_YM)
  
  inst_name <- instr_vars_d
  
  # Generate 12 lags
  price_indispo_merge_feols_YM <- price_indispo_merge_feols_YM %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1),
        lag2 = ~dplyr::lag(.x, 2),
        lag3 = ~dplyr::lag(.x, 3),
        lag4 = ~dplyr::lag(.x, 4),
        lag5 = ~dplyr::lag(.x, 5),
        lag6 = ~dplyr::lag(.x, 6),
        lag7 = ~dplyr::lag(.x, 7),
        lag8 = ~dplyr::lag(.x, 8),
        lag9 = ~dplyr::lag(.x, 9),
        lag10 = ~dplyr::lag(.x, 10),
        lag11 = ~dplyr::lag(.x, 11),
        lag12 = ~dplyr::lag(.x, 12)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(inst_name, "_lag", 1, collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "log(gen_tot_da) ~ ", rhs_vars_s, " + ", lag_controls,
      " | log(price + 1) ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_feols_YM)
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  # Stocker dans la liste
  iv_results_feols_YM_lag [[suffix]] <- list(
    supply = list(model = iv_supply, #nw = nw_s, 
                  fstat = fstat_s$ivf1$stat),
    demand = list(model =iv_demand, #nw = nw_d, 
                  fstat = fstat_d$ivf1$stat)
  )
}


# Préparer les modèles à mettre dans le tableau
all_models_supply_feols_YM_lag <- list()
all_models_demand_feols_YM_lag <- list()

F_stat_supply_feols_YM_lag <- c()
F_stat_demand_feols_YM_lag <- c()
suffix <- "fit_supply_unmatched"
for(suffix in names(iv_results_feols_YM_lag)){
  
  # Supply
  all_models_supply_feols_YM_lag <- c(all_models_supply_feols_YM_lag, list(iv_results_feols_YM_lag[[suffix]]$supply$model))
  #column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply_feols_YM_lag  <- c(F_stat_supply_feols_YM_lag , round(iv_results_feols_YM_lag[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand_feols_YM_lag <- c(all_models_demand_feols_YM_lag, list(iv_results_feols_YM_lag[[suffix]]$demand$model))
  #column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand_feols_YM_lag <- c(F_stat_demand_feols_YM_lag, round(iv_results_feols_YM_lag[[suffix]]$demand$fstat, 2))
}
F_stat_supply_feols_YM
F_stat_demand_feols_YM
F_stat_supply_feols_YM_lag
F_stat_demand_feols_YM_lag

# Extraire le coefficient spécifique de chaque modèle


coef_supply_feols_YM_lag <- sapply(all_models_supply_feols_YM_lag, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})

# Demand
coef_demand_feols_YM_lag <- sapply(all_models_demand_feols_YM_lag, function(mod) {
  c(coef = summary(mod)$coefficients["fit_log(price + 1)"], se = summary(mod)$se["fit_log(price + 1)"])
})
coef_supply_feols_YM_lag
coef_demand_feols_YM_lag

coef_table_feols_YM <- data.frame(
  Model = rep(c("Delta","Unmatched_FE", "Unmatched"), 4),
  Coef_log_price = c(coef_supply_feols_YM[1,], coef_demand_feols_YM[1,], coef_supply_feols_YM_lag[1,], coef_demand_feols_YM_lag[1,]),
  Std_Error      = c(coef_supply_feols_YM[2,],   coef_demand_feols_YM[2,],   coef_supply_feols_YM_lag[2,],   coef_demand_feols_YM_lag[2,]),
  Type = rep(c("Supply","Demand","Supply","Demand"), each = 3),
  FE   = "YM_WH",
  Lag  = rep(c("0","0","1","1"), each = 3)
)
coef_table_feols_YM 

getwd()
write.csv(coef_table_feols_YM, "coef_table_feols_YM.csv")

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
coef_table_feols_YM <- read.csv( "coef_table_feols_YM.csv")

coef_table_wide_feols_YM <- coef_table_feols_YM %>%
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


gt_table_feols_YM <- coef_table_wide_feols_YM %>%
  gt(rowname_col = "Type") %>%
  tab_header(
    title = "Coefficient Estimates",
    subtitle = "Delta, Unmatched FE, and Unmatched for Lag 0 and Lag 12"
  ) %>%
  # Labels propres
  cols_label(
    Lag0_Delta = "(1)",
    Lag0_Unmatched_FE = "(2)",
    #Lag0_Unmatched = "Unmatched",
    Lag1_Delta = "(1)",
    Lag1_Unmatched_FE = "(2)",
    #Lag12_Unmatched = "Unmatched"
  ) %>%
  # Spanners (groupes de colonnes)
  tab_spanner(
    label = "Baseline",
    columns = c(Lag0_Delta, Lag0_Unmatched_FE)#, Lag0_Unmatched)
  ) %>%
  tab_spanner(
    label = "Lag",
    columns = c(Lag1_Delta, Lag1_Unmatched_FE)#, Lag12_Unmatched)
  ) %>%
  # Mise en forme
  opt_table_outline() %>%
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = cells_column_labels(columns = everything())
  )
gt_table_feols_YM


coef_table_wide_tot <- rbind(coef_table_wide_feols_YMWH, 
                             coef_table_wide_feols_YM)
gt_table_feols_tot <- coef_table_wide_tot %>%
  gt(rowname_col = "Type") %>%
  tab_header(
    title = "Supply and Demand Elasticities",
    subtitle = ""
  ) %>%
  # Labels propres
  cols_label(
    Lag0_Delta = "(1)",
    Lag0_Unmatched_FE = "(2)",
    #Lag0_Unmatched = "Unmatched",
    Lag1_Delta = "(1)",
    Lag1_Unmatched_FE = "(2)",
    #Lag12_Unmatched = "Unmatched"
  ) %>%
  # Row groups manuels par indices
  tab_row_group(
    label = "FE: Year, Month, Weekday, Hour",
    rows = 1:2   # indices des lignes pour le premier bloc
  ) %>%
  tab_row_group(
    label = "FE: Year*Month, Weekday, Hour",
    rows = 3:4   # indices des lignes pour le deuxi?me bloc
  ) %>%
  # Spanners (groupes de colonnes)
  tab_spanner(
    label = "Baseline",
    columns = c(Lag0_Delta, Lag0_Unmatched_FE)#, Lag0_Unmatched)
  ) %>%
  tab_spanner(
    label = "Lag",
    columns = c(Lag1_Delta, Lag1_Unmatched_FE)#, Lag12_Unmatched)
  ) %>%
  # Mise en forme
  opt_table_outline() %>%
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = cells_column_labels(columns = everything())
  )
gt_table_feols_tot


