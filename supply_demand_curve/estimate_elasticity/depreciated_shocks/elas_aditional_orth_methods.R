

################ OPTION 1 lagger aussi le control

iv_results_feols_all_lag_bis <- list()
s_col  <- "shock_supply_unmatched_orth"
# d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_npc)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  #if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  
  #controls_vec <- c("year", "month", "weekday", "creneau")
  # controls_vec <- c("delivery_date", "creneau")
  # controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))
  # 
  # lag_exog_s <- paste0( s_col, "_lag")
  # lag_exog_d <- paste0(d_col, "_lag")
  rhs_vars_s <- paste( paste(c(controls_vec, d_col), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec, s_col), collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  # Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
  inst_name <- instr_vars_s
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name,d_col), "_lag", 1, collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "log(load_tot) ~ ", rhs_vars_s, " + ", lag_controls,
      " | log(price + 1) ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  
  inst_name <- instr_vars_d
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name,s_col), "_lag", 1, collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "log(load_tot) ~ ", rhs_vars_d, " + ", lag_controls,
      " | log(price + 1) ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  "datetime" %in% colnames(price_indispo_merge_esp_npc)
  class(price_indispo_merge_esp_npc$datetime)
  
  
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  # Stocker dans la liste
  # iv_results_feols_all_lag_bis[[suffix]] <- list(
  #   supply = list(model = iv_supply, #nw = nw_s, 
  #                 fstat = fstat_s$ivf1$stat),
  #   demand = list(model =iv_demand, #nw = nw_d, 
  #                 fstat = fstat_d$ivf1$stat)
  # )
  iv_results_feols_all_lag_bis[[suffix]] <- list(
    supply  = iv_supply, 
    demand =iv_demand)
}

################# OPTION2 orthogonaliser

iv_results_feols_all_lag_ter <- list()

s_col  <- cols_fitted_supply_npc[1]
# d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_npc)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  
  # Orthogonaliser s_col par rapport à d_col
  price_indispo_merge_esp_npc[[paste0(s_col, "_orth")]] <- resid(
    lm(formula = as.formula(paste(s_col, "~", d_col)),
       data = price_indispo_merge_esp_npc,
       na.action = na.exclude)
  )
  
  # Orthogonaliser d_col par rapport à s_col
  price_indispo_merge_esp_npc[[paste0(d_col, "_orth")]] <- resid(
    lm(formula = as.formula(paste(d_col, "~", s_col)),
       data = price_indispo_merge_esp_npc,
       na.action = na.exclude)
  )

  # Vérifier que la colonne existe
  #if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  
  #controls_vec <- c("year", "month", "weekday", "creneau")
  # controls_vec <- c("delivery_date", "creneau")
  # controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))
  # 
  # lag_exog_s <- paste0( s_col, "_lag")
  # lag_exog_d <- paste0(d_col, "_lag")
  rhs_vars_s <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  instr_vars_s <- paste0(s_col, "_orth")
  instr_vars_d <- paste0(d_col, "_orth")
  
  # Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
  inst_name <- instr_vars_s
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name), "_lag", 1, collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "log(load_tot) ~ ", rhs_vars_s, " + ", lag_controls,
      " | log(price + 1) ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  
  inst_name <- instr_vars_d
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name), "_lag", 1, collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "log(load_tot) ~ ", rhs_vars_d, " + ", lag_controls,
      " | log(price + 1) ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

  

  
  # Stocker dans la liste
  # iv_results_feols_all_lag_ter[[suffix]] <- list(
  #   supply = list(model = iv_supply, #nw = nw_s, 
  #                 fstat = fstat_s$ivf1$stat),
  #   demand = list(model =iv_demand, #nw = nw_d, 
  #                 fstat = fstat_d$ivf1$stat)
  # )
  iv_results_feols_all_lag_ter[[suffix]] <- list(
    supply  = iv_supply, 
    demand =iv_demand)
}

names(iv_results_feols_all_lag)
names(iv_results_feols_all_lag_ter)

iv_results_feols_comparable <- iv_results_feols_all_lag["fit_supply_unmatched"]
iv_results_feols_comparable_bis <- iv_results_feols_all_lag_bis["fit_supply_unmatched"]
iv_results_feols_comparable_ter <- iv_results_feols_all_lag_ter["fit_supply_unmatched"]

# Aplatir tous les modèles dans une seule liste
all_models_comparable <- unlist(
  lapply(c(iv_results_feols_comparable, iv_results_feols_comparable_bis, iv_results_feols_comparable_ter), function(x) x),
  recursive = FALSE
)

etable(all_models_comparable,
       keep = c("price"),
       #headers = c("Supply Unmatched","Demand Unmatched", "Supply Matched", "Demand Matched"),
       fitstat = ~ ivf.stat +  r2+ n)





#####################

######### Linear price response






################ OPTION 1 lagger aussi le control

iv_results_feols_all_lin_lag_bis <- list()
s_col  <- "shock_supply_unmatched_orth"
# d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_npc)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  #if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  
  #controls_vec <- c("year", "month", "weekday", "creneau")
  # controls_vec <- c("delivery_date", "creneau")
  # controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))
  # 
  # lag_exog_s <- paste0( s_col, "_lag")
  # lag_exog_d <- paste0(d_col, "_lag")
  rhs_vars_s <- paste( paste(c(controls_vec, d_col), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec, s_col), collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  # Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
  inst_name <- instr_vars_s
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name,d_col), "_lag", 1, collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "load_tot ~ ", rhs_vars_s, " + ", lag_controls,
      " | price ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  
  inst_name <- instr_vars_d
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name,s_col), "_lag", 1, collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "load_tot ~ ", rhs_vars_d, " + ", lag_controls,
      " | price ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  "datetime" %in% colnames(price_indispo_merge_esp_npc)
  class(price_indispo_merge_esp_npc$datetime)
  
  
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  # Stocker dans la liste
  # iv_results_feols_all_lin_lag_bis[[suffix]] <- list(
  #   supply = list(model = iv_supply, #nw = nw_s, 
  #                 fstat = fstat_s$ivf1$stat),
  #   demand = list(model =iv_demand, #nw = nw_d, 
  #                 fstat = fstat_d$ivf1$stat)
  # )
  iv_results_feols_all_lin_lag_bis[[suffix]] <- list(
    supply  = iv_supply, 
    demand =iv_demand)
}

################# OPTION2 orthogonaliser

iv_results_feols_all_lin_lag_ter <- list()

s_col  <- cols_fitted_supply_npc[1]
# d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_npc)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  
  # Orthogonaliser s_col par rapport à d_col
  price_indispo_merge_esp_npc[[paste0(s_col, "_orth")]] <- resid(
    lm(formula = as.formula(paste(s_col, "~", d_col)),
       data = price_indispo_merge_esp_npc,
       na.action = na.exclude)
  )
  
  # Orthogonaliser d_col par rapport à s_col
  price_indispo_merge_esp_npc[[paste0(d_col, "_orth")]] <- resid(
    lm(formula = as.formula(paste(d_col, "~", s_col)),
       data = price_indispo_merge_esp_npc,
       na.action = na.exclude)
  )
  
  # Vérifier que la colonne existe
  #if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  
  #controls_vec <- c("year", "month", "weekday", "creneau")
  # controls_vec <- c("delivery_date", "creneau")
  # controls_vec <- c("spot_gas",paste0( "factor(", controls_vec, ")"))
  # 
  # lag_exog_s <- paste0( s_col, "_lag")
  # lag_exog_d <- paste0(d_col, "_lag")
  rhs_vars_s <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  instr_vars_s <- paste0(s_col, "_orth")
  instr_vars_d <- paste0(d_col, "_orth")
  
  # Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
  inst_name <- instr_vars_s
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name), "_lag", 1, collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "load_tot ~ ", rhs_vars_s, " + ", lag_controls,
      " | price ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  
  inst_name <- instr_vars_d
  
  # Generate 12 lags
  price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
    mutate(across(
      all_of(inst_name),
      list(
        lag1 = ~dplyr::lag(.x, 1)
      ),
      .names = "{.col}_{.fn}"
    )) 
  
  # Build the formula dynamically
  lag_controls <- paste0(c(inst_name), "_lag", 1, collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "load_tot ~ ", rhs_vars_d, " + ", lag_controls,
      " | price ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
  
  
  
  
  # Stocker dans la liste
  # iv_results_feols_all_lin_lag_ter[[suffix]] <- list(
  #   supply = list(model = iv_supply, #nw = nw_s, 
  #                 fstat = fstat_s$ivf1$stat),
  #   demand = list(model =iv_demand, #nw = nw_d, 
  #                 fstat = fstat_d$ivf1$stat)
  # )
  iv_results_feols_all_lin_lag_ter[[suffix]] <- list(
    supply  = iv_supply, 
    demand =iv_demand)
}
# names(iv_results_feols_all)
names(iv_results_feols_all_lag_lin)
names(iv_results_feols_all_lin_lag_ter)

iv_results_feols_comparable <- iv_results_feols_all_lag_lin["fit_supply_unmatched"]
iv_results_feols_comparable_bis <- iv_results_feols_all_lin_lag_bis["fit_supply_unmatched"]
iv_results_feols_comparable_ter <- iv_results_feols_all_lin_lag_ter["fit_supply_unmatched"]

# Aplatir tous les modèles dans une seule liste
all_models_comparable <- unlist(
  lapply(c(iv_results_feols_comparable, iv_results_feols_comparable_bis, iv_results_feols_comparable_ter), function(x) x),
  recursive = FALSE
)

etable(all_models_comparable,
       keep = c("price"),
       #headers = c("Supply Unmatched","Demand Unmatched", "Supply Matched", "Demand Matched"),
       fitstat = ~ ivf.stat +  r2+ n)














