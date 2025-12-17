
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# Sauvegarder

# coef_tab
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_no_CBF.parquet")
price_indispo_merge_esp_FE <- read_parquet("price_indispo_merge_esp_FE.parquet")
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_esp_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo.parquet") 
price_indispo_merge_bronte <- read_parquet("price_indispo_merge_delta_supply_demand_renewable.parquet")

select_fitted <- function(x, type) {
  grep(paste0(type, ".*unmatched"), names(x), value = TRUE) |>
    (\(v) grep("indispo", v, invert = TRUE, value = TRUE))()
}

cols_fitted_supply_FE <- select_fitted(price_indispo_merge_esp_FE, "supply")
cols_fitted_demand_FE <- select_fitted(price_indispo_merge_esp_FE, "demand")
cols_fitted_supply_npc <- select_fitted(price_indispo_merge_esp_npc, "supply")
cols_fitted_demand_npc <- select_fitted(price_indispo_merge_esp_npc, "demand")







price_indispo_merge_compare <- merge(price_indispo_merge_esp_npc %>% 
                                       dplyr::select(datetime,
                                              cols_fitted_supply_npc , cols_fitted_demand_npc   ),
                                     price_indispo_merge_esp_delta
)
price_indispo_merge_compare <- merge(price_indispo_merge_esp_FE %>% 
                                       dplyr::select(datetime,
                                              cols_fitted_supply_FE , cols_fitted_demand_FE   ),
                                     price_indispo_merge_compare
)

price_indispo_merge_compare <- merge(price_indispo_merge_bronte %>% 
                                       dplyr::select(datetime,wind_speed_100m,cloud_cover, global_tilted_irradiance  ),
                                     price_indispo_merge_compare
)
dim(price_indispo_merge_compare)
#attention maintenant ça commence en 2018
#puisque je prends direct le choc d'offre et de demande


price_indispo_merge_compare %>% 
  dplyr::select(-datetime) %>% 
  dplyr::select(c("delta_s_total",  cols_fitted_supply_FE,cols_fitted_supply_npc)) %>% 
  cor(use = "complete.obs")

price_indispo_merge_compare %>% 
  dplyr::select(-datetime) %>% 
  dplyr::select(c("delta_d_total", cols_fitted_demand_FE,cols_fitted_demand_npc)) %>% 
  cor(use = "complete.obs")



price_indispo_merge_feols_all <- price_indispo_merge_compare %>%
  dplyr::select(-log_price_diff, -price_diff) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.)) %>%           # garde seulement les lignes sans NA dans ces colonnes
  distinct()%>% 
  mutate(
    year = factor(year),
    month = factor(month),
    creneau = factor(creneau)
  ) %>% 
  rename(supply_delta = delta_s_total,
         demand_delta =delta_d_total)


# Sélectionner toutes les colonnes fit
cols_fitted <- grep("(supply_|demand_)", names(price_indispo_merge_feols_all), value = TRUE)

setDT(price_indispo_merge_feols_all)
for(col in cols_fitted){
  if(grepl("supply", col)){
    target_col <- sub("supply", "demand", col)
  } else if(grepl("demand", col)){
    target_col <- sub("demand", "supply", col)
  } else {
    next
  }
  
  new_col <- paste0(col, "_orth")
  
  if(target_col %in% names(price_indispo_merge_feols_all)){
    
    # Fit regression with datetime clustering
    formula_iv <- as.formula(paste(col, "~", target_col))
    fit <- feols(formula_iv, data = price_indispo_merge_feols_all, cluster = ~datetime)
    
    # Extract residuals and assign as a new column
    price_indispo_merge_feols_all[, (new_col) := residuals(fit)]
  }
}

cols_fitted_bis <- setdiff(grep("(supply_|demand_)", names(price_indispo_merge_feols_all), value = TRUE), cols_fitted)

cols_fitted_supply_orth <- grep("supply", cols_fitted_bis, value = TRUE)

price_indispo_merge_feols_all <- price_indispo_merge_feols_all %>% 
  mutate(demand_bonte = cold_excess_demand,
         supply_bonte = wind_speed_100m)


cols_fitted_supply_orth <- c(cols_fitted_supply_orth , c( "supply_bonte"))

write_parquet(price_indispo_merge_feols_all, "price_indispo_merge_feols_all.parquet")


price_indispo_merge_feols_all <- read_parquet( "price_indispo_merge_feols_all.parquet")
colnames(price_indispo_merge_feols_all)
controls_vec <- c("year", "month", "weekday", "creneau", "spot_gas")

rhs_vars <- paste( paste(controls_vec, collapse = " + "), sep = " + ")
ols_formula <- as.formula(
  paste0(
    "gen_tot_da ~ ", rhs_vars, "+ price")
)
m_ols <- feols(ols_formula, data = price_indispo_merge_feols_all, vcov_NW("datetime", lag = 24))
summary(m_ols)


# Créer une liste pour stocker les résultats
iv_results_feols_all <- list()
s_col  <- "supply_bonte"
d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_orth)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  #corriger demand bonte pour mettre cold_excess + heat_excess
  
  if (d_col == "demand_bonte") {
    d_col <- c("cold_excess_demand", "heat_excess_demand")
  }
  
  print(suffix)

  
  rhs_vars <- paste( paste(controls_vec, collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "gen_tot_da ~ ", rhs_vars, " | price ~ ", instr_vars_s
    )
  )
  
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "gen_tot_da ~ ", rhs_vars, " | price ~ ", instr_vars_d
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_feols_all, vcov_NW("datetime", lag = 24))
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_feols_all, vcov_NW("datetime", lag = 24))
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  
  # Stocker dans la liste
  iv_results_feols_all[[suffix]] <- list(
    supply = list(model = iv_supply, #nw = nw_s, 
                  fstat = fstat_s$ivf1$stat),
    demand = list(model =iv_demand, #nw = nw_d, 
                  fstat = fstat_d$ivf1$stat)
  )
}


# Préparer les modèles à mettre dans le tableau
all_models_supply_feols_all <- list()
all_models_demand_feols_all <- list()

F_stat_supply_feols_all <- c()
F_stat_demand_feols_all <- c()
suffix <- "fit_supply_unmatched"
for(suffix in names(iv_results_feols_all)){
  
  # Supply
  all_models_supply_feols_all <- c(all_models_supply_feols_all, list(iv_results_feols_all[[suffix]]$supply$model))
  #column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply_feols_all  <- c(F_stat_supply_feols_all , round(iv_results_feols_all[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand_feols_all <- c(all_models_demand_feols_all, list(iv_results_feols_all[[suffix]]$demand$model))
  #column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand_feols_all <- c(F_stat_demand_feols_all, round(iv_results_feols_all[[suffix]]$demand$fstat, 2))
}



# Extraire le coefficient spécifique de chaque modèle
# coef_price_supply_feols_all <- sapply(all_models_supply_feols_all, function(mod) {
#   summary(mod)$se["fit_price", "Std. Error"]
# })

# Supply
coef_supply_feols_all <- sapply(all_models_supply_feols_all, function(mod) {
  c(coef = summary(mod)$coefficients["fit_price"], se = summary(mod)$se["fit_price"])
})

# Demand
coef_demand_feols_all <- sapply(all_models_demand_feols_all, function(mod) {
  c(coef = summary(mod)$coefficients["fit_price"], se = summary(mod)$se["fit_price"])
})


coef_supply_feols_all
coef_demand_feols_all
F_stat_supply_feols_all
F_stat_demand_feols_all

iv_results_feols_all_lag <- list()
s_col  <- "shock_supply_unmatched_orth"
# d_col <- "fitted_select_nfedemand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_orth)){
  
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
  rhs_vars_s <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec), collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  
  # Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
  inst_name <- instr_vars_s
  
  # Generate 12 lags
  price_indispo_merge_feols_all <- price_indispo_merge_feols_all %>%
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
      "gen_tot_da ~ ", rhs_vars_s, " + ", lag_controls,
      " | price ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_feols_all, vcov_NW("datetime", lag = 24))
  
  inst_name <- instr_vars_d
  
  # Generate 12 lags
  price_indispo_merge_feols_all <- price_indispo_merge_feols_all %>%
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
      "gen_tot_da ~ ", rhs_vars_s, " + ", lag_controls,
      " | price ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_feols_all, vcov_NW("datetime", lag = 24))
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  # Stocker dans la liste
  iv_results_feols_all_lag[[suffix]] <- list(
    supply = list(model = iv_supply, #nw = nw_s, 
                  fstat = fstat_s$ivf1$stat),
    demand = list(model =iv_demand, #nw = nw_d, 
                  fstat = fstat_d$ivf1$stat)
  )
}

names(iv_results_feols_all_lag)
# Préparer les modèles à mettre dans le tableau
all_models_supply_feols_all_lag <- list()
all_models_demand_feols_all_lag <- list()

F_stat_supply_feols_all_lag <- c()
F_stat_demand_feols_all_lag <- c()

for (suffix in sub("_orth$", "", cols_fitted_supply_orth)) {
  # Vérifie que le nom existe (utile en debug)
  if (!suffix %in% names(iv_results_feols_all_lag)) next
  # Supply
  all_models_supply_feols_all_lag <- c(
    all_models_supply_feols_all_lag, 
    list(iv_results_feols_all_lag[[suffix]]$supply$model)
  )
  F_stat_supply_feols_all_lag <- c(
    F_stat_supply_feols_all_lag, 
    round(iv_results_feols_all_lag[[suffix]]$supply$fstat, 2)
  )
  
  # Demand
  all_models_demand_feols_all_lag <- c(
    all_models_demand_feols_all_lag, 
    list(iv_results_feols_all_lag[[suffix]]$demand$model)
  )
  F_stat_demand_feols_all_lag <- c(
    F_stat_demand_feols_all_lag, 
    round(iv_results_feols_all_lag[[suffix]]$demand$fstat, 2)
  )
}


# Extraire le coefficient spécifique de chaque modèle


coef_supply_feols_all_lag <- sapply(all_models_supply_feols_all_lag, function(mod) {
  c(coef = summary(mod)$coefficients["fit_price"], se = summary(mod)$se["fit_price"])
})

# Demand
coef_demand_feols_all_lag <- sapply(all_models_demand_feols_all_lag, function(mod) {
  c(coef = summary(mod)$coefficients["fit_price"], se = summary(mod)$se["fit_price"])
})
coef_supply_feols_all_lag
coef_demand_feols_all_lag
F_stat_supply_feols_all_lag
F_stat_demand_feols_all_lag 



coef_table_feols_all <- data.frame(
  Model = rep(c("Curve_shocks", "Unmatched_FE", "Unmatched", "Delta", "Bonte"), 4),
  Coef_log_price = c(coef_supply_feols_all[1,], coef_demand_feols_all[1,], coef_supply_feols_all_lag[1,], coef_demand_feols_all_lag[1,]),
  Std_Error      = c(coef_supply_feols_all[2,],   coef_demand_feols_all[2,],   coef_supply_feols_all_lag[2,],   coef_demand_feols_all_lag[2,]),
  F_stat = c(F_stat_supply_feols_all, F_stat_demand_feols_all, F_stat_supply_feols_all_lag, F_stat_demand_feols_all_lag),
  Type = rep(c("Supply","Demand","Supply","Demand"), each = 5),
  FE   = "YMWH",
  Lag  = rep(c("0","0","1","1"), each = 5)
)
coef_table_feols_all 

getwd()
write.csv(coef_table_feols_all, "coef_table_feols_all_linear.csv")

coef_table_feols_all <- read.csv( "coef_table_feols_all_linear.csv")

# coef_table_wide_feols_all <- coef_table_feols_all %>%
#   # filter(Model != "Unmatched") %>% 
#   mutate(
#     coef_se = sprintf("%.3f (%.3f)", Coef_log_price, Std_Error),
#     Lag = paste0("Lag", Lag)  # pour nommer clairement les colonnes
#   ) %>%
#   dplyr::select(Type, Lag, Model, coef_se) %>%
#   pivot_wider(
#     names_from = c(Lag, Model),
#     values_from = coef_se
#   )


# Prepare long format table for stacking
coef_table_long <- coef_table_feols_all %>%
  filter(Model != "Unmatched_FE" & Model != "Delta") %>% 
  mutate(
    coef_se = sprintf("%.3f (%.3f)", Coef_log_price, Std_Error),
    Lag = paste0("Lag", Lag)
  ) %>%
  dplyr::select(Type, Lag, Model, coef_se) %>%
  pivot_wider(names_from = Model, values_from = coef_se) %>%
  arrange(Lag, Type)  # ensures Baseline (Lag0) first

# Optional: create a factor to order Lag0 first, Lag1 second
coef_table_long$Lag <- factor(coef_table_long$Lag, levels = c("Lag0", "Lag1"))

# Build gt table with row groups for Baseline / Lag
gt_table_stacked <- coef_table_long %>%
  gt(rowname_col = "Type") %>%
  tab_header(title = md("**Elasticity Estimates**")) %>%
  cols_label(
    #Delta = "Delta",
    #Unmatched_FE = "Unmatched FE",
    Unmatched = "Exogenous Projection",
    Curve_shocks = "Curve",
    Bonte = "Exogenous"
  ) %>%
  # Define row groups using Lag
  tab_row_group(
    label = "Baseline",
    rows = Lag == "Lag0"
  ) %>%
  tab_row_group(
    label = "Lag",
    rows = Lag == "Lag1"
  ) %>%
  # Hide Lag column from display
  cols_hide(columns = "Lag") %>%
  # Styling
  opt_table_outline() %>%
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = cells_row_groups()
  )

gt_table_stacked



# Chemin où sauvegarder
save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
save_file <- file.path(save_plot_path, "gt_table_feols_all_linear.tex")

# Sauvegarder en LaTeX
gtsave(gt_table_stacked, save_file)



# Prepare long format table for stacking
F_stat_table_long <- F_stat_table_feols_all %>%
  filter(Model != "Unmatched_FE" & Model != "Delta") %>% 
  mutate(Lag = paste0("Lag", Lag)
  ) %>%
  dplyr::select(Type, Lag, Model, F_stat) %>%
  pivot_wider(names_from = Model, values_from = F_stat) %>%
  arrange(Lag, Type)  # ensures Baseline (Lag0) first

# Optional: create a factor to order Lag0 first, Lag1 second
F_stat_table_long$Lag <- factor(F_stat_table_long$Lag, levels = c("Lag0", "Lag1"))

# Build gt table with row groups for Baseline / Lag
F_stat_table_stacked <- F_stat_table_long %>%
  gt(rowname_col = "Type") %>%
  tab_header(title = md("**Relevance Estimates**")) %>%
  cols_label(
    #Delta = "Delta",
    #Unmatched_FE = "Unmatched FE",
    Unmatched = "Exogenous Projection",
    Curve_shocks = "Curve",
    Bonte = "Exogenous"
  ) %>%
  # Define row groups using Lag
  tab_row_group(
    label = "Baseline",
    rows = Lag == "Lag0"
  ) %>%
  tab_row_group(
    label = "Lag",
    rows = Lag == "Lag1"
  ) %>%
  # Hide Lag column from display
  cols_hide(columns = "Lag") %>%
  # Styling
  opt_table_outline() %>%
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = cells_row_groups()
  )

F_stat_table_stacked


# Chemin où sauvegarder
save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
save_file <- file.path(save_plot_path, "fstat_table_feols_all_linear.tex")

# Sauvegarder en LaTeX
gtsave(F_stat_table_stacked, save_file)

