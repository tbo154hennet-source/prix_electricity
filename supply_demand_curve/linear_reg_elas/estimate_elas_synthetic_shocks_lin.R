output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all.parquet")

#un faible nombre de duplicates
price_indispo_merge_esp_npc %>%
  group_by(datetime) %>% 
  summarise(n = n()) %>% 
  filter(n >1)

price_indispo_merge_esp_npc <- 
  price_indispo_merge_esp_npc %>%
  distinct(datetime, .keep_all = TRUE)


colnames(price_indispo_merge_esp_npc)

select_fitted <- function(x, type) {
  grep(paste0(type, ".*matched"), names(x), value = TRUE) |>
    (\(v) grep("indispo", v, invert = TRUE, value = TRUE))()
}
cols_fitted_supply_npc <- select_fitted(price_indispo_merge_esp_npc, "supply")
cols_fitted_demand_npc <- select_fitted(price_indispo_merge_esp_npc, "demand")




############### IV estimation using my aggregate Supply and demand shocks


# Créer une liste pour stocker les résultats
iv_results_feols_all <- list()

# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c(cols_fitted_supply_npc)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fitted_select_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  if(!d_col %in% names(price_indispo_merge_esp_npc)) next
  
  
  
  rhs_vars_s <- paste( paste(c(controls_vec,d_col), collapse = " + "), sep = " + ")
  instr_vars_s <- paste(c(s_col), collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "gen_tot_da ~ ", rhs_vars_s, " | price ~ ", instr_vars_s
    )
  )
  
  instr_vars_d <- paste(c(d_col), collapse = " + ")
  rhs_vars_d <- paste( paste(c(controls_vec,s_col), collapse = " + "), sep = " + ")
  iv_formula_d <- as.formula(
    paste0(
      "gen_tot_da ~ ", rhs_vars_d, " | price ~ ", instr_vars_d
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc, NW(24) ~ datetime)
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_esp_npc, NW(24) ~ datetime)
  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  
  # Stocker dans la liste
  # iv_results_feols_all[[suffix]] <- list(
  #   supply = list(model = iv_supply, #nw = nw_s, 
  #                 fstat = fstat_s$ivf1$stat),
  #   demand = list(model =iv_demand, #nw = nw_d, 
  #                 fstat = fstat_d$ivf1$stat)
  # )
  iv_results_feols_all[[suffix]] <- list(
    supply  = iv_supply, 
    demand  =iv_demand)
}

###################### with Lag


iv_results_feols_all_lag_lin <- list()
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
  lag_controls <- paste0(inst_name, "_lag", 1, collapse = " + ")
  
  iv_formula_s <- as.formula(
    paste0(
      "gen_tot_da ~ ", rhs_vars_s, " + ", lag_controls,
      " | price ~ ", instr_vars_s
    )
  )
  iv_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc, NW(24) ~ datetime)
  
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
  lag_controls <- paste0(inst_name, "_lag", 1, collapse = " + ")
  
  iv_formula_d <- as.formula(
    paste0(
      "gen_tot_da ~ ", rhs_vars_d, " + ", lag_controls,
      " | price ~ ", instr_vars_d
    )
  )
  
  iv_demand <- feols(iv_formula_d, data = price_indispo_merge_esp_npc, NW(24) ~ datetime)
  "datetime" %in% colnames(price_indispo_merge_esp_npc)
  class(price_indispo_merge_esp_npc$datetime)
  

  
  
  # Classic F-stat for the instruments
  fstat_s <- fitstat(summary(iv_supply, stage = 1), type = "ivf")
  fstat_d <- fitstat(summary(iv_demand, stage = 1), type = "ivf")
  
  # Stocker dans la liste
  # iv_results_feols_all_lag_lin[[suffix]] <- list(
  #   supply = list(model = iv_supply, #nw = nw_s, 
  #                 fstat = fstat_s$ivf1$stat),
  #   demand = list(model =iv_demand, #nw = nw_d, 
  #                 fstat = fstat_d$ivf1$stat)
  # )
  iv_results_feols_all_lag_lin[[suffix]] <- list(
    supply  = iv_supply, 
    demand =iv_demand)
}


c(cols_fitted_supply_npc)

#on se débarrasse des _nfe
names(iv_results_feols_all_lag_lin)

iv_results_feols_all_fe <- iv_results_feols_all[!grepl("nfe", names(iv_results_feols_all))]

# Aplatir tous les modèles dans une seule liste
all_models_lin <- unlist(
  lapply(iv_results_feols_all_fe, function(x) x),
  recursive = FALSE
)

etable(all_models_lin,
       keep = c("price"),
       headers = c("Supply Unmatched","Demand Unmatched", "Supply Matched", "Demand Matched"),
       fitstat = ~ ivf.stat +  r2+ n)

iv_results_feols_all_lag_lin_fe <- iv_results_feols_all_lag_lin[!grepl("nfe", names(iv_results_feols_all_lag_lin))]

# Aplatir tous les modèles dans une seule liste
all_models_lag_lin <- unlist(
  lapply(iv_results_feols_all_lag_lin_fe, function(x) x),
  recursive = FALSE
)

etable(all_models_lag_lin,
       keep = c("price"),
       headers = c("Supply Unmatched","Demand Unmatched", "Supply Matched", "Demand Matched"),
       fitstat = ~ ivf.stat +  r2+ n)
#write.csv(coef_table_feols_all, "coef_table_feols_all.csv")



# Liste combinée
all_models_combined_lin <- c(all_models_lin, all_models_lag_lin)

# Extraire infos
summary_table_lin <- map_dfr(all_models_combined_lin, ~ extract_coef_ivf(.x, "fit_price"))
summary_table_lin$Model <- rep(c("Supply", "Demand"), 4)
summary_table_lin$Matching <- rep(c(rep("Unmatched",2), rep("Matched",2)), 2)
summary_table_lin$Lag <-c(rep(0,4), rep(1,4))
summary_table_lin


save_path <- "Z:/prix_electricite_gitlab/supply_demand_curve/estimate_elasticity/outputs"
setwd(save_path)
write.csv(summary_table_lin, "elasticity_synthetic_lin.csv")







