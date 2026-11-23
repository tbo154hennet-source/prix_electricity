

# Install if needed
# install.packages(c("fixest","sandwich","lmtest","relaimpo"))


# Packages
library(relaimpo)
library(fixest)
library(arrow)       # for read_parquet()
library(dplyr)
library(sandwich)
library(lmtest)
library(stringr)

# Files to process
files_to_load <- c(
  "reconstruct_full_curve_shift_qty_supply_unmatched.parquet",
  "reconstruct_full_curve_shift_qty_supply_matched.parquet",
  "reconstruct_full_curve_shift_qty_demand_unmatched.parquet",
  "reconstruct_full_curve_shift_qty_demand_matched.parquet"
)

input_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs"


# Example variables you already have in your workspace:
# price_grid_sparse, price_indispo_merge_esp_npc,
# instruments_tot, factors, factor_vars, factors_indispo

# Create a my_results list
my_results <- list()

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


price_indispo_merge_esp_FE <- price_indispo_merge_esp %>%
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

file_name <-  files_to_load[1]
for (file_name in files_to_load) {
  setwd(input_dir)
  cat("Processing:", file_name, "\n")
  parts <- str_split(file_name, "_")[[1]]
  last_two <- tail(parts, 2)
  last_two <- str_remove(last_two, "\\.parquet$")  # enlever l’extension au cas où
  # --- 1. Load and filter data ---
  df <- read_parquet(file.path(input_dir, file_name))
  df <- df[PRICE %in% price_grid_sparse, ]
  
  reg_data <- merge(
    df,
    price_indispo_merge_esp,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour")
  ) %>% 
    mutate(DATE = factor(DATE))
  
  reg_data <- reg_data[
    complete.cases(reg_data[, .SD, .SDcols = c("QTY", "PRICE", factor_vars, factors)]),
  ]
  

  
  # Instruments + price
  rhs_formula_instr <- as.formula(
    paste0(
      "QTY ~ ", 
      paste(c(instruments_tot, "PRICE"), collapse = " + "),
      " | ",
      paste(factor_vars, collapse = " + ")
    )
  )
  
  # --- 3. Estimate demand model (baseline and instrumented versions) ---
  feols_model <- feols(rhs_formula_instr, data = reg_data,
                       cluster = ~ year + month + weekday + creneau)
  
  # Instruments + price
  rhs_formula_simple <- as.formula(
    paste0(
      "QTY ~ ", 
      paste(c( "PRICE"), collapse = " + "),
      " | ",
      paste(factor_vars, collapse = " + ")
    )
  )
  feols_model_bis <- feols(rhs_formula_simple , data = reg_data,
                           cluster = ~ year + month + weekday + creneau)
  # --- 4. Compute demand shocks ---
  reg_data$shock <- resid(feols_model_bis)

  new_col_shock <- paste0("shock_", last_two[[1]], "_", last_two[[2]])
  # Créer une data.table temporaire sans modifier reg_data
  shock_mean <- as.data.table(reg_data)[, .(new_col_shock = mean(shock, na.rm = TRUE)), by = datetime]
  

  price_indispo_merge_esp_FE = merge(price_indispo_merge_esp_FE,
                                     shock_mean,
                                     by = "datetime")

  
  price_indispo_merge_esp_FE <- price_indispo_merge_esp_FE %>%
    rename_with(~ paste0("shock_", last_two[[1]], "_", last_two[[2]]),
                .cols = new_col_shock)
    
 
  # --- 5. Regress shocks on instruments ---
  instr_formula <- as.formula(
    paste(
      "shock ~", 
      paste(instruments_tot, collapse = " + "),
      "|",
      paste(factor_vars, collapse = " + ")
    )
  )
  
  instr_fit <- feols(
    instr_formula,
    data = reg_data,
    cluster = ~ year + month + weekday + creneau,
    warn = FALSE
  )
  
  # --- 6. Compute share of variance explained ---
  fitted_vals <- predict(instr_fit)
  share_var <- var(fitted_vals, na.rm = TRUE) / var(reg_data$shock, na.rm = TRUE)

  
  lm_formula <-  as.formula(
    paste(
      new_col_shock,"~", 
      paste(c(instruments_tot,factor_vars), collapse = " + ")
    )
  )
  # OLS classique (sans FE) pour illustration
  lm_model <- lm(lm_formula , data =price_indispo_merge_esp_FE)
  summary(lm_model)
  
  # Calcul des contributions relatives
  contrib_relative <- calc.relimp(lm_model, type = "lmg") # LMG = variance expliquée moyenne marginale

  # --- 7. Store key metrics ---
  my_results[[file_name]] <- list(
    Type = last_two[[1]],
    Matching = last_two[[2]],
    
    # adjusted within r2
    within_R2 = r2(feols_model, "war2"),
    within_R2_resid_shock =r2(instr_fit, "war2"),#encore conditionnel au PRICE par observation
    R2_resid_longi_shock = contrib_relative@R2,#régression avec chasque observation en pas de temps
    share_var = share_var,
    contrib = contrib_relative@lmg
  )
}

# Suppose list_parquet est ta liste de fichiers chargés
list_contribs <- lapply(my_results, function(f) {
  data.table(
    variable = names(f$contrib),
    contrib = as.numeric(f$contrib)# optionnel, pour garder l'identifiant du fichier
  )
})

# Combiner tous
all_contribs <- rbindlist(list_contribs)


# --- 8. Combine into a summary table ---
my_results_df <- bind_rows(lapply(my_results, as.data.frame))

my_results_df <- merge(my_results_df,
                       all_contribs,
                       by= "contrib"
                       ) %>% 
  arrange(variable, Type, Matching)


getwd()
write.csv( my_results_df, "share_var_explained_reg_cbf.csv")
my_results_df <- read.csv("share_var_explained_reg_cbf.csv")
print(my_results_df)


#etable(instr_fit, feols_model, feols_model_bis, final_fit)

colnames(price_indispo_merge_esp_FE)
write_parquet(price_indispo_merge_esp_FE, "price_indispo_merge_esp_FE.parquet")
#regarder la corrélation avec le chocs construits précedemment
library(gt)

# Créer la table gt
table_gt_instruments <- my_results_df %>%
  dplyr::select(Type, Matching, within_R2_resid_shock, R2_resid_longi_shock) %>% 
  distinct(.keep_all = TRUE) %>% 
  gt() %>%
  # Labels propres
  cols_label(
    within_R2_resid_shock = "(1)",
    R2_resid_longi_shock= "(2)"
    # Lag0_Unmatched = "(3)",
    # Lag0_Curve_shocks = "(4)",
    # Lag1_Delta = "(1)",
    # Lag1_Unmatched_FE = "(2)",
    # Lag1_Unmatched = "(3)",
    # Lag1_Curve_shocks = "(4)"
  ) %>%
  tab_header(
    title = "Variance share explained by instruments"
  ) %>%
  fmt_number(
    columns = everything(),  # formater toutes les colonnes numériques
    decimals = 3
  ) %>%
  opt_row_striping() %>%
  tab_footnote(
    footnote = "* (1) R² conditional to price (2) R² by timestep",
    locations = cells_title(groups = "title")
  )
table_gt_instruments
# Chemin où sauvegarder
save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/post_seminaire'
save_file <- file.path(save_plot_path, "instrument_var_share.tex")

# Sauvegarder en LaTeX
gtsave(table_gt_instruments, save_file)





# appuyer ma notion de localité
#quelle variabilité d'offre et de demande
#mesurés en prix
#conditionnels aux FE


price_indispo_merge_esp_FE_bis <- price_indispo_merge_esp %>%
  mutate(
    year_month = factor(paste0(year, '_', month)),
    month   = factor(month),
    year    = factor(year),
    creneau = factor(creneau),
    weekday = factor(weekday)
  )

files_to_load_bis<- c(
  "reconstruct_full_curve_shift_qty_supply_matched.parquet",
  "reconstruct_full_curve_shift_qty_demand_matched.parquet"
)
for (file_name in files_to_load_bis) {
  cat("Processing:", file_name, "\n")
  parts <- str_split(file_name, "_")[[1]]
  last_two <- tail(parts, 2)
  last_two <- str_remove(last_two, "\\.parquet$")  # enlever l’extension au cas où
  # --- 1. Load and filter data ---
  df <- read_parquet(file.path(input_dir, file_name))
  df <- df[PRICE %in% price_grid_sparse[13:(length(price_grid_sparse)-10)], ]
  
  reg_data <- merge(
    df,
    price_indispo_merge_esp_npc,
    by.x = c("DATE", "Hour"),
    by.y = c("delivery_date", "Hour"),
    all.x = TRUE
  ) %>% 
    mutate(DATE = factor(DATE)) %>% 
    filter(QTY >= 15000)
  
  reg_data <- reg_data[
    complete.cases(reg_data[, .SD, .SDcols = c("QTY", "PRICE", factor_vars, factors)]),
  ]
  
  
  
  # Instruments + price
  rhs_formula_instr <- as.formula(
    paste0(
      "PRICE ~ ", 
      paste(c(instruments_tot, "QTY"), collapse = " + "),
      " | ",
      paste(factor_vars, collapse = " + ")
    )
  )
  
  # --- 3. Estimate demand model (baseline and instrumented versions) ---
  feols_model <- feols(rhs_formula_instr, data = reg_data,
                       cluster = ~ year + month + weekday + creneau)
  
  # Instruments + price
  rhs_formula_simple <- as.formula(
    paste0(
      "PRICE ~ ", 
      paste(c( "QTY"), collapse = " + "),
      " | ",
      paste(factor_vars, collapse = " + ")
    )
  )
  feols_model_bis <- feols(rhs_formula_simple , data = reg_data,
                           cluster = ~ year + month + weekday + creneau)
  # --- 4. Compute demand shocks ---
  reg_data$shock <- resid(feols_model_bis)
  
  new_col_shock <- paste0("shock_", last_two[[1]], "_", last_two[[2]])
  # Créer une data.table temporaire sans modifier reg_data
  shock_mean <- as.data.table(reg_data)[, .(new_col_shock = mean(shock, na.rm = TRUE)), by = datetime]
  
  
  price_indispo_merge_esp_FE_bis = merge(price_indispo_merge_esp_FE_bis,
                                     shock_mean,
                                     by = "datetime")
  
  
  price_indispo_merge_esp_FE_bis <- price_indispo_merge_esp_FE_bis %>%
    rename_with(~ paste0("shock_", last_two[[1]], "_", last_two[[2]]),
                .cols = new_col_shock)

}

getwd()
write_parquet(price_indispo_merge_esp_FE_bis, "price_indispo_merge_esp_FE_in_price_units.parquet")

library(dplyr)
library(stats)

# 1) Estimer le modèle des FE
fe_model <- lm(price ~ factor(year) + factor(month) + factor(weekday) + factor(creneau),
               data = price_indispo_merge_esp_FE_bis)

# 2) Extraire les résidus
price_indispo_merge_esp_FE <- price_indispo_merge_esp_FE_bis %>%
  mutate(resid_price = residuals(fe_model))

# 3) Calculer l'écart-type des mouvements de prix conditionnel aux FE
sd_resid_price <- sd(price_indispo_merge_esp_FE_bis$resid_price, na.rm = TRUE)

sd_resid_price


sd(price_indispo_merge_esp_FE_bis$shock_demand_matched)

sd(price_indispo_merge_esp_FE_bis$shock_supply_matched)

colnames(price_indispo_merge_esp_FE_bis)






