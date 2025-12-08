
# Packages
library(fixest)
library(arrow)       # for read_parquet()
library(dplyr)
library(sandwich)
library(lmtest)

# Files to process
files_to_load <- c(
  # "reconstruct_full_curve_shift_qty_supply_unmatched.parquet",
  # "reconstruct_full_curve_shift_qty_supply_matched.parquet",
  "reconstruct_full_curve_shift_qty_demand_unmatched.parquet"#,
  #"reconstruct_full_curve_shift_qty_demand_matched.parquet"
)

input_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs"
setwd(input_dir)

# Example variables you already have in your workspace:
# price_grid_sparse, price_indispo_merge_esp_npc,
# instruments_tot, factors, factor_vars, factors_indispo

# Create a my_results list
my_results <- list()

price_indispo_merge_esp_FE_int <- price_indispo_merge_esp %>%
  mutate(
    year_month_creneau = factor(paste0(year, '_', month, '_', creneau)),
    year_month = factor(paste0(year, '_', month)),
    month   = factor(month),
    year    = factor(year),
    creneau = factor(creneau),
    weekday = factor(weekday)
  )
library(stringr)
file_name <- files_to_load[1]
for (file_name in files_to_load) {
  cat("Processing:", file_name, "\n")
  parts <- str_split(file_name, "_")[[1]]
  last_two <- tail(parts, 2)
  last_two <- str_remove(last_two, "\\.parquet$")  # enlever l’extension au cas où
  # --- 1. Load and filter data ---
  df <- read_parquet(file.path(input_dir, file_name))
  df <- df[PRICE %in% price_grid_sparse[13:(length(price_grid_sparse)-10)], ]
  
  reg_data <- merge(
    df,
    price_indispo_merge_esp_FE_int,
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
      "log(QTY) ~ ", 
      paste(c(instruments_tot, "log(PRICE + 1)"), collapse = " + "),
      " | ",
      "year_month_creneau + weekday"
    )
  )
  
  # --- 3. Estimate demand model (baseline and instrumented versions) ---
  feols_model <- feols(rhs_formula_instr, data = reg_data,
                       cluster = ~ year + month + weekday + creneau)
  
  # Instruments + price
  rhs_formula_simple <- as.formula(
    paste0(
      "log(QTY) ~ ", 
      paste(c( "log(PRICE + 1)"), collapse = " + "),
      " | ",
      "year_month_creneau + weekday"
    )
  )
  feols_model_bis <- feols(rhs_formula_simple , data = reg_data,
                           cluster = ~ year + month + weekday + creneau)
  # --- 4. Compute demand shocks ---
  reg_data$shock <- resid(feols_model_bis)
  
  new_col_shock <- paste0("shock_", last_two[[1]], "_", last_two[[2]])
  # Créer une data.table temporaire sans modifier reg_data
  shock_mean <- as.data.table(reg_data)[, .(new_col_shock = mean(shock, na.rm = TRUE)), by = datetime]
  
  
  price_indispo_merge_esp_FE_int = merge(price_indispo_merge_esp_FE_int,
                                     shock_mean,
                                     by = "datetime")
  
  
  price_indispo_merge_esp_FE_int <- price_indispo_merge_esp_FE_int %>%
    rename_with(~ paste0("shock_", last_two[[1]], "_", last_two[[2]]),
                .cols = new_col_shock)
  
  
  # --- 5. Regress shocks on instruments ---
  instr_formula <- as.formula(
    paste(
      "shock ~", 
      paste(instruments_tot, collapse = " + "),
      "|",
      "year_month_creneau + weekday"
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
  
  
  
  # --- 5. Regress shocks on instruments ---
  final_formula <- as.formula(
    paste(
      new_col_shock,"~", 
      paste(instruments_tot, collapse = " + "),
      "|",
      "year_month_creneau + weekday"
    )
  )
  
  final_fit <- feols(
    final_formula,
    data = price_indispo_merge_esp_FE_int,
    cluster = ~ year + month + weekday + creneau,
    warn = FALSE
  )
  
  # --- 7. Store key metrics ---
  my_results[[file_name]] <- list(
    Type = last_two[[1]],
    Matching = last_two[[2]],
    
    # adjusted within r2
    within_R2 = r2(feols_model, "war2"),
    within_R2_resid_shock =r2(instr_fit, "war2"),#encore conditionnel au PRICE par observation
    R2_resid_longi_shock = r2(final_fit, "war2"),#régression avec chasque observation en pas de temps
    share_var = share_var
  )
}

# --- 8. Combine into a summary table ---
my_results_df <- bind_rows(lapply(my_results, as.data.frame))
getwd()
#write.csv( my_results_df, "share_var_explained.csv")
print(my_results_df)


etable(instr_fit, feols_model, feols_model_bis, final_fit)

colnames(price_indispo_merge_esp_FE_int)
setwd(input_dir)
price_indispo_merge_esp_FE <- read_parquet("price_indispo_merge_esp_FE.parquet")

write_parquet(price_indispo_merge_esp_FE_int, "price_indispo_merge_esp_FE_int.parquet")
#regarder la corrélation avec le chocs construits précedemment
colnames(price_indispo_merge_esp_FE)
getwd()
cols <- c("shock_supply_unmatched", "shock_demand_unmatched") 

# rename columns in the *_int dataframe before merging
price_indispo_merge_esp_FE_int <- price_indispo_merge_esp_FE_int %>%
  rename_with(~ paste0(.x, "_int"), all_of(cols))

# now merge
price_indispo_merge_esp_FEs <- merge(
  price_indispo_merge_esp_FE %>% 
    select(all_of(cols), datetime),
  price_indispo_merge_esp_FE_int%>% 
    select(all_of(paste0(cols, "_int")), datetime),
  all = TRUE  # or FALSE, depending on what you want
)
colnames(price_indispo_merge_esp_FEs)
#, "datetime"
price_indispo_merge_esp_FEs %>% 
  select(-datetime) %>% 
  cor(use = "complete.obs")




