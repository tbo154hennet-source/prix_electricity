coef_table_feols_all <- read.csv( "coef_table_feols_all.csv")

# coef_table_wide_feols_all <- coef_table_feols_all %>%
#   # filter(Model != "Unmatched") %>% 
#   mutate(
#     coef_se = sprintf("%.3f (%.3f)", Coef_log_price, Std_Error),
#     Lag = paste0("Lag", Lag)  # pour nommer clairement les colonnes
#   ) %>%
#   select(Type, Lag, Model, coef_se) %>%
#   pivot_wider(
#     names_from = c(Lag, Model),
#     values_from = coef_se
#   )


# Prepare long format table for stacking
coef_table_long <- coef_table_feols_all %>%
  mutate(
    coef_se = sprintf("%.3f (%.3f)", Coef_log_price, Std_Error),
    Lag = paste0("Lag", Lag)
  ) %>%
  select(Type, Lag, Model, coef_se) %>%
  pivot_wider(names_from = Model, values_from = coef_se) %>%
  arrange(Lag, Type)  # ensures Baseline (Lag0) first

# Optional: create a factor to order Lag0 first, Lag1 second
coef_table_long$Lag <- factor(coef_table_long$Lag, levels = c("Lag0", "Lag1"))

# Build gt table with row groups for Baseline / Lag
gt_table_stacked <- coef_table_long %>%
  gt(rowname_col = "Type") %>%
  tab_header(title = md("**Coefficient Estimates**")) %>%
  cols_label(
    Delta = "Delta",
    Unmatched_FE = "Unmatched FE",
    Unmatched = "Unmatched",
    Curve_shocks = "Curve"
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
save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/post_seminaire'
save_file <- file.path(save_plot_path, "gt_table_feols_all.tex")

# Sauvegarder en LaTeX
gtsave(gt_table_stacked, save_file)