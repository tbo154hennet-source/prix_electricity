

# Construire les noms de colonnes
cols_supply<- paste0("delta_s_", instruments)
cols_demand<- paste0("delta_d_", instruments)

estimate_iv_sd <- function(instr, controls = controls_vars, data, mode = "demand") {
  if (mode == "demand") {
    dep_var <- "log(price)"
    endog_var <- "log(gen_tot)"
  } else if (mode == "supply") {
    dep_var <- "log(gen_tot)"
    endog_var <- "log(price)"
  } else {
    stop("mode must be either 'demand' or 'supply'")
  }
  
  # Build IV formula
  iv_formula <- as.formula(paste(
    paste(dep_var, "~", paste(c(endog_var, controls), collapse = " + ")),
    paste(paste(c(instr, controls), collapse = " + ")),
    sep = " | "
  ))
  
  model <- ivreg(iv_formula, data = data)
  return(model)
}

# Build the result list for both modes
# Build the result list with names
results_list_demand <- setNames(
  lapply(cols_demand, function(instr) {
    estimate_iv_sd(instr = instr, 
                   controls = c(controls_vars),
                   data = price_indispo_merge_delta_clean, mode = "demand")
  }),
  nm = cols_demand
)


results_list_supply <- setNames(
  lapply(cols_supply, function(instr) {
    estimate_iv_sd(instr = instr, 
                   controls = c(controls_vars),
                   data = price_indispo_merge_delta_clean, mode = "supply")
  }),
  nm = cols_supply
)

# Combine into a single list
results_list_all <- c(results_list_demand, results_list_supply)


# Extract estimates and SEs
extract_coef_se <- function(model, variable) {
  s <- summary(model)
  if (!(variable %in% rownames(s$coefficients))) {
    return(c(Estimate = NA, StdError = NA))
  }
  est <- s$coefficients[variable, "Estimate"]
  se <- s$coefficients[variable, "Std. Error"]
  return(c(Estimate = est, StdError = se))
}

# Summarize results
iv_results_df <- data.frame(
  Instrument = rep(instruments, each = 2),
  Mode = rep(c("Supply","Demand"), times = length(instruments)),
  Estimate = NA,
  StdError = NA
)
length(instruments)

for (i in seq_along(results_list_all)[1:length(instruments)]) {
  res_d <- extract_coef_se(model =results_list_all[[i]], variable="log(gen_tot)")
  res_s <- extract_coef_se(results_list_all[[i+length(instruments)]], "log(price)")
  iv_results_df$Estimate[(2*i - 1)] <- res_d["Estimate"]
  iv_results_df$StdError[(2*i - 1)] <- res_d["StdError"]
  iv_results_df$Estimate[(2*i)] <- res_s["Estimate"]
  iv_results_df$StdError[(2*i)] <- res_s["StdError"]
}

print(iv_results_df)
#Step 1: Compute elasticity and SE using delta method
iv_results_df <- iv_results_df %>%
  mutate(
    Elasticity_SE = ifelse(Mode == "Demand", Estimate, 1 / Estimate),
    StdError_SE = ifelse(
      Mode == "Demand",
      StdError,
      StdError / (Estimate^2)
    )
)

# Step 2: Format string with SE in parentheses
iv_results_df <- iv_results_df %>%
  mutate(
    Elasticity_CI = paste0(
      round(Elasticity_SE, 3), " (", round(StdError_SE, 3), ")"
    )
  )

# Step 3: Add Instrument and Type columns
iv_results_df <- iv_results_df %>%
  rename(Instrument = Instrument, Type = Mode) %>%
  mutate(
    Type = case_when(
      Type == "Demand" ~ "Demand",
      Type == "Supply" ~ "Supply"
    ),
    Instrument = case_when(
      Instrument == "renewable_day_ahead_forecast" ~ "Renewables",
      Instrument == "heat_excess" ~ "Heat",
      Instrument ==  "cold_excess" ~ "Cold",
      Instrument == "Published.Predicted.Unavailable.Capacity" ~ "Unavailability" ,
      Instrument == "cbf_es" ~ "CBF" 
    )
  )

# Step 4: Format wide table: rows = Type, columns = Instrument
iv_results_df_plot_table_wide <- iv_results_df %>%
  dplyr::select(Type, Instrument, Elasticity_CI) %>%
  complete(Type, Instrument, fill = list(Elasticity_CI = "-")) %>%
  pivot_wider(names_from = Instrument, values_from = Elasticity_CI)


iv_results_df_plot_table_wide

gt_table <- iv_results_df_plot_table_wide %>%
  gt() %>%
  tab_header(
    title = "Instrument Heterogeneity of Supply and Demand Elasticities"
  )

save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table,
  filename = "heterogeneity_elasticity_table.tex"
)




