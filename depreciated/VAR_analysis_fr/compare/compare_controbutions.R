


save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
historical_decomp_iv_mr <-  readRDS("hd_iv_4v.rds")
hd_cholesky <-   readRDS( "hd_chol_4v.rds")
historical_decomp_iv_double <- readRDS( "hd_iv_4v_double.rds")



save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)
historical_decomp_iv_1var <-  readRDS( "hd_iv_4v.rds")


#historical_decomp_iv_tibo <- historical_decomp_iv_tibo[, , c(2, 1)]
hd_cholesky <- readRDS( "hd_chol_4v.rds")
# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest

# Example: get contribution of electricity price shocks to inflation

dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]

compute_contributions <- function(start_date, end_date, 
                                  dates_formatted, 
                                  IV_data_log_diff_esp, 
                                  hd_cholesky, 
                                  historical_decomp_iv_mr,
                                  historical_decomp_iv_1var,
                                  historical_decomp_iv_double, 
                                  series = 4) {
  
  # Generate the date sequence and convert to yearmon
  desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  desired_period_yearmon <- as.yearmon(desired_period)
  
  # Get matching indices
  matching_indices <- which(dates_formatted %in% desired_period_yearmon)
  
  # Total inflation during the period
  cumulative_inflation <- sum(IV_data_log_diff_esp$HICP_log_diff[matching_indices + p])
  
  # Cholesky contributions
  contrib_demand_chol   <- sum(hd_cholesky[, series, 1][matching_indices]) / cumulative_inflation
  contrib_supply_chol <- sum(hd_cholesky[, series, 2][matching_indices]) / cumulative_inflation
  
  contrib_demand_mr   <- sum(historical_decomp_iv_mr[, series, 1][matching_indices]) / cumulative_inflation
  contrib_supply_mr <- sum(historical_decomp_iv_mr[, series, 2][matching_indices]) / cumulative_inflation

  contrib_demand_iv_1var   <- sum(historical_decomp_iv_1var[, series, 1][matching_indices]) / cumulative_inflation
  contrib_supply_iv_1var <- sum(historical_decomp_iv_1var[, series, 2][matching_indices]) / cumulative_inflation
  
  contrib_demand_iv_double   <- sum(historical_decomp_iv_double[, series, 1][matching_indices]) / cumulative_inflation
  contrib_supply_iv_double <- sum(historical_decomp_iv_double[, series, 2][matching_indices]) / cumulative_inflation
  

  # Return a tidy data frame (long format)
  tibble::tibble(
    Identification = rep(c("Cholesky", "IV MR", "IV SD", "Joint IV SD"), each = 2),
    Contribution_Type = rep(c("Demand", "Supply"), times = 4),
    `Contribution (pp)` = 100 * cumulative_inflation * c(
      contrib_demand_chol,  contrib_supply_chol,
      contrib_demand_mr,    contrib_supply_mr,
      contrib_demand_iv_1var, contrib_supply_iv_1var,
      contrib_demand_iv_double, contrib_supply_iv_double
    ),
    `Contribution (%)` = 100 * c(
      contrib_demand_chol,  contrib_supply_chol,
      contrib_demand_mr,    contrib_supply_mr,
      contrib_demand_iv_1var, contrib_supply_iv_1var,
      contrib_demand_iv_double, contrib_supply_iv_double
    )
  )
  
  
  
}

# Compute contributions for 2021 Q3 to 2022 Q3
contribution_data_HICP <- compute_contributions(
  start_date = "2021-07-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_mr = historical_decomp_iv_mr ,
  historical_decomp_iv_1var = historical_decomp_iv_1var,
  historical_decomp_iv_double = historical_decomp_iv_double,
  series = 4
)

contribution_table_wide_HICP <- contribution_data_HICP %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = c(`Contribution (pp)`, `Contribution (%)`)
  )


contribution_table_clean <- contribution_table_wide_HICP %>%
  filter(Identification != "Joint IV SD") %>% 
  dplyr::rename(
    `Demand (pp)` = `Contribution (pp)_Demand`,
    `Supply (pp)` = `Contribution (pp)_Supply`,
    `Demand (%)`  = `Contribution (%)_Demand`,
    `Supply (%)`  = `Contribution (%)_Supply`
    # ajouter `Supply (%)` si elle existe dans tes donn?es
  )
gt_table_HICP <- contribution_table_clean %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4")
  ) %>%
  gt::cols_label(
    Identification = "Identification",
    `Demand (pp)`  = "Demand (pp)",
    `Supply (pp)`  = "Supply (pp)",
    `Demand (%)`   = "Demand (%)"
    # idem pour Supply (%) si tu l'as
  ) %>%
  gt::fmt_number(
    columns = c(`Demand (pp)`, `Supply (pp)`, `Demand (%)`),
    decimals = 2
  )


gt_table_HICP

save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table_HICP,
  filename = "cumulative_contributions.tex"
)



gt_table_HICP_supply <- contribution_table_clean%>%
  dplyr::select(Identification, `Supply (pp)`, `Supply (%)`) %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4")
  ) %>%
  # Only keep Supply columns
  gt::cols_label(
    Identification = "Identification",
    `Supply (pp)`  = "Supply (pp)",
    `Supply (%)`   = "Supply (%)"
  ) %>%
  # Format only the remaining numeric columns
  gt::fmt_number(
    columns = c(`Supply (pp)`, `Supply (%)`),
    decimals = 2
  )

# Display the table
gt_table_HICP_supply

# Save to LaTeX
save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table_HICP_supply,
  filename = "cumulative_contributions_supply.tex"
)












# Compute contributions for 2021 Q3 to 2022 Q3
contribution_data_IPI <- compute_contributions(
  start_date = "2021-07-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_mr = historical_decomp_iv_mr ,
  historical_decomp_iv_1var = historical_decomp_iv_1var,
  historical_decomp_iv_double = historical_decomp_iv_double,
  series = 3
)

contribution_table_wide_IPI <- contribution_data_IPI %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = c(`Contribution (pp)`, `Contribution (%)`)
  )


contribution_table_clean_IPI <- contribution_table_wide_IPI %>%
  dplyr::rename(
    `Demand (pp)` = `Contribution (pp)_Demand`,
    `Supply (pp)` = `Contribution (pp)_Supply`,
    `Demand (%)`  = `Contribution (%)_Demand`,
    `Supply (%)`  = `Contribution (%)_Supply`
    # ajouter `Supply (%)` si elle existe dans tes donn?es
  )
gt_table_IPI<- contribution_table_clean_IPI %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4")
  ) %>%
  gt::cols_label(
    Identification = "Identification",
    `Demand (pp)`  = "Demand (pp)",
    `Supply (pp)`  = "Supply (pp)",
    `Demand (%)`   = "Demand (%)"
    # idem pour Supply (%) si tu l'as
  ) %>%
  gt::fmt_number(
    columns = c(`Demand (pp)`, `Supply (pp)`, `Demand (%)`),
    decimals = 2
  )


gt_table_IPI


# Renommer colonnes pour éviter conflit
HICP_wide <- contribution_table_wide_HICP %>%
  dplyr::rename(
    Supply_HICP = Supply,
    Demand_HICP = Demand
  )

IPI_wide <- contribution_table_wide_IPI %>%
  dplyr::rename(
    Supply_IPI = Supply,
    Demand_IPI = Demand
  )

# Joindre par Identification
contribution_table_side_by_side <- dplyr::full_join(
  HICP_wide, IPI_wide, by = "Identification"
)

# Tableau gt côte à côte
gt_table_side_by_side <- contribution_table_side_by_side %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contributions to HICP and IPI**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  gt::cols_label(
    Identification = "Identification",
    Supply_HICP = gt::md("Supply (HICP)"),
    Demand_HICP = gt::md("Demand (HICP)"),
    Supply_IPI = gt::md("Supply (IPI)"),
    Demand_IPI = gt::md("Demand (IPI)")
  ) %>%
  gt::fmt_number(
    columns = c(Supply_HICP, Demand_HICP, Supply_IPI, Demand_IPI),
    decimals = 2
  )

gt_table_side_by_side








