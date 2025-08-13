
#headline inflation Cholesky 60
# sign + zero restrictions 60
# The model includes energy, food and core inflation and negotiated 
# wage growth, all measured as year-o-year (y-on-y, henceforth) percentage changes, and the 
# unemployment rate. 


# this specification omits variables related to the monetary 
# such as policy rates and inflation expectations.

#se comparer avec le papier de bank of Italy






inflation_12m =  slide_dbl(IV_data_log_diff_esp$HICP_log_diff, sum, .before = 11, .complete = TRUE)

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)

hd_cholesky <- readRDS( "hd_chol_4v.rds")
historical_decomp_iv_tibo <- readRDS( "hd_iv_4v.rds")

historical_decomp_iv_tibo
inflation_12m =  slide_dbl(IV_data_log_diff_esp$HICP_log_diff, sum, .before = 11, .complete = TRUE)

library(slider)

compute_contributions_gliss <- function(start_date, end_date, 
                                  dates_formatted, 
                                  IV_data_log_diff_esp, 
                                  hd_cholesky, 
                                  historical_decomp_iv_tibo, 
                                  series = 4) {
  
  
  decomp_12m_sum_iv <- get_12_gliss(historical_decomp_iv_tibo)
  decomp_12m_sum_chol <- get_12_gliss(hd_cholesky)
  
  # Generate the date sequence and convert to yearmon
  desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  desired_period_yearmon <- as.yearmon(desired_period)
  
  # Get matching indices
  matching_indices <- which(dates_formatted %in% desired_period_yearmon)
  
  # Total inflation during the period
  cumulative_inflation <- sum(inflation_12m[matching_indices + p])
  
  # Cholesky contributions
  contrib_price <- sum(decomp_12m_sum_chol[, series, 2][matching_indices ]) / cumulative_inflation
  contrib_gen   <- sum(decomp_12m_sum_chol[, series, 1][matching_indices]) / cumulative_inflation
  
  
  contrib_price_iv <- sum(decomp_12m_sum_iv [, series, 2][matching_indices]) / cumulative_inflation
  contrib_gen_iv   <- sum(decomp_12m_sum_iv[, series, 1][matching_indices]) / cumulative_inflation
  
  
  # Return a tidy data frame (long format)
  tibble::tibble(
    Identification = rep(c("Cholesky", "IV"), each = 2),
    Contribution_Type = rep(c("$P_{elec}$", "$Q_{elec}$"), times = 2),
    #Contribution_Type = rep(c("Supply", "Demand"), times = 2),
    Contribution = 100 * c(contrib_price, contrib_gen, contrib_price_iv, contrib_gen_iv)
  )
}
get_12_gliss <- function(y) {
  decomp_12m_sum <- y
  # Loop over variables and models to apply 12-month rolling sum on the time dimension
  for (j in seq(dim(y)[2])) {
    for (k in seq(dim(y)[3])) {
      decomp_12m_sum[, j, k] <- slide_dbl(
        .x = y[, j, k],
        .f = sum,
        .before = 11,
        .complete = TRUE
      )
    }
  }
  return(decomp_12m_sum)
}


# Compute contributions for 2021 Q3 to 2022 Q3
contribution_data <- compute_contributions_gliss(
  start_date = "2022-10-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide <- contribution_data %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )


save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)

hd_cholesky <- readRDS( "hd_chol_5v.rds")
historical_decomp_iv_tibo <- readRDS( "hd_iv_5v.rds")


contribution_data <- compute_contributions_gliss(
  start_date = "2022-10-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide_mp <- contribution_data %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )


save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)

historical_decomp_iv_tibo_double <- readRDS("hd_iv_4v_double.rds")
hd_cholesky_double <- readRDS( "hd_chol_4v.rds")



# Compute contributions for 2021 Q3 to 2022 Q3
contribution_data <- compute_contributions_gliss(
  start_date = "2022-10-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo_double,
  series = 4
)

contribution_table_wide_double <- contribution_data %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )



save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)

historical_decomp_iv_tibo_double <- readRDS("hd_iv_5v_double.rds")
hd_cholesky_double <- readRDS( "hd_chol_5v.rds")



# Compute contributions for 2021 Q3 to 2022 Q3
contribution_data <- compute_contributions_gliss(
  start_date = "2022-10-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo_double,
  series = 4
)

contribution_table_wide_double_mp <- contribution_data %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )



# Add model identifiers
contribution_table_wide <- contribution_table_wide %>%
  mutate(Model = "Baseline")

contribution_table_wide_mp <- contribution_table_wide_MP %>%
  mutate(Model = "MP")

contribution_table_wide_double <- contribution_table_wide_double %>%
  mutate(Model = "Baseline")

contribution_table_wide_double_mp <- contribution_table_wide_double_MP %>%
  mutate(Model = "MP")

contribution_combined <- bind_rows(
  contribution_table_wide,
  contribution_table_wide_MP,
  contribution_table_wide_double %>% 
    filter(Identification == "IV") %>% 
    mutate(Identification = "Joint IV"),
  contribution_table_wide_double_MP %>% 
    filter(Identification == "IV") %>% 
    mutate(Identification = "Joint IV")
) %>%
  relocate(Model, Identification) %>%
  arrange(Model, Identification)


# Reformat
contribution_pivoted <- contribution_combined %>%
  pivot_wider(
    id_cols = Identification,
    names_from = Model,
    values_from = c("$P_{elec}$", "$Q_{elec}$")
  )

colnames(contribution_pivoted) <- c(
  "Identification",
  "$P_{elec}$",
  "$P_{elec}$ MP",
  "$Q_{elec}$",
  "$Q_{elec}$ MP"
)


gt_table_MP_mp <- contribution_pivoted %>%
  dplyr::select(
    Identification,
    "$P_{elec}$",
    "$Q_{elec}$",
    "$P_{elec}$ MP",
    "$Q_{elec}$ MP"
  ) %>% 
  gt(rowname_col = "Identification") %>%
  fmt_number(columns = everything(), decimals = 1) %>%
  tab_spanner(
    label = "Baseline",
    columns = c("$P_{elec}$","$Q_{elec}$")
  ) %>%
  tab_spanner(
    label = "MP",
    columns = c("$P_{elec}$ MP","$Q_{elec}$ MP")
  ) %>%
  tab_header(
    title = "Shock Contributions to Electricity Price and Quantity",
    subtitle = "Comparison across instrument strategies"
  ) %>%
  # cols_label(
  #   "$P_{elec}$" = md("`$P_{elec}$`"),
  #   "$Q_{elec}$" = md("`$Q_{elec}$`"),
  #   "$P_{elec}$ MP" = md("`$P_{elec}$`"),
  #   "$Q_{elec}$ MP" = md("`$Q_{elec}$`")
  # ) %>%
  cols_label(
    "$P_{elec}$" = md("Supply"),
    "$Q_{elec}$" = md("Demand"),
    "$P_{elec}$ MP" = md("Supply"),
    "$Q_{elec}$ MP" = md("Demand")
  ) %>%
  tab_options(
    table.font.size = "small"
  )

gt_table_MP_mp




