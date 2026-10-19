# Backward-compatible contribution-table entry point.
# Estimation, date alignment, transformations, tables and figures are all
# produced by the same integrated pipeline.

pipeline_file <- file.path(
  'Z:/prix_electricite_gitlab', 'VAR_analysis_fr', 'supply_demand',
  'HD_FEVD', 'esp', 'compare_HD_with_without_gas.R'
)

if (!file.exists(pipeline_file)) {
  stop('Integrated HD pipeline not found: ', pipeline_file)
}

source(pipeline_file, local = FALSE)

# Share of electricity-induced inflation (or moderation) due to each shock.
# For normalization, signs are reversed: positive means more moderation.
electricity_inflation_summary <- table_long |>
  dplyr::filter(Section == 'Inflation', Measure == 'annualized percent') |>
  dplyr::mutate(
    Episode = dplyr::recode(
      Regime,
      'Energy crisis' = 'Inflation increase',
      'Post-Crisis Normalization' = 'Inflation moderation'
    ),
    Demand_effect = dplyr::if_else(
      Regime == 'Post-Crisis Normalization', -Demand, Demand
    ),
    Supply_effect = dplyr::if_else(
      Regime == 'Post-Crisis Normalization', -Supply, Supply
    ),
    Electricity_effect = Demand_effect + Supply_effect,
    Demand_share = dplyr::if_else(
      abs(Electricity_effect) > sqrt(.Machine$double.eps),
      100 * Demand_effect / Electricity_effect,
      NA_real_
    ),
    Supply_share = dplyr::if_else(
      abs(Electricity_effect) > sqrt(.Machine$double.eps),
      100 * Supply_effect / Electricity_effect,
      NA_real_
    )
  ) |>
  dplyr::select(
    Episode, Specification, Period, Electricity_effect,
    Demand_effect, Supply_effect, Demand_share, Supply_share
  )

readr::write_csv(
  electricity_inflation_summary,
  file.path(dirs[['tables']], 'electricity_inflation_summary.csv')
)

summary_gt <- electricity_inflation_summary |>
  gt::gt(groupname_col = 'Episode') |>
  gt::cols_label(
    Specification = 'Specification', Period = 'Period',
    Electricity_effect = 'Electricity effect',
    Demand_effect = 'Demand', Supply_effect = 'Supply',
    Demand_share = 'Demand share (%)', Supply_share = 'Supply share (%)'
  ) |>
  gt::fmt_number(columns = where(is.numeric), decimals = 2) |>
  gt::opt_row_striping() |>
  gt::opt_table_outline()

gt::gtsave(
  summary_gt,
  file.path(dirs[['tables']], 'electricity_inflation_summary.tex')
)

# Legacy object names retained for interactive notebooks.
tab_df <- contribution_table
historical_decomp_iv_tibo <- models[['Including Gas']][['hd']]
historical_decomp_iv_without_gas <- models[['Electricity Specific']][['hd']]

print(summary_gt)
message('Contribution tables and HD comparison figure updated.')
