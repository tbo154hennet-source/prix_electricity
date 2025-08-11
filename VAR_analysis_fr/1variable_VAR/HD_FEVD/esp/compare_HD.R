setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/contribution")


contribution_table_wide <- read_csv( "contribution_table_wide.csv")
contribution_table_wide_gas <- read_csv( "contribution_table_wide_gas.csv")
contribution_table_wide_double <- read_csv( "contribution_table_wide_double.csv")
contribution_table_wide_double_gas <- read_csv( "contribution_table_wide_double_gas.csv")


contribution_table_wide
contribution_table_wide_gas
contribution_table_wide_double
contribution_table_wide_double_gas



#unify in a single table


# Add model identifiers
contribution_table_wide <- contribution_table_wide %>%
  mutate(Model = "Baseline")

contribution_table_wide_gas <- contribution_table_wide_gas %>%
  mutate(Model = "Gas + MP")

contribution_table_wide_double <- contribution_table_wide_double %>%
  mutate(Model = "Baseline")

contribution_table_wide_double_gas <- contribution_table_wide_double_gas %>%
  mutate(Model = "Gas + MP")

contribution_combined <- bind_rows(
  contribution_table_wide,
  contribution_table_wide_gas,
  contribution_table_wide_double %>% 
    filter(Identification == "IV") %>% 
    mutate(Identification = "Joint IV"),
  contribution_table_wide_double_gas %>% 
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
  "$P_{elec}$ Gas",
  "$Q_{elec}$",
  "$Q_{elec}$ Gas"
)


gt_table_gas_mp <- contribution_pivoted %>%
  dplyr::select(
    Identification,
    "$P_{elec}$",
    "$Q_{elec}$",
    "$P_{elec}$ Gas",
    "$Q_{elec}$ Gas"
  ) %>% 
  gt(rowname_col = "Identification") %>%
  fmt_number(columns = everything(), decimals = 1) %>%
  tab_spanner(
    label = "Baseline",
    columns = c("$P_{elec}$","$Q_{elec}$")
  ) %>%
  tab_spanner(
    label = "Gas + MP",
    columns = c("$P_{elec}$ Gas","$Q_{elec}$ Gas")
  ) %>%
  tab_header(
    title = "Shock Contributions to Electricity Price and Quantity",
    subtitle = "Comparison across instrument strategies"
  ) %>%
  # cols_label(
  #   "$P_{elec}$" = md("`$P_{elec}$`"),
  #   "$Q_{elec}$" = md("`$Q_{elec}$`"),
  #   "$P_{elec}$ Gas" = md("`$P_{elec}$`"),
  #   "$Q_{elec}$ Gas" = md("`$Q_{elec}$`")
  # ) %>%
  cols_label(
    "$P_{elec}$" = md("Supply"),
    "$Q_{elec}$" = md("Demand"),
    "$P_{elec}$ Gas" = md("Supply"),
    "$Q_{elec}$ Gas" = md("Demand")
  ) %>%
  tab_options(
    table.font.size = "small"
  )

gt_table_gas_mp
# Save the table

save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table_gas_mp,
  filename = "contribution_inflation_with_gas_compare.tex"
)

