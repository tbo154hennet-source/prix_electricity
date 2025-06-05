library(gt)
library(dplyr)
library(tibble)

# Données
df <- tribble(
  ~Market, ~Announcement, ~Nuclear, ~Thermal, ~Predicted,
  "Day-ahead", "Announced", "0.142*** (0.023)", "-0.012*** (0.002)", "0.109*** (0.015)",
  "Day-ahead", "Unannounced", "-0.026 (0.050)", "0.020* (0.023)", "0.062** (0.027)",
  "Intraday", "Announced", "0.027*** (0.009)", "0.009 (0.008)", "0.146*** (0.059)",
  "Intraday", "Unannounced", "-0.002 (0.089)", "0.015 (0.011)", "0.042 (0.047)"
)

# Création du tableau
table_unavail <- df %>%
  gt(rowname_col = "Announcement", groupname_col = "Market") %>%
  tab_header(
    title = "Plant Unavailability Price elasticity"
  ) %>%
  cols_label(
    Nuclear = "Nuclear",
    Thermal = "Thermal",
    Predicted = "Predicted"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(
    row_group.as_column = FALSE,             # fusion verticale automatique
    row_group.font.weight = "bold",
    table.font.size = 14,
    table.align = "center",
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#F9F9F9",
    data_row.padding = px(5),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.align = "center"
  ) %>%
  opt_table_lines()  # active les lignes horizontales



# Données du second tableau
df2 <- tribble(
  ~Market, ~Plant_Unavailability, ~Renewables, ~Crossborder_Flows,
  "Day-ahead", "0.109*** (0.015)", "-0.048*** (0.001)", "-0.029*** (0.004)",
  "Intraday", "0.146** (0.059)", "-0.040*** (0.009)", "-"
)

# Création du tableau gt
table_all <- df2 %>%
  gt(rowname_col = "Market") %>%
  tab_header(
    title = "Price Elasticity by Event Type"
  ) %>%
  cols_label(
    Plant_Unavailability = "Plant Unavailability",
    Renewables = "Renewables",
    Crossborder_Flows = "Crossborder Flows"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(
    table.font.size = 14,
    row_group.as_column = FALSE,
    row_group.font.weight = "bold",
    table.align = "center",
    data_row.padding = px(5),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.align = "center"
  ) %>%
  opt_table_lines()

# Données du second tableau
df3 <- tribble(
  ~Market, ~Cold_Excess, ~Heat_Excess,
  "Day-ahead", "0.007*** (0.0003)", "0.012*** (0.0003)"
)

# Création du tableau gt
table_demand <- df3 %>%
  gt(rowname_col = "Market") %>%
  tab_header(
    title = "Demand Elasticity by Event Type"
  ) %>%
  cols_label(
    Cold_Excess = "Cold Excess",
    Heat_Excess = "Heat Excess"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(
    table.font.size = 14,
    row_group.as_column = FALSE,
    row_group.font.weight = "bold",
    table.align = "center",
    data_row.padding = px(5),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.align = "center"
  ) %>%
  opt_table_lines()


