



save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
# Create a data frame with all passthrough values
library(gt)

# Read passthrough values
passthrough_values <- list(
  esp = readRDS("gen_benchmark_esp_supply_with_crisis.rds")$lambda_0[12,4,2] ,
  fr  = readRDS("gen_benchmark_fr_supply_with_crisis.rds")$lambda_0[12,4,2] ,
  esp_core = readRDS("gen_benchmark_esp_mr_core.rds")$lambda_0[12,4,2] ,
  fr_core  = readRDS("gen_benchmark_fr_mr_core.rds")$lambda_0[12,4,2],
  esp_mp = readRDS("gen_benchmark_esp_mp_supply_with_crisis.rds")$lambda_0[12,4,2] ,
  fr_mp  = readRDS("gen_benchmark_fr_mp_supply_with_crisis.rds")$lambda_0[12,4,2] ,
  esp_mp_core = readRDS("gen_benchmark_esp_mp_mr_core.rds")$lambda_0[12,4,2] ,
  fr_mp_core  = readRDS("gen_benchmark_fr_mp_mr_core.rds")$lambda_0[12,4,2] 
)

# Create a wide format data frame
# Create a wide format data frame with valid column names
passthrough_df <- data.frame(
  Country = c("ESP", "FR"),
  Headline = c(passthrough_values$esp, passthrough_values$fr),
  `MP_Headline` = c(passthrough_values$esp_mp, passthrough_values$fr_mp),
  Core     = c(passthrough_values$esp_core, passthrough_values$fr_core),
  `MP_Core`     = c(passthrough_values$esp_mp_core, passthrough_values$fr_mp_core)
)


# Create gt table with multi-level headers
gt_table <- passthrough_df %>%
  gt() %>%
  tab_spanner(
    label = "Headline Inflation",
    columns = vars(Headline, MP_Headline)
  ) %>%
  tab_spanner(
    label = "Core Inflation",
    columns = vars(Core, MP_Core)
  )%>%
  # Rename sub-columns
  cols_label(
    Headline = "Baseline",
    MP_Headline = "MP",
    Core = "Baseline",
    MP_Core = "MP"
  ) %>%
  fmt_number(
    columns = vars(Headline, MP_Headline, Core, MP_Core),
    decimals = 5
  ) %>%
  tab_header(
    title = "One-Year Cumulative Pass-Through"
  )

# Display table
gt_table




# Compute passthrough values pre-crisis using the same denominators as baseline
passthrough_values_pre_crisis <- list(
  esp = readRDS("gen_benchmark_esp_pre_crisis_supply.rds")$lambda_0[12,4,2] ,
  fr  = readRDS("gen_benchmark_fr_pre_crisis_supply.rds")$lambda_0[12,4,2] ,
  esp_core = readRDS("gen_benchmark_esp_pre_crisis_mr_core.rds")$lambda_0[12,4,2] ,
  fr_core  = readRDS("gen_benchmark_fr_pre_crisis_mr_core.rds")$lambda_0[12,4,2],
  esp_mp = readRDS("gen_benchmark_esp_mp_pre_crisis_supply.rds")$lambda_0[12,4,2],
  fr_mp  = readRDS("gen_benchmark_fr_mp_pre_crisis_supply.rds")$lambda_0[12,4,2],
  esp_mp_core = readRDS("gen_benchmark_esp_mp_pre_crisis_mr_core.rds")$lambda_0[12,4,2],
  fr_mp_core  = readRDS("gen_benchmark_fr_mp_pre_crisis_mr_core.rds")$lambda_0[12,4,2]
)


# Combine baseline and pre-crisis into a single data frame
passthrough_df_combined <- data.frame(
  Country = rep(c("ESP", "FR"), 2),
  Period = c(rep("Including Crisis", 2), rep("Pre-Crisis", 2)),
  Identification = c(rep("MR",4)),
  Baseline_Headline = c(passthrough_values$esp, passthrough_values$fr, passthrough_values_pre_crisis$esp, passthrough_values_pre_crisis$fr),
  MP_Headline       = c(passthrough_values$esp_mp, passthrough_values$fr_mp, passthrough_values_pre_crisis$esp_mp, passthrough_values_pre_crisis$fr_mp),
  Baseline_Core     = c(passthrough_values$esp_core, passthrough_values$fr_core, passthrough_values_pre_crisis$esp_core, passthrough_values_pre_crisis$fr_core),
  MP_Core           = c(passthrough_values$esp_mp_core, passthrough_values$fr_mp_core, passthrough_values_pre_crisis$esp_mp_core, passthrough_values_pre_crisis$fr_mp_core)
)


# Create gt table
gt_table_clean <- passthrough_df_combined %>%
  dplyr::select(-Identification, -Period) %>% 
  gt(rowname_col = "Country") %>%
  # Top-level headers
  tab_spanner(label = "Headline Inflation", columns = vars(Baseline_Headline, MP_Headline)) %>%
  tab_spanner(label = "Core Inflation", columns = vars(Baseline_Core, MP_Core)) %>%
  # Rename sub-columns
  cols_label(
    Baseline_Headline = "Baseline",
    MP_Headline       = "MP",
    Baseline_Core     = "Baseline",
    MP_Core           = "MP"
  ) %>%
  # Row groups by period
  tab_row_group(
    label = "Pre-Crisis",
    #rows = which(passthrough_df_combined$Period == "Pre-Crisis")
    rows = c(3,4)
  ) %>%
  tab_row_group(
    label = "Including Crisis",
    rows = c(1,2)
    #rows = which(passthrough_df_combined$Period == "Including Crisis")
  ) %>%
  # Format numbers
  fmt_number(
    columns = vars(Baseline_Headline, MP_Headline, Baseline_Core, MP_Core),
    decimals = 3
  ) %>%
  # Table title
  tab_header(title = "One-Year Cumulative Pass-Through")

# Display table

gt_table_clean


save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)
# Read passthrough values
passthrough_values_supply <- list(
  # Espagne
  esp_pre_crisis       = readRDS("gen_benchmark_esp_pre_crisis_supply_sd.rds")$lambda_0[12,4,1],
  esp_with_crisis      = readRDS("gen_benchmark_esp_supply_with_crisis_sd.rds")$lambda_0[12,4,1],
  esp_mp_pre_crisis    = readRDS("gen_benchmark_esp_mp_pre_crisis_supply_sd.rds")$lambda_0[12,4,1],
  esp_mp_with_crisis   = readRDS("gen_benchmark_esp_mp_supply_with_crisis_sd.rds")$lambda_0[12,4,1],
  esp_core_pre_crisis  = readRDS("gen_benchmark_esp_pre_crisis_supply_core_sd.rds")$lambda_0[12,4,1],
  esp_core_with_crisis = readRDS("gen_benchmark_esp_supply_with_crisis_core_sd.rds")$lambda_0[12,4,1],
  esp_mp_core_pre_crisis  = readRDS("gen_benchmark_esp_mp_pre_crisis_supply_core_sd.rds")$lambda_0[12,4,1],
  esp_mp_core_with_crisis = readRDS("gen_benchmark_esp_mp_supply_with_crisis_core_sd.rds")$lambda_0[12,4,1]
  
)


passthrough_df_combined_plus <- rbind(
  passthrough_df_combined,
  data.frame(
  Country = rep(c("ESP"), 2),
  Period = c(rep("Including Crisis"), rep("Pre-Crisis")),
  Identification = c(rep("SD",2)),
  Baseline_Headline = c(passthrough_values_supply$esp_with_crisis, passthrough_values_supply$esp_pre_crisis),
  MP_Headline       = c(passthrough_values_supply$esp_mp_with_crisis, passthrough_values_supply$esp_mp_pre_crisis),
  Baseline_Core     = c(passthrough_values_supply$esp_core_with_crisis,  passthrough_values_supply$esp_core_pre_crisis),
  MP_Core           = c(passthrough_values_supply$esp_mp_core_with_crisis,  passthrough_values_supply$esp_mp_core_pre_crisis)
)
)



gt_table_clean <- passthrough_df_combined_plus %>%
  dplyr::select( -Country) %>% 
  gt(rowname_col = "Period") %>%
  # Top-level headers
  tab_spanner(label = "Headline Inflation", columns = vars(Baseline_Headline, MP_Headline)) %>%
  tab_spanner(label = "Core Inflation", columns = vars(Baseline_Core, MP_Core)) %>%
  # Rename sub-columns
  cols_label(
    Baseline_Headline = "Baseline",
    MP_Headline       = "MP",
    Baseline_Core     = "Baseline",
    MP_Core           = "MP"
  ) %>%
  # Row groups by period
  tab_row_group(
    label = "ESP",
    #rows = which(passthrough_df_combined$Period == "Pre-Crisis")
    rows = c(1,3,5,6)
  ) %>%
  tab_row_group(
    label = "FR",
    rows = c(2,4)
    #rows = which(passthrough_df_combined$Period == "Including Crisis")
  ) %>%
  # Format numbers
  fmt_number(
    columns = vars(Baseline_Headline, MP_Headline, Baseline_Core, MP_Core),
    decimals = 3
  ) %>%
  # Table title
  tab_header(title = "One-Year Cumulative Pass-Through")

# Display table

gt_table_clean

save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table_clean,
  filename = "pass_through_table.tex"
)



