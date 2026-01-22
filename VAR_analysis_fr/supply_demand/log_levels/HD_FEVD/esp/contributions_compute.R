library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(latex2exp)


save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)
historical_decomp_iv_tibo <- readRDS( "hd_iv_log.rds")
historical_decomp_iv_wedge <- readRDS( "hd_iv_log_incorpo.rds")

dates_formatted <- as.yearmon(time(x_demand))

historical_decomp_matrix <- historical_decomp_iv_wedge
decomp_tot_data <- data.frame(
  time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
  historical_decomp_matrix[,series,])
# Plot the decomposition
colnames(decomp_tot_data)[-1] <-  c("Supply", "Demand")
decomp_tot_data <- decomp_tot_data %>%
  mutate(index = row_number())

k <- 12  # horizon 12 mois

pi_ann_series <- (IV_data_log_esp$HICP_log[(k+1):(T+p)] - IV_data_log_esp$HICP_log[1:((T+p)-k)]) 

# Colonnes de chocs/contributions (tout sauf time et index)
comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))

decomp_infl <- decomp_tot_data %>%
  arrange(time) %>%
  # Différence à 12 mois de chaque contribution
  mutate(across(all_of(comp_cols),
                ~ . - dplyr::lag(., 1),
                .names = "{.col}")) %>%
  # Supprimer les 12 premières lignes (NA)
  filter(!is.na(.data[[comp_cols[1]]])) 


decomp_tot_data <- data.frame(
  time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
  historical_decomp_matrix[,1,])
# Plot the decomposition
colnames(decomp_tot_data)[-1] <- names
decomp_tot_data <- decomp_tot_data %>%
  mutate(index = row_number())
# Colonnes de chocs/contributions (tout sauf time et index)
comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))

decomp_price <- decomp_tot_data %>%
  arrange(time) %>%
  # Différence à 12 mois de chaque contribution
  mutate(across(all_of(comp_cols),
                ~ . - dplyr::lag(., 1),
                .names = "{.col}")) %>%
  # Supprimer les 12 premières lignes (NA)
  filter(!is.na(.data[[comp_cols[1]]]))


# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest

# Example: get contribution of electricity price shocks to inflation

dates_formatted <- as.yearmon(time(x_demand))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]

start_date = "2021-01-01"
end_date = "2022-06-01"
# Generate the date sequence and convert to yearmon
desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
desired_period_yearmon <- as.yearmon(desired_period)
# Get matching indices
matching_indices_price <- which(dates_formatted %in% desired_period_yearmon)

start_date = "2021-01-01"
end_date = "2022-06-01"
# Generate the date sequence and convert to yearmon
desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
desired_period_yearmon <- as.yearmon(desired_period)
# Get matching indices
matching_indices_inflation <- which(dates_formatted %in% desired_period_yearmon)


HICP_log_diff <- IV_data_log_esp$HICP_log - lag(IV_data_log_esp$HICP_log)
price_log_diff <- IV_data_log_esp$spot_price_log- lag(IV_data_log_esp$spot_price_log)


# Total inflation during the period
cumulative_inflation <- sum(HICP_log_diff[matching_indices_inflation + p])
cumulative_price <- sum(price_log_diff[matching_indices_price + p])

contributions_inflation <- colSums(decomp_infl[matching_indices_inflation - 1,  c("Supply", "Demand")], na.rm = TRUE)
contributions_inflation_pp <- contributions_inflation/cumulative_inflation
contributions_inflation <- 100 * (exp(contributions_inflation) - 1)

contributions_price <- colSums(decomp_price[matching_indices_price - 1,  c("Supply", "Demand")], na.rm = TRUE)
contributions_price_pp <- contributions_price/cumulative_price
contributions_price <- 100 * (exp(contributions_price) - 1)

contributions_inflation
contributions_inflation_pp
contributions_price
contributions_price_pp

# 1) Construire la table de base
tab_df <- tibble(
  Section = c("Inflation", "Inflation", "Elec Price", "Elec Price"),
  Measure = c("points", "share", "points", "share"),
  Demand  = c(
    contributions_inflation["Demand"],
    contributions_inflation_pp["Demand"],
    contributions_price["Demand"],
    contributions_price_pp["Demand"]
  ),
  Supply  = c(
    contributions_inflation["Supply"],
    contributions_inflation_pp["Supply"],
    contributions_price["Supply"],
    contributions_price_pp["Supply"]
  )
)

# 2) Créer la table gt
gt_tab <- tab_df %>%
  gt(groupname_col = "Section") %>%   # <- sections horizontales
  
  # Renommer colonnes
  cols_label(
    Measure = "",
    Demand  = "Demand",
    Supply  = "Supply"
  ) %>%
  
  # Format numérique
  fmt_number(
    columns = c(Demand, Supply),
    decimals = 4
  ) %>%
  
  # Renommer les groupes (sections)
  tab_stubhead(label = "") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  
  # Lignes de séparation propres
  opt_row_striping() %>%
  opt_table_outline()

gt_tab


save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_tab,
  filename = "cumulative_contributions.tex"
)

#robustness
historical_decomp_matrix <- historical_decomp_iv_tibo

# > contributions_inflation
# Supply    Demand 
# 1.0399704 0.8694183 
# > contributions_inflation_pp
# Supply     Demand 
# 0.09008065 0.07537142 
# > contributions_price
# Supply    Demand 
# 41.62992 -11.13152 
# > contributions_price_pp
# Supply      Demand 
# 0.28527172 -0.09672733 
