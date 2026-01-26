library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(latex2exp)
library(lubridate)
library(zoo)

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)
historical_decomp_iv_tibo <- readRDS( "hd_iv_log.rds")
historical_decomp_iv_wedge <- readRDS( "hd_iv_log_incorpo.rds")

dates_formatted <- as.yearmon(time(x_demand))

h <- 12  # horizon 12 mois
series <- 5
historical_decomp_matrix <- historical_decomp_iv_tibo
decomp_tot_data <- data.frame(
  time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
  historical_decomp_matrix[,series,])
# Plot the decomposition

decomp_tot_data <- decomp_tot_data %>%
  mutate(index = row_number())


pi_ann_series <- (IV_data_log_esp$HICP_log[(h+1):(T+p)] - IV_data_log_esp$HICP_log[1:((T+p)-h)]) 

names<- c("Pelec"   ,    "Qelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
# Colonnes de chocs/contributions (tout sauf time et index)
comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))



decomp_tot_data <- data.frame(
  time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
  historical_decomp_matrix[,1,])
# Plot the decomposition
colnames(decomp_tot_data)[-1] <-  c("Supply", "Demand")
decomp_tot_data <- decomp_tot_data %>%
  mutate(index = row_number())
# Colonnes de chocs/contributions (tout sauf time et index)
comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))
decomp_price <- decomp_tot_data %>%
  arrange(time) %>%
  # Différence à 12 mois de chaque contribution
  mutate(across(all_of(comp_cols),
                .names = "{.col}")) %>%
  # Supprimer les 12 premières lignes (NA)
  filter(!is.na(.data[[comp_cols[1]]]))




decomp_tot_data <- data.frame(
  time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
  historical_decomp_matrix[,5,])
# Plot the decomposition
colnames(decomp_tot_data)[-1] <-  c("Supply", "Demand")
decomp_tot_data <- decomp_tot_data %>%
  mutate(index = row_number())
# Colonnes de chocs/contributions (tout sauf time et index)
comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))


decomp_infl_12m <- decomp_tot_data %>%
  arrange(time) %>%
  # Différence à 12 mois de chaque contribution
  mutate(across(all_of(comp_cols),
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
library(zoo)

get_matching_indices <- function(dates_formatted, start_date, end_date) {
  desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  desired_period_yearmon <- as.yearmon(desired_period)
  which(dates_formatted %in% desired_period_yearmon)
}

periods <- list(
  price_up = c("2021-01-01", "2022-06-01"),
  inflation_up = c("2021-07-01", "2022-12-01"),
  price_down = c("2022-07-01", "2023-12-01"),
  inflation_down = c("2023-01-01", "2023-12-01")
)

matching_indices <- lapply(periods, \(x) get_matching_indices(dates_formatted, x[1], x[2]))

matching_indices_price          <- matching_indices$price_up
matching_indices_inflation      <- matching_indices$inflation_up
matching_indices_price_down     <- matching_indices$price_down
matching_indices_inflation_down <- matching_indices$inflation_down


HICP_log_diff <- IV_data_log_esp$HICP_log - lag(IV_data_log_esp$HICP_log)
price_log_diff <- IV_data_log_esp$spot_price_log- lag(IV_data_log_esp$spot_price_log)


library(data.table)
# Total inflation during the period
cumulative_inflation <- sum(HICP_log_diff[matching_indices_inflation + p])
cumulative_price <- sum(price_log_diff[matching_indices_price + p])



contributions_inflation <- colSums(decomp_infl_12m[matching_indices_inflation - 1,  c("Supply", "Demand")], na.rm = TRUE)
contributions_inflation_pp <- contributions_inflation/cumulative_inflation
N_inflation <- length(matching_indices_inflation)
infl_annualized_pct <- 100 * (exp(contributions_inflation)^(12/N_inflation) - 1)


contributions_price <- colSums(decomp_price[matching_indices_price - 1,  c("Supply", "Demand")], na.rm = TRUE)
contributions_price_pp <- contributions_price/cumulative_price
P0 <- mean(exp(IV_data_log_esp$spot_price_log[matching_indices_price + p]),
           na.rm = TRUE)
contrib_price_eur <- P0 * contributions_price
#contributions_price <- 100 * (exp(contributions_price) - 1)



cumulative_inflation_down <- sum(HICP_log_diff[matching_indices_inflation_down + p])
cumulative_price_down <- sum(price_log_diff[matching_indices_price_down + p])


contributions_inflation_down <- colSums(decomp_infl_12m[matching_indices_inflation_down - 1,  c("Supply", "Demand")], na.rm = TRUE)
contributions_inflation_pp_down <- contributions_inflation_down/cumulative_inflation_down
infl_annualized_pct_down <- 100 * (exp(contributions_inflation_down) - 1)

contributions_price_down <- colSums(decomp_price[matching_indices_price_down - 1,  c("Supply", "Demand")], na.rm = TRUE)
contributions_price_pp_down <- contributions_price_down/cumulative_price_down
P0 <- mean(exp(IV_data_log_esp$spot_price_log[matching_indices_price_down  + p]),
           na.rm = TRUE)
contrib_price_eur_down  <- P0 * contributions_price_down 
#contributions_price_down <- 100 * (exp(contributions_price_down) - 1)


contributions_inflation
#contributions_inflation_pp
infl_annualized_pct
contributions_price
#contributions_price_pp
contrib_price_eur

contributions_inflation_down
#contributions_inflation_pp_down
infl_annualized_pct_down
contributions_price_down
#contributions_price_pp_down
contrib_price_eur_down 


# 1) Construire la table de base
quarter_label <- function(x) {
  d <- as.Date(x)
  paste0(format(d, "%Y"), "Q", ((as.integer(format(d, "%m")) - 1) %/% 3) + 1)
}

make_period <- function(period) {
  start= period[[1]]
  end = period[[2]]
  paste0(quarter_label(start), " ", quarter_label(end))
}
library(gt)
tab_df <- bind_rows(
  # -------- UP (inflation / price up) --------
  tibble(
    Regime  = "Energy crisis",
    Section = c("Inflation", "Inflation", "Elec Price", "Elec Price"),
    Period  = c(
      make_period(period = periods$inflation_up),
      make_period(periods$inflation_up),
      make_period(periods$price_up),
      make_period(periods$price_up)
    ),
    Measure = c("log-points", "points", "log-points", "euros"),
    Demand = c(
      contributions_inflation["Demand"],
      infl_annualized_pct["Demand"],
      contributions_price["Demand"],
      contrib_price_eur["Demand"]
    ),
    Supply = c(
      contributions_inflation["Supply"],
      infl_annualized_pct["Supply"],
      contributions_price["Supply"],
      contrib_price_eur["Supply"]
    )
  ),
  
  # -------- DOWN (disinflation / price down) --------
  tibble(
    Regime  = "Post-Crisis Normalization",
    Section = c("Inflation", "Inflation", "Elec Price", "Elec Price"),
    Period  = c(
      make_period(period = periods$inflation_down),
      make_period(periods$inflation_down),
      make_period(periods$price_down),
      make_period(periods$price_down)
    ),
    Measure = c("log-points", "points", "log-points", "euros"),
    Demand = c(
      contributions_inflation_down["Demand"],
      infl_annualized_pct_down["Demand"],
      contributions_price_down["Demand"],
      contrib_price_eur_down["Demand"]
    ),
    Supply = c(
      contributions_inflation_down["Supply"],
      infl_annualized_pct_down["Supply"],
      contributions_price_down["Supply"],
      contrib_price_eur_down["Supply"]
    )
  )
) %>%
  # option: n'afficher la période qu'une fois par (Regime, Section)
  group_by(Regime, Section) %>%
  mutate(Period = if_else(row_number() == 1, Period, "")) %>%
  mutate(Section = if_else(row_number() == 1,  Section, "")) %>%
  ungroup()

gt_tab <- tab_df %>%
  gt(groupname_col = "Regime") %>%
  cols_label(
    Section = "",
    Measure = "",
    Period  = "Period",
    Demand  = "Demand",
    Supply  = "Supply"
  ) %>%
  fmt_number(columns = c(Demand, Supply), decimals = 4) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  opt_row_striping() %>%
  opt_table_outline() %>%
  cols_align(align = "center", columns = c(Period, Demand, Supply))

# 1) Construire la table de base

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
