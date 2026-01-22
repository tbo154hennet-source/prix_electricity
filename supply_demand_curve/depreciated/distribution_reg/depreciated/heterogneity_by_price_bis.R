



library(arrow)
library(dplyr)
library(purrr)
library(gt)

# --- 1. Charger les datasets ---
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")

local_slopes_demand_5   <- read_parquet("local_slopes_demand_5.parquet")
local_slopes_demand_10  <- read_parquet("local_slopes_demand.parquet")
local_slopes_demand_15  <- read_parquet("local_slopes_demand_15.parquet")
local_slopes_demand_pond <- read_parquet("local_slopes_demand_pond.parquet")


local_slopes_demand_5 %>% 
  summarise(n_na_slope = sum(is.na(slope)))
local_slopes_demand_5 <- local_slopes_demand_5%>% 
  filter(!is.na(slope))

local_slopes_demand_pond %>% 
  summarise(n_na_slope = sum(!is.finite(slope)))

local_slopes_demand_pond <- local_slopes_demand_pond%>% 
  filter(!is.na(slope))
# local_slopes_demand_pond <- local_slopes_demand_pond %>% 
#   mutate(ifelse = is.na(slope), 0, slope)

# --- 1. Charger les datasets ---
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")

local_slopes_supply_5   <- read_parquet("local_slopes_supply_5.parquet")
local_slopes_supply_10  <- read_parquet("local_slopes_supply.parquet")
local_slopes_supply_15  <- read_parquet("local_slopes_supply_15.parquet")
local_slopes_supply_pond <- read_parquet("local_slopes_supply_pond.parquet")
local_slopes_supply_5  <- local_slopes_supply_5 %>% 
  filter(!is.na(slope))



datasets_demand <- list(
  demand_5   = local_slopes_demand_5,
  demand_10  = local_slopes_demand_10,
  demand_15  = local_slopes_demand_15
  #demand_pond = local_slopes_demand_pond
)

datasets_supply <- list(
  supply_5   = local_slopes_supply_5,
  supply_10  = local_slopes_supply_10,
  supply_15  = local_slopes_supply_15
  #demand_pond = local_slopes_demand_pond
)
#data = local_slopes_supply_10
Z_vars <- "shock_demand_unmatched_orth"

price_indispo_merge_feols_all <- read_parquet("price_indispo_merge_feols_all.parquet")


# --- 2. Fonction d’estimation (copiée/adaptée) ---
estimate_eta_components <- function(data, Z_vars) {
  local_slopes_reg <- merge(data,
                            price_indispo_merge_feols_all %>% 
                              select(price, load_tot, delivery_date, Hour,
                                     year, month, weekday,
                                     cols_fitted_bis),
                            by.x = c("DATE", "Hour"), by.y = c("delivery_date", "Hour")
  ) %>% 
    mutate(log_price = log(price + 1)) 

  fs_formula <- as.formula(
    paste0("log_price ~ ", paste(Z_vars, collapse = " + "),
           " + factor(year) + factor(month) + factor(weekday) + factor(Hour)")
  )
  
  fs <- feols(fs_formula, data = local_slopes_reg)
  local_slopes_reg <- local_slopes_reg %>%
    mutate(P_hat = fitted(fs))
  
  FE_coarse_fit <- lm(
    intercept ~ factor(year) + factor(month) + factor(weekday) + factor(Hour),
    data = local_slopes_reg,
    subset = !is.na(intercept) & !is.na(year) & !is.na(month) & !is.na(weekday) & !is.na(Hour)
  )
  
  local_slopes_reg <- local_slopes_reg %>%
    mutate(
      FE_coarse = if_else(!is.na(intercept) & !is.na(year) & !is.na(month) & !is.na(weekday) & !is.na(Hour),
                          fitted(FE_coarse_fit),
                          NA_real_),
      FE_diff = intercept - FE_coarse,
      eps = log(load_tot) - intercept - slope * log_price
    )
  
  
  # FE_coarse_fit <- lm(intercept ~ factor(year) + factor(month) + factor(weekday) + factor(Hour), data = local_slopes_reg)
  # local_slopes_reg <- local_slopes_reg %>%
  #   mutate(
  #     FE_coarse = fitted(FE_coarse_fit),
  #     FE_diff = intercept - FE_coarse,
  #     eps = log(load_tot) - intercept - slope * log_price
  #   )
  # 
  cov_slope_part <- cov(local_slopes_reg$P_hat, local_slopes_reg$slope * local_slopes_reg$log_price, use = "complete.obs")
  cov_FE_part    <- cov(local_slopes_reg$P_hat, local_slopes_reg$FE_diff, use = "complete.obs")
  cov_eps_part   <- cov(local_slopes_reg$P_hat, local_slopes_reg$eps, use = "complete.obs")
  denom          <- cov(local_slopes_reg$P_hat, local_slopes_reg$log_price, use = "complete.obs")
  
  eta_2sls_empirical <- (cov_slope_part + cov_FE_part + cov_eps_part) / denom
  
  list(
    eta_mean = mean(local_slopes_reg$slope, na.rm = TRUE),
    eta_weighted = cov_slope_part / denom,
    eta_2sls_empirical = eta_2sls_empirical
  )
}



# --- Fonction pour résumer une liste de datasets ---
process_dataset_list <- function(datasets, Z_vars, type_name) {
  results_list <- map(datasets, ~ estimate_eta_components(.x, Z_vars))
  
  summary_df <- map_dfr(names(results_list), function(name) {
    res <- results_list[[name]]
    data.frame(
      dataset = name,
      eta_mean = res$eta_mean,
      eta_weighted = res$eta_weighted,
      eta_2sls_empirical = res$eta_2sls_empirical
    )
  })
  
  # Extraire Type et price_bracket automatiquement
  summary_df <- summary_df %>%
    separate(dataset, into = c("Type_tmp", "price_bracket"), sep = "_", convert = TRUE) %>%
    mutate(Type = type_name) %>%
    select(Type, price_bracket, eta_mean, eta_weighted, eta_2sls_empirical)
  
  return(summary_df)
}

# --- Appliquer aux datasets demand et supply ---
summary_df_demand <- process_dataset_list(datasets_demand, Z_vars = c("shock_supply_unmatched_orth"), type_name = "demand")
summary_df_supply <- process_dataset_list(datasets_supply, Z_vars = c("shock_demand_unmatched_orth"), type_name = "supply")

summary_df_demand_instru <- process_dataset_list(datasets_demand, Z_vars = c("fit_supply_unmatched_orth"), type_name = "demand")
summary_df_supply_instru <- process_dataset_list(datasets_supply, Z_vars = c("fit_demand_unmatched_orth"), type_name = "supply")

# --- Combiner les deux ---
summary_df_all <- bind_rows(summary_df_demand %>% 
                              mutate(Instrument = "Shock"), 
                            summary_df_supply%>% 
                              mutate(Instrument = "Shock"),
                            summary_df_demand_instru%>% 
                              mutate(Instrument = "Instrument"), 
                            summary_df_supply_instru%>% 
                              mutate(Instrument = "Instrument"))

summary_df_new <- rbind(summary_df_all %>% 
  filter(Instrument=="Shock") %>% 
  rename(eta = eta_mean)%>% 
    mutate(Instrument = "Local_mean") %>% 
    select(eta, Instrument, price_bracket, Type) ,
summary_df_all %>% 
  filter(Instrument=="Shock") %>% 
  rename(eta = eta_2sls_empirical)%>% 
  select(eta, Instrument, price_bracket, Type),
summary_df_all %>% 
  filter(Instrument=="Instrument") %>% 
  rename(eta = eta_2sls_empirical)%>% 
  select(eta, Instrument, price_bracket, Type))

# --- Reformat wide ---
# Pivot to wide format and remove Type
df_wide <- summary_df_new %>%
  pivot_wider(names_from = Instrument, values_from = eta) %>%
  select(Type, price_bracket, Local_mean, Shock, Instrument) %>%
  arrange(Type, price_bracket)

# Create formatted Type variable for LaTeX grouping
df_wide$Type <- ifelse(df_wide$Type == "demand", "Demand", "Supply")

# Create HTML table
df_wide %>%
  kbl(
    format = "html",
    col.names = c(
      "Type",
      "Price bracket",
      "&bar;(η<sub>t</sub>)",
      "η<sup>2SLS</sup><sub>Shock</sub>",
      "η<sup>2SLS</sup><sub>Instrument</sub>"
    ),
    caption = "Elasticity Estimates by Type",
    escape = FALSE
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  collapse_rows(columns = 1, valign = "middle")

# Generate LaTeX kable table
latex_table <- df_wide %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    col.names = c(
      "Type",
      "Price bracket",
      "$\\bar{\\eta_t}$",
      "$\\eta^{2SLS}_{Shock}$",
      "$\\eta^{2SLS}_{Instrument}$"
    ),
    caption = "Elasticity Estimates by Type",
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    position = "center"
  ) %>%
  collapse_rows(columns = 1, valign = "middle")


# --- Export LaTeX ---
save_file <- "Z:/Overleaf_elec/images/post_seminaire/eta_wide_table.tex"
cat(as.character(latex_table), file = save_file)




