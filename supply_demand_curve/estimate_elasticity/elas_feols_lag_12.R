

library(fixest)

fs_supply <- feols(
  log(price+1) ~ delta_s_total  + delta_d_total  + spot_gas | delivery_date + creneau,
  data = price_indispo_merge_delta_clean
)

# Predicted price
price_hat_supply <- fitted(fs_supply)


fs_demand <- feols(
  log(price+1) ~ delta_s_total  + delta_d_total  + spot_gas | delivery_date + creneau,
  data = price_indispo_merge_delta_clean
)

price_hat_demand <- fitted(fs_demand)

price_indispo_merge_delta_clean$price_hat_supply <- price_hat_supply
price_indispo_merge_delta_clean$price_hat_demand <- price_hat_demand




price_indispo_merge_delta_clean_panel <- price_indispo_merge_delta_clean %>% 
  group_by(delivery_date, creneau) %>% 
  mutate( n= n()) %>% 
  filter( n ==1)
# Refit with panel index so NW knows what to use
ss_supply <- feols(
  log(gen_tot_da) ~ price_hat_supply + delta_d_total + spot_gas | delivery_date + creneau,
  data = price_indispo_merge_delta_clean_panel,
  panel.id = ~ delivery_date + creneau # <-- panel index (time, id)
)

# Demand elasticity (2SLS)
ss_demand <- feols(
  log(gen_tot_da) ~ price_hat_demand +delta_s_total  + spot_gas | delivery_date + creneau,
  data = price_indispo_merge_delta_clean_panel,
  panel.id = ~ delivery_date + creneau
)


# Newey-West SE with lag = 24
coef_se_supply <- coeftable(ss_supply, vcov = NW(24))
coef_se_demand <- coeftable(ss_demand, vcov = NW(24))






# Construire les lags
lag_exog_s <- paste0( exog_instr_s, "_lag", 1:12)
lag_exog_d <- paste0(exog_instr_d, "_lag", 1:12)

colnames(price_indispo_merge_delta_clean)
#construire lags dans le dataset
price_indispo_merge_delta_clean <- price_indispo_merge_delta_clean %>%
  arrange(delivery_date, creneau) %>% 
  mutate(
    across(
      .cols = all_of(c(exog_instr_s, exog_instr_d)),
      .fns  = list(
        lag1 = ~ dplyr::lag(.x, 1),
        lag2 = ~ dplyr::lag(.x, 2),
        lag3 = ~ dplyr::lag(.x, 3),
        lag4 = ~ dplyr::lag(.x, 4),
        lag5 = ~ dplyr::lag(.x, 5),
        lag6 = ~ dplyr::lag(.x, 6),
        lag7 = ~ dplyr::lag(.x, 7),
        lag8 = ~ dplyr::lag(.x, 8),
        lag9 = ~ dplyr::lag(.x, 9),
        lag10 = ~ dplyr::lag(.x, 10),
        lag11 = ~ dplyr::lag(.x, 11),
        lag12 = ~ dplyr::lag(.x, 12)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()




# formule propre (juste un seul as.formula)
iv_formula_supply_lag12 <- as.formula(paste(
  "log(price+1) ~ delta_s_total + delta_d_total + spot_gas +",
  paste(c(lag_exog_s, lag_exog_d), collapse = " + ", "| delivery_date + creneau")
))

# estimation
fs_supply_lag12 <- feols(
  iv_formula_supply_lag12,
  data = price_indispo_merge_delta_clean,
  panel.id = ~ delivery_date + creneau
)

price_indispo_merge_delta_clean$price_hat_supply_lag12 <- NA
price_indispo_merge_delta_clean$price_hat_supply_lag12[obs(fs_supply_lag12)] <- fitted(fs_supply_lag12)


iv_formula_demand_lag12 <- as.formula(paste(
  "log(price+1) ~ delta_s_total + delta_d_total + spot_gas +",
  paste(c(lag_exog_s, lag_exog_d), collapse = " + ", "| delivery_date + creneau")
))

fs_demand_lag12 <- feols(
  iv_formula_demand_lag12,
  data = price_indispo_merge_delta_clean,
  panel.id = ~ delivery_date + creneau
)

price_indispo_merge_delta_clean$price_hat_demand_lag12 <- NA
price_indispo_merge_delta_clean$price_hat_demand_lag12[obs(fs_demand_lag12)] <- fitted(fs_demand_lag12)


# formule propre (juste un seul as.formula)
iv_formula_supply_lag12_ss <- as.formula(paste(
  "log(gen_tot_da) ~ price_hat_supply_lag12 + delta_d_total + spot_gas +",
  paste(c(lag_exog_s, lag_exog_d), collapse = " + "), "| delivery_date + creneau"
))


ss_supply_lag12 <- feols(
  iv_formula_supply_lag12_ss,
  data = price_indispo_merge_delta_clean,
  panel.id = ~ delivery_date + creneau
)

#coef_se_supply_lag12 <- coeftable(ss_supply_lag12, vcov = NW(24))

iv_formula_demand_lag12_ss <- as.formula(paste(
  "log(gen_tot_da) ~ price_hat_demand_lag12 + delta_s_total + spot_gas +",
  paste(c(lag_exog_s, lag_exog_d), collapse = " + "
), "| delivery_date + creneau"))


ss_demand_lag12 <- feols(
  iv_formula_demand_lag12_ss,
  data = price_indispo_merge_delta_clean,
  panel.id = ~ delivery_date + creneau
)


etable(
  list(ss_supply, ss_supply_lag12, ss_demand, ss_demand_lag12),
  dict = c(
    "price_hat_supply"       = "Log Price",
    "price_hat_supply_lag12" = "Log Price",
    "price_hat_demand"       = "Log Price",
    "price_hat_demand_lag12" = "Log Price"
  ),
  drop = "lag[0-9]+",   # <- supprime tous les coefficients contenant "lag1", "lag2", ... "lag12"
  title = "2SLS Elasticities with Newey-West SE"
)

