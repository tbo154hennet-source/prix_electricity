

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



library(fixest)

fs_supply_orth <- feols(
  log(price+1) ~ delta_s_orth   + spot_gas | delivery_date + creneau,
  data = price_indispo_merge_delta_clean
)

# Predicted price
price_hat_supply <- fitted(fs_supply_orth)


fs_demand_orth <- feols(
  log(price+1) ~  delta_d_orth  + spot_gas |delivery_date + creneau,
  data = price_indispo_merge_delta_clean
)

price_hat_demand <- fitted(fs_demand_orth)

price_indispo_merge_delta_clean$price_hat_supply <- price_hat_supply
price_indispo_merge_delta_clean$price_hat_demand <- price_hat_demand


# Supply elasticity (2SLS)
ss_supply_orth <- feols(
  log(gen_tot_da) ~ price_hat_supply   | delivery_date + creneau,
  data = price_indispo_merge_delta_clean
)

# Demand elasticity (2SLS)
ss_demand_orth <- feols(
  log(gen_tot_da) ~ price_hat_demand  | delivery_date + creneau,
  data = price_indispo_merge_delta_clean
)
summary(ss_demand_orth)

etable(
  list(ss_supply,ss_supply_orth, ss_demand, ss_demand_orth),
  
  #se = list(coef_se_supply[, "Std. Error"], coef_se_demand[, "Std. Error"]),
  dict = c(
    "price_hat_supply" = "Log Price",
    "price_hat_demand" = "Log Price"
  ),
  # model.names = c("Supply", "Supply Ortho", 
  #                 "Demand", "Demand Ortho"),  
  title = "2SLS Elasticities with Newey-West SE"
)



# library(fixest)
# 
# fs_supply_orth <- feols(
#   log(price+1) ~ delta_s_ortho_total + spot_gas | delivery_date + creneau,
#   data =price_indispo_merge_delta_tot_clean
# )
# 
# # Predicted price
# price_hat_supply <- fitted(fs_supply_orth)
# 
# 
# fs_demand_orth <- feols(
#   log(price+1) ~  delta_d_ortho_total  + spot_gas | delivery_date + creneau,
#   data = price_indispo_merge_delta_tot_clean
# )
# 
# price_hat_demand <- fitted(fs_demand_orth)
# 
# price_indispo_merge_delta_tot_clean$price_hat_supply <- price_hat_supply
# price_indispo_merge_delta_tot_clean$price_hat_demand <- price_hat_demand
# 
# 
# # Supply elasticity (2SLS)
# ss_supply_orth <- feols(
#   log(gen_tot_da) ~ price_hat_supply   | delivery_date + creneau,
#   data = price_indispo_merge_delta_tot_clean
# )
# 
# 
# # Demand elasticity (2SLS)
# ss_demand_orth <- feols(
#   log(gen_tot_da) ~ price_hat_demand  | delivery_date + creneau,
#   data = price_indispo_merge_delta_tot_clean
# )
# 
# 
# etable(
#   list(ss_supply,ss_supply_orth, ss_demand, ss_demand_orth),
#   
#   #se = list(coef_se_supply[, "Std. Error"], coef_se_demand[, "Std. Error"]),
#   dict = c(
#     "price_hat_supply" = "Log Price",
#     "price_hat_demand" = "Log Price"
#   ),
#   # model.names = c("Supply", "Supply Ortho", 
#   #                 "Demand", "Demand Ortho"),  
#   title = "2SLS Elasticities with Newey-West SE"
# )

