library(limSolve)
library(MASS)
library(future.apply)
library(progress)
library(future)
library(progressr)
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)
library(sandwich)
library(lmtest)
library(ggplot2)


# ensure it's a data.table
setDT(reconstruct_full_curve_shift_qty_supply_matched_local)


# Example grid
price_grid <- seq(2, 150, by = 1)

# Store results
elasticity_results <- data.table(price = price_grid, elasticity = NA_real_, se = NA_real_)
# Store results
elasticity_results_demand <- data.table(price = price_grid, elasticity = NA_real_, se = NA_real_)



# Loop over grid
for (i in seq_along(price_grid)) {
  p0 <- price_grid[i]
  
  # Keep local window (say +/- 20 around the grid price)
  local_data <- reconstruct_full_curve_shift_qty_supply_matched_local[
    PRICE >= p0 - 20 & PRICE <= p0
  ]
  
  if (nrow(local_data) > 50) {  # need enough obs
    # Regression with fixed effects
    fit <- feols(log(QTY) ~ log(PRICE) | DATE + Hour, data = local_data)
    
    # Extract coefficient on log(PRICE)
    elasticity_results[i, elasticity := coef(fit)[["log(PRICE)"]]]
    elasticity_results[i, se := sqrt(vcov(fit)[["log(PRICE)", "log(PRICE)"]])]
  }
}


# Loop over grid
for (i in seq_along(price_grid)) {
  p0 <- price_grid[i]
  
  # Keep local window (say +/- 20 around the grid price)
  local_data <- reconstruct_full_curve_shift_qty_demand_matched_local[
    PRICE >= p0 - 20 & PRICE <= p0 + 20
  ]
  
  if (nrow(local_data) > 50) {  # need enough obs
    # Regression with fixed effects
    fit <- feols(log(QTY) ~ log(PRICE) | DATE + Hour, data = local_data)
    
    # Extract coefficient on log(PRICE)
    elasticity_results_demand[i, elasticity := coef(fit)[["log(PRICE)"]]]
    elasticity_results_demand[i, se := sqrt(vcov(fit)[["log(PRICE)", "log(PRICE)"]])]
  }
}



ggplot() +
  geom_line(data = elasticity_results, aes(x = price, y = elasticity, color = "Supply")) +
  geom_ribbon(data =elasticity_results, aes(x = price, ymin = elasticity - 1.96*se,
                  ymax = elasticity + 1.96*se, fill = "Supply"),
              alpha = 0.2) +
  geom_line(data = elasticity_results_demand, aes(x = price, y = elasticity, color = "Demand")) +
  geom_ribbon(data =elasticity_results_demand, aes(x = price, ymin = elasticity - 1.96*se,
                                      ymax = elasticity + 1.96*se, fill = "Demand"),
              alpha = 0.2) +
  labs(
    title = "Local Price Elasticity of Supply",
    x = "Price (???/MWh)",
    y = "Elasticity"
  ) +
  theme_minimal()




#refaire plot supply/demand avec matched et unmatched curve


setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift_qty_demand_unmatched<- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')
# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_demand_unmatched)

reconstruct_full_curve_shift_qty_supply_unmatched<- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')


price_indispo_merge_esp %>% 
  filter(delivery_date == selected_day) %>% 
  dplyr::select(datetime, price, gen_tot_da)

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_supply_unmatched)
# Select one day
selected_day <- as.Date("2021-09-27")  # replace with your date

day_data_unmatched <- reconstruct_full_curve_shift_qty_demand_unmatched[DATE == selected_day & Hour ==9]
day_data_matched <- reconstruct_full_curve_shift_qty_demand_matched[DATE == selected_day & Hour ==9]

day_data_unmatched_bis <- reconstruct_full_curve_shift_qty_supply_unmatched[DATE == selected_day & Hour ==9]
day_data_matched_bis <- reconstruct_full_curve_shift_qty_supply_matched[DATE == selected_day & Hour ==9]


# Plot: Quantity vs Price
ggplot() +
  geom_line(data = day_data_unmatched, aes(x = QTY, y = PRICE, color = "Unmatched")) +
  geom_line(data = day_data_matched, aes(x = QTY, y = PRICE, color = "Matched")) +
  geom_line(data = day_data_unmatched_bis, aes(x = QTY, y = PRICE, color = "Unmatched")) +
  geom_line(data = day_data_matched_bis, aes(x =QTY, y = PRICE, color = "Matched")) +
  #geom_line(aes(group = Hour), alpha = 0.4, color = "darkblue") +
  labs(
    title = paste("Electricity Demand Curve on", selected_day),
    x = "Quantity (MWh)",
    y = "Price (???/MWh)"
  ) +
  theme_minimal(base_size = 14)


