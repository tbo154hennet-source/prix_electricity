

library(systemfit)

# Use your dataframe

# Define the equations
# Equation 1: Price determined by quantity and instruments
eqsupply <- log(price) ~ log(gen_tot)  +  delta_s_total

# Equation 2: Quantity determined by price and instruments
eqdemand <- log(gen_tot) ~ log(price) + delta_d_total


# Define system
system <- list(eqsupply = eqsupply, eqdemand = eqdemand)

# Define instruments: both shocks
inst <- ~ delta_s_total + delta_d_total


# 2SLS version
fit_2sls <- systemfit(system, data = price_indispo_merge_delta_clean, method = "2SLS", inst = inst)


summary(fit_2sls)


my_coefficients <- coefficients(fit_2sls)
eta_D <- my_coefficients[2]
eta_S <- my_coefficients[5]

print(paste("Elasticity of Demand (eta_D):", 1/eta_D))
print(paste("Elasticity of Supply (eta_S):", eta_S))



colSums(is.na(price_indispo_merge_delta_clean[, c("spot_gas",
     "delta_s_total", "delta_d_total", "gen_tot", "price")])) 


