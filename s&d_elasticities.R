library(vars)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("auxiliary_control.R")
source("bootstrap_auxiliary_control_bis.R")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)

IV_data_log <- read_parquet('low_freq_data_log.parquet')

colnames(IV_data_log)
my_plot <- ggplot(data = IV_data_log) +
  geom_point(aes(x = gen_tot_log, y = spot_price_log)) +
  labs(title = "Phase diagram",
       x = "Quantity",
       y = "Price") +
  theme_minimal()

my_plot


Sigma_eta = cov(res)

# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)
# Retrieve original variances
original_variances <- diag(Sigma_eta)

# Adjust for scaling (unscaled structural shocks)
unscaled_structural_shocks <- structural_shocks %*% diag(sqrt(original_variances))
#get the var admissible elasticities
sigma_eps <- cov(unscaled_structural_shocks)

#determine elasticities from the var model
#More formally, given values for the entries of Σε, 
#the variance-covariance structure of the VAR imposes a unique relationship between ηS and ηD.




#use IV method to determine elasticities

colnames(IV_data_log)
library(systemfit)

# Define the equations
demand <- gen_tot_log ~ spot_price_log + T_avg_log
supply <- gen_tot_log ~ spot_price_log + gas_supply_shock

# Combine into a system
system <- list(demand = demand, supply = supply)

# Fit the system
fit <- systemfit(system, data = IV_data_log, method = "2SLS", inst = ~ T_avg_log + gas_supply_shock)

summary(fit)


# First-stage regression for price
first_stage_manual <- lm( spot_price_log ~  T_avg_log + gas_supply_shock, data = IV_data_log)

# Get the F-statistic
summary(first_stage_manual)$fstatistic



# Get the coefficients
my_coefficients <- coefficients(fit)
eta_D <- my_coefficients[2]
eta_S <- my_coefficients[5]

# Results
cat("Supply elasticity (eta_S):", eta_S, "\n")
cat("Demand elasticity (eta_D):", eta_D, "\n")

my_plot <- ggplot(data = IV_data_log) +
  geom_point(aes(x = spot_price_log, y = gen_tot_log)) +
  geom_abline(intercept = 0, slope = eta_S, color = "red") +
  geom_abline(intercept = 0, slope = eta_D, color = "red") +
  labs(title = "Phase diagram",
       x = "Quantity",
       y = "Price") +
  theme_minimal()


my_plot
