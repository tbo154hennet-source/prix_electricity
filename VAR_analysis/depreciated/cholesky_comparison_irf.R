
x_named <-x
colnames(x_named) <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$NE Index$")
vl = vars::VAR(x_named, p, type = "none")

# Compute impulse response functions (IRFs)
irf_model <- irf(vl,           # Your VAR model
                 impulse = NULL,   # All variables as shocks
                 response = NULL,  # All variables as responses
                 n.ahead = 19,     # Horizon
                 boot = TRUE,      # Bootstrapped confidence intervals
                 ci = 0.95)        # 95% confidence intervals
#names(irf_model$irf) <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$NE Index$")

par(mar = c(4, 4, 2, 2)) 
# Plot the IRFs
plot(irf_model)

# Manually scale the responses (e.g., shock of size 2)
scaled_irf <- irf_model
scaled_irf$irf <- lapply(irf_model$irf, function(x) x * 10) # Scale responses
scaled_irf$Lower <- lapply(irf_model$Lower, function(x) x * 10) # Scale CI lower
scaled_irf$Upper <- lapply(irf_model$Upper, function(x) x * 10) # Scale CI upper

# Plot the scaled IRFs
plot(scaled_irf)
