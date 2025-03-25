
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("auxiliary_control.R")
source("bootstrap_auxiliary_control.R")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp')


IV_data_log_diff <- read_parquet('low_freq_data_log_diff.parquet')

colnames(IV_data_log_diff)

headers_proxies = c("cold_excess_instrument", 
            # "abs_gas_shock",                 
            # "rel_gas_shock" ,
            "heat_excess_instrument",        
            "renewable_generation_log_diff", 
            "tot_indispo_log_diff",          
            "nuclear_indispo_log_diff" 
            )

proxies <- ts(IV_data_log_diff[, c(headers_proxies)], frequency=12)

headers_var = c("gen_tot_log_diff",
                "spot_price_log_diff",
                "IPI_log_diff",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                "NE_Index_log_diff"
                )
vars<- ts(IV_data_log_diff[, c(headers_var)], frequency=12)
headers_controls = c("gas_price_log_diff"
)
controls <- ts(IV_data_log_diff[, c(headers_controls)], frequency=12)


p = 4
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2022,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
n <- ncol(y)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- ncol(proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)

matrix_12 <- rbind( diag(k) , zero_matrix)



estimate <- first_var_estimation(x,y,p, proxies, irhor)
theta_0_esp_gas <- estimate$theta_0
gamma <- estimate$gamma
phi <- estimate$phi
var_coef <- estimate$coef


AL <- do.call(cbind, lapply(var_coef, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef)


p <- 4  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)



from <- (n+1)
to <- n*p
to_bis <- n*(p-1)

lambda_0_esp_gas <- get_lambda_0(C, theta_0)

nboot<-100
gamma_estimate_GK <- gamma_bootstrap_GK(nboot, vars, theta_0_esp)
# Initialize variables

#attention ces bootstrap ne convergent que sous des hypothèses
#d'instruments forts

list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                     "ncvg_lambda_01", "ncvg_lambda_02")
sample_matrices_GK <- vector("list", length(list_of_samples))
cov_matrices <- vector("list", length(list_of_samples))
names(sample_matrices_GK) <- list_of_samples

# Extract and store bootstrapped samples
for (name in list_of_samples) {
  sample_matrices_GK[[name]] <- gamma_estimate_GK[[paste0("boot_", name)]]
}


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp')

saveRDS(sample_matrices_GK, "sample_matrices_GK_esp_gas.rds")

# Save bootstrapped samples
# Initialize a 3D array to store the quantiles for both lambda_01 and lambda_02
# Dimensions: (irhor, n, 2) for lower (10%) and upper (90%) quantiles
strong_instruments_lambda_GK <- array(NA, dim = c(2, irhor, n,  2)) 
dimnames(strong_instruments_lambda_GK ) <- list(
  c("lower", "upper"), NULL, NULL, c("lambda_01", "lambda_02")
)

# Calculate quantiles and store them in the array
quantile_probs <- c(0.05, 0.95)
print(class(sample_matrices_GK[[lambda_name]]))

for (lambda_idx in 1:2) {
  lambda_name <- paste0("ncvg_lambda_0", lambda_idx)
  quantiles <- apply(sample_matrices_GK[[lambda_name]],
                     c(1:2), function(x) quantile(x, probs = quantile_probs))
  
  strong_instruments_lambda_GK[,,,lambda_idx] <- array(quantiles, dim = c(irhor, n, 2))
}

#strong_instruments_lambda_GK["lower",,,"lambda_01"]

# Permuter les dimensions 1 et 3
strong_instruments_lambda_GK <- aperm(strong_instruments_lambda_GK, perm = c(2, 3, 1, 4))
dim(strong_instruments_lambda_GK)

#dim(weak_instruments_pointwise_GK)

strong_instruments_lambda_GK[,,"lower","lambda_01"]


strong_std_lambda1 <- apply(sample_matrices_GK$ncvg_lambda_01, c(1, 2), sd)

strong_std_lambda2 <- apply(sample_matrices_GK$ncvg_lambda_02, c(1, 2), sd)

c0 <- theta_0_esp_gas[1,2][[1]]
phi_c0 <- matrix(c(1,0, -c0, 1), nrow = 2, ncol = 2) %*% t(matrix_12)  %*% gamma
alpha <- 0.10

#rajouter un paramètre de précision de la grille avec une valeur par défaut

weak_bootstrap_result <- bootstrap_weak_instrument(gamma, c_0, phi_c0,
                                                   alpha, ngrid = 100, sample_matrices = sample_matrices_GK)


weak_instruments_pointwise_GK <- weak_bootstrap_result$weak_lambda_bounds_Wald_stat
#weak_instruments_pointwise_GK <- aperm(weak_instruments_pointwise_GK, perm = c(1, 2, 4, 3))
weak_instruments_pointwise_GK[hori, i,"lower","lambda_01"]

weak_instruments_bondferroni_GK <- weak_bootstrap_result$weak_lambda_bounds_bonferroni
#weak_instruments_bondferroni_GK <- aperm(weak_instruments_bondferroni_GK, perm = c(1, 2, 4, 3))
getwd()

saveRDS(weak_instruments_pointwise_GK, "weak_instruments_pointwise_GK_esp_gas.rds")
saveRDS(weak_instruments_bondferroni_GK, "weak_instruments_bondferroni_GK_esp_gas.rds")
saveRDS(strong_instruments_lambda_GK, "strong_instruments_lambda_GK_esp_gas.rds")

