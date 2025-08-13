library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("auxiliary_control.R")
source("bootstrap_auxiliary_control_bis.R")

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)

IV_data_log <- read_parquet('low_freq_data_log.parquet')

colnames(IV_data_log)

headers_proxies = c("T_avg_log",
                    "renewable_generation_log",
                    "gas_supply_shock"
)

proxies <- ts(IV_data_log[, c(headers_proxies)], frequency=12)

headers_var = c(
                "spot_price_log",
                "gen_tot_log",
                "IPI_log",
                # "ECB_rate_log_diff",
                # "E_Index_log_diff",
                # "Exp_pi_log_diff",
                # "UnE_log_diff" ,
                "NE_Index_log"
)
vars<- ts(IV_data_log[, c(headers_var)], frequency=12)

p = 4     
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- ncol(proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)

matrix_12 <- rbind( diag(k) , zero_matrix)




estimate_log_renew <- first_var_estimation(x,y,p, proxies, irhor)
theta_0_log_renew <- estimate_log_renew$theta_0
gamma_log_renew <- estimate_log_renew$gamma
phi_log <- estimate_log_renew$phi
var_coef_log <- estimate_log_renew$coef

AL <- do.call(cbind, lapply(var_coef_log, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log)
p <- 4  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_log_renew <- get_lambda_0(C, theta_0_log_renew)

nboot<-100
gamma_estimate_log_GK <- gamma_bootstrap_GK(nboot, vars, theta_0_log_renew)


list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                     "ncvg_lambda_01", "ncvg_lambda_02")
sample_matrices_log_GK <- vector("list", length(list_of_samples))
cov_matrices <- vector("list", length(list_of_samples))
names(sample_matrices_log_GK) <- list_of_samples

# Extract and store bootstrapped samples
for (name in list_of_samples) {
  sample_matrices_log_GK[[name]] <- gamma_estimate_log_GK[[paste0("boot_", name)]]
}

save_mat_path <- 'Z:/prix_electricite_gitlab/VAR_analysis/VAR_output/sample_matrices'
setwd(save_mat_path)
saveRDS(sample_matrices_log_GK, "sample_matrices_log_GK_renew.rds")


# Save bootstrapped samples
# Initialize a 3D array to store the quantiles for both lambda_01 and lambda_02
# Dimensions: (irhor, n, 2) for lower (10%) and upper (90%) quantiles
strong_instruments_lambda_log_GK_68_renew <- array(NA, dim = c(2, irhor, n,  2)) 
dimnames(strong_instruments_lambda_log_GK_68_renew ) <- list(
  c("lower", "upper"), NULL, NULL, c("lambda_01", "lambda_02")
)

# Calculate quantiles and store them in the array
quantile_probs <- c(0.16, 0.84)

for (lambda_idx in 1:2) {
  lambda_name <- paste0("ncvg_lambda_0", lambda_idx)
  quantiles <- apply(sample_matrices_log_GK[[lambda_name]],
                     c(1:2), function(x) quantile(x, probs = quantile_probs))
  
  strong_instruments_lambda_log_GK_68_renew[,,,lambda_idx] <- array(quantiles, dim = c(irhor, n, 2))
}

#strong_instruments_lambda_log_GK_68_renew["lower",,,"lambda_01"]

# Permuter les dimensions 1 et 3
strong_instruments_lambda_log_GK_68_renew <- aperm(strong_instruments_lambda_log_GK_68_renew, perm = c(2, 3, 1, 4))
dim(strong_instruments_lambda_log_GK_68_renew)


strong_instruments_lambda_log_GK_68_renew[,,"lower","lambda_01"]

strong_std_lambda1 <- apply(sample_matrices_log_GK$ncvg_lambda_01, c(1, 2), sd)

strong_std_lambda2 <- apply(sample_matrices_log_GK$ncvg_lambda_02, c(1, 2), sd)

c0 <- theta_0_log_renew[1,2][[1]]
phi_c0 <- matrix(c(1,0, -c0, 1), nrow = 2, ncol = 2) %*% t(matrix_12)  %*% gamma_log_renew
alpha_68 <- 0.32


#rajouter un paramètre de précision de la grille avec une valeur par défaut



weak_bootstrap_result <- bootstrap_weak_instrument(gamma = gamma_log_renew ,c_0, phi_c0,
                                                   alpha_68, ngrid =100,
                                                   sample_matrices_log_GK, lambda_0 = lambda_0_log)


weak_instruments_pointwise_log_GK_68_renew <- weak_bootstrap_result$weak_lambda_bounds_Wald_stat
#weak_instruments_pointwise_log_GK_68_renew <- aperm(weak_instruments_pointwise_log_GK_68_renew, perm = c(1, 2, 4, 3))
#weak_instruments_pointwise_log_GK_68_renew[hori, i,"lower","lambda_01"]

weak_instruments_bondferroni_log_GK_68_renew <- weak_bootstrap_result$weak_lambda_bounds_bonferroni
#weak_instruments_bondferroni_log_GK_68_renew <- aperm(weak_instruments_bondferroni_log_GK_68_renew, perm = c(1, 2, 4, 3))

save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis/VAR_output/plot_irfs'
setwd(save_rds_path)
saveRDS(weak_instruments_pointwise_log_GK_68_renew, "weak_instruments_pointwise_log_GK_68_renew.rds")
saveRDS(weak_instruments_bondferroni_log_GK_68_renew, "weak_instruments_bondferroni_log_GK_68_renew.rds")
saveRDS(strong_instruments_lambda_log_GK_68_renew, "strong_instruments_lambda_log_GK_68_renew.rds")



