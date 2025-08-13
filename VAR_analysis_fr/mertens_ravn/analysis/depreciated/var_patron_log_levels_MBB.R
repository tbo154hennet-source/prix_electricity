library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control.R")
source("bootstrap_auxiliary_control_bis.R")


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)
IV_data_log_esp <- IV_data_log_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply
  )



proxies <- ts(IV_data_log_esp[, c(headers_proxies)], frequency=12)
proxies[is.na(proxies)] <- 0

vars<- ts(IV_data_log_esp[, c(headers_var_gen_first)], frequency=12)

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

#controls = ts(IV_data_log_esp[, headers_controls_esp], frequency = 12)


estimate_log <- first_var_estimation(x,y,p, proxies, irhor)#, controls =  controls)
theta_0_log <- estimate_log$theta_0
gamma_log <- estimate_log$gamma
phi_log <- estimate_log$phi
var_coef_log <- estimate_log$coef

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

lambda_0_log <- get_lambda_0(C, theta_0_log)

nboot<-100
gamma_estimate_log_MBB <- gamma_bootstrap_mbb_original(nboot, vars,
                   BlockSize= 15, theta_0_log)


list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                     "ncvg_lambda_01", "ncvg_lambda_02")
sample_matrices_log_MBB <- vector("list", length(list_of_samples))
cov_matrices <- vector("list", length(list_of_samples))
names(sample_matrices_log_MBB) <- list_of_samples

# Extract and store bootstrapped samples
for (name in list_of_samples) {
  sample_matrices_log_MBB[[name]] <- gamma_estimate_log_MBB[[paste0("boot_", name)]]
}

save_mat_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/sample_matrices'
setwd(save_mat_path)
saveRDS(sample_matrices_log_MBB, "sample_matrices_log_MBB.rds")

# Save bootstrapped samples
# Initialize a 3D array to store the quantiles for both lambda_01 and lambda_02
# Dimensions: (irhor, n, 2) for lower (10%) and upper (90%) quantiles
strong_instruments_lambda_log_MBB <- array(NA, dim = c(2, irhor, n,  2)) 
dimnames(strong_instruments_lambda_log_MBB ) <- list(
  c("lower", "upper"), NULL, NULL, c("lambda_01", "lambda_02")
)

# Calculate quantiles and store them in the array
quantile_probs <- c(0.05, 0.95)
print(class(sample_matrices_log_MBB[[lambda_name]]))

for (lambda_idx in 1:2) {
  lambda_name <- paste0("ncvg_lambda_0", lambda_idx)
  quantiles <- apply(sample_matrices_log_MBB[[lambda_name]],
                     c(1:2), function(x) quantile(x, probs = quantile_probs))
  
  strong_instruments_lambda_log_MBB[,,,lambda_idx] <- array(quantiles, dim = c(irhor, n, 2))
}

#strong_instruments_lambda_log_MBB["lower",,,"lambda_01"]

# Permuter les dimensions 1 et 3
strong_instruments_lambda_log_MBB <- aperm(strong_instruments_lambda_log_MBB, perm = c(2, 3, 1, 4))
dim(strong_instruments_lambda_log_MBB)

strong_instruments_lambda_log_MBB[,,"lower","lambda_02"]
strong_instruments_lambda_log_MBB[,,"upper","lambda_02"]





