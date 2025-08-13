library(MASS)


source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("reliability_auxiliary.R")
source("auxiliary_control.R")

save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)

detrended_data <- read_parquet('low_freq_data_all_news.parquet')
IV_data_log <- read_parquet('low_freq_data_log_all_news.parquet')
IV_data_log_diff <- read_parquet('low_freq_data_log_diff_all_news.parquet')
headers_proxies = c(#"cold_excess_instrument", 
  "T_avg_log",
  "gas_supply_shock"#,        
  #"renewable_generation_log"
)

colnames(IV_data_log)

proxies <- ts(IV_data_log[, c(headers_proxies)], frequency=12)


headers_var = c("gen_tot_log",
                "spot_price_log",
                "IPI_log",
                # "ECB_rate_log",
                # "E_Index_log",
                # "Exp_pi_log",
                # "UnE_log" ,
                "NE_Index_log"
)
vars<- ts(IV_data_log[, c(headers_var)], frequency=12)


IV_data_tot <- cbind(IV_data_log, IV_data_log_diff, detrended_data)
# Régression linéaire
price_model <- lm(data = IV_data_tot, spot_price_log ~ gas_supply_shock + nuclear_indispo_log
                  + T_avg_log)

summary(price_model)


demand_model <- lm(data = IV_data_tot, gen_tot_log ~ gas_supply_shock + nuclear_indispo_log + T_avg_log)

summary(demand_model)


p = 4     
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
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

vl = vars::VAR(x, p, type = "none")
coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta <- cov(res) 

T = nrow(y)

Z <- window(proxies, start = start(proxies) + c(0, p))

# Defining the projector on the instruments
PZ <- Z %*% solve(t(Z)%*%Z) %*% t(Z)

if (nrow(PZ)== ncol(PZ)){
  MZ <- diag(nrow(PZ)) -PZ
}
# βˆ2SLS =  (t(X)PZ X)^−1 t(X)PZy

X <- res[,1:k]
Y <- res[,(k+1):n]


# first solution utilisant le projete

b21ib11_TSLS <- ginv(t(X) %*% PZ %*% X) %*% t(X) %*% PZ %*% Y
b21ib11 <- t(b21ib11_TSLS)
Sig11 <- Sigma_eta[(1:k),(1:k)]
Sig21 <- Sigma_eta[(k+1):n,1:k]
Sig22 <- Sigma_eta[(k+1):n,(k+1):n]
ZZp   <- b21ib11 %*% Sig11 %*% t(b21ib11) - (Sig21 %*% t(b21ib11) + b21ib11 %*% t(Sig21)) + Sig22
b12b12p <- t(Sig21- b21ib11%*%Sig11) %*% (ginv(ZZp) %*% (Sig21- b21ib11%*%Sig11))
b11b11p <- Sig11 - b12b12p
res_TSLS <- Y - X %*% b21ib11_TSLS

#reliability Mertens and Ravn

proxies_postlag <- tryCatch(
  {
    # Code that might fail
    proxies[c(-1:-p),]
  },
  
  error = function(e) {
    # Handle errors
    return(proxies[c(-1:-p)])  # Return a value in case of error
  }
)
mbis <- ginv(proxies_postlag)

## Calculate Sigmm
Sigmm <- mbis  %*% t(mbis)/ T
# a priori ED compte les observations non-nulles
#  dans le contexte des tax shocks, plein d'obs a 0
# chez nous le choc est toujours non-nul

# Calculate ED
#ED <- diag(k) * sum(rowSums(mbis != 0))/T/n_ins

# Calculate mu1
sig_mu1 <- mbis %*% res[, 1:k] / T

# Calculate PhiPhip
PhiPhip <- sig_mu1 %*% solve(b11b11p) %*% t(sig_mu1)

# Calculate RM
RM <- solve(Sigmm) %*% PhiPhip #%*% solve(ED)

# Calculate eigenvalues and sort them
RMeigs <- sort(eigen(RM)$values, decreasing = TRUE)

reliability_measure <- Re(RMeigs)

print(reliability_measure)

print(nrow(proxies))
print(nrow(res))



data <- data.frame(cbind(res[,1:k], Z))
colnames(data) <- c('res_gen_tot_log', 'res_spot_price_log',headers_proxies)

# Régression linéaire
# price_model <- lm(data = data, res_spot_price_log ~ gas_supply_shock + renewable_generation_log
#                   +  T_avg_log)
# 
# summary(price_model)
# 
# 
# demand_model <- lm(data = data, res_gen_tot_log ~ gas_supply_shock + renewable_generation_log
#                    + cold_excess_instrument)
# 
# summary(demand_model)


headers_proxies <- c("T_avg_log",
                     "gas_supply_shock"
                     ,"renewable_generation_log"
)
# 
data$renewable_generation_log <- IV_data_log$renewable_generation_log[c(-1:-p)]
# data$nuclear_indispo_log <- IV_data_log$nuclear_indispo_log[c(-1:-p)]
# data$unexpected_nuclear_indispo_log <- IV_data_log$unexpected_nuclear_indispo.x_log[c(-1:-p)]
# data$tot_indispo_log <- IV_data_log$tot_indispo_log[c(-1:-p)]

# Construct the formula
formula_price <- as.formula(paste('res_spot_price_log', "~",
                                  paste(headers_proxies, collapse = "+")))
# Construct the formula
formula_demand <- as.formula(paste('res_gen_tot_log', "~",
                                   paste(headers_proxies, collapse = "+")))

m_price <- lm( formula_price, data = data)


m_demand <- lm(formula_demand, data = data)

# Get all the statistical coefficients
coef_price <- summary(m_price)$coefficients
# Convert to a data frame for easier subsetting
coef_price_df <- as.data.frame(coef_price)
# Subset the data frame to keep only significant coefficients at the 10 percent level
coef_price_df <- coef_price_df[coef_price_df[["Pr(>|t|)"]] < 0.10, ]
# Apply the function to the p-values and create new row names
significance_stars <- sapply(coef_price_df[, "Pr(>|t|)"], get_significance_stars)

price_names <- paste(rownames(coef_price_df), significance_stars)

# the R^2 in the regression 
fstat_price <- as.numeric(summary(m_price)$fstat[1])# Get all the statistical coefficients


coef_gen <- summary(m_demand)$coefficients
# Convert to a data frame for easier subsetting
coef_gen_df <- as.data.frame(coef_gen)
# Subset the data frame to keep only significant coefficients at the 10 percent level
coef_gen_df <- coef_gen_df[coef_gen_df[["Pr(>|t|)"]] < 0.10, ]

# Apply the function to the p-values and create new row names
significance_stars <- sapply(coef_gen_df[, "Pr(>|t|)"], get_significance_stars)

gen_names <- paste(rownames(coef_gen_df), significance_stars)
# the R^2 in the regression 
fstat_gen <-as.numeric(summary(m_demand)$fstat[1])

print(fstat_price)
print(fstat_gen)
# stock and yogo formulation

# nrow(X)
# sigma_vv <- t(X)%*% MZ %*% X /(T-k)^2
# eigen_M <- eigen(sigma_vv)
# # Calcul de la racine carrée de la matrice
# sqrt_matrix <- eigen_M$vectors %*% diag(sqrt(eigen_M$values)) %*% t(eigen_M$vectors)
# 
# G_t <- ginv(sqrt_matrix) %*% t(X) %*%  PZ  %*% X %*% ginv(sqrt_matrix)
# GCraggDonald_stat <- min(eigen(G_t)$values) 
# print(GCraggDonald_stat)



# Assume y (dependent variable), X1 (endogenous variables), Z (instruments)

# Number of observations
n <- nrow(Z)

# Step 1: Compute Z'Z, Z'X1, and X1'X1
ZtZ <- t(Z) %*% Z
ZtX <- t(Z) %*% X
X1tX1 <- t(X) %*% X

# Step 2: Compute the key matrix
matrix_C <- solve(ZtZ) %*% ZtX %*% solve(X1tX1) %*% t(ZtX)

# Step 3: Find the smallest eigenvalue
lambda_min <- min(eigen(matrix_C)$values)

# Step 4: Compute the Cragg-Donald statistic
cragg_donald_stat <- n * lambda_min

print(cragg_donald_stat)

# Save the results




