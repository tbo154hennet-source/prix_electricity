

#For France


p = 4     
irhor = 40


proxies <- ts(IV_data_diff_fr[, c(headers_proxies)], frequency=12)
proxies[is.na(proxies)] <- 0
vars<- ts(IV_data_diff_fr[, c(headers_var_gen_first_mp)], frequency=12)

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


controls <- ts(IV_data_diff_fr[, headers_controls_fr] , start=c(2015,1), end=c(2023,12), frequency=12)

estimate_diff <- first_var_estimation(x,y,p, proxies, irhor, controls = controls)
theta_0 <- estimate_diff$theta_0
gamma <- estimate_diff$gamma
phi <- estimate_diff$phi
var_coef_diff <- estimate_diff$coef

AL <- do.call(cbind, lapply(var_coef_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_diff)

library(latex2exp)
colors_shock <- c("purple", "yellow",  "blue", "red", "green", "orange")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$HICP$", "ECB_rate", "Gas")
#custom legend part
vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))
x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]

vl = vars::VAR(x, p, type = "none", exogen = controls)
k  = ncol(proxies)

coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)

irf_cholesky <- irf(vl, impulse = colnames(x), rfronse = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

# Apply cumsum columnwise for each shock-rfronse matrix
cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))

#plot(irf_cholesky)
# save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
# setwd(save_rds_path)
# saveRDS(irf_cholesky, file = "irf_cholesky_fr.rds")



# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute historical decomposition for each variable
# Initialize matrix to store historical decomposition
n_obs <- nrow(res)
n_vars <- ncol(res)
hd_cholesky <-array(NA, dim = c(n_obs,  n_vars, n_vars))
y_hat <-array(NA, dim = c(n_obs,   n_vars))
# class(irf_model$irf)
# length(irf_model$irf)
# nrow(irf_model$irf[[1]])
if (!is.null(controls)) {
  B <- sapply(coef, function(coef_list) {
    # Extract the rows corrfronding to controls
    ctrl_idx <- (n * p + 1):nrow(coef_list)
    coef_list[ctrl_idx, , drop = FALSE]
  }, simplify = "array")
  B <- matrix(unlist(B), nrow = n, byrow = TRUE)[,1]
}


hd_controls <- array(0, dim = c(T, n))  # T x number of endogenous vars

for (t in 1:T) {
    hd_controls[t, ] <- B * rep(controls[t, ], n)
}
t <-1
j <-1
#j is the variable of interest
for (j in c(1:n)){
  for (shock in c(1:n)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- cum_irfs_all[[shock]][hori,j]*structural_shocks[shock_horizon,shock] 
        # contrib_hori <- irf_model$irf[[shock]][hori,j]*
        #   structural_shocks[shock_horizon,shock]
        # 
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      hd_cholesky[t,j, shock ] <- contribution
    }
    
  }
  
}

for (j in c(1:n)){
    for (t in c(1:T)){
      y_hat[t, j] <- sum(hd_cholesky[t, j, ]) + hd_controls[t, j]
    }
}


#hd_pelec <- plot_hd(hd_cholesky, series = 1)
#print(hd_pelec)

# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step



#calculate sigma_aa
Sigma_a <- gamma %*%  ginv(t(gamma) %*% ginv(Sigma_eta) %*% gamma) %*% t(gamma)

Sigma_theta <- t(theta_0) %*% theta_0
D <- solve(Sigma_theta)  %*% t(theta_0) %*%  Sigma_a %*% theta_0 %*% solve(Sigma_theta)

#almost diagonal matrix

right_term <- t(gamma) %*% ginv(Sigma_eta) %*% t(res)
left_matrix <- t(phi) %*% solve(D)

# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV <- solve(t(left_matrix) %*% left_matrix) %*% t(left_matrix) %*% right_term
structural_shocks_IV <- scale(structural_shocks_IV)

#Plot the shocks
data_IV <- as.data.frame(t(structural_shocks_IV))
colnames(data_IV) <- c("price", "quantity")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[5:length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$price, color = "price"))+
  geom_line(aes(x = dates_formatted, y = data_IV$quantity, color = "quantity")) +
  theme_minimal()

# Eigenvalue decomposition
# eig <- eigen(Sigma_a)
# Q <- eig$vectors  # Orthogonal eigenvectors (Q_0,1:r)
# D <- diag(eig$values)  # Diagonal matrix of eigenvalues (D_1:r,1:r)
# eps_t <- t(Q) %*% t(res)
# 
# 
# cov(t(eps_t)[,1:2], t(structural_shocks_IV))

#les deux mesures ne sont pas du tout correlées



#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)
historical_decomp_iv_tibo <-array(NA, dim = c(n_obs,  n_vars, k))


hori <- 40 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_iv <- get_lambda_0(C, theta_0, irhor= hori)
# lambda_0_iv <- copy(lambda_0)
# 
# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]

k<-2
#j is the variable of interest
for (j in c(1:n)){
  for (shock in c(1:k)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0_iv[hori,j, shock]*structural_shocks_IV[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo[t,j, shock] <- contribution
    }
  }
  
}


historical_decomp_iv_tibo
apply(historical_decomp_iv_tibo, c(2,3), function(x) cumsum(x))

class(hd_cholesky)

historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
historical_decomp_extended[,,1:n_vars] <- hd_cholesky
historical_decomp_extended[,,(n_vars +1)] <- hd_controls

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo, "hd_iv_fr.rds")
saveRDS(hd_cholesky, "hd_chol_fr.rds")
saveRDS(historical_decomp_extended, "hd_chol_gas_fr.rds")




#Cumulative contribution to Inflation

series = 4
desired_period <- seq(as.Date("2021-07-01"), as.Date("2022-12-01"), by = "month")
# Convert to yearmon
desired_period_yearmon <- as.yearmon(desired_period)

# Get indices of matching dates
matching_indices <- which(dates_formatted %in% desired_period_yearmon)

inflation_contrib_from_price <- historical_decomp_extended[,series,2]
cumulative_contrib_period_price <- sum(inflation_contrib_from_price[matching_indices])

inflation_contrib_from_gen <- historical_decomp_extended[,series,1]
cumulative_contrib_period_gen <- sum(inflation_contrib_from_gen[matching_indices])

inflation_contrib_from_gas <- historical_decomp_extended[,series,6]
cumulative_contrib_from_gas <- sum(inflation_contrib_from_gas[matching_indices])

inflation_contrib_from_mon_pol <- historical_decomp_extended[,series,5]
cumulative_contrib_from_mon_pol <- sum(inflation_contrib_from_mon_pol[matching_indices])


decomp_tot_data <- data.frame(
  time= as.Date(dates_formatted),
  historical_decomp_extended[,series,])
# Plot the decomposition
colnames(decomp_tot_data)[-1] <- names
decomp_tot_data <- decomp_tot_data %>%
  mutate(index = row_number())
# Reshaper les données en format long pour ggplot2
long_data <- pivot_longer(decomp_tot_data, 
                          cols = -c(time, index),  names_to = "category",
                          values_to = "value")

mean_vector =  vars[5:nrow(vars),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 

#transform the HICP category
long_data <- long_data %>% 
  mutate(value = ifelse(category == "$HICP$", value + index*mean_vector[series], value))

y_inflation <- long_data %>% 
  group_by(time) %>% 
  summarise(HICP = sum(value)) %>% 
  ungroup() %>% 
  pull(HICP)

cumulative_inflation <- y_inflation[matching_indices]
delta_inflation <- sum(cumulative_inflation)


contrib_price <- cumulative_contrib_period_price/delta_inflation
contrib_gen <- cumulative_contrib_period_gen/delta_inflation
contrib_gas <- cumulative_contrib_from_gas /delta_inflation
contrib_mon_pol <- cumulative_contrib_from_mon_pol /delta_inflation


#Cumulative contribution to Inflation with IV

historical_decomp_iv_tibo_bis <- apply(historical_decomp_iv_tibo, c(2,3), function(x) cumsum(x))
inflation_contrib_from_price_iv <- historical_decomp_iv_tibo_bis[,series,2]
cumulative_contrib_period_price_iv <- sum(inflation_contrib_from_price_iv[matching_indices])

inflation_contrib_from_gen_iv <- historical_decomp_iv_tibo_bis[,series,1]
cumulative_contrib_period_gen_iv <- sum(inflation_contrib_from_gen_iv[matching_indices])

contrib_price_iv <- cumulative_contrib_period_price_iv/delta_inflation
contrib_gen_iv <- cumulative_contrib_period_gen_iv /delta_inflation




#faire la table finale



contribution_table <- data.frame(
  Identification = rep(c("Cholesky", "IV"), each = 4),
  Contribution_Type = rep(c("$P_{elec}$", "$Q_{elec}$", "$P_{gas}$", "Mon. Policy"), times = 2),
  Contribution = c(
    round(100 * contrib_price,2),
    round(100 * contrib_gen, 2),
    round(100 * contrib_gas, 2),
    round(100 * contrib_mon_pol, 2),
    round(100 * contrib_price_iv,2),
    round(100 * contrib_gen_iv, 2),
    "-",
    "-"
  )
)


# Pivot to wide format
contribution_table_wide <- pivot_wider(
  contribution_table,
  names_from = Contribution_Type,
  values_from = Contribution
)

library(gt)
# Generate the gt table
gt_table <- contribution_table_wide %>%
  gt() %>%
  tab_header(
    title = md("**Cumulative Contribution to Inflation**"),
    subtitle = md("from 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  cols_label(
    Identification = "Identification",
    `$P_{elec}$` = md("$P_{elec}$"),
    `$Q_{elec}$` = md("$Q_{elec}$"),
    `$P_{gas}$` = md("$P_{gas}$"),
    `Mon. Policy` = md("Mon. Policy")
  ) %>%
  fmt_number(
    columns = c(`$P_{elec}$`, `$Q_{elec}$`),
    decimals = 2
  )

# Print the table
gt_table



