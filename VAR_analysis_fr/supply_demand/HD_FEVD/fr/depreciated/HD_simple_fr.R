
library(gt)
#For France


p = 12    
irhor = 40


  
proxies <- ts(IV_data_log_diff_fr[, c(headers_proxies_fr)], frequency=12)
proxies[is.na(proxies)] <- 0
vars<- ts(IV_data_log_diff_fr[, c(headers_var_gen_first)], frequency=12)

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


controls <- NULL

estimate_log_diff <- first_var_estimation(x,y,p, proxies, irhor)
theta_0 <- estimate_log_diff$theta_0
gamma <- estimate_log_diff$gamma
phi <- estimate_log_diff$phi
var_coef_log_diff <- estimate_log_diff$coef

AL <- do.call(cbind, lapply(var_coef_log_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log_diff)

library(latex2exp)
colors_shock <- c( "yellow",  "blue", "red", "green")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$HICP$")
#custom legend part
vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))



x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]

#vl = vars::VAR(x, p, type = "none", exogen = controls)
vl = vars::VAR(x, p, type = "none")#, exogen = controls)
k  = ncol(proxies)


coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)

irf_cholesky <- irf(vl, impulse = colnames(x), response = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

# Apply cumsum columnwise for each shock-response matrix
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
hd_cholesky_index <-array(NA, dim = c(n_obs,  n_vars, n_vars))


#j is the variable of interest
for (j in c(1:n)){
  for (shock in c(1:n)){
    for (t in c(1:T)){
      contribution <-0
      contribution_index <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- irf_cholesky$irf[[shock]][hori,j]*structural_shocks[shock_horizon,shock] 
        contrib_hori_index <- cum_irfs_all[[shock]][hori,j]*structural_shocks[shock_horizon,shock] 
        # contrib_hori <- irf_model$irf[[shock]][hori,j]*
        #   structural_shocks[shock_horizon,shock]
        # 
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          contribution_index <- contribution_index + contrib_hori_index
        }
      }
      hd_cholesky[t,j, shock ] <- contribution
      hd_cholesky_index[t,j, shock ] <- contribution_index
    }
    #y_hat[t, j] <- sum(hd_cholesky[t, j, ]) + hd_controls[t, j]
  }
  
}

#je voudrais bien montrer que l'inflation augmente tendanciellement

headers_var_gen_first
headers_index <- c("gen_tot",    "spot_price", "IPI" ,       "HICP")
drift <- as.numeric(IV_data_log_diff_fr[1, c(headers_index)])
result <- sweep(apply(vars, c(2), function(x) cumsum(x)), 2, drift, "+")


mean_vector <- colMeans(vars, na.rm = TRUE)
vars_demeaned <- sweep(vars, 2, mean_vector, "-")
vars_reconstructed <- sweep(vars_demeaned, 2, mean_vector, "+")


#hd_pelec <- plot_hd(hd_cholesky, series = 1)
#print(hd_pelec)

# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step



#calculate sigma_aa
Sigma_a <- gamma %*%  ginv(t(gamma) %*% ginv(Sigma_eta) %*% gamma) %*% t(gamma)

Sigma_theta <- t(theta_0) %*% theta_0
D <- solve(Sigma_theta)  %*% t(theta_0) %*%  Sigma_a %*% theta_0 %*% solve(Sigma_theta)

#almost diagonal matrix

right_term <- t(gamma) %*% ginv(Sigma_eta) %*% t(res)
D_inv <- try(solve(D), silent = TRUE); if (inherits(D_inv, "try-error")) D_inv <- ginv(D)

left_matrix <- t(phi) %*% D_inv
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV <- solve(t(left_matrix) %*% left_matrix) %*% t(left_matrix) %*% right_term


#Plot the shocks
data_IV <- as.data.frame(t(structural_shocks_IV))
colnames(data_IV) <- c( "quantity", "price")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$price, color = "price"))+
  geom_line(aes(x = dates_formatted, y = data_IV$quantity, color = "quantity")) +
  theme_minimal()

summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE)
)


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



# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo, "hd_iv_4v_fr.rds")
saveRDS(hd_cholesky, "hd_chol_4v_fr.rds")





# hd_pelec <- plot_hd(historical_decomp_iv_tibo, series = 1)
# print(hd_pelec)




zero_errors <- matrix(0, nrow = dim(res)[1], ncol = dim(res)[2])

reconstruct_derive_without_errors_ter <- iterate_var_ter(x=x,
                                                         initial_obs = x[1:p, ],
                                                         errors = zero_errors,
                                                         var_model = vl
)

# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest

# Example: get contribution of electricity price shocks to inflation


compute_contributions <- function(start_date, end_date, 
                                  dates_formatted, 
                                  IV_data_log_diff_fr, 
                                  hd_cholesky, 
                                  historical_decomp_iv_tibo, 
                                  series = 4) {
  
  # Generate the date sequence and convert to yearmon
  desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  desired_period_yearmon <- as.yearmon(desired_period)
  
  # Get matching indices
  matching_indices <- which(dates_formatted %in% desired_period_yearmon)
  
  # Total inflation during the period
  cumulative_inflation <- sum(IV_data_log_diff_fr$HICP_log_diff[p +matching_indices])
  
  #cumulative_elec_price <- sum(IV_data_log_diff_fr$spot_price_log_diff[matching_indices])
  
  # Cholesky contributions
  contrib_price <- sum(hd_cholesky[, series, 2][matching_indices]) / cumulative_inflation
  contrib_gen   <- sum(hd_cholesky[, series, 1][matching_indices]) / cumulative_inflation
  

  
  # price_data <- hd_cholesky_index[, series, 2][matching_indices]
  # # qty_data <- hd_cholesky_index[, series, 1][matching_indices]
  # diff_contrib_price <- price_data[length(price_data)] -  
  #   price_data[1]
  # sum(diff(price_data))

  # methode 2
  # inflation_data <- (log(IV_data_log_diff_fr[(p+1):nrow(IV_data_log_diff_fr), "HICP"] ))[[1]][matching_indices]
  # cumulative_inflation <- inflation_data[length(matching_indices)] - inflation_data[1]
  # 
  
  
  # IV contributions
  contrib_price_iv <- sum(historical_decomp_iv_tibo[, series, 2][matching_indices]) / cumulative_inflation
  contrib_gen_iv   <- sum(historical_decomp_iv_tibo[, series, 1][matching_indices]) / cumulative_inflation
  
  
  
  # Return a tidy data frame (long format)
  tibble::tibble(
    Identification = rep(c("Cholesky", "IV"), each = 2),
    Contribution_Type = rep(c("$P_{elec}$", "$Q_{elec}$"), times = 2),
    Contribution = 100 * c(contrib_price, contrib_gen, contrib_price_iv, contrib_gen_iv)
  )
}


# Compute contributions for 2021 Q3 to 2022 Q4
contribution_data <- compute_contributions(
  start_date = "2021-07-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_fr = IV_data_log_diff_fr,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide <- contribution_data %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )

#what is my study period ?


contribution_data_2 <- compute_contributions(
  start_date = "2021-01-01",
  end_date = "2022-07-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_fr = IV_data_log_diff_fr,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide_2 <- contribution_data_2 %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )


contribution_data_3 <- compute_contributions(
  start_date = "2021-01-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_fr = IV_data_log_diff_fr,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide_3 <- contribution_data_3 %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )



gt_table <- contribution_table_wide %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  gt::cols_label(
    Identification = "Identification",
    `$P_{elec}$` = gt::md("$P_{elec}$"),
    `$Q_{elec}$` = gt::md("$Q_{elec}$")
  ) %>%
  gt::fmt_number(
    columns = c(`$P_{elec}$`, `$Q_{elec}$`),
    decimals = 2
  )

gt_table




