

library(gt)

#For Spain
# pour cholesky fait une hypothèse demande inélastique


p = 12     
irhor = 40


vars<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)
n_vars <- ncol(vars)
library(latex2exp)
colors_shock <- c( "yellow",  "blue", "red", "green")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$HICP$")
#custom legend part
vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))
x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]

#vl = vars::VAR(x, p, type = "none", exogen = controls)
vl = vars::VAR(x, p, type = "none")#, exogen = controls)


coef =coef(vl)
bet = do.call(cbind, lapply(1:n_vars, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)

irf_cholesky <- irf(vl, impulse = colnames(x), response = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

# Apply cumsum columnwise for each shock-response matrix
cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))

#plot(irf_cholesky)
# save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
# setwd(save_rds_path)
# saveRDS(irf_cholesky, file = "irf_cholesky_esp.rds")


# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute historical decomposition for each variable
# Initialize matrix to store historical decomposition
n_obs <- nrow(res)
hd_cholesky <-array(NA, dim = c(n_obs,  n_vars, n_vars))
hd_cholesky_index <-array(NA, dim = c(n_obs,  n_vars, n_vars))


#j is the variable of interest
for (j in c(1:n_vars)){
  for (shock in c(1:n_vars)){
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



# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step
vars_gen <- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)

proxies_gen <- ts(IV_data_log_diff_esp[, c(headers_proxies_demand)], frequency=12)
proxies_gen[is.na(proxies_gen)] <- 0
x_gen =ts(vars_gen , start=c(2015,1), end=c(2023,12), frequency=12)
y_gen =vars_gen[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <-1
#nbre d'instruments
l <- ncol(proxies_gen)

vl_gen = vars::VAR(x_gen, p, type = "none")#, exogen = controls)
coef_gen =coef(vl_gen)
bet_gen = do.call(cbind, lapply(1:n_vars, function(x) coef_gen[[x]][,1]))
res_gen = residuals(vl_gen)
Sigma_eta_gen = cov(res_gen)

controls <- NULL


estimate_log_diff_gen <- first_var_estimation(x_gen,y_gen,p, proxies_gen, irhor)
theta_0_gen <- estimate_log_diff_gen$theta_0
gamma_gen <- estimate_log_diff_gen$gamma
phi_gen <- estimate_log_diff_gen$phi
var_coef_log_diff_gen <- estimate_log_diff_gen$coef

AL_gen <- do.call(cbind, lapply(var_coef_log_diff_gen, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL_gen) <- names(var_coef_log_diff_gen)


#calculate sigma_aa
Sigma_a_gen <- gamma_gen %*%  ginv(t(gamma_gen) %*% ginv(Sigma_eta_gen) %*% gamma_gen) %*% t(gamma_gen)

Sigma_theta_gen <- t(theta_0_gen) %*% theta_0_gen
D_gen <- solve(Sigma_theta_gen)  %*% t(theta_0_gen) %*%  Sigma_a_gen %*% theta_0_gen %*% solve(Sigma_theta_gen)

#almost diagonal matrix

right_term_gen <- t(gamma_gen) %*% ginv(Sigma_eta_gen) %*% t(res_gen)
D_inv_gen <- try(solve(D_gen), silent = TRUE); if (inherits(D_inv_gen, "try-error")) D_inv_gen <- ginv(D_gen)

left_matrix_gen <- t(phi_gen) %*% D_inv_gen
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_gen <- solve(t(left_matrix_gen) %*% left_matrix_gen) %*% t(left_matrix_gen) %*% right_term_gen




# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step
vars_spot <- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp)], frequency=12)

proxies_spot <- ts(IV_data_log_diff_esp[, c(headers_proxies_supply)], frequency=12)
proxies_spot[is.na(proxies_spot)] <- 0
x_spot =ts(vars_spot , start=c(2015,1), end=c(2023,12), frequency=12)
y_spot =vars_spot[-1:-p, ]
T = nrow(y)
#variables dans VAR

#nbre de variables instrumentees
k <-1
#nbre d'instruments
l <- ncol(proxies_spot)

vl_spot = vars::VAR(x_spot, p, type = "none")#, exogen = controls)
coef_spot =coef(vl_spot)
bet_spot = do.call(cbind, lapply(1:n_vars, function(x) coef_spot[[x]][,1]))
res_spot = residuals(vl_spot)
Sigma_eta_spot = cov(res_spot)

controls <- NULL


estimate_log_diff_spot <- first_var_estimation(x_spot,y_spot,p, proxies_spot, irhor)
theta_0_spot <- estimate_log_diff_spot$theta_0
gamma_spot <- estimate_log_diff_spot$gamma
phi_spot <- estimate_log_diff_spot$phi
var_coef_log_diff_spot <- estimate_log_diff_spot$coef

AL_spot <- do.call(cbind, lapply(var_coef_log_diff_spot, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL_spot) <- names(var_coef_log_diff_spot)


#calculate sigma_aa
Sigma_a_spot <- gamma_spot %*%  ginv(t(gamma_spot) %*% ginv(Sigma_eta_spot) %*% gamma_spot) %*% t(gamma_spot)

Sigma_theta_spot <- t(theta_0_spot) %*% theta_0_spot
D_spot <- solve(Sigma_theta_spot)  %*% t(theta_0_spot) %*%  Sigma_a_spot %*% theta_0_spot %*% solve(Sigma_theta_spot)

#almost diagonal matrix

right_term_spot <- t(gamma_spot) %*% ginv(Sigma_eta_spot) %*% t(res_spot)
D_inv_spot <- try(solve(D_spot), silent = TRUE); if (inherits(D_inv_spot, "try-error")) D_inv_spot <- ginv(D_spot)

left_matrix_spot <- t(phi_spot) %*% D_inv_spot
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_spot <- solve(t(left_matrix_spot) %*% left_matrix_spot) %*% t(left_matrix_spot) %*% right_term_spot






#Plot the shocks
data_IV <- as.data.frame(t(rbind(structural_shocks_IV_gen, structural_shocks_IV_spot)))
colnames(data_IV) <- c( "demand", "supply")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$demand, color = "Demand"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply, color = "Supply")) +
  theme_minimal()+
  labs(title = "Structural Shocks from IV Analysis",
       x = "Date",
       y = "Shock Value")

summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE)
)

data_IV %>% 
  cor()

#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)


hori <- 40 # Forecast horizon

# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C_gen  <- MARep(t(AL_gen), p, hori)
C_spot  <- MARep(t(AL_spot), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_iv_gen <- get_lambda_0(C_gen, theta_0_gen, irhor= hori)
lambda_0_iv_spot <- get_lambda_0(C_spot, theta_0_spot, irhor= hori)


apply(lambda_0_iv_gen , c(2, 3), function(x) cumsum(x))
apply(lambda_0_iv_spot , c(2, 3), function(x) cumsum(x))

# lambda_0_iv <- copy(lambda_0)
# 
# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]
historical_decomp_iv_tibo <-array(NA, dim = c(n_obs,  n_vars, 2))

#j is the variable of interest
for (j in c(1:n_vars)){
  for (t in c(1:T)){
    contribution_gen <-0
    contribution_spot <- 0
    #hori = 1
    for (hori in c(1:irhor)){
      # ne doit pas dépasser l'horizon pour lequel on calcule
      # restriction pour les premiers chocs où l'on a pas les 20 précédents
      #pn prend juste ceux qui sont disponibles
      shock_horizon = max(0,t-hori+1)
      contrib_hori_gen <- lambda_0_iv_gen[hori,j,1]*structural_shocks_IV_gen[1,shock_horizon]
      contrib_hori_spot <- lambda_0_iv_spot[hori,j,1]*structural_shocks_IV_spot[1,shock_horizon]
      if (!length(as.numeric(contrib_hori_gen))==0){
        contribution_gen <- contribution_gen + contrib_hori_gen
      }
      if (!length(as.numeric(contrib_hori_spot))==0){
        contribution_spot <- contribution_spot + contrib_hori_spot
      }
    }
    historical_decomp_iv_tibo[t,j,1] <- contribution_gen
    historical_decomp_iv_tibo[t,j,2] <- contribution_spot
  }
}

historical_decomp_iv_tibo
apply(historical_decomp_iv_tibo, c(2,3), function(x) cumsum(x))



# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls


save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo, "hd_iv_5v.rds")
saveRDS(hd_cholesky, "hd_chol_5v.rds")

hd_cholesky <- readRDS( "hd_chol_5v.rds")
historical_decomp_iv_tibo <- readRDS( "hd_iv_5v.rds")




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
                                  IV_data_log_diff_esp, 
                                  hd_cholesky, 
                                  historical_decomp_iv_tibo, 
                                  series = 4) {
  
  # Generate the date sequence and convert to yearmon
  desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  desired_period_yearmon <- as.yearmon(desired_period)
  
  # Get matching indices
  matching_indices <- which(dates_formatted %in% desired_period_yearmon)
  
  # Total inflation during the period
  cumulative_inflation <- sum(IV_data_log_diff_esp$HICP_log_diff[matching_indices + p])
  
  # Cholesky contributions
  contrib_price <- sum(hd_cholesky[, series, 2][matching_indices]) / cumulative_inflation
  contrib_gen   <- sum(hd_cholesky[, series, 1][matching_indices]) / cumulative_inflation
  
  
  contrib_price_iv <- sum(historical_decomp_iv_tibo[, series, 2][matching_indices]) / cumulative_inflation
  contrib_gen_iv   <- sum(historical_decomp_iv_tibo[, series, 1][matching_indices]) / cumulative_inflation
  
  
  # Return a tidy data frame (long format)
  tibble::tibble(
    Identification = rep(c("Cholesky", "IV"), each = 2),
    Contribution_Type = rep(c("$P_{elec}$", "$Q_{elec}$"), times = 2),
    #Contribution_Type = rep(c("Supply", "Demand"), times = 2),
    Contribution = 100 * c(contrib_price, contrib_gen, contrib_price_iv, contrib_gen_iv)
  )
}




# Compute contributions for 2021 Q3 to 2022 Q3
contribution_data <- compute_contributions(
  start_date = "2021-04-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide <- contribution_data %>%
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
    # `$P_{elec}$` = gt::md("$P_{elec}$"),
    # `$Q_{elec}$` = gt::md("$Q_{elec}$")
    `$P_{elec}$` = gt::md("Supply"),
    `$Q_{elec}$` = gt::md("Demand")
  ) %>%
  gt::fmt_number(
    columns = c(`$P_{elec}$`, `$Q_{elec}$`),
    decimals = 2
  )

gt_table

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/contribution")
write_csv(contribution_table_wide, "contribution_table_wide_mp.csv")















