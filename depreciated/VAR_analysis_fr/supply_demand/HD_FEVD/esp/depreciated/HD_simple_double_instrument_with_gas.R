
library(gt)
#For France


p = 12     
irhor = 40


headers_controls = c("avg_spot_price_log_diff")
controls <- ts(IV_data_log_diff_esp[, headers_controls] , start=c(2015,1), end=c(2023,12), frequency=12)



# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step
vars_double_gas <- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)

proxies_double_gas <- ts(IV_data_log_diff_esp[, c(headers_proxies_supply_demand)], frequency=12)
proxies_double_gas[is.na(proxies_double_gas)] <- 0
x_double_gas =ts(vars_double_gas , start=c(2015,1), end=c(2023,12), frequency=12)
y_double_gas =vars_double_gas[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k<-2
#nbre d'instruments
l <- ncol(proxies_double_gas)

vl_double_gas = vars::VAR(x_double_gas, p, type = "const", exogen = controls)
coef_double_gas =coef(vl_double_gas)
bet_double_gas = do.call(cbind, lapply(1:n, function(x) coef_double_gas[[x]][,1]))
res_double_gas = residuals(vl_double_gas)
Sigma_eta_double_gas = cov(res_double_gas)




estimate_log_diff_double_gas <- first_var_estimation(x = x_double_gas, y = y_double_gas, p =12 , 
                                                     proxies = proxies_double_gas, irhor, controls = controls, elas = -0.29)
theta_0_double_gas <- estimate_log_diff_double_gas$theta_0
gamma_double_gas <- estimate_log_diff_double_gas$gamma
phi_double_gas <- estimate_log_diff_double_gas$phi
var_coef_log_diff_double_gas <- estimate_log_diff_double_gas$coef

AL_double_gas <- do.call(cbind, lapply(var_coef_log_diff_double_gas, function(df) df[, 1]))
# Optionally, set column names if needed
#colnames(AL_double_gas) <- names(var_coef_log_diff_double_gas)


#calculate sigma_aa
Sigma_a_double_gas <- gamma_double_gas %*%  ginv(t(gamma_double_gas) %*% ginv(Sigma_eta_double_gas) %*% gamma_double_gas) %*% t(gamma_double_gas)

Sigma_theta_double_gas <- t(theta_0_double_gas) %*% theta_0_double_gas
D_double_gas <- solve(Sigma_theta_double_gas)  %*% t(theta_0_double_gas) %*%  Sigma_a_double_gas %*% theta_0_double_gas %*% solve(Sigma_theta_double_gas)

#almost diagonal matrix

right_term_double_gas <- t(gamma_double_gas) %*% ginv(Sigma_eta_double_gas) %*% t(res_double_gas)
D_inv_double_gas <- try(solve(D_double_gas), silent = TRUE); if (inherits(D_inv_double_gas, "try-error")) D_inv_double_gas <- ginv(D_double_gas)

left_matrix_double_gas <- t(phi_double_gas) %*% D_inv_double_gas
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_double_gas <- solve(t(left_matrix_double_gas) %*% left_matrix_double_gas) %*% t(left_matrix_double_gas) %*% right_term_double_gas


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
historical_decomp_iv_tibo_double_gas <-array(NA, dim = c(n_obs,  n_vars, k))


hori <- 40 # Forecast horizon

# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C_double_gas  <- MARep(t(AL_double_gas), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_iv_double_gas <- get_lambda_0(C = C_double_gas, theta_0 =  theta_0_double_gas, irhor= hori)
apply(lambda_0_iv_double_gas , c(2, 3), function(x) cumsum(x))
# lambda_0_iv <- copy(lambda_0)
# 
# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]

k<-2
#j is the variable of interest
for (j in c(1:n_vars)){
  for (shock in c(1:k)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0_iv_double_gas[hori,j, shock]*structural_shocks_IV_double_gas[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo_double_gas[t,j, shock] <- contribution
    }
  }
  
}


historical_decomp_iv_tibo_double_gas
apply(historical_decomp_iv_tibo_double_gas, c(2,3), function(x) cumsum(x))



# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls

save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo_double_gas, "hd_iv_4v_double_gas.rds")
historical_decomp_iv_tibo_double_gas <- readRDS( "hd_iv_4v_double_gas.rds")
hd_cholesky_double_gas <- readRDS( "hd_chol_gas.rds")


# hd_pelec <- plot_hd(historical_decomp_iv_tibo_double_gas, series = 1)
# print(hd_pelec)





#historical_decomp_iv_tibo_double_gas <- historical_decomp_iv_tibo_double_gas[, , c(2, 1)]
# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest

# Example: get contribution of electricity price shocks to inflation

# Compute contributions for 2021 Q2 to 2022 Q4
contribution_data_double_gas <- compute_contributions(
  start_date = "2021-04-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky_double_gas,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo_double_gas,
  series = 4
)

contribution_table_wide_double_gas <- contribution_data_double_gas %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/contribution")
write_csv(contribution_table_wide_double_gas, "contribution_table_wide_double_gas.csv")




gt_table <- contribution_table_wide_double_gas %>%
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




