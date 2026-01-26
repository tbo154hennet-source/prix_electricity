
library(gt)

#For Spain
# pour cholesky fait une hypothèse demande inélastique


#pour normaliser par rapport à la demand
vars_demand<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp_gas)], frequency=12)

p = 12     
irhor = 40
x_demand =ts(vars_demand , start=c(2015,1), end=c(2025,9), frequency=12)
y =vars_demand[-1:-p, ]
T = nrow(y)
#variables dans VAR
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1


# Fit the VAR model
vl_demand <- if (is.null(controls)) {
  vars::VAR(x_demand, p, type = "const")
} else {
  vars::VAR(x_demand, p, type = "const", exogen = controls)
}

coef = coef(vl_demand)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res_demand = residuals(vl_demand)


proxies_demand <- ts(IV_data_log_esp[, c("delta_demand")[[1]]], frequency=12)
proxies_supply <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)



#vars_demand<- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)
n_vars_demand <- ncol(vars_demand)
library(latex2exp)
colors_shock <- c(  "purple", "yellow",  "blue", "green", "red", "orange", "pink")
names <- c( "Load" ,"$P_{elec}$", "Generation", 
            "IPI",  "$HICP$", "ECB rate","$P_{gas}$")

Sigma_eta_demand = cov(res_demand)

irf_cholesky <- irf(vl_demand, impulse = colnames(x_demand), response = colnames(x_demand),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

# Apply cumsum columnwise for each shock-response matrix_demand
cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))

#plot(irf_cholesky)
# save_rds_path <- 'Z:/prix_demand_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
# setwd(save_rds_path)
# saveRDS(irf_cholesky, file = "irf_cholesky_esp.rds")


# Perform Cholesky decomposition
P <- chol(Sigma_eta_demand)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res_demand %*%  solve(P)


# Compute historical decomposition for each variable
# Initialize matrix_demand to store historical decomposition
n_obs <- nrow(res_demand)
hd_cholesky <-array(NA, dim = c(n_obs,  n_vars_demand, n_vars_demand))


#j is the variable of interest
for (j in c(1:n_vars_demand)){
  for (shock in c(1:n_vars_demand)){
    for (t in c(1:T)){
      contribution <-0
      contribution_index <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux_demand qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- irf_cholesky$irf[[shock]][hori,j]*structural_shocks[shock_horizon,shock] 
        # contrib_hori <- irf_model$irf[[shock]][hori,j]*
        #   structural_shocks[shock_horizon,shock]
        # 
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
        }
      }
      hd_cholesky[t,j, shock ] <- contribution
    }
    #y_hat[t, j] <- sum(hd_cholesky[t, j, ]) + hd_controls[t, j]
  }
  
}




demand_estimation <- first_var_estimation_bis(y, res_demand, p, proxies_demand, irhor, controls = NULL, elas = 0)
theta_0_demand <- demand_estimation$theta_0
gamma_demand  <- demand_estimation$gamma
phi_demand  <- demand_estimation$phi
#calculate sigma_aa
Sigma_a_demand <- gamma_demand %*%  ginv(t(gamma_demand) %*% ginv(Sigma_eta_demand) %*% gamma_demand) %*% t(gamma_demand)

Sigma_theta_demand <- t(theta_0_demand) %*% theta_0_demand
D_demand <- solve(Sigma_theta_demand)  %*% t(theta_0_demand) %*%  Sigma_a_demand %*% theta_0_demand %*% solve(Sigma_theta_demand)

#almost diagonal matrix_supply
right_term_demand <- t(gamma_demand) %*% ginv(Sigma_eta_demand) %*% t(res_demand)
D_inv_demand <- try(solve(D_demand), silent = TRUE); if (inherits(D_inv_demand, "try-error")) D_inv_demand <- ginv(D_demand)

left_matrix_demand <- t(phi_demand) %*% D_inv_demand
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_demand <- solve(t(left_matrix_demand) %*% left_matrix_demand) %*% t(left_matrix_demand) %*% right_term_demand

AL <- do.call(cbind, lapply(coef(vl), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl))
p <- 12  # Number of lags in the VAR model
hori <- 40 # Forecast horizon
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)
lambda_0_log_demand <- get_lambda_0(C, theta_0_demand, irhor=hori)




# Fit the VAR model
vl_supply <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "const")
} else {
  vars::VAR(x_supply, p, type = "const", exogen = controls)
}

coef = coef(vl_supply)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res_supply = residuals(vl_supply)
Sigma_eta_supply = cov(res_supply)

supply_estimation <- first_var_estimation_bis(y, res_supply, p, proxies_supply, irhor, controls = NULL, elas = 0)
theta_0_supply <- supply_estimation$theta_0
gamma_supply  <- supply_estimation$gamma
phi_supply  <- supply_estimation$phi
#calculate sigma_aa
Sigma_a_supply <- gamma_supply %*%  ginv(t(gamma_supply) %*% ginv(Sigma_eta_supply) %*% gamma_supply) %*% t(gamma_supply)

Sigma_theta_supply <- t(theta_0_supply) %*% theta_0_supply
D_supply <- solve(Sigma_theta_supply)  %*% t(theta_0_supply) %*%  Sigma_a_supply %*% theta_0_supply %*% solve(Sigma_theta_supply)

#almost diagonal matrix_supply
right_term_supply <- t(gamma_supply) %*% ginv(Sigma_eta_supply) %*% t(res_supply)
D_inv_supply <- try(solve(D_supply), silent = TRUE); if (inherits(D_inv_supply, "try-error")) D_inv_supply <- ginv(D_supply)

left_matrix_supply <- t(phi_supply) %*% D_inv_supply
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_supply <- solve(t(left_matrix_supply) %*% left_matrix_supply) %*% t(left_matrix_supply) %*% right_term_supply

lambda_0_log_supply <- get_lambda_0(C, theta_0_supply, irhor=hori)


# save_rds_path_hd <- "Z:/prix_supply_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/structur_shock"
# setwd(save_rds_path_hd)
# #(as.data.frame(structural_shocks_IV_demand), "structural_shocks_IV_demand.parquet" )
# write_parquet(as.data.frame(structural_shocks_IV_gen), "structural_shocks_IV_gen.parquet" )




#Plot the shocks
data_IV <- as.data.frame(t(rbind(structural_shocks_IV_supply, structural_shocks_IV_demand)))
colnames(data_IV) <- c(  "supply", "demand")
dates_formatted <- as.yearmon(time(x_supply))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
my_stru_shock_simple <- ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$demand, color = "Demand"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply/10, color = "Supply")) +
  theme_minimal()+
  labs(title = "Structural Shocks from IV Analysis",
       x_supply = "Date",
       y = "Shock Value")

summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE)
)

my_stru_shock_simple
data_IV %>% 
  cor()

# save_plot_path <- 'Z:/Overleaf_elec/images/macro'
# setwd(save_plot_path)
# # Assume your ggplot object is called 'p'
# ggsave("structural_shocks_simple.png", plot = my_stru_shock_simple , width = 12, height = 6, dpi = 300)


#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)


hori <- 40 # Forecast horizon

# Caux_supply        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1


# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]
n_vars_supply <- ncol(x_supply)
historical_decomp_iv_tibo <-array(NA, dim = c(n_obs,  n_vars_supply, 2))

#j is the variable of interest
for (j in c(1:n_vars_supply)){
  for (t in c(1:T)){
    contribution_supply <-0
    contribution_demand <- 0
    #hori = 1
    for (hori in c(1:irhor)){
      # ne doit pas dépasser l'horizon pour lequel on calcule
      # restriction pour les premiers chocs où l'on a pas les 20 précédents
      #pn prend juste ceux_supply qui sont disponibles
      shock_horizon = max(0,t-hori+1)
      contrib_hori_supply <- lambda_0_log_supply[hori,j,1]*structural_shocks_IV_supply[1,shock_horizon]
      contrib_hori_demand <- lambda_0_log_demand[hori,j,1]*structural_shocks_IV_demand[1,shock_horizon]
      if (!length(as.numeric(contrib_hori_supply))==0){
        contribution_supply <- contribution_supply + contrib_hori_supply
      }
      if (!length(as.numeric(contrib_hori_demand))==0){
        contribution_demand <- contribution_demand + contrib_hori_demand
      }
    }
    historical_decomp_iv_tibo[t,j,1] <- contribution_supply
    historical_decomp_iv_tibo[t,j,2] <- contribution_demand
  }
}

historical_decomp_iv_tibo
apply(historical_decomp_iv_tibo, c(2,3), function(x_supply) cumsum(x_supply))



# historical_decomp_ex_supplytended <- array(NA, dim = c(n_obs,  n_vars_supply, (n_vars_supply +1)))
# historical_decomp_ex_supplytended[,,1:n_vars_supply] <- hd_cholesky
# historical_decomp_ex_supplytended[,,(n_vars_supply +1)] <- hd_controls

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo, "hd_iv_log.rds")
saveRDS(hd_cholesky, "hd_chol_log.rds")

# hd_cholesky <- readRDS( "hd_chol_4v.rds")
# historical_decomp_iv_tibo <- readRDS( "hd_iv_4v.rds")




# hd_pelec <- plot_hd(historical_decomp_iv_tibo, series = 1)
# print(hd_pelec)


















