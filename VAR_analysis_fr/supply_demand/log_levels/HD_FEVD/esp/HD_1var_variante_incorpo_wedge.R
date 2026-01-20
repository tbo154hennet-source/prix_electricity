



# Fit the VAR model
vl_supply_incorpo <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "none")
} else {
  vars::VAR(x_supply, p, type = "none", exogen = controls)
}

coef = coef(vl_supply_incorpo)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res_supply_incorpo = residuals(vl_supply_incorpo)
Sigma_eta_supply_incorpo = cov(res_supply_incorpo)

supply_estimation <- first_var_estimation_bis(y, res_supply_incorpo, p, proxies_supply_incorpo, irhor, controls = NULL, elas = 0)
theta_0_supply_incorpo <- supply_estimation$theta_0
gamma_supply_incorpo  <- supply_estimation$gamma
phi_supply_incorpo  <- supply_estimation$phi
#calculate sigma_aa
Sigma_a_supply_incorpo <- gamma_supply_incorpo %*%  ginv(t(gamma_supply_incorpo) %*% ginv(Sigma_eta_supply_incorpo) %*% gamma_supply_incorpo) %*% t(gamma_supply_incorpo)

Sigma_theta_supply_incorpo <- t(theta_0_supply_incorpo) %*% theta_0_supply_incorpo
D_supply_incorpo <- solve(Sigma_theta_supply_incorpo)  %*% t(theta_0_supply_incorpo) %*%  Sigma_a_supply_incorpo %*% theta_0_supply_incorpo %*% solve(Sigma_theta_supply_incorpo)

#almost diagonal matrix_supply
right_term_supply_incorpo <- t(gamma_supply_incorpo) %*% ginv(Sigma_eta_supply_incorpo) %*% t(res_supply_incorpo)
D_inv_supply_incorpo <- try(solve(D_supply_incorpo), silent = TRUE); if (inherits(D_inv_supply_incorpo, "try-error")) D_inv_supply_incorpo <- ginv(D_supply_incorpo)

left_matrix_supply <- t(phi_supply_incorpo) %*% D_inv_supply_incorpo
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_supply_incorpo <- solve(t(left_matrix_supply) %*% left_matrix_supply) %*% t(left_matrix_supply) %*% right_term_supply_incorpo

lambda_0_log_supply_incorpo <- get_lambda_0(C, theta_0_supply_incorpo, irhor=hori)


# save_rds_path_hd <- "Z:/prix_supply_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/structur_shock"
# setwd(save_rds_path_hd)
# #(as.data.frame(structural_shocks_IV_demand), "structural_shocks_IV_demand.parquet" )
# write_parquet(as.data.frame(structural_shocks_IV_gen), "structural_shocks_IV_gen.parquet" )




#Plot the shocks
data_IV_incorpo <- as.data.frame(t(rbind(structural_shocks_IV_supply_incorpo, structural_shocks_IV_demand)))
colnames(data_IV_incorpo) <- c(  "supply_incorpo", "demand")
dates_formatted <- as.yearmon(time(x_supply))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
data_IV_incorpo_all$observation_date <- dates_formatted

data_IV_incorpo_all <- cbind(data_IV, data_IV_incorpo$supply_incorpo)
colnames(data_IV_incorpo_all) <- c(  "supply", "demand",  "supply_incorpo")


my_stru_shock_simple <- ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV_incorpo_all$demand, color = "Demand"))+
  geom_line(aes(x = dates_formatted, y = data_IV_incorpo_all$supply/10, color = "Supply")) +
  geom_line(aes(x = dates_formatted, y = data_IV_incorpo_all$supply_incorpo/10, color = "Supply With Wedge")) +
  theme_minimal()+
  labs(title = "Structural Shocks from IV Analysis",
       x_supply = "Date",
       y = "Shock Value")+
  scale_color_manual(values = c( "Demand" = "red",
                                 "Supply" = "lightblue",
                                 "Supply With Wedge" ="darkblue"
                                 )
  )

my_stru_shock_simple


data_IV_incorpo_all %>% 
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
n_vars_supply_incorpo <- ncol(x_supply)
historical_decomp_iv_tibo <-array(NA, dim = c(n_obs,  n_vars_supply_incorpo, 2))

#j is the variable of interest
for (j in c(1:n_vars_supply_incorpo)){
  for (t in c(1:T)){
    contribution_supply_incorpo <-0
    contribution_demand <- 0
    #hori = 1
    for (hori in c(1:irhor)){
      # ne doit pas dépasser l'horizon pour lequel on calcule
      # restriction pour les premiers chocs où l'on a pas les 20 précédents
      #pn prend juste ceux_supply qui sont disponibles
      shock_horizon = max(0,t-hori+1)
      contrib_hori_supply_incorpo <- lambda_0_log_supply_incorpo[hori,j,1]*structural_shocks_IV_supply_incorpo[1,shock_horizon]
      contrib_hori_demand <- lambda_0_log_demand[hori,j,1]*structural_shocks_IV_demand[1,shock_horizon]
      if (!length(as.numeric(contrib_hori_supply_incorpo))==0){
        contribution_supply_incorpo <- contribution_supply_incorpo + contrib_hori_supply_incorpo
      }
      if (!length(as.numeric(contrib_hori_demand))==0){
        contribution_demand <- contribution_demand + contrib_hori_demand
      }
    }
    historical_decomp_iv_tibo[t,j,1] <- contribution_supply_incorpo
    historical_decomp_iv_tibo[t,j,2] <- contribution_demand
  }
}

historical_decomp_iv_tibo
apply(historical_decomp_iv_tibo, c(2,3), function(x_supply) cumsum(x_supply))



# historical_decomp_ex_supplytended <- array(NA, dim = c(n_obs,  n_vars_supply_incorpo, (n_vars_supply_incorpo +1)))
# historical_decomp_ex_supplytended[,,1:n_vars_supply_incorpo] <- hd_cholesky
# historical_decomp_ex_supplytended[,,(n_vars_supply_incorpo +1)] <- hd_controls

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo, "hd_iv_log_incorpo.rds")


# hd_cholesky <- readRDS( "hd_chol_4v.rds")
# historical_decomp_iv_tibo <- readRDS( "hd_iv_4v.rds")




# hd_pelec <- plot_hd(historical_decomp_iv_tibo, series = 1)
# print(hd_pelec)


















