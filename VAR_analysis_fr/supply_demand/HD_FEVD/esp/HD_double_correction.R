
library(gt)
#For France


p = 12     
irhor = 40


n_vars <- length(headers_var_gen_first_mp)

# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step
vars_double <- ts(IV_data_log_diff_esp[, c(headers_var_gen_first_mp)], frequency=12)

proxies_double <- ts(IV_data_log_diff_esp[, c(rev(headers_proxies_supply_demand))], frequency=12)
proxies_double[is.na(proxies_double)] <- 0
x_double =ts(vars_double , start=c(2015,1), end=c(2023,12), frequency=12)
y_double =vars_double[-1:-p, ]

#nbre de variables instrumentees
k<-2
#nbre d'instruments
l <- ncol(proxies_double)

vl_double = vars::VAR(x_double, p, type = "none")#, exogen = controls)
coef_double =coef(vl_double)
bet_double = do.call(cbind, lapply(1:n_vars, function(x) coef_double[[x]][,1]))
res_double = residuals(vl_double)
Sigma_eta_double = cov(res_double)

controls <- NULL


estimate_log_diff_double <- first_var_estimation(x_double,y_double,p, proxies_double, irhor, elas = -0.29)
theta_0_double <- estimate_log_diff_double$theta_0
gamma_double <- estimate_log_diff_double$gamma
phi_double <- estimate_log_diff_double$phi
var_coef_log_diff_double <- estimate_log_diff_double$coef

AL_double <- do.call(cbind, lapply(var_coef_log_diff_double, function(df) df[, 1]))
# Optionally, set column names if needed
#colnames(AL_double) <- names(var_coef_log_diff_double)


#calculate sigma_aa
Sigma_a_double <- gamma_double %*%  ginv(t(gamma_double) %*% ginv(Sigma_eta_double) %*% gamma_double) %*% t(gamma_double)

Sigma_theta_double <- t(theta_0_double) %*% theta_0_double
D_double <- solve(Sigma_theta_double)  %*% t(theta_0_double) %*%  Sigma_a_double %*% theta_0_double %*% solve(Sigma_theta_double)

#almost diagonal matrix

right_term_double <- t(gamma_double) %*% ginv(Sigma_eta_double) %*% t(res_double)
D_inv_double <- try(solve(D_double), silent = TRUE); if (inherits(D_inv_double, "try-error")) D_inv_double <- ginv(D_double)

left_matrix_double <- t(phi_double) %*% D_inv_double
# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV_double <- solve(t(left_matrix_double) %*% left_matrix_double) %*% t(left_matrix_double) %*% right_term_double



#on se rend compte qu'il m'inverse les chocs de supply et demand 
#comment faire ?

structural_shocks_IV_double_bis <- structural_shocks_IV_double[c(2,1), ]
structural_shocks_IV_double_bis[2,] <- -structural_shocks_IV_double_bis[2,]


save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/structur_shock"
setwd(save_rds_path_hd)
write_parquet(as.data.frame(structural_shocks_IV_double_bis), "structural_shocks_IV_supply_demand.parquet" )



#Plot the shocks
dim(structural_shocks_IV_double_bis)
data_IV <- as.data.frame(t(rbind(structural_shocks_IV_double_bis,
                                 structural_shocks_IV_gen, structural_shocks_IV_spot) )
)
colnames(data_IV) <- c( "demand_double_bis", "supply_double_bis", "demand", "supply")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
my_stru_shock <- ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$demand_double_bis/10, color = "Demand", linetype = "Double"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply_double_bis, color = "Supply", linetype = "Double")) +
  geom_line(aes(x = dates_formatted, y = data_IV$demand, color = "Demand", linetype = "Baseline"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply/10, color = "Supply", linetype = "Baseline")) +
  theme_minimal()+
  scale_y_continuous(
    name = "Demand Shock",
    sec.axis = sec_axis(~ . * 10, name = "Supply Shock")
  ) +
  labs(title = "Structural Shocks from IV Analysis",
       x = "Date",
       y = "Shock Value")

my_stru_shock 
save_plot_path <- 'Z:/Overleaf_elec/images/macro'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("structural_shocks.png", plot = my_stru_shock , width = 12, height = 6, dpi = 300)





summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE)
)

data_IV %>% 
  cor()


library(data.table)
historical_decomp_iv_tibo_double <-array(NA, dim = c(n_obs,  n_vars, k))


hori <- 40 # Forecast horizon

# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C_double  <- MARep(t(AL_double), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_iv_double <- get_lambda_0(C = C_double, theta_0 =  theta_0_double, irhor= hori)
apply(lambda_0_iv_double , c(2, 3), function(x) cumsum(x))
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
        contrib_hori <- lambda_0_iv_double[hori,j, shock]*structural_shocks_IV_double[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo_double[t,j, shock] <- contribution
    }
  }
  
}


historical_decomp_iv_tibo_double
apply(historical_decomp_iv_tibo_double, c(2,3), function(x) cumsum(x))



# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls




lambda_0_iv_double_bis <- lambda_0_iv_double[,,c(2,1)]
lambda_0_iv_double_bis[,,2] <- -lambda_0_iv_double_bis[,,2]
apply(lambda_0_iv_double_bis , c(2, 3), function(x) cumsum(x))


#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)
historical_decomp_iv_tibo_double_bis <-array(NA, dim = c(n_obs,  n_vars, k))

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
        contrib_hori <- lambda_0_iv_double_bis[hori,j, shock]*structural_shocks_IV_double_bis[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo_double_bis[t,j, shock] <- contribution
    }
  }
  
}


historical_decomp_iv_tibo_double_bis
apply(historical_decomp_iv_tibo_double_bis, c(2,3), function(x) cumsum(x))

historical_decomp_iv_tibo_double_bis <- historical_decomp_iv_tibo_double_bis[, , c(2, 1)]

# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls

save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo_double_bis, "hd_iv_4v_double_bis.rds")
hd_cholesky_double_bis <- readRDS( "hd_chol_4v.rds")


# hd_pelec <- plot_hd(historical_decomp_iv_tibo_double_bis, series = 1)
# print(hd_pelec)





