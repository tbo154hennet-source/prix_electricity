
#For Esp
headers_var_gen_first_decomp = c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  # "ECB_rate_log",
  "E_Index_log",
  # "Exp_pi_log",
  # "UnE_log" ,
  "NE_Index_log"
)
#hd_pelec <- plot_hd(hd_cholesky, series = 1)
#print(hd_pelec)

# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step


proxies <- ts(IV_data_log_esp[, c(headers_proxies)], frequency=12)
proxies[is.na(proxies)] <- 0


vars<- ts(IV_data_log_esp[, c(headers_var_gen_first_decomp)], frequency=12)

p = 4     
irhor = 40
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



estimate_log <- first_var_estimation(x,y,p, proxies, irhor)
theta_0 <- estimate_log$theta_0
gamma <- estimate_log$gamma
phi <- estimate_log$phi
var_coef_log <- estimate_log$coef

AL <- do.call(cbind, lapply(var_coef_log, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log)

n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)


lambda_0_iv <- get_lambda_0(C, theta_0, irhor = 40)



gen_benchmark_esp_decomp <- run_VAR_bootstrap(df = IV_data_log_esp,
                                          headers_var = headers_var_gen_first_decomp ,
                                          headers_proxies = headers_proxies,
                                          p = 4,
                                          irhor = 40,
                                          nboot = 1000,
                                          block_size = 15,
                                          start_ts = c(2015, 1),
                                          end_ts = c(2023, 12))

setwd(save_rds_path)
saveRDS(gen_benchmark_esp_decomp, "gen_benchmark_esp_decomp.rds")



library(latex2exp)
colors_shock <- c("blue", "red", "green", "yellow", "purple")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI", "$E Index$", "$NE Index$")
#custom legend part
vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))
x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]

vl = vars::VAR(x, p, type = "none")
k  = ncol(proxies)
n = ncol(y)
coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)

irf_cholesky <- irf(vl, impulse = colnames(x), response = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)
#plot(irf_cholesky)
# save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
# setwd(save_rds_path)
# saveRDS(irf_cholesky, file = "irf_cholesky_esp.rds")



# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute historical decomposition for each variable
# Initialize matrix to store historical decomposition
n_obs <- nrow(res)
n_vars <- ncol(res)
hd_cholesky <-array(NA, dim = c(n_obs,  n_vars, n_vars))

# class(irf_model$irf)
# length(irf_model$irf)
# nrow(irf_model$irf[[1]])

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
  }
  
}









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


setwd(save_rds_path)
saveRDS(historical_decomp_iv_tibo, "hd_iv_E_NE.rds")
saveRDS(hd_cholesky, "hd_chol_E_NE.rds")


# hd_pelec <- plot_hd(historical_decomp_iv_tibo, series = 1)
# print(hd_pelec)







#je pense que ça revient à multiplier et diviser par la sd
# pas de correction à faire


