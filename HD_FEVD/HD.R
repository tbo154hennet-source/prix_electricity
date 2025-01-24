library(latex2exp)
colors_shock <- c("blue", "red", "green", "yellow")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$NE Index$")
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


# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute impulse response functions (IRFs)
irf_model <- irf(vl, impulse=NULL, response=NULL, n.ahead=19)
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
      for (hori in c(1:20)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- irf_model$irf[[shock]][hori,j]*structural_shocks[shock_horizon,shock]
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


#hd_pelec <- plot_hd(hd_cholesky, series = 1)
#print(hd_pelec)

# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step

k <- ncol(proxies)

#rename the log parameters
gamma <- gamma_log
theta_0 <- theta_0_log
phi <- phi_log
lambda_0 <- lambda_0_log


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

historical_decomp_iv_tibo <-array(NA, dim = c(n_obs,  n_vars, k))
library(data.table)

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
      for (hori in c(1:20)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0[hori,j, shock]*structural_shocks_IV[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo[t,j, shock] <- contribution
    }
  }
  
}

#hd_pelec <- plot_hd(historical_decomp_iv_tibo, series = 1)
print(hd_pelec)





#On se demande à quel point il ne faut pas ajuster les structural shocks IV en renormalisant
#on regarde des summary stat

colnames(structural_shocks)
as.data.frame(structural_shocks) %>% 
  select(gen_tot_log, spot_price_log) %>% 
  summary()

t(structural_shocks_IV) %>% 
  summary()


irf_model$irf[[1]]
irf_model$irf[[2]]
lambda_0_log

#je pense que ça revient à multiplier et diviser par la sd
# pas de correction à faire


