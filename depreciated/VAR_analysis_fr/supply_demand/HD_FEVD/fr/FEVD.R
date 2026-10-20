# vars<- ts(IV_data_log_esp[, c(headers_var)], frequency=12)
# x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
# vl = vars::VAR(x, p, type = "const")
# k  = ncol(proxies)
# n = ncol(y)
# coef = coef(vl)
# bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
# 
# res = residuals(vl)
# Sigma_eta = cov(res)
# # Perform Cholesky decomposition
# P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u
# # Transform residuals into structural shocks using P
# structural_shocks <-   res %*%  solve(P)
# # Compute impulse response functions (IRFs)
# irf_model <- irf(vl, impulse=NULL, response=NULL, n.ahead=19)



fevd_contrib <-array(NA, dim = c(irhor,  n, n))
fevd <-array(NA, dim = c(irhor,  n, n))
#j is the variable of interest
for (j in c(1:n)){
  for (forecast_horizon in c(1:irhor)){
    #forecast_horizon =5
    for (shock in c(1:n)){
      contribution <-0
      for (hori in c(1:forecast_horizon)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        #shock_horizon = max(0,t-hori+1)
        contrib_hori <- irf_cholesky$irf[[shock]][hori,j]^2
        #lambda_0[hori, j, shock]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      fevd_contrib[forecast_horizon ,j, shock ] <- contribution
    }
    total <- sum(fevd_contrib[forecast_horizon ,j,])
    fevd[forecast_horizon ,j,] <- fevd_contrib[forecast_horizon ,j,]/total
  }
}
View(fevd)

#version IV


# Calculate the standard deviation of the residuals
sigma_diag <- apply(res, 2, sd)


partial_shock <- t(gamma) %*% solve(Sigma_eta) %*% t(res)



library(data.table)

lambda_0_iv <- copy(lambda_0)

lambda_0_iv[,,1] <- lambda_0[,,1] * irf_model$irf[[1]][1,1]
lambda_0_iv[,,2] <- lambda_0[,,2] * irf_model$irf[[2]][1,2]

k=2
fevd_contrib_iv <-array(NA, dim = c(irhor,  n, shock))
fevd_iv <-array(NA, dim = c(irhor,  n, shock))
#j is the variable of interest
for (j in c(1:n)){
  for (forecast_horizon in c(1:irhor)){
    #forecast_horizon =5
    for (shock in c(1:k)){
      contribution <-0
      for (hori in c(1:forecast_horizon)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        #shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0_iv[hori,j, shock]^2
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      fevd_contrib_iv[forecast_horizon ,j, shock ] <- contribution
    }
    #use total in cholesky
    total <- sum(fevd_contrib[forecast_horizon ,j,])
    fevd_iv[forecast_horizon ,j,] <- fevd_contrib_iv[forecast_horizon ,j,]/total
  }
}


# scaled_irf <- lambda_0
# for (j in c(1:n)){
#     #forecast_horizon =5
#     for (shock in c(1:k)){
#       contribution <-0
#       for (hori in c(1:irhor)){
#         scaled_irf[hori,j, shock] <- lambda_0[hori,j, shock]*sigma_diag[j]
#       }
#     }
# }

