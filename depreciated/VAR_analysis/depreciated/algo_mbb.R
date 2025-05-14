
library(arrow)

path = 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/python_bootstrap_matrix'  # Remplacez par votre chemin
setwd(path)
mbb_bootstrap_indices <- read_parquet("X_bootstrapped.parquet")

gamma_bootstrap_mbb_bboot <- function(nboot, vars, BlockSize, theta_0, controls=NULL, log_level=FALSE){
  #nboot <-100
  #BlockSize <- 15
  # Create an empty list to hold the elements
  n = ncol(y)
  k <- 2
  boot_gamma <- array(NA, dim = c(n, l, nboot))
  boot_psi <- array(NA, dim = c(n, l, nboot))
  boot_ncvg_theta_0 <- array(NA, dim = c(n, k, nboot))
  boot_ncvg_lambda_01 <- array(NA, dim = c(irhor, n, nboot))
  boot_ncvg_lambda_02 <- array(NA, dim = c(irhor, n, nboot))
  boot_inv <- array(NA, dim = c(n*irhor,n , nboot))
  
  
  nBlock <- ceiling(T / BlockSize)
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  res = residuals(vl)
  
  
  # Bootstrap loop
  b <- 1
  while (b <= nboot) {
  #différence d'indexation 0 et 1 en ptyhon/R
    mbb_indexes <- as.numeric(t(mbb_bootstrap_indices[b,])) +1
    res_sample <- res[as.numeric(mbb_indexes),]
    dim(res_sample)
    length(mbb_indexes)
    varbsnew <- iterate_var_ter(vars[1:p,], res_sample, vl, controls)
    
    
    xnew =ts(varbsnew , start=c(2015,1), end=c(2023,12), frequency=12)
    ynew = varbsnew[-1:-p, ]

    vnew <- if (is.null(controls)) {
      vars::VAR(xnew, p, type = "none")
    } else {
      vars::VAR(xnew, p, type = "none", exogen = controls)
    }
    # 
    #partie pour l'estimation de C_k(A)
    ALnew <- do.call(cbind, lapply(coef(vnew), function(df) df[, 1]))
    Marep_new <- MARep(t(ALnew), p, hori)
    boot_inv[,,b] <- Marep_new
    
    proxies_boot_sample <- proxies[-1:-p, ][as.numeric(mbb_indexes),]
    proxies_boot <- rbind(proxies[1:p,], proxies_boot_sample)
    
    #partie pour estimer theta_0
    estimatenew <- first_var_estimation(xnew, ynew,
                                        p, proxies_boot , irhor, controls)
    # estimatenew <- first_var_estimation(xnew, ynew,
    #                                     p, proxies , irhor)
    boot_gamma[,,b] <- estimatenew$gamma
    boot_psi[,,b] <- sqrt(T) * (estimatenew$gamma - theta_0 %*% estimatenew$phi)
    phi_hat <- estimatenew$phi
    boot_ncvg_theta_0[,,b] <- estimatenew$theta_0
    # on notera que pour des instruments très faibles l'intervalle va exploser
    # en effet phi tend vers 0 comme son determinant
    
    boot_ncvg_lambda_01[,,b]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,1]
    boot_ncvg_lambda_02[,,b]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,2]
    # boot_det_theta_0[,,i] <- my_theta_0
    if (b %% 50 == 0) {
      print(b)
    }
    b<-b+1
    
  }
  return(list(boot_gamma= boot_gamma, boot_psi =boot_psi ,
              boot_ncvg_theta_0 = boot_ncvg_theta_0,
              boot_ncvg_lambda_01 = boot_ncvg_lambda_01,
              boot_ncvg_lambda_02 = boot_ncvg_lambda_02,
              boot_inv= boot_inv)) #, boot_reliability = boot_reliability))
}


