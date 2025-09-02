# on estime la matrice de covariance associée à gamma
# car il existe un TCL sur gamma
#l is block length

library(dplyr)
library(tidyr)
library(arrow)
library(lubridate)

gamma_bootstrap_mbb_alternative_bis <- function(x,nboot, vars, BlockSize, theta_0, proxies, controls=NULL, elas = 0,
                                            start_ts = c(2015,1), end_ts =c(2023,12)
                                            ){
  #nboot <-100
  #BlockSize <- 15
  # Create an empty list to hold the elements
  n = ncol(vars)
  theta_0<- as.matrix(theta_0)
  k <- ncol(theta_0)
  T_bis <- nrow(x) -p
  boot_gamma <- array(NA, dim = c(n, l, nboot))
  boot_psi <- array(NA, dim = c(n, l, nboot))
  boot_ncvg_theta_0 <- array(NA, dim = c(n, k, nboot))
  boot_ncvg_lambda_01 <- array(NA, dim = c(irhor, n, nboot))
  boot_ncvg_lambda_02 <- array(NA, dim = c(irhor, n, nboot))
  boot_inv <- array(NA, dim = c(n*irhor,n , nboot))
  
  
  nBlock <- ceiling((T_bis) / BlockSize)
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  res = residuals(vl)
  for (irep in 1:nboot) {
    #irep=1
    # Preallocate resampled matrices
    X_bt <- matrix(NA, nrow = T_bis, ncol = n)
    Z_bt <- matrix(NA, nrow = T_bis, ncol = l)
    
    # Step 1: Pre-sample block endpoints
    block_indices <- c()  # to collect row indices
    while (length(block_indices) < T_bis) {
      endpoint <- sample(BlockSize:T_bis, size = 1)
      start_idx <- endpoint - BlockSize + 1
      this_block <- start_idx:endpoint
      block_indices <- c(block_indices, this_block)
    }
    
    # Step 2: Trim to T_bis
    block_indices <- block_indices[1:T_bis]
    
    # Step 3: Resample both res and proxies using same indices
    X_bt <- res[block_indices, , drop = FALSE]
    Z_bt <- as.data.frame(proxies)[block_indices, , drop = FALSE]
    
    
    # Fit VAR to bootstrapped data
    boot_df <- as.data.frame(X_bt)
    colnames(boot_df) <- paste0("V", 1:n)
    
    varbsnew <- iterate_var_ter(x, vars[1:p,], X_bt, vl, controls)
  
    xnew =ts(varbsnew , start= start_ts , end=end_ts, frequency=12)
    ynew = varbsnew[-1:-p, ]
    
    vnew <- if (is.null(controls)) {
      vars::VAR(xnew, p, type = "none")
    } else {
      vars::VAR(xnew, p, type = "none", exogen = controls)
    }
    # 
    #partie pour l'estimation de C_k(A)
    ALnew <- do.call(cbind, lapply(coef(vnew), function(df) df[, 1]))
    Marep_new <- MARep(t(ALnew), p, irhor)
    boot_inv[,,irep] <- Marep_new
    
    if (ncol(Z_bt) ==1) {
      proxies_boot <- tryCatch({
        if ("ts" %in% class(proxies)) {
          head_part <- window(proxies, end = time(proxies)[p])
        } else {
          head_part <- proxies[seq_len(p), , drop = FALSE]
        }
        
        head_mat <- as.matrix(head_part)
        Z_mat    <- as.matrix(Z_bt)
        
        rbind(head_mat, Z_mat)
        
      }, error = function(e) {
        message("Erreur dans la construction de proxies_boot : ", e$message)
        return(NULL)
      })
      
      
    }else{
      proxies_boot <- rbind(proxies[1:p,],Z_bt)
    }
    
    
    
    #partie pour estimer theta_0
    estimatenew <- first_var_estimation(xnew, ynew,
                                        p, proxies_boot , irhor, controls, elas)
    # estimatenew <- first_var_estimation(xnew, ynew,
    #                                     p, proxies , irhor)
    boot_gamma[,,irep] <- estimatenew$gamma
    boot_psi[,,irep] <- sqrt(T) * (estimatenew$gamma - theta_0 %*% estimatenew$phi)
    phi_hat <- estimatenew$phi
    boot_ncvg_theta_0[,,irep] <- estimatenew$theta_0
    # on notera que pour des instruments très faibles l'intervalle va exploser
    # en effet phi tend vers 0 comme son determinant
    
    boot_ncvg_lambda_01[,,irep]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,1]
    if (k >=2){
      boot_ncvg_lambda_02[,,irep]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,2] 
    }
    
    
    if (irep %% 50 == 0) cat("Replication", irep, "\n")
    
  }
  return(list(boot_gamma= boot_gamma, boot_psi =boot_psi ,
              boot_ncvg_theta_0 = boot_ncvg_theta_0,
              boot_ncvg_lambda_01 = boot_ncvg_lambda_01,
              boot_ncvg_lambda_02 = boot_ncvg_lambda_02,
              boot_inv= boot_inv)) #, boot_reliability = boot_reliability))
}




eval_composantes_V0 <- function(i, hori, sample_matrices){
  #i=2
  e_i <- rep(0, n)
  # Mettre 1 à la position j
  e_i[i] <- 1
  # dim(sample_matrices$gamma)
  # sample_matrices$gamma[,,1]
  boot_terme_gauche <- matrix(0, nrow=nboot , ncol=l )
  boot_covariance <- array(NA, dim = c(nboot, k, k**2))
  boot_terme_droite <- matrix(0, nrow=nboot , ncol=k*l)
  
  list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                       "ncvg_lambda_01", "ncvg_lambda_02", "A")
  for (b in c(1:nboot)){
    b=1
    gamma_hat <- matrix(sample_matrices[["gamma"]][,,b], nrow=n, ncol=l)
    
    Marep_hat <- matrix(sample_matrices[["inv"]][,,b],nrow = n*(irhor),ncol = n) 
    
    C_hat <- array(Marep_hat, dim = c(n, n, irhor))
    
    
    phi_c0_hat <- matrix(c(1,0, -c0, 1), nrow = 2, ncol = 2) %*%
      t(matrix_12)  %*% gamma_hat
    boot_terme_gauche[b,] <- t(e_i) %*% C_hat[,,hori] %*% gamma_hat
    boot_terme_droite[b,] <- as.vector(phi_c0_hat)
  }

  
  gauche <- cov(boot_terme_gauche)
  droite <- cov(boot_terme_droite)
  my_cov <- cov(boot_terme_gauche ,boot_terme_droite )
  
  # mean_matrices <- apply(boot_covariance, c( 2, 3), mean)
  # # my_cov <- matrix( apply(boot_covariance, 2, mean), nrow= k, ncol=k**2)
  # View(t(apply(boot_covariance, c( 2, 3), mean)))
  return( list(gauche = gauche, droite=droite, my_cov = my_cov))
  
}

#V0 <- eval_V0(a_0, b_0, gauche, my_cov, droite)
## 
# Q_T <- sqrt(T)*(t(e_i) %*% C[,,hori] %*% gamma -
#                   t(c(a_0, b_0)) %*% phi_c0)
# Wald_stat <-  Q_T %*% ginv(V0) %*% t(Q_T)


eval_V0 <- function(a_0, b_0,gauche, my_cov, droite){
  
  mat_vec <-  cbind(diag(rep(a_0, times = l)), diag(rep(b_0, times = l)))
  #mat_vec %*% as.vector(t(phi_c0_hat))
  #t(phi_c0_hat) %*% matrix(c(a_0, b_0), ncol = 1)
  # a_0* phi_c0_hat[1,] + b_0* phi_c0_hat[2,]
  # return(T*(gauche -  2*my_cov%*% t(mat_vec) +
  # mat_vec %*% droite  %*% t(mat_vec)))
  
  return(T*(gauche -  2*my_cov%*% t(mat_vec) +
              mat_vec %*% droite  %*% t(mat_vec)))
  
}



#fonction bootstrap simple en réchantillonant avec va eta
#selon Gonzales and Kilian(2004)
gamma_bootstrap_GK <- function(nboot, vars, theta_0, controls= NULL){
  # conf<- my_conf
  # nboot <-100
  # Create an empty list to hold the elements
  n = ncol(y)
  l <- ncol(proxies)
  boot_gamma <- array(NA, dim = c(n, l, nboot))
  boot_psi <- array(NA, dim = c(n, l, nboot))
  boot_ncvg_theta_0 <- array(NA, dim = c(n, k, nboot))
  boot_ncvg_lambda_01 <- array(NA, dim = c(irhor, n, nboot))
  boot_ncvg_lambda_02 <- array(NA, dim = c(irhor, n, nboot))
  boot_inv <- array(NA, dim = c(n*irhor,n , nboot))
  
  
  for (i in c(1:nboot)){
    #i=1
    vl <- if (is.null(controls)) {
      vars::VAR(x, p, type = "none")
    } else {
      vars::VAR(x, p, type = "none", exogen = controls)
    }
    res = residuals(vl)
    rr <- sample(c(-1,1), size=T, replace=TRUE)
    #mean(res)
    #res2 <- resid(lm(res ~ 1))
    res2 <- res*rr
    #varbsnew2 <- rbind(vars[1:p,], iterate_var_bis(vars[1:p,], res2, vl, controls))
    
    
    varbsnew <-  iterate_var_ter(vars[1:p,], res2, vl, controls)
    
    xnew =ts(varbsnew , start=c(2015,1), end=c(2023,12), frequency=12)
    ynew <- varbsnew[-1:-p, ]
    proxiesnew <-rbind(proxies[c(1:p),],proxies[c(-1:-p),]*rr)
    vnew <- if (is.null(controls)) {
      vars::VAR(xnew, p, type = "none")
    } else {
      vars::VAR(xnew, p, type = "none", exogen = controls)
    }
    #partie pour l'estimation de C_k(A)
    ALnew <- do.call(cbind, lapply(coef(vnew), function(df) df[, 1]))
    Marep_new <- MARep(t(ALnew), p, hori)
    boot_inv[,,i] <- Marep_new
    
    #partie pour estimer theta_0
    estimatenew <- first_var_estimation(xnew, ynew,  p, proxiesnew , irhor, controls)
    boot_gamma[,,i] <- estimatenew$gamma
    boot_psi[,,i] <- sqrt(T) * (estimatenew$gamma - theta_0 %*% estimatenew$phi)
    phi_hat <- estimatenew$phi
    boot_ncvg_theta_0[,,i] <- estimatenew$theta_0
    
    #print(estimatenew$theta_0[1,2])
    # on notera que pour des instruments très faibles l'intervalle va exploser
    # en effet phi tend vers 0 comme son determinant
    
    boot_ncvg_lambda_01[,,i]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,1]
    boot_ncvg_lambda_02[,,i]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,2]
    # boot_det_theta_0[,,i] <- my_theta_0
    
    # ginv(phi_hat)*det(phi_hat)
    #pas sur de savoir si l'on doit prendre l'estimation contemporaine
    # du boostrap sample ou bien l'estimation centrale pour calculer
    # les derives associees
    
    # boot_reliability[i,] <- reliability(b11b11p)
    
    if (i %% 50 == 0) {
      print(i)
    }
    
  }
  return(list(boot_gamma= boot_gamma, boot_psi =boot_psi ,
              boot_ncvg_theta_0 = boot_ncvg_theta_0,
              boot_ncvg_lambda_01 = boot_ncvg_lambda_01,
              boot_ncvg_lambda_02 = boot_ncvg_lambda_02,
              boot_inv= boot_inv )) #, boot_reliability = boot_reliability))
}




#V0 <- eval_V0(a_0, b_0, gauche, my_cov, droite)

bootstrap_weak_instrument <- function(gamma, c_0, phi_c0, alpha, ngrid = 100, sample_matrices, lambda_0){
  #lambda_0 = lambda_0_log
  #sample_matrices <- sample_matrices_log_GK
  #gamma <- gamma_log
  
  q_alpha_bonferroni <- qchisq(1 - alpha/irhor, df= 2)
  q_alpha_dim2 <- qchisq(1 - alpha, df= 2)
  
  # Initialize a 4D array to store all bounds
  # Dimensions: (irhor, n, k, 2), where the last dimension is for standard and Bonferroni bounds
  
  weak_lambda_bounds_Wald_stat <- array(NA, dim = c(irhor, n, 2, 2)) 
  dimnames(weak_lambda_bounds_Wald_stat) <- list(
    NULL, NULL, c("lower", "upper"), c("lambda_01", "lambda_02")
  )
  weak_lambda_bounds_bonferroni <- array(NA, dim = c(irhor, n, 2, 2)) 
  dimnames(weak_lambda_bounds_bonferroni) <- list(
    NULL, NULL, c("lower", "upper"), c("lambda_01", "lambda_02")
  )
  
  
  #lambda_0 <- lambda_0_log
  
  for (hori in c(1:irhor)){
    print(hori)
    for (i in c(1:n)){
      # hori=2
      #i=3
      e_i <- rep(0, n)
      # Mettre 1 à la position j
      e_i[i] <- 1
      a_central <- lambda_0[hori,i,1]
      b_central <- lambda_0[hori,i,2]
      
      step_a <- strong_std_lambda1[hori,i]
      step_b <- strong_std_lambda2[hori,i]
      # Construire la séquence
      grid_a <- seq(from = a_central - step_a,
                    to = a_central + step_a, length.out = ngrid)
      
      grid_b <- seq(from = b_central - step_b,
                    to = b_central + step_b, length.out = ngrid)
      #je suis oblige d'explorer la frontiere computationnellement
      #on doit echantilloner suffisamment finement
      # Example
      # Génération de la grille
      grid_ab <- expand.grid(list(grid_a, grid_b))
      
      #sample_matrices <- sample_matrices_log_GK
      
      compo_V0 <- eval_composantes_V0(i, hori, sample_matrices)
      gauche <- compo_V0$gauche
      my_cov <- compo_V0$my_cov
      droite <- compo_V0$droite

      list_a <- c()
      list_b <- c()
      reject_a <- c()
      reject_b <- c()
      list_a_bonferroni <- c()
      list_b_bonferroni <- c()
      
      
      for (igrid in c(1:nrow(grid_ab))){
        #igrid <- 2
        ab_test <- grid_ab[igrid,]
        a_0 <- ab_test[[1]]
        b_0 <- ab_test[[2]]
        
        #gamma <- gamma_log
        V0 <- eval_V0(a_0, b_0, gauche, my_cov, droite)
        
        Q_T <- sqrt(T)*(t(e_i) %*% C[,,hori] %*% gamma -
                          t(c(a_0, b_0)) %*% phi_c0)
        Wald_stat <-  Q_T %*% ginv(V0) %*% t(Q_T)
        
        
        if (Wald_stat < q_alpha_dim2 ){
          list_a <- c(list_a, a_0)
          list_b <- c(list_b, b_0)
        } else{
          reject_a <- c(reject_a, a_0)
          reject_b <- c(reject_b, b_0)
          #comparaison V_0 et V_0_bis
          # + impact sur la Wald stat
        }
        if (Wald_stat < q_alpha_bonferroni ){
          list_a_bonferroni <- c(list_a_bonferroni, a_0)
          list_b_bonferroni <- c(list_b_bonferroni, b_0)
          
        }
      }
      #check le pas discret d'exploration
      # de l'despace des a,b est très inferieur à l'erreur trouvee
      
      step_a
      min(list_a)
      max(list_a)
      
      # Populate the array
      #weak_lambda_bounds_Wald_stat[hori, i,,] <- c(min(list_a), min(list_b),
      #                              max(list_a), max(list_b))
      weak_lambda_bounds_Wald_stat[hori, i,"lower","lambda_01"] <- min(list_a)
      weak_lambda_bounds_Wald_stat[hori, i,"lower","lambda_02"] <- min(list_b)
      weak_lambda_bounds_Wald_stat[hori, i,"upper","lambda_01"] <- max(list_a)
      weak_lambda_bounds_Wald_stat[hori, i,"upper","lambda_02"] <- max(list_b)
      
      weak_lambda_bounds_bonferroni[hori, i,"lower","lambda_01"] <- min(list_a_bonferroni)
      weak_lambda_bounds_bonferroni[hori, i,"lower","lambda_02"] <- min(list_b_bonferroni)
      weak_lambda_bounds_bonferroni[hori, i,"upper","lambda_01"] <- max(list_a_bonferroni)
      weak_lambda_bounds_bonferroni[hori, i,"upper","lambda_02"] <- max(list_b_bonferroni)
      
      
    }
  }
  return(list(weak_lambda_bounds_Wald_stat= weak_lambda_bounds_Wald_stat,
              weak_lambda_bounds_bonferroni = weak_lambda_bounds_bonferroni))
}



# gamma_bootstrap_mbb_alternative_var_res <- function(x,nboot, vars, BlockSize, theta_0, proxies, controls=NULL, elas = 0){
#   #nboot <-100
#   #BlockSize <- 15
#   # Create an empty list to hold the elements
#   n = ncol(vars)
#   theta_0<- as.matrix(theta_0)
#   k <- ncol(theta_0)
#   T_bis <- nrow(vars) - p
#   boot_gamma <- array(NA, dim = c(n, l, nboot))
#   boot_psi <- array(NA, dim = c(n, l, nboot))
#   boot_ncvg_theta_0 <- array(NA, dim = c(n, k, nboot))
#   boot_ncvg_lambda_01 <- array(NA, dim = c(irhor, n, nboot))
#   boot_ncvg_lambda_02 <- array(NA, dim = c(irhor, n, nboot))
#   boot_inv <- array(NA, dim = c(n*irhor,n , nboot))
#   boot_vl_coef <-
#   boot_res <-
#   
#   nBlock <- ceiling((T_bis) / BlockSize)
#   vl <- if (is.null(controls)) {
#     vars::VAR(x, p, type = "none")
#   } else {
#     vars::VAR(x, p, type = "none", exogen = controls)
#   }
#   res = residuals(vl)
#   for (irep in 1:nboot) {
#     #irep=1
#     # Preallocate resampled matrices
#     X_bt <- matrix(NA, nrow = T_bis, ncol = n)
#     Z_bt <- matrix(NA, nrow = T_bis, ncol = l)
#     
#     # Step 1: Pre-sample block endpoints
#     block_indices <- c()  # to collect row indices
#     while (length(block_indices) < T_bis) {
#       endpoint <- sample(BlockSize:T_bis, size = 1)
#       start_idx <- endpoint - BlockSize + 1
#       this_block <- start_idx:endpoint
#       block_indices <- c(block_indices, this_block)
#     }
#     
#     # Step 2: Trim to T_bis
#     block_indices <- block_indices[1:T_bis]
#     
#     # Step 3: Resample both res and proxies using same indices
#     X_bt <- res[block_indices, , drop = FALSE]
#     Z_bt <- proxies[block_indices, , drop = FALSE]
#     
#     
#     # Fit VAR to bootstrapped data
#     boot_df <- as.data.frame(X_bt)
#     colnames(boot_df) <- paste0("V", 1:n)
#     
#     varbsnew <- iterate_var_ter(x, vars[1:p,], X_bt, vl, controls)
#     
#     xnew =ts(varbsnew , start=c(2015,1), end=c(2023,12), frequency=12)
#     ynew = varbsnew[-1:-p, ]
#     
#     vnew <- if (is.null(controls)) {
#       vars::VAR(xnew, p, type = "none")
#     } else {
#       vars::VAR(xnew, p, type = "none", exogen = controls)
#     }
#     # 
#     #partie pour l'estimation de C_k(A)
#     ALnew <- do.call(cbind, lapply(coef(vnew), function(df) df[, 1]))
#     Marep_new <- MARep(t(ALnew), p, irhor)
#     boot_inv[,,irep] <- Marep_new
#     
#     if (ncol(Z_bt) ==1) {
#       proxies_boot <- rbind(matrix(proxies[1:p,]),Z_bt)
#     }else{
#       proxies_boot <- rbind(proxies[1:p,],Z_bt)
#     }
#     
#     
#     
#     #partie pour estimer theta_0
#     estimatenew <- first_var_estimation(xnew, ynew,
#                                         p, proxies_boot , irhor, controls, elas)
#     # estimatenew <- first_var_estimation(xnew, ynew,
#     #                                     p, proxies , irhor)
#     boot_res[,,irep] <- X_bt
#     boot_vl_coef[,,irep] <- estimatenew$gamma
#     
#     boot_gamma[,,irep] <- estimatenew$gamma
#     boot_psi[,,irep] <- sqrt(T) * (estimatenew$gamma - theta_0 %*% estimatenew$phi)
#     phi_hat <- estimatenew$phi
#     boot_ncvg_theta_0[,,irep] <- estimatenew$theta_0
#     # on notera que pour des instruments très faibles l'intervalle va exploser
#     # en effet phi tend vers 0 comme son determinant
#     
#     boot_ncvg_lambda_01[,,irep]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,1]
#     if (k >=2){
#       boot_ncvg_lambda_02[,,irep]  <- get_lambda_0(Marep_new, estimatenew$theta_0)[,,2] 
#     }
#     
#     
#     if (irep %% 50 == 0) cat("Replication", irep, "\n")
#     
#   }
#   return(list(boot_gamma= boot_gamma, boot_psi =boot_psi ,
#               boot_ncvg_theta_0 = boot_ncvg_theta_0,
#               boot_ncvg_lambda_01 = boot_ncvg_lambda_01,
#               boot_ncvg_lambda_02 = boot_ncvg_lambda_02,
#               boot_inv= boot_inv)) #, boot_reliability = boot_reliability))
# }

