# 
############### Modèle d'utilisation



# # Fit the VAR model
# vl <- if (is.null(controls)) {
#   vars::VAR(x, p, type = "none")
# } else {
#   vars::VAR(x, p, type = "none", exogen = controls)
# }
# 
# #k  = ncol(proxies)
# #n = ncol(y)
# 
# coef = coef(vl)
# bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
# res = residuals(vl)
# 
# theta_0 <- first_var_estimation_bis(y, res, p, proxies, irhor, controls = NULL, elas = 0)$theta_0
# 

# quantile_probs_90 <- c(0.05, 0.95)
# quantile_probs_68 <- c(0.16, 0.84)
# quantile_probs_50 <- c(0.5, 0.5)
# # Function to compute quantiles along the third dimension (simulations)
# compute_quantile_bands <- function(boot_array, probs) {
#   # boot_array: horizon+1 x nvar x nsim
#   apply(boot_array, c(1, 2), function(x) quantile(x, probs = probs, na.rm = TRUE))
# }
# theta_0_log_diff 
# bootIRFs <-gamma_bootstrap_mbb_corrected(x, nboot, vars, BlockSize, theta_0 = theta_0_log_diff , proxies = proxies, controls=NULL, elas = 0,
#                                          start_ts = c(2015,1), end_ts =c(2023,12))
# # 90% confidence bands (5%-95%)
# IRF_90 <- compute_quantile_bands(bootIRFs, quantile_probs_90)
# dim(IRF_90)
# IRF_90
# dim(bootIRFs)
# IRF_68 <- compute_quantile_bands(bootIRFs, quantile_probs_68)
# IRF_50 <- compute_quantile_bands(bootIRFs, quantile_probs_50)[1,,]

gamma_bootstrap_mbb_corrected <- function(x,nboot,  theta_0, proxies, controls=NULL, elas = 0,
                                          start_ts = c(2015,1), end_ts =c(2023,12), irhor=irhor){
  T_bis <- nrow(x) - p
  n <- ncol(x)
  BlockSize <- round(5 * T_bis**0.25)
  nBlock <- ceiling((T_bis) / BlockSize)

  
  # Center VAR residuals
  VARcentering <- sapply(1:BlockSize, function(j) {
    colMeans(res[j:(T - BlockSize + j), ])
  })
  VARcentering <- matrix(rep(VARcentering, each = nBlock), nrow = T, ncol = n, byrow = TRUE)
  centered_res <- res - VARcentering
  
  # Create blocks for residuals
  VARBlocks <- array(NA, dim = c(BlockSize, n, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    VARBlocks[,,j] <- centered_res[j:(j + BlockSize - 1), ]
  }
  
  # Proxy blocks
  if (k==1){
    proxies_short <- proxies[(p+1):length(proxies)]
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j)]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[1] != 0])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = 1, byrow = TRUE)
  } else{
    proxies_short <- proxies[(p+1):nrow(proxies), ]
    # Center proxy variables
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j), 1]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[,1] != 0, 1])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = ncol(proxies), byrow = TRUE)
    
  }
  centered_proxies <- proxies_short - Proxycentering
  
  ProxyBlocks <- array(NA, dim = c(BlockSize, k, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    if (k==1){
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1)]
    } else{
     ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1),]
    }
    
  }
  
  if(k==1){
    bootIRFs <- array(NA, dim = c(irhor, n, nboot))
  }else{
    bootIRFs <- array(NA, dim = c(irhor, n, nboot,k))
  }
  # bootDatas <- array(0, dim = c(T + p, n, nboot))
  # ProxyCount <- matrix(0, nrow = nboot, ncol = ncol(proxies))
  
  set.seed(123)
  j =1
  block_index_list <- replicate(
    nboot,
    sample(1:(T - BlockSize + 1), nBlock, replace = TRUE),
    simplify = FALSE
  )
  
  for (j in 1:nboot) {
    index <- block_index_list[[j]]
    # Reconstruct bootstrap residuals
    bootres <- do.call(rbind, lapply(index, function(ii) VARBlocks[,,ii]))
    #bootres <- bootres[(p+1):nrow(bootres),]
    bootProxy <- do.call(rbind, lapply(index, function(ii) ProxyBlocks[,,ii]))
    # Rassembler les blocs dans l’ordre
    if(k==1){
      bootProxy <- unlist(lapply(index, function(ii) ProxyBlocks[, ,ii]))
      bootProxy <- c(proxies[1:p, , drop = FALSE],bootProxy)
    }else{
      bootProxy <-rbind( proxies[1:p, , drop = FALSE],  bootProxy)
    }

    theta_0_boot <- first_var_estimation_bis(x, bootres, p, 
                                             bootProxy ,
                irhor, controls = NULL, elas = elas)$theta_0

    lambda_0_boot <- get_lambda_0(C, theta_0_boot, irhor=irhor)
    if(k==1){
      bootIRFs[,,j] <- lambda_0_boot
    }else{
      bootIRFs[,,j,] <- lambda_0_boot
    }
    
    # Re-estimate VAR (in R you can use vars::VAR)
    # bootvarEst <- vars::VAR(bootData[(p+1):(T+p), ], p = p, type = "const")
    
  }
  return(bootIRFs)
}





gamma_bootstrap_mbb_corrected_bis <- function(x,nboot, BlockSize, theta_0, proxies, controls=NULL, elas = 0,
                                              start_ts = c(2015,1), end_ts =c(2023,12), irhor=irhor){
  T_bis <- nrow(x) - p
  
  BlockSize <- round(5 * T_bis**0.25)
  nBlock <- ceiling((T_bis) / BlockSize)
  
  
  # Center VAR residuals
  VARcentering <- sapply(1:BlockSize, function(j) {
    colMeans(res[j:(T - BlockSize + j), ])
  })
  VARcentering <- matrix(rep(VARcentering, each = nBlock), nrow = T, ncol = n, byrow = TRUE)
  centered_res <- res - VARcentering
  
  # Create blocks for residuals
  VARBlocks <- array(NA, dim = c(BlockSize, n, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    VARBlocks[,,j] <- centered_res[j:(j + BlockSize - 1), ]
  }
  
  # Proxy blocks
  if (k==1){
    proxies_short <- proxies[(p+1):length(proxies)]
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j)]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[1] != 0])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = 1, byrow = TRUE)
  } else{
    proxies_short <- proxies[(p+1):nrow(proxies), ]
    # Center proxy variables
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j), 1]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[,1] != 0, 1])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = ncol(proxies), byrow = TRUE)
    
  }
  centered_proxies <- proxies_short - Proxycentering
  
  ProxyBlocks <- array(NA, dim = c(BlockSize, k, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    if (k==1){
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1)]
    } else{
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1),]
    }
    
  }
  
  if(k==1){
    bootIRFs <- array(NA, dim = c(irhor, n, nboot))
  }else{
    bootIRFs <- array(NA, dim = c(irhor, n, nboot,k))
  }
  # bootDatas <- array(0, dim = c(T + p, n, nboot))
  # ProxyCount <- matrix(0, nrow = nboot, ncol = ncol(proxies))
  
  set.seed(123)
  j =1
  block_index_list <- replicate(
    nboot,
    sample(1:(T - BlockSize + 1), nBlock, replace = TRUE),
    simplify = FALSE
  )
  
  for (j in 1:nboot) {
    index <- block_index_list[[j]]
    # Reconstruct bootstrap residuals
    bootres <- do.call(rbind, lapply(index, function(ii) VARBlocks[,,ii]))
    bootres <- bootres[(p+1):nrow(bootres),]
    bootProxy <- do.call(rbind, lapply(index, function(ii) ProxyBlocks[,,ii]))
    # Rassembler les blocs dans l’ordre
    if(k==1){
      bootProxy <- unlist(lapply(index, function(ii) ProxyBlocks[, ,ii]))
    }
    theta_0_boot <- first_var_estimation_double_bis(y, bootres, p, bootProxy, irhor, controls = NULL, elas = elas)$theta_0
    
    lambda_0_boot <- get_lambda_0(C, theta_0_boot, irhor=irhor)
    if(k==1){
      bootIRFs[,,j] <- lambda_0_boot
    }else{
      bootIRFs[,,j,] <- lambda_0_boot
    }
    
    # Re-estimate VAR (in R you can use vars::VAR)
    # bootvarEst <- vars::VAR(bootData[(p+1):(T+p), ], p = p, type = "const")
    
  }
  return(bootIRFs)
}


gamma_bootstrap_mbb_corrected_final <- function(x,nboot,  theta_0, proxies, controls=NULL, elas = 0,
                                          start_ts = c(2015,1), end_ts =c(2023,12), irhor=irhor){
  T_bis <- nrow(x) - p
  
  BlockSize <- round(5 * T_bis**0.25)
  nBlock <- ceiling((T_bis) / BlockSize)
  
  
  # Center VAR residuals
  VARcentering <- sapply(1:BlockSize, function(j) {
    colMeans(res[j:(T - BlockSize + j), ])
  })
  VARcentering <- matrix(rep(VARcentering, each = nBlock), nrow = T, ncol = n, byrow = TRUE)
  centered_res <- res - VARcentering
  
  # Create blocks for residuals
  VARBlocks <- array(NA, dim = c(BlockSize, n, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    VARBlocks[,,j] <- centered_res[j:(j + BlockSize - 1), ]
  }
  
  # Proxy blocks
  if (k==1){
    proxies_short <- proxies[(p+1):length(proxies)]
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j)]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[1] != 0])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = 1, byrow = TRUE)
  } else{
    proxies_short <- proxies[(p+1):nrow(proxies), ]
    # Center proxy variables
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j), 1]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[,1] != 0, 1])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = ncol(proxies), byrow = TRUE)
    
  }
  centered_proxies <- proxies_short - Proxycentering
  
  ProxyBlocks <- array(NA, dim = c(BlockSize, k, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    if (k==1){
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1)]
    } else{
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1),]
    }
    
  }
  
  if(k==1){
    bootIRFs <- array(NA, dim = c(irhor, n, nboot))
  }else{
    bootIRFs <- array(NA, dim = c(irhor, n, nboot,k))
  }
  
  
  # ProxyCount <- matrix(0, nrow = nboot, ncol = ncol(proxies))
  
  
  
  set.seed(123)
  j =1
  block_index_list <- replicate(
    nboot,
    sample(1:(T - BlockSize + 1), nBlock, replace = TRUE),
    simplify = FALSE
  )

  for (j in 1:nboot) {
    index <- block_index_list[[j]]
    # Reconstruct bootstrap residuals
    bootres <- do.call(rbind, lapply(index, function(ii) VARBlocks[,,ii]))
    bootres <- bootres[c(1:T),]
    bootProxy <- do.call(rbind, lapply(index, function(ii) ProxyBlocks[,,ii])
                         )

    # Rassembler les blocs dans l’ordre
    if(k==1){
      bootProxy <- unlist(lapply(index, function(ii) ProxyBlocks[, ,ii]))
    }
    bootProxy <- bootProxy[1:T, drop = FALSE]
    
    bootData <- iterate_var_ter(x, x[1:p,], bootres, vl, controls)
    
    # Fit the VAR model
    vboot <- if (is.null(controls)) {
      vars::VAR(bootData, p, type = "none")
    } else {
      vars::VAR(bootData, p, type = "none", exogen = controls)
    }
    AL_boot <- do.call(cbind, lapply(coef(vboot), function(df) df[, 1]))

    # Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
    # Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
    C_boot  <- MARep(t(AL_boot), p, hori)
    
    if (k ==1){
      theta_0_boot <- first_var_estimation_bis(x, res =bootres, p, proxies =bootProxy,# c(proxies[1:p],bootProxy),
                                               irhor, controls = NULL, elas = elas)$theta_0
    }else{
      theta_0_boot <- first_var_estimation_bis(x, res =bootres, p, proxies = rbind(proxies[1:p,],bootProxy),
                                               irhor, controls = NULL, elas = elas)$theta_0
    }
    
    lambda_0_boot <- get_lambda_0(C_boot, theta_0_boot, irhor=irhor)
    if(k==1){
      bootIRFs[,,j] <- lambda_0_boot
    }else{
      bootIRFs[,,j,] <- lambda_0_boot
    }
    
    # Re-estimate VAR (in R you can use vars::VAR)
    # bootvarEst <- vars::VAR(bootData[(p+1):(T+p), ], p = p, type = "const")
    
  }
  return(bootIRFs)
}

run_bootstrap_for_elas_bis <- function(elas_value) {
  
  # Estimation initiale
  theta_0 <- first_var_estimation_double_bis(
    y, res, p, proxies, irhor,
    controls = NULL,
    elas = elas_value
  )$theta_0
  
  lambda_0_log <- get_lambda_0(C, theta_0)
  # Bootstrap
  bootIRFs <- gamma_bootstrap_mbb_corrected_bis(
    x = x,
    nboot = nboot,
    BlockSize = BlockSize,
    theta_0 = theta_0,
    proxies = proxies,
    controls = NULL,
    elas = elas_value,
    start_ts = c(2015,1),
    end_ts   = c(2023,12),
    irhor = irhor
  )
  
  # Confidence bands
  list(
    elas     = elas_value,
    bootIRFs = bootIRFs,
    lambda_0 = lambda_0_log,
    IRF_90   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_90),
    IRF_68   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_68),
    IRF_50   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_50)[1,,,]
  )
}


run_bootstrap_for_elas <- function(elas_value) {
  
  # Estimation initiale
  theta_0 <- first_var_estimation_bis(
    y, res, p, proxies, irhor,
    controls = NULL,
    elas = elas_value
  )$theta_0
  
  lambda_0_log <- get_lambda_0(C, theta_0)
  # Bootstrap
  bootIRFs <- gamma_bootstrap_mbb_corrected(
    x = x,
    nboot = nboot,
    BlockSize = BlockSize,
    theta_0 = theta_0,
    proxies = proxies,
    controls = NULL,
    elas = elas_value,
    start_ts = c(2015,1),
    end_ts   = c(2023,12),
    irhor = irhor
  )
  
  # Confidence bands
  list(
    elas     = elas_value,
    bootIRFs = bootIRFs,
    lambda_0 = lambda_0_log,
    IRF_90   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_90),
    IRF_68   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_68),
    IRF_50   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_50)[1,,,]
  )
}



run_bootstrap_for_elas_bis <- function(elas_value) {
  
  # Estimation initiale
  theta_0 <- first_var_estimation_double_bis(
    y, res, p, proxies, irhor,
    controls = NULL,
    elas = elas_value
  )$theta_0
  
  lambda_0_log <- get_lambda_0(C, theta_0)
  # Bootstrap
  bootIRFs <- gamma_bootstrap_mbb_corrected_bis(
    x = x,
    nboot = nboot,
    BlockSize = BlockSize,
    theta_0 = theta_0,
    proxies = proxies,
    controls = NULL,
    elas = elas_value,
    start_ts = c(2015,1),
    end_ts   = c(2023,12),
    irhor = irhor
  )
  
  # Confidence bands
  list(
    elas     = elas_value,
    bootIRFs = bootIRFs,
    lambda_0 = lambda_0_log,
    IRF_90   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_90),
    IRF_68   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_68),
    IRF_50   = compute_quantile_bands_dim3(bootIRFs, quantile_probs_50)[1,,,]
  )
}






gamma_bootstrap_mbb_corrected_counterfactual <- function(x,nboot,  theta_0, proxies, controls=NULL, elas = 0,
                                                start_ts = c(2015,1), end_ts =c(2023,12), irhor=irhor){
  T_bis <- nrow(x) - p
  
  BlockSize <- round(5 * T_bis**0.25)
  nBlock <- ceiling((T_bis) / BlockSize)
  
  
  # Center VAR residuals
  VARcentering <- sapply(1:BlockSize, function(j) {
    colMeans(res[j:(T - BlockSize + j), ])
  })
  VARcentering <- matrix(rep(VARcentering, each = nBlock), nrow = T, ncol = n, byrow = TRUE)
  centered_res <- res - VARcentering
  
  # Create blocks for residuals
  VARBlocks <- array(NA, dim = c(BlockSize, n, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    VARBlocks[,,j] <- centered_res[j:(j + BlockSize - 1), ]
  }
  
  # Proxy blocks
  if (k==1){
    proxies_short <- proxies[(p+1):length(proxies)]
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j)]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[1] != 0])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = 1, byrow = TRUE)
  } else{
    proxies_short <- proxies[(p+1):nrow(proxies), ]
    # Center proxy variables
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies[j:(T - BlockSize + j), 1]
      mean(subProxy[subProxy != 0]) - mean(proxies[proxies[,1] != 0, 1])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = ncol(proxies), byrow = TRUE)
    
  }
  centered_proxies <- proxies_short - Proxycentering
  
  ProxyBlocks <- array(NA, dim = c(BlockSize, k, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    if (k==1){
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1)]
    } else{
      ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1),]
    }
    
  }
  
  if (k == 1) {
    bootIRFs <- array(NA, dim = c(irhor, n, nboot))
    boot_structural_shocks <- matrix(NA_real_, nrow = nboot, ncol = T)  # 1 shock x T, répété nboot fois
  }else{
    bootIRFs <- array(NA, dim = c(irhor, n, nboot,k))
  }
  
  
  # ProxyCount <- matrix(0, nrow = nboot, ncol = ncol(proxies))
  
  
  
  set.seed(123)
  j =1
  block_index_list <- replicate(
    nboot,
    sample(1:(T - BlockSize + 1), nBlock, replace = TRUE),
    simplify = FALSE
  )

  for (j in 1:nboot) {
    index <- block_index_list[[j]]
    # Reconstruct bootstrap residuals
    bootres <- do.call(rbind, lapply(index, function(ii) VARBlocks[,,ii]))
    bootres <- bootres[c(1:T),]
    bootProxy <- do.call(rbind, lapply(index, function(ii) ProxyBlocks[,,ii])
    )
    
    # Rassembler les blocs dans l’ordre
    if(k==1){
      bootProxy <- unlist(lapply(index, function(ii) ProxyBlocks[, ,ii]))
    }
    bootProxy <- bootProxy[1:T, drop = FALSE]
    bootData <- iterate_var_ter(x, x[1:p,], bootres, vl, controls)
    
    # Fit the VAR model
    vboot <- if (is.null(controls)) {
      vars::VAR(bootData, p, type = "none")
    } else {
      vars::VAR(bootData, p, type = "none", exogen = controls)
    }
    AL_boot <- do.call(cbind, lapply(coef(vboot), function(df) df[, 1]))
    
    # Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
    # Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
    C_boot  <- MARep(t(AL_boot), p, hori)
    
    if (k ==1){
      boot_estimation <- first_var_estimation_bis(x, res =bootres, p, proxies =c(proxies[1:p],bootProxy),
                                               irhor, controls = NULL, elas = elas)
      
      theta_0_boot <- boot_estimation$theta_0
      gamma_boot  <- boot_estimation$gamma
      phi_boot <- boot_estimation$phi
      lambda_0_boot <- get_lambda_0(C_boot, theta_0_boot, irhor=irhor)
      
      
      
      Sigma_eta_boot = cov(bootres)
      #calculate sigma_aa
      Sigma_a_boot <- gamma_boot %*%  ginv(t(gamma_boot) %*% ginv(Sigma_eta_boot) %*% gamma_boot) %*% t(gamma_boot)
      
      Sigma_theta_boot <- t(theta_0_boot) %*% theta_0_boot
      D_boot <- solve(Sigma_theta_boot)  %*% t(theta_0_boot) %*%  Sigma_a_boot %*% theta_0_boot %*% solve(Sigma_theta_boot)
      
      #almost diagonal matrix_supply
      right_term_boot <- t(gamma_boot) %*% ginv(Sigma_eta_boot) %*% t(bootres)
      D_inv_boot <- try(solve(D_boot), silent = TRUE); if (inherits(D_inv_boot, "try-error")) D_inv_boot <- ginv(D_boot)
      
      left_matrix_supply <- t(phi_boot) %*% D_inv_boot
      # with this strategy the system is undertemined,
      #I impose additional assumptions
      
      structural_shocks_IV_boot <- solve(t(left_matrix_supply) %*% left_matrix_supply) %*% t(left_matrix_supply) %*% right_term_boot
      
    }else{
      theta_0_boot <- first_var_estimation_bis(x, res =bootres, p, proxies = rbind(proxies[1:p,],bootProxy),
                                               irhor, controls = NULL, elas = elas)$theta_0
    }
    
    
    
    
    if(k==1){
      bootIRFs[,,j] <- lambda_0_boot
      boot_structural_shocks[j, ] <- as.numeric(structural_shocks_IV_boot[1, ])  # stocke le DRAW j
    }else{
      bootIRFs[,,j,] <- lambda_0_boot
    }
    
    # Re-estimate VAR (in R you can use vars::VAR)
    # bootvarEst <- vars::VAR(bootData[(p+1):(T+p), ], p = p, type = "const")
    
  }
  return(list(bootIRFs = bootIRFs, boot_structural_shocks = boot_structural_shocks))
}

