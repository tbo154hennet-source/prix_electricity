


run_VAR_bootstrap_alternative_3 <- function(df,
                                          headers_var,
                                          headers_proxies,
                                          p = 4,
                                          irhor = 20,
                                          nboot = 100,
                                          block_size = 15,
                                          start_ts = c(2015, 1),
                                          end_ts = c(2023, 12),
                                          headers_controls = NULL) {
  # Crée les objets VAR et proxies
  
  # controls =NULL
  
  proxies <- ts(df[, headers_proxies], frequency = 12)
  proxies[is.na(proxies)] <- 0
  
  vars <- ts(df[, headers_var], frequency = 12)
  
  x <- ts(vars, start = start_ts, end = end_ts, frequency = 12)
  y <- vars[-1:-p, ]
  T <- nrow(y)
  n <- ncol(y)
  k <- 3
  l <- ncol(proxies)
  
  controls <- if (is.null(headers_controls)) {
    NULL
  } else {
    ts(df[, headers_controls], frequency = 12)
  }
  
  estimate_diff <- first_var_estimation_3(x, y, p, proxies, irhor, controls)
  theta_0_diff <- estimate_diff$theta_0
  gamma_diff <- estimate_diff$gamma
  phi_diff <- estimate_diff$phi
  var_coef_diff <- estimate_diff$coef
  
  AL <- do.call(cbind, lapply(var_coef_diff, function(df) df[, 1]))
  names(AL) <- names(var_coef_diff)
  
  C <- MARep(t(AL), p, irhor)
  lambda_0_diff <- get_lambda_0_3(C, theta_0_diff, irhor =irhor)
  
  gamma_estimate_diff_MBB <- gamma_bootstrap_mbb_alternative_3(
    x =x,
    nboot = nboot,
    vars = vars,
    BlockSize = block_size,
    theta_0 = theta_0_diff,
    proxies = proxies,
    controls = controls
  )
  gamma_estimate_diff_MBB$boot_ncvg_lambda_03
  list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                       "ncvg_lambda_01", "ncvg_lambda_02", "ncvg_lambda_03")
  sample_matrices <- lapply(list_of_samples, function(name) {
    gamma_estimate_diff_MBB[[paste0("boot_", name)]]
  })

  names(sample_matrices) <- list_of_samples
  
  # Appliquer cumsum sur l'axe 1 (temps), pour chaque variable et simulation
  sample_matrices$ncvg_lambda_01 <- apply(sample_matrices$ncvg_lambda_01, c(2, 3), function(x) cumsum(x))
  sample_matrices$ncvg_lambda_02 <- apply(sample_matrices$ncvg_lambda_02, c(2, 3), function(x) cumsum(x))
  sample_matrices$ncvg_lambda_03 <- apply(sample_matrices$ncvg_lambda_03, c(2, 3), function(x) cumsum(x))

  #attention on veut prendre les quantiles sur les IRFs cumulées
  #pour ne pas accumuler l'erreur
  
  lambda_0_diff <- apply(lambda_0_diff, c(2, 3), function(x) cumsum(x))
  
  
  bandwith_90 <- get_strong_instruments_3(sample_matrices, quantile_probs_90)
  bandwith_68 <- get_strong_instruments_3(sample_matrices, quantile_probs_68)
  median_50 <- get_strong_instruments_3(sample_matrices, quantile_prob_50)
  
  return(list(
    headers_var = headers_var,
    headers_proxies = headers_proxies,
    theta_0 = theta_0_diff,
    lambda_0 = lambda_0_diff,
    sample_matrices = sample_matrices,
    bandwith_90 = bandwith_90,
    bandwith_68 = bandwith_68,
    median_50 = median_50
  ))
}


gamma_bootstrap_mbb_alternative_3 <- function(x,nboot, vars, BlockSize, theta_0, proxies, controls=NULL){
  #nboot <-100
  #BlockSize <- 15
  # Create an empty list to hold the elements
  n = ncol(vars)
  k <- 3
  l <- ncol(proxies)
  T_bis <- nrow(vars) - p
  boot_gamma <- array(NA, dim = c(n, l, nboot))
  boot_psi <- array(NA, dim = c(n, l, nboot))
  boot_ncvg_theta_0 <- array(NA, dim = c(n, k, nboot))
  boot_ncvg_lambda_01 <- array(NA, dim = c(irhor, n, nboot))
  boot_ncvg_lambda_02 <- array(NA, dim = c(irhor, n, nboot))
  boot_ncvg_lambda_03 <- array(NA, dim = c(irhor, n, nboot))
  boot_inv <- array(NA, dim = c(n*irhor,n , nboot))
  
  
  nBlock <- ceiling((T_bis) / BlockSize)
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  res = residuals(vl)
  for (irep in 1:nboot) {
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
    Z_bt <- proxies[block_indices, , drop = FALSE]
    
    
    # Fit VAR to bootstrapped data
    boot_df <- as.data.frame(X_bt)
    colnames(boot_df) <- paste0("V", 1:p)
    
    varbsnew <- iterate_var_ter(x, vars[1:p,], X_bt, vl, controls)
    
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
    Marep_new <- MARep(t(ALnew), p, irhor)
    boot_inv[,,irep] <- Marep_new
    
    proxies_boot <- rbind(proxies[1:p,],Z_bt)
    
    
    #partie pour estimer theta_0
    estimatenew <- first_var_estimation_3(xnew, ynew,
                                          p, proxies_boot , irhor, controls)
    # estimatenew <- first_var_estimation_3xnew, ynew,
    #                                     p, proxies , irhor)
    boot_gamma[,,irep] <- estimatenew$gamma
    boot_psi[,,irep] <- sqrt(T) * (estimatenew$gamma - theta_0 %*% estimatenew$phi)
    phi_hat <- estimatenew$phi
    boot_ncvg_theta_0[,,irep] <- estimatenew$theta_0
    # on notera que pour des instruments très faibles l'intervalle va exploser
    # en effet phi tend vers 0 comme son determinant
    
    boot_ncvg_lambda_01[,,irep]  <- get_lambda_0_3(Marep_new, estimatenew$theta_0)[,,1]
    boot_ncvg_lambda_02[,,irep]  <-get_lambda_0_3(Marep_new, estimatenew$theta_0)[,,2]
    boot_ncvg_lambda_03[,,irep]  <- get_lambda_0_3(Marep_new, estimatenew$theta_0)[,,3]
    if (irep %% 50 == 0) cat("Replication", irep, "\n")
    
  }
  return(list(boot_gamma= boot_gamma, boot_psi =boot_psi ,
              boot_ncvg_theta_0 = boot_ncvg_theta_0,
              boot_ncvg_lambda_01 = boot_ncvg_lambda_01,
              boot_ncvg_lambda_02 = boot_ncvg_lambda_02,
              boot_ncvg_lambda_03 = boot_ncvg_lambda_03,
              boot_inv= boot_inv)) #, boot_reliability = boot_reliability))
}


get_lambda_0_3 <-function(C, theta_0, irhor=20){
  k <- 3
  n <- dim(C)[1]  # number of variables
  # on essaye d'abord de repliquer a l'identique stock and watson
  lambda_0 <- array(NA, dim = c(irhor, n, k))
  
  for (h in c(1:3)){
    for (i in c(1:n)){
      e_i <- rep(0, n)
      
      # Mettre 1 à la position j
      e_i[i] <- 1
      # Appliquer la fonction pour extraire les éléments
      lambda_0[, i, h] <- as.numeric(lapply(1:irhor, function(x) {
        return(t(e_i) %*% C[,,x] %*% theta_0[,h]  )    }))
    }
  }
  return(lambda_0)
}


first_var_estimation_3 <- function(x, y, p, proxies, irhor, controls = NULL) {
  # x: endogenous variables
  # y: dependent variables
  # p: lag order
  # proxies: instruments or proxies
  # irhor: impulse response horizon (unused here, but kept for potential future use)
  # controls: optional exogenous variables
  
  # proxies <- proxies_boot
  # x <- xnew
  # y <- ynew
  
  # Fit the VAR model
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  
  l  = ncol(proxies)
  n = ncol(y)
  coef = coef(vl)
  bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
  res = residuals(vl)
  
  T = nrow(y)
  Sigma_eta <- cov(res) 
  
  #suivre identification simplifiee de olea and al
  gamma <- t( t(proxies[c(-1:-p),]) %*% res)
  
  
  #calculate sigma_aa
  Sigma_a <- gamma %*%  ginv(t(gamma) %*% ginv(Sigma_eta) %*% gamma) %*% t(gamma)
  
  
  
  theta_01 <- gamma  %*% ginv(t(matrix_12) %*% gamma ) %*% c(1,0, 0)
  
  matrix_12_bis <- rbind( diag(3) , zero_matrix)
  matrix_12_bis[,1] <-  ginv(Sigma_eta) %*% theta_01
  
  theta_02 <- gamma  %*% ginv( t(matrix_12_bis)  %*% gamma ) %*% c(0, 1, 0)
  
  matrix_12_ter <- rbind( diag(3) , zero_matrix)
  matrix_12_ter[,1] <-  ginv(Sigma_eta) %*% theta_01
  matrix_12_ter[,1] <-  ginv(Sigma_eta) %*% theta_02
  
  theta_03 <- gamma  %*% ginv( t(matrix_12_bis)  %*% gamma ) %*% c(0, 0, 1)
  
  theta_0 <- cbind(theta_01, theta_02, theta_03)
  
  
  
  phi <- ginv(theta_0) %*% gamma
  
  
  
  return(list(theta_0= theta_0,  gamma=  gamma,
              phi = phi, coef = coef(vl)))
  
}

get_strong_instruments_3 <- function(sample_matrices, quantile_probs){
  # Calcul des quantiles pour lambda_01 et lambda_02
  strong_instr_array <- array(NA, dim = c(2, irhor, n, 3))
  dimnames(strong_instr_array) <- list(
    c("lower", "upper"), NULL, NULL, c("lambda_01", "lambda_02", "lambda_03")
  )
  for (lambda_idx in 1:3) {
    lambda_name <- paste0("ncvg_lambda_0", lambda_idx)
    quantiles <- apply(sample_matrices[[lambda_name]],
                       c(1, 2), function(x) quantile(x, probs = quantile_probs))
    
    strong_instr_array[,,,lambda_idx] <- array(quantiles, dim = c(irhor, n, 2))
  }
  
  strong_instr_array <- aperm(strong_instr_array, perm = c(2, 3, 1, 4))
  return(strong_instr_array)
}

