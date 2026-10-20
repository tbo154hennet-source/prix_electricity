
library(expm)

nw_covariance <- function(ZV, e) {
  T <- nrow(ZV)
  L <- ceiling(1.3 * sqrt(T))  # Lazarus et al. (2018) lag
  
  K <- ncol(ZV)
  D <- ncol(e)
  
  # Initialize W and Sig with j = 0 terms
  W <- crossprod(ZV) / T
  Sig <- crossprod(e) / T
  
  # Loop over lags
  for (j in 1:L) {
    w_l <- 1 - j / (L + 1)  # Bartlett kernel
    
    ZV_lag  <- ZV[(j + 1):T, , drop = FALSE]
    ZV_lead <- ZV[1:(T - j), , drop = FALSE]
    
    e_lag  <- e[(j + 1):T, , drop = FALSE]
    e_lead <- e[1:(T - j), , drop = FALSE]
    
    # Cross-covariance for ZV
    gamma_ZV <- (t(ZV_lag) %*% ZV_lead + t(ZV_lead) %*% ZV_lag) / T
    W <- W + w_l * gamma_ZV
    
    # Cross-covariance for e
    gamma_e <- (t(e_lag) %*% e_lead + t(e_lead) %*% e_lag) / T
    Sig <- Sig + w_l * gamma_e
  }
  
  return(list(W = W, Sig = Sig))
}


gweakivtest <- function(y, Y, X, Z,
                        cov_type = "EHW",
                        alpha = 0.05,
                        tau = 0.10,
                        points = 1000,
                        target = "beta",
                        crit = "abs") {
  # Ensure matrices
  y <- as.matrix(y)
  Y <- as.matrix(Y)
  X <- as.matrix(X)
  Z <- as.matrix(Z)
  
  # Drop missing rows
  sel <- complete.cases(cbind(y, Y, Z, X))
  y <- y[sel, , drop = FALSE]
  Y <- Y[sel, , drop = FALSE]
  Z <- Z[sel, , drop = FALSE]
  X <- X[sel, , drop = FALSE]
  
  T <- nrow(y)
  
  # Add constant if not included
  if (ncol(X) == 0 || all(apply(X, 2, stats::var) == 0)) {
    X <- matrix(1, T, 1)
  } else {
    X <- cbind(X[, apply(X, 2, stats::var) != 0, drop = FALSE], 1)
  }
 
  Zo <- residuals(lm(Z ~ X - 1))
  Zo <- scale(Zo, center = TRUE, scale = FALSE)  # mean zero
  Sigma_Zo <- crossprod(Zo) / T
  if (nrow(Sigma_Zo)==1 & ncol(Sigma_Zo)==1){
    Zo <- Zo/sqrt(Sigma_Zo[1,1])
  }else{
    Zo <- Zo %*% solve(sqrtm(Sigma_Zo))
  }
  Zo <- as.matrix(Zo)   # devient une matrice T x 1
  
  Yo <- residuals(lm(Y ~ X - 1))
  yo <- residuals(lm(y ~ X - 1))
  PYo <- Zo %*% solve(crossprod(Zo), crossprod(Zo, Yo))
  Pyo <- Zo %*% solve(crossprod(Zo), crossprod(Zo, yo))
  beta_hat <- tryCatch(
    solve(crossprod(PYo), crossprod(PYo, Pyo)),    # essai avec solve()
    error = function(e) {
      message("Matrix nearly singular, using pseudo-inverse instead.")
      library(MASS)
      beta_hat <- ginv(crossprod(PYo)) %*% crossprod(PYo, Pyo)
      
    }
  )

  v1 <- yo - Pyo
  v2 <- Yo - PYo
  e <- cbind(v1, v2)
  
  class(Zo)
  # ZV construction (Kronecker-like)
  ZV <- NULL
  for (j in 1:(ncol(e))) {
    ZV <- cbind(ZV, Zo * matrix(e[, j], nrow = T, ncol = ncol(Zo)))
  }
  
  # Compute W and Sig
  if (cov_type == "NW") {
    # Implement NW estimator later
    cov_nw <- nw_covariance(ZV, e)
    W <- cov_nw$W
    Sig <- cov_nw$Sig
    
  } else {
    W <- crossprod(ZV) / T
    Sig <- crossprod(e) / T
  }
  
  W <- W * T / (T - ncol(Z) - ncol(X))
  Sig <- Sig * T / (T - ncol(Z) - ncol(X))
  #Compute generalized g-min statistic 
  K <- ncol(Z)
  N <- ncol(Y)
  
  RNK <- kronecker(diag(N), as.vector(diag(K)))
  W2 <- W[(K + 1):ncol(W), (K + 1):ncol(W)]
  Phi <- t(RNK) %*% kronecker(W2, diag(K)) %*% RNK
  gmin_gen <- min(eigen(solve(sqrtm(Phi)) %*% crossprod(PYo) %*% solve(sqrtm(Phi)), only.values = TRUE)$values)
  
  # 7. Compute Stock-Yogo test statistic
  
  
  sy_stat <- tryCatch({
    # Step 1: Estimate Svv
    Svv <- crossprod(v2) / (T - ncol(Z) - ncol(X))
    
    # Step 2: Compute the middle matrix
    ZtZ_inv <- solve(crossprod(Zo))
    ZtYo <- crossprod(Zo, Yo)  # L × k
    YoZt <- crossprod(Yo, Zo)  # k × L
    middle_matrix <- ZtYo %*% ZtZ_inv %*% YoZt  # k × k
    
    # Step 3: Compute W+
    Svv_root_inv <- solve(sqrtm(Svv))
    W_plus <- Svv_root_inv %*% middle_matrix %*% Svv_root_inv
    
    # Step 4: Extract test statistic
    if (is.null(ncol(Yo))) {
      W_plus[1, 1]
    } else {
      min(eigen(W_plus / ncol(Yo), only.values = TRUE)$values)
    }
  }, error = function(e) {
    NA  # In case of any error, assign NA
  })
  
  
  
  return(list(
    nobs = T,
    beta_2SLS = drop(beta_hat),
    gmin_generalized = gmin_gen,
    stock_yogo_test_statistic = sy_stat
    # critical values to be implemented
  ))
}






  
  
