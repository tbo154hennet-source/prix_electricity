

gamma_bootstrap_mbb_corrected <- function(x,nboot, vars, BlockSize, theta_0, proxies, controls=NULL, elas = 0,
                                                start_ts = c(2015,1), end_ts =c(2023,12)){
  T_bis <- nrow(x) - p
  
  BlockSize <- round(5 * T_bis**0.25)
  nBlock <- ceiling((T_bis) / BlockSize)
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  res = residuals(vl)
  
  dim(res)
  
  
  # Create blocks for residuals
  VARBlocks <- array(NA, dim = c(BlockSize, n, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    VARBlocks[,,j] <- res[j:(j + BlockSize - 1), ]
  }
  
  # Proxy blocks
  if (k==1){
    proxies_kanzig_short <- proxies_kanzig[(p+1):length(proxies_kanzig)]
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies_kanzig[j:(T - BlockSize + j)]
      mean(subProxy[subProxy != 0]) - mean(proxies_kanzig[proxies_kanzig[1] != 0])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = 1, byrow = TRUE)
  } else{
    proxies_kanzig_short <- proxies_kanzig[(p+1):nrow(proxies_kanzig), ]
    # Center proxy variables
    Proxycentering <- sapply(1:BlockSize, function(j) {
      subProxy <- proxies_kanzig[j:(T - BlockSize + j), 1]
      mean(subProxy[subProxy != 0]) - mean(proxies_kanzig[proxies_kanzig[,1] != 0, 1])
    })
    Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = ncol(proxies_kanzig), byrow = TRUE)
    
  }
  ProxyBlocks <- array(NA, dim = c(BlockSize, k, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    
    #on ne peut pas prendre le premier bloc pour lequel pas de résidus
    ProxyBlocks[,,j] <- proxies_kanzig_short[j:(j + BlockSize - 1)]
  }
  
  
  # Center VAR residuals
  VARcentering <- sapply(1:BlockSize, function(j) {
    colMeans(res[j:(T - BlockSize + j), ])
  })
  VARcentering <- matrix(rep(VARcentering, each = nBlock), nrow = T, ncol = n, byrow = TRUE)
  
  
  
  bootIRFs <- array(NA, dim = c(irhor, n, nboot))
  # bootDatas <- array(0, dim = c(T + p, n, nboot))
  # ProxyCount <- matrix(0, nrow = nboot, ncol = ncol(proxies_kanzig))
  
  set.seed(123)
  j =1
  for (j in 1:nboot) {
    
    # Draw block indices
    index <- sample(1:(T - BlockSize + 1), nBlock, replace = TRUE)
    
    # Reconstruct bootstrap residuals
    bootres <- do.call(rbind, lapply(index, function(ii) VARBlocks[,,ii]))
    bootres <- bootres[1:T, ] - VARcentering  # center
    
    
    if (k==1){
      bootProxy <- c(sapply(index, function(ii) ProxyBlocks[,,ii]))
      bootProxy <- bootProxy[1:T]
      bootProxy <- bootProxy - Proxycentering[ 1]
      proxies_kanzig_new = c(proxies_kanzig[1:p], bootProxy)
    } else{
      bootProxy <- do.call(rbind, lapply(index, function(ii) ProxyBlocks[,,ii]))
      bootProxy <- bootProxy[1:T, ]
      # Center proxies_kanzig
      for (k in 1:ncol(proxies_kanzig)) {
        idx <- which(bootProxy[,k] != 0)
        bootProxy[idx, k] <- bootProxy[idx, k] - Proxycentering[idx, k]
      }
      proxies_kanzig_new = cbind(proxies_kanzig[1:p,], bootProxy)
    }
    # Reconstruct bootstrap proxy
  
    # Adjust for identification sample
    #bootProxy <- bootProxy[smplStartProxyVARInd-p:smplEndProxyVARInd-p, ]
    
    # Count nonzero proxies_kanzig
    # ProxyCount[j, ] <- colSums(abs(bootProxy) > 0)
    # 
    # if (any(ProxyCount[j, ] < 15)) next
    # 
    # Generate bootstrap VAR data
    bootData <- matrix(0, nrow = T + p, ncol = n)
    bootData[1:p, ] <- x[1:p, ]  # initial values
    
    # Récupérer les coefficients de toutes les séries
    var_coefs_list <- coef(vl)
    # Pour chaque série, ne garder que la colonne "Estimate" et transformer en vecteur horizontal
    # 1. Extraire uniquement la colonne Estimate pour chaque série
    coef_matrices <- lapply(var_coefs_list, function(x) x[, "Estimate", drop = FALSE])
    
    # 2. Combiner toutes les colonnes pour reconstruire la matrice des coefficients
    # Chaque élément devient une colonne de la matrice
    coef_matrix <- as.data.frame(do.call(cbind, coef_matrices))
    
    bootData <- iterate_var_ter(x, vars_kanzig[1:p,], bootres, vl, controls)
  
    x_new =ts(bootData  , start=c(1974,1), end=c(2017,12), frequency=12)
    y_new  = vars_kanzig[-1:-p, ]
    proxies_kanzig_new <- ts(proxies_kanzig_new , frequency=12)
    
    estimate_boot <- first_var_estimation(x_new ,y_new ,p, proxies_kanzig_new , irhor)
    theta_0_boot <- estimate_boot$theta_0
    gamma_boot <- estimate_boot$gamma
    phi_boot <- estimate_boot$phi
    var_coef_boot <- estimate_boot$coef
    
    
    AL <- do.call(cbind, lapply(var_coef_boot, function(df) df[, 1]))
    # Optionally, set column names if needed
    names(AL) <- names(var_coef_boot)
    p <- 12  # Number of lags in the VAR model
    hori <- 50 # Forecast horizon
    n<- ncol(y)
    # Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
    # Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
    C  <- MARep(t(AL), p, hori)
    
    
    from <- (l+1)
    to <- l*p
    to_bis <- l*(p-1)
    
    lambda_0_boot <- get_lambda_0(C, theta_0_boot, irhor=50)
    bootIRFs[,,j] <- lambda_0_boot
    # Re-estimate VAR (in R you can use vars::VAR)
    # bootvarEst <- vars::VAR(bootData[(p+1):(T+p), ], p = p, type = "const")
    
  }
  return(bootIRFs)
}
quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_probs_50 <- c(0.5, 0.5)
# Function to compute quantiles along the third dimension (simulations)
compute_quantile_bands <- function(boot_array, probs) {
  # boot_array: horizon+1 x nvar x nsim
  apply(boot_array, c(1, 2), function(x) quantile(x, probs = probs, na.rm = TRUE))
}

# 90% confidence bands (5%-95%)
IRF_90 <- compute_quantile_bands(bootIRFs, quantile_probs_90)
dim(IRF_90)
IRF_90
dim(bootIRFs)
IRF_68 <- compute_quantile_bands(bootIRFs, quantile_probs_68)
IRF_50 <- compute_quantile_bands(bootIRFs, quantile_probs_50)[1,,]


