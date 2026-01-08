
vech <- function(M) {
  M[lower.tri(M, diag = TRUE)]
}

duplication_matrix <- function(n) {
  D <- matrix(0, n*n, n*(n+1)/2)
  count <- 1
  for (j in 1:n) {
    for (i in j:n) {
      e_ij <- matrix(0, n, n)
      e_ij[i,j] <- 1
      e_ij[j,i] <- ifelse(i==j, 0, 1)
      D[,count] <- as.vector(e_ij)
      count <- count + 1
    }
  }
  D
}
commutation_matrix <- function(m, n) {
  K <- matrix(0, m*n, m*n)
  for (i in 1:m) {
    for (j in 1:n) {
      E_ij <- matrix(0, m, n)
      E_ij[i,j] <- 1
      K[, (j-1)*m + i] <- as.vector(t(E_ij))
    }
  }
  K
}



f_theta <- function(theta, k, r, g) {
  
  B1 <- matrix(theta[1:(k*g)], g, k)
  Pz <- matrix(theta[(k*g+1):(k*g+r*k)], r, k)
  
  Sigma_zu_model <- Pz %*% t(B1)
  Sigma_zz_model <- Pz %*% t(Pz)
  
  c(
    as.vector(Sigma_zu_model),
    vech(Sigma_zz_model)
  )
}

#install.packages("numDeriv")  # si pas encore installé
library(numDeriv)
# theta_hat : ton estimateur minimum distance
# k, r, g : dimensions de B1 et Pz


# Sigma_zu   <-   t( Sigma_uz) 
# Sigma_zz <- Sigma_zu %*% ginv(Sigma_u) %*% Sigma_uz
# 
# 
# Pi_hat <- c(
#   as.vector(Sigma_zu),
#   vech( Sigma_zz)
# )

#estimate the Pi variance with a bootstrap
#but no need to run nboot the opt command

estimate_variance_pi_bootstrap <- function(x,nboot,Pi_hat,  theta_0, proxies, controls=NULL,
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
  
  proxies_short <- proxies[(p+1):nrow(proxies), ]
  # Center proxy variables
  Proxycentering <- sapply(1:BlockSize, function(j) {
    subProxy <- proxies[j:(T - BlockSize + j), 1]
    mean(subProxy[subProxy != 0]) - mean(proxies[proxies[,1] != 0, 1])
  })
  Proxycentering <- matrix(rep(Proxycentering, each = nBlock), nrow = T, ncol = ncol(proxies), byrow = TRUE)

  centered_proxies <- proxies_short - Proxycentering
  
  ProxyBlocks <- array(NA, dim = c(BlockSize, k, T - BlockSize + 1))
  for (j in 1:(T - BlockSize + 1)) {
    ProxyBlocks[,,j] <- proxies_short[j:(j + BlockSize - 1),]
 
  }
  
  bootPIs <- array(NA, dim = c(length(Pi_hat), nboot))
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
    bootProxy <-rbind( proxies[1:p, , drop = FALSE],  bootProxy)
    
    boot_proxies_scaled = scale(bootProxy)
    Sigma_u_boot    <- cov(bootres)
    Sigma_uz_boot <- tryCatch({
      t( t(boot_proxies_scaled [c(-1:-p),]) %*% bootres)
    }, error = function(e) {
      proxies_cut <- window(boot_proxies_scaled, start = time(boot_proxies)[p+1])
      return(t( t(proxies_cut) %*% res))  # ou matrix(NA, nrow=..., ncol=...)
    }) #gamma
    
    Sigma_zu_boot   <-   t( Sigma_uz_boot ) 
    Sigma_zz_boot <- Sigma_zu_boot  %*% ginv(Sigma_u_boot ) %*% Sigma_uz_boot 
    
    
    Pi_boot <- c(
      as.vector(Sigma_zu_boot),
      vech( Sigma_zz_boot)
    )
    bootPIs[,j] <- Pi_boot
    # Re-estimate VAR (in R you can use vars::VAR)
    # bootvarEst <- vars::VAR(bootData[(p+1):(T+p), ], p = p, type = "const")
    
  }
  
  return(cov(t(bootPIs)))
}


#proxies <- proxies_kanzig
minimum_distance_var_estimation <- function(x, res, p, proxies, irhor, controls = NULL, elas = 0, target_elas) {
  # x: endogenous variables
  # y: dependent variables
  # p: lag order
  # proxies: instruments or proxies
  # irhor: impulse response horizon (unused here, but kept for potential future use)
  # controls: optional exogenous variables
  
  # proxies <- proxies_boot
  # x <- xnew dim(y)
  # y <- ynew
  # n <- dim(x)[2]
  T = nrow(x) -p
  n <- ncol(x)
  
  Sigma_u    <- cov(res)#sigma_eta
  
  #gros nombres dans les proxies donc on commence par les scales
  
  proxies = scale(proxies)
  #suivre identification simplifiee de olea and al
  #gamma <- t( t(proxies[c(-1:-p),]) %*% res)
  Sigma_uz <- tryCatch({
    t( t(proxies[c(-1:-p),]) %*% res)
  }, error = function(e) {
    proxies_cut <- window(proxies, start = time(proxies)[p+1])
    return(t( t(proxies_cut) %*% res))  # ou matrix(NA, nrow=..., ncol=...)
  }) #gamma
  
  Sigma_zu   <-   t( Sigma_uz) 
  Sigma_zz <- Sigma_zu %*% ginv(Sigma_u) %*% Sigma_uz
  
  
  Pi_hat <- c(
    as.vector(Sigma_uz),
    vech( Sigma_zz)
  )
  
  #theta = c(vec(B1), vec(Pz))
  chol_estimate <- first_var_estimation_bis(x, res, p, proxies, irhor, controls = NULL, target_elas)
  theta_init <-c(as.vector(chol_estimate$theta_0), as.vector(chol_estimate$phi))
  #theta_init <- rnorm(k*n + l*l, sd = 0.1)
  # length(theta_init)
  # length(c(as.vector(chol_estimate$theta_0), as.vector(chol_estimate$phi)))
  # length(as.vector(chol_estimate$theta_0))
  
  W= diag(length(Pi_hat))
  # md_objective(theta = theta_init, Pi_hat= Pi_hat, 
  #              W= diag(length(Pi_hat)), k =k, r= l, g= n)
  
  theta_init_red <- theta_init[c(-1, -(n+2))]
  
  
  md_objective <- function(theta) {
    diff <- Pi_hat - f_theta(theta, k = k, r = l, g =n)
    as.numeric(t(diff) %*% W %*% diff)
  }
  
  lower_bounds <- c( rep(-50, n_pi +2), rep(-Inf, n_theta)) #mat var-cov
  upper_bounds <- c( rep(50, n_pi+2), rep(Inf, n_theta))
  opt <- optim(
    par = theta_init,
    fn = md_objective,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(maxit = 1000)
  )
  
  opt
  
  theta_hat <- opt$par
  
  
  
  
  
  # theta_opt= theta_init_red
  md_objective_norm <- function(theta_opt) {
    theta_full <- numeric(length(theta_init))
    theta_full[1] <- 1          # normalisation 1
    theta_full[n + 2] <- 1      # normalisation 2
    theta_full[-c(1, n+2)] <- theta_opt  # remplir le reste avec theta_opt
    
    # Calculer l'objectif
    diff <- Pi_hat - f_theta(theta_full, k = k, r = l, g = n)
    as.numeric(t(diff) %*% W %*% diff)
  }
  
  n_pi <- length(as.vector(chol_estimate$theta_0)) - 2
  n_theta <- length(as.vector(chol_estimate$phi))
  lower_bounds <- c( rep(-50, n_pi), rep(-Inf, n_theta)) #mat var-cov
  upper_bounds <- c( rep(50, n_pi), rep(Inf, n_theta))
  
  opt_norm <- optim(
    par = theta_init_red,
    fn = md_objective_norm,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(maxit = 1000)
  )
  
  opt_norm
  
  print(md_objective_norm(theta_init_red) )
  print(md_objective_norm(opt_norm$par))
  
  theta_hat <- numeric(length(theta_init))
  theta_hat[1] <- 1          # normalisation 1
  theta_hat[n + 2] <- 1      # normalisation 2
  theta_hat[-c(1, n+2)] <- opt_norm$par 
  

  #est ce que avec cette optimisation de theta_hat
  #aurais je du introduire les contraintes sup sur 
  

  B1 <- cbind(
    theta_hat[1:n],
    theta_hat[(n+1):(n*2)]
  )
  B1[,1] <-   B1[,1]/  B1[1,1]
  B1[,2] <-   B1[,2]/  B1[2,2]
  
  B1
  
  Pz_hat <- cbind(
    theta_hat[(2*n+1):(2*n + k)],
    theta_hat[(2*n+1 + k):(2*(n+k))]
  )
  

  Sigma_zz
  
  Jacobian_f_theta <- jacobian(
    func = function(th) f_theta(th, k, r, g),
    x = theta_hat
  )

  V_pi <- estimate_variance_pi_bootstrap(x,nboot,Pi_hat, theta_hat, proxies, controls=NULL,
                                             start_ts = c(2015,1), end_ts =c(2023,12), irhor=irhor)
  
  V_theta <- ginv(t(Jacobian_f_theta)%*%  ginv(V_pi)  %*% Jacobian_f_theta)
  
  return(list(theta_0= B1,  Pz_hat = Pz_hat, Pi_hat = Pi_hat))
  
}


# I_k <- diag(k)
# I_r <- diag(l)
# 
# D_r <- duplication_matrix(l)
# K_kg <- commutation_matrix(k, n)
# 
# # Partie pour Sigma_zu Sigma_zu'
# F_theta_upper <- matrix(0, l*(l+1)/2, k*n)  # 0_r(r+1)/2 x kg
# 
# dim(F_theta_upper)
# 
# dim(kronecker(Pz_hat , I_r))
# dim(K_kg)
# #compléter Pz_hat avec des zéros
# matrix_12_pz <- matrix_12
# matrix_12_pz[1:r, 1:r] <- Pz_hat
# 
# length(vech(Sigma_zz))
# length(as.vector(Sigma_zu))
# 
# length(theta)
# 
# length(f_theta(theta, k = k, r = l, g =n))
