




weak_bootstrap_result <- bootstrap_weak_instrument(gamma = gamma_log ,c_0, phi_c0,
                                                   alpha, ngrid =100,
                                                   sample_matrices_log_GK, lambda_0 = lambda_0_log)


weak_instruments_pointwise_log_GK <- weak_bootstrap_result$weak_lambda_bounds_Wald_stat
weak_instruments_bondferroni_log_GK <- weak_bootstrap_result$weak_lambda_bounds_bonferroni


#no nan in the pointwise values



bootstrap_weak_instrument_c_local <- function(gamma, c_0, phi_c0, alpha,
                                              ngrid_ab = 10, ngrid_c = 10, sample_matrices, lambda_0) {
  #lambda_0 = lambda_0_log
  #sample_matrices <- sample_matrices_log_GK
  #gamma <- gamma_log
  
  q_alpha_bonferroni <- qchisq(1 - alpha/irhor, df = 2)
  q_alpha_dim2 <- qchisq(1 - alpha, df = 2)
  
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
  
  for (hori in c(1:irhor)) {
    print(hori)
    for (i in c(1:n)) {
      e_i <- rep(0, n)
      # Mettre 1 à la position j
      e_i[i] <- 1
      
      a_first_upper <- weak_instruments_pointwise_log_GK[hori, i,"upper", "lambda_01"]
      b_first_upper <- weak_instruments_pointwise_log_GK[hori, i,"upper", "lambda_02"]
      c_central <- c_0
      
      a_first_lower <- weak_instruments_pointwise_log_GK[hori, i,"lower", "lambda_01"]
      b_first_lower <- weak_instruments_pointwise_log_GK[hori, i,"lower", "lambda_02"]
      
      step_a <- first_std_lambda1[hori, i]
      step_b <- first_std_lambda2[hori, i]
      step_c <- first_std_c
      
      # Construire la séquence
      grid_a_upper <- seq(from = a_first_upper,
                          to = a_first_upper + step_a, length.out = ngrid_ab)
      grid_a_lower <- seq(from = a_first_lower - step_a,
                          to = a_first_lower, length.out = ngrid_ab)
      
      grid_b_upper <- seq(from = b_first_upper,
                          to = b_first_upper + step_b, length.out = ngrid_ab)
      grid_b_lower <- seq(from = b_first_lower - step_b,
                          to = b_first_lower, length.out = ngrid_ab)
      
      grid_a <- c(grid_a_upper, grid_a_lower)
      grid_b <- c(grid_b_upper, grid_b_lower)
      grid_c <- seq(from = c_central - step_c, to = c_central + step_c, length.out = ngrid_c)
      
      # Génération de la grille
      grid_abc <- expand.grid(list(grid_a, grid_b, grid_c))
      
      compo_V0 <- eval_composantes_V0(i, hori, sample_matrices)
      gauche <- compo_V0$gauche
      my_cov <- compo_V0$my_cov
      droite <- compo_V0$droite
      
      list_a <- c()
      list_b <- c()
      list_c <- c()
      list_a_bonferroni <- c()
      list_b_bonferroni <- c()
      list_c_bonferroni <- c()
      
      for (igrid in c(1:nrow(grid_abc))) {
        ab_test <- grid_abc[igrid,]
        a <- ab_test[[1]]
        b <- ab_test[[2]]
        c <- ab_test[[3]]
        
        V0 <- eval_V0(a, b, gauche, my_cov, droite)
        
        phi_c <- matrix(c(1, 0, -c, 1), nrow = 2, ncol = 2) %*% t(matrix_12) %*% gamma_log
        
        Q_T <- sqrt(T) * (t(e_i) %*% C[,,hori] %*% gamma - t(c(a, b)) %*% phi_c)
        Wald_stat <- Q_T %*% ginv(V0) %*% t(Q_T)
        
        if (Wald_stat < q_alpha_bonferroni) {
          if (Wald_stat < q_alpha_dim2) {
            list_a <- c(list_a, a)
            list_b <- c(list_b, b)
            list_c <- c(list_c, c)
          }
          
          list_a_bonferroni <- c(list_a_bonferroni, a)
          list_b_bonferroni <- c(list_b_bonferroni, b)
          list_c_bonferroni <- c(list_c_bonferroni, c)
        }
      }
      
      # Check if the condition holds
      if (step_a / ngrid < max(list_a) - min(list_a) |
          step_b / ngrid < max(list_b) - min(list_b) |
          step_c / ngrid < max(list_c) - min(list_c)) {
        warning("The grid is too coarse")
      }
      
      # Populate the array
      weak_lambda_bounds_Wald_stat[hori, i, "lower", "lambda_01"] <- min(list_a)
      weak_lambda_bounds_Wald_stat[hori, i, "lower", "lambda_02"] <- min(list_b)
      weak_lambda_bounds_Wald_stat[hori, i, "upper", "lambda_01"] <- max(list_a)
      weak_lambda_bounds_Wald_stat[hori, i, "upper", "lambda_02"] <- max(list_b)
      
      weak_lambda_bounds_bonferroni[hori, i, "lower", "lambda_01"] <- min(list_a_bonferroni)
      weak_lambda_bounds_bonferroni[hori, i, "lower", "lambda_02"] <- min(list_b_bonferroni)
      weak_lambda_bounds_bonferroni[hori, i, "upper", "lambda_01"] <- max(list_a_bonferroni)
      weak_lambda_bounds_bonferroni[hori, i, "upper", "lambda_02"] <- max(list_b_bonferroni)
    }
  }
  
  return(list(
    weak_lambda_bounds_Wald_stat = weak_lambda_bounds_Wald_stat,
    weak_lambda_bounds_bonferroni = weak_lambda_bounds_bonferroni
  ))
}




weak_bootstrap_result_c_local_2 <- bootstrap_weak_instrument_c_local(gamma = gamma_log ,c0, phi_c0,
                                                                     alpha, ngrid_ab = 100, ngrid_c = 100,
                                                                     sample_matrices_log_GK, lambda_0 = lambda_0_log)


