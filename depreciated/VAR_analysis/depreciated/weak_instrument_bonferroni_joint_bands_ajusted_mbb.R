
c0 <- theta_0[1,2][[1]]
phi_c0 <- matrix(c(1,0, -c0, 1), nrow = 2, ncol = 2) %*% t(matrix_12)  %*% gamma


eval_composantes_V0 <- function(i, hori){
  #i=2
  e_i <- rep(0, n)
  # Mettre 1 à la position j
  e_i[i] <- 1
  
  boot_terme_gauche <- matrix(0, nrow=nboot , ncol=k )
  boot_covariance <- array(NA, dim = c(nboot, k, k**2))
  boot_terme_droite <- matrix(0, nrow=nboot , ncol=2*k)
  
  for (b in c(1:nboot)){
    gamma_hat <- matrix(gamma_vec_sample[b,], nrow=n, ncol=k)
    
    Marep_hat <- matrix(inv_sample[b,],nrow = n*irhor,ncol = n) 
    
    C_hat <- array(Marep_hat, dim = c(n, n, irhor))
    
    phi_c0_hat <- matrix(c(1,0, -c0, 1), nrow = 2, ncol = 2) %*%
      t(matrix_12)  %*% gamma_hat
    boot_terme_gauche[b,] <- t(e_i) %*% C_hat[,,hori] %*% gamma_hat
    boot_terme_droite[b,] <- as.vector(phi_c0_hat)
  }
  
  
  
  gauche <- cov(boot_terme_gauche)
  droite <- cov(boot_terme_droite)
  my_cov <- cov(boot_terme_droite, boot_terme_gauche)
  
  # mean_matrices <- apply(boot_covariance, c( 2, 3), mean)
  # # my_cov <- matrix( apply(boot_covariance, 2, mean), nrow= k, ncol=k**2)
  # View(t(apply(boot_covariance, c( 2, 3), mean)))
  return( list(gauche = gauche, droite=droite, my_cov = my_cov))
  
}

eval_V0 <- function(a_0, b_0,gauche, my_cov, droite){
  mat_vec <- matrix(c(a_0, 0,  b_0, 0, 0, a_0, 0 , b_0), nrow=2, ncol=4 )
  # return(T*(gauche -  2*my_cov%*% t(mat_vec) +
  # mat_vec %*% droite  %*% t(mat_vec)))
  return(T*(gauche -  2*t(my_cov) %*% t(mat_vec) +
              mat_vec %*% droite  %*% t(mat_vec)))
  
}

fake_var_lambda1 <- matrix(sqrt(diag(sigma_fake_lambda_01)),
                           nrow=20, ncol =4)
fake_var_lambda2 <- matrix(sqrt(diag(sigma_fake_lambda_02)),
                           nrow=20, ncol =4)

k <- ncol(proxies)
n <- ncol(y)
alpha <- 0.05
q_alpha_bonferroni <- qchisq(1 - alpha/irhor, df= 2)
q_alpha_dim2 <- qchisq(1 - alpha, df= 2)

lambda_0_inf <- array(NA, dim = c(irhor, n, k))
lambda_0_sup <- array(NA, dim = c(irhor, n, k))
lambda_0_inf_bonferroni <- array(NA, dim = c(irhor, n, k))
lambda_0_sup_bonferroni <- array(NA, dim = c(irhor, n, k))


for (hori in c(1:irhor)){
  print(hori)
  for (i in c(1:5)){
    # hori=2
    # i=3
    e_i <- rep(0, n)
    # Mettre 1 à la position j
    e_i[i] <- 1
    ngrid <- 100
    a_central <- lambda_0[hori,i,1]
    b_central <- lambda_0[hori,i,2]
    
    step_a <- 3*fake_var_lambda1[hori,i]
    step_b <- 3*fake_var_lambda2[hori,i]
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
    
    
    compo_V0 <- eval_composantes_V0(i, hori)
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
    
    lambda_0_inf[hori,i,1] <- min(list_a)
    lambda_0_sup[hori,i,1] <- max(list_a)
    lambda_0_inf[hori,i,2] <- min(list_b)
    lambda_0_sup[hori,i,2] <- max(list_b)
    
    
    lambda_0_inf_bonferroni[hori,i,1] <- min(list_a_bonferroni)
    lambda_0_sup_bonferroni[hori,i,1] <- max(list_a_bonferroni)
    lambda_0_inf_bonferroni[hori,i,2] <- min(list_b_bonferroni)
    lambda_0_sup_bonferroni[hori,i,2] <- max(list_b_bonferroni)
    
    
    
    if (lambda_0_sup_bonferroni[hori,i,1]- lambda_0_inf_bonferroni[hori,i,1] < step_a/ngrid*10 
        | lambda_0_sup_bonferroni[hori,i,2]- lambda_0_inf_bonferroni[hori,i,2] < step_b/ngrid *10){
      print("Le pas de discretisation est trop faible")
    }
    
  }
}




