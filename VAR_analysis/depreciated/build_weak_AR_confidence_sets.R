# test if my strong bands are included
#in IV bands


dim(strong_instruments_lambda_log_GK)

check_bands <- function(hori, i){
  a_0 <- strong_instruments_lambda_log_GK[hori, i,"upper", "lambda_01"]
  b_0 <- strong_instruments_lambda_log_GK[hori, i,"upper", "lambda_02"]
  
  #gamma <- gamma_log
  V0 <- eval_V0(a_0, b_0, gauche, my_cov, droite)
  
  Q_T <- sqrt(T)*(t(e_i) %*% C[,,hori] %*% gamma -
                    t(c(a_0, b_0)) %*% phi_c0)
  Wald_stat <-  Q_T %*% ginv(V0) %*% t(Q_T)
  
  dim2 <- TRUE
  bonferroni <- TRUE
  if (Wald_stat > q_alpha_dim2){
    dim2 <- FALSE
    if (Wald_stat > q_alpha_bonferroni){
      bonferroni <- FALSE
    }
  }
  return(list(dim2 = dim2, bonferroni = bonferroni))
}



# Create an empty list to store results
results <- list()

# Loop over the horizons (hori) and indices (i)
for (hori in 1:20) {
  for (i in 1:4) {
    # Apply the check_bands function for each combination of hori and i
    result <- check_bands(hori, i)
    # Append the results to the list
    results[[length(results) + 1]] <- c(hori = hori, i = i, dim2 = result$dim2, bonferroni = result$bonferroni)
  }
}

# Convert the list of results into a dataframe
df_results <- do.call(rbind, results)
df_results <- as.data.frame(df_results)

# Print the dataframe
print(df_results)



c0 <- theta_0_log[1,2][[1]]
dim(sample_matrices_log_GK$ncvg_theta_0)
strong_std_c <- sd(sample_matrices_log_GK$ncvg_theta_0[1,2,])


bootstrap_weak_instrument_c <- function(gamma, c_0, phi_c0, alpha, ngrid = 100, sample_matrices, lambda_0) {
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
      a_central <- lambda_0[hori, i, 1]
      b_central <- lambda_0[hori, i, 2]
      c_central <- c_0
      
      step_a <- strong_std_lambda1[hori, i]
      step_b <- strong_std_lambda2[hori, i]
      step_c <- strong_std_c
      
      # Construire la séquence
      grid_a <- seq(from = a_central - step_a, to = a_central + step_a, length.out = ngrid)
      grid_b <- seq(from = b_central - step_b, to = b_central + step_b, length.out = ngrid)
      grid_c <- seq(from = c_central - step_c, to = c_central + step_c, length.out = ngrid)
      
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


weak_bootstrap_result_c <- bootstrap_weak_instrument_c(gamma = gamma_log ,c0, phi_c0,
                                                   alpha, ngrid =100,
                                                   sample_matrices_log_GK, lambda_0 = lambda_0_log)


# Measure the time taken by a command
execution_time_c <- system.time({
  # Your command goes here
  weak_bootstrap_result_c <- bootstrap_weak_instrument_c(gamma = gamma_log ,c0, phi_c0,
                                                         alpha, ngrid =10,
                                                         sample_matrices_log_GK, lambda_0 = lambda_0_log)
  # Replace with the actual command you want to time
})

# Print the execution time
print(execution_time_c)



#My idea is to speed up the results
#I could investigate in the vicinity of the bounds I have found from strong instruments


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
      
      a_strong_upper <- strong_instruments_lambda_log_GK[hori, i,"upper", "lambda_01"]
      b_strong_upper <- strong_instruments_lambda_log_GK[hori, i,"upper", "lambda_02"]
      c_central <- c_0
      
      a_strong_lower <- strong_instruments_lambda_log_GK[hori, i,"lower", "lambda_01"]
      b_strong_lower <- strong_instruments_lambda_log_GK[hori, i,"lower", "lambda_02"]
      
      step_a <- strong_std_lambda1[hori, i]
      step_b <- strong_std_lambda2[hori, i]
      step_c <- strong_std_c
      
      # Construire la séquence
      grid_a_upper <- seq(from = a_strong_upper - step_a/2,
                          to = a_strong_upper + step_a/2, length.out = ngrid_ab)
      grid_a_lower <- seq(from = a_strong_lower - step_a/2,
                          to = a_strong_lower + step_a/2, length.out = ngrid_ab)
      
      grid_b_upper <- seq(from = b_strong_upper - step_b/2,
                          to = b_strong_upper + step_b/2, length.out = ngrid_ab)
      grid_b_lower <- seq(from = b_strong_lower - step_b/2,
                          to = b_strong_lower + step_b/2, length.out = ngrid_ab)
      
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




# Measure the time taken by a command
execution_time_c_local <- system.time({
  # Your command goes here
  weak_bootstrap_result_c_local <- bootstrap_weak_instrument_c_local(gamma = gamma_log ,c0, phi_c0,
                                                         alpha, ngrid_ab = 10, ngrid_c = 10,
                                                         sample_matrices_log_GK, lambda_0 = lambda_0_log)
  # Replace with the actual command you want to time
})

# Print the execution time
print(execution_time_c_local)

weak_bootstrap_result_c_local_2 <- bootstrap_weak_instrument_c_local(gamma = gamma_log ,c0, phi_c0,
                                                                   alpha, ngrid_ab = 20, ngrid_c = 10,
                                                                   sample_matrices_log_GK, lambda_0 = lambda_0_log)




weak_instruments_pointwise_log_GK <- weak_bootstrap_result$weak_lambda_bounds_Wald_stat
weak_instruments_bondferroni_log_GK <- weak_bootstrap_result$weak_lambda_bounds_bonferroni

weak_instruments_pointwise_log_GK_c <- weak_bootstrap_result_c$weak_lambda_bounds_Wald_stat
weak_instruments_bondferroni_log_GK_c <- weak_bootstrap_result_c$weak_lambda_bounds_bonferroni

weak_instruments_pointwise_log_GK_c_local <- weak_bootstrap_result_c_local_2$weak_lambda_bounds_Wald_stat
weak_instruments_bondferroni_log_GK_c_local <- weak_bootstrap_result_c_local_2$weak_lambda_bounds_bonferroni


#compare the bands from all the methods

library(latex2exp)

names <- c('$P_{elec}$','$Q_{elec}$', "IPI", '$NE Index$')

n <- length(names)
plot_irf_mini_panel_compare <- function(i){
  #i=1
  main_prediction <- lambda_0_log[, i, 1]
  time_horizon <- 1:20
  
  
  weak_instruments_pointwise_log_GK
  weak_instruments_pointwise_log_GK_c
  weak_instruments_pointwise_log_GK_c_local
  
  old_bands_inf <- weak_instruments_pointwise_log_GK[,i,"lower","lambda_01"]
  old_bands_sup <- weak_instruments_pointwise_log_GK[,i,"upper","lambda_01"]
  
  new_bands_inf <- weak_instruments_pointwise_log_GK_c[,i,"lower","lambda_01"]
  new_bands_sup <- weak_instruments_pointwise_log_GK_c[,i,"upper","lambda_01"]
  
  new_bands_inf_local <- weak_instruments_pointwise_log_GK_c_local[,i,"lower","lambda_01"]
  new_bands_sup_local <- weak_instruments_pointwise_log_GK_c_local[,i,"upper","lambda_01"]
  
  
  # Plot the responses for each variable
  
  my_plot <- ggplot()+
    
    geom_ribbon(aes(x=time_horizon, ymin = old_bands_inf,
                    ymax = old_bands_sup), fill = "lightblue", alpha = 0.5) +
    
    geom_ribbon(aes(x=time_horizon, ymin = new_bands_inf,
                    ymax = new_bands_sup), fill = "blue", alpha = 0.5) +
    
    geom_ribbon(aes(x=time_horizon, ymin = new_bands_inf_local,
                    ymax = new_bands_sup_local), fill = "darkblue", alpha = 0.5) +
    
    # ggtitle(expression(paste(names[i]))) +
    labs(title = TeX(names[i]),
         x = "Time",
         y = "Unit Points") +
    geom_line(aes(x=time_horizon, y= main_prediction), linetype='dashed')+
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
    scale_x_continuous()+
    scale_y_continuous()+
    # Add confidence bands
    theme_minimal()
  return(my_plot)
}


# Apply the function to the sequence 3:10
results <- lapply(c(1:n) , plot_irf_mini_panel_compare)

# Arrange the plots in a grid
plots_grid <- do.call("grid.arrange", c(results, nrow = 2))

final_plot <- plot_grid(plots_grid, custom_legend , ncol = 1, rel_heights = c(1, 0.1))

title <- ggdraw() + 
  draw_label(TeX('$Q_{elec}$ shock IRF'), fontface = 'bold', x = 0.5, hjust = 0.5)

# Combine the title and the final plot with legend
final_plot_with_title <- plot_grid(title, final_plot, ncol = 1, rel_heights = c(0.1, 1))

# Display the final plot
print(final_plot_with_title)







library(latex2exp)

names <- c('$P_{elec}$','$Q_{elec}$', "IPI", '$NE Index$')

n <- length(names)
plot_irf_mini_panel_compare <- function(i){
  #i=1
  main_prediction <- lambda_0_log[, i, 1]
  time_horizon <- 1:20
  

  
  old_bands_inf <- weak_instruments_bondferroni_log_GK[,i,"lower","lambda_01"]
  old_bands_sup <- weak_instruments_bondferroni_log_GK[,i,"upper","lambda_01"]
  
  new_bands_inf <- weak_instruments_bondferroni_log_GK_c[,i,"lower","lambda_01"]
  new_bands_sup <- weak_instruments_bondferroni_log_GK_c[,i,"upper","lambda_01"]
  
  new_bands_inf_local <- weak_instruments_bondferroni_log_GK_c_local[,i,"lower","lambda_01"]
  new_bands_sup_local <- weak_instruments_bondferroni_log_GK_c_local[,i,"upper","lambda_01"]
  
  
  # Plot the responses for each variable
  
  my_plot <- ggplot()+
    
    geom_ribbon(aes(x=time_horizon, ymin = old_bands_inf,
                    ymax = old_bands_sup), fill = "lightblue", alpha = 0.5) +
    
    geom_ribbon(aes(x=time_horizon, ymin = new_bands_inf,
                    ymax = new_bands_sup), fill = "blue", alpha = 0.5) +
    
    geom_ribbon(aes(x=time_horizon, ymin = new_bands_inf_local,
                    ymax = new_bands_sup_local), fill = "darkblue", alpha = 0.5) +
    
    # ggtitle(expression(paste(names[i]))) +
    labs(title = TeX(names[i]),
         x = "Time",
         y = "Unit Points") +
    geom_line(aes(x=time_horizon, y= main_prediction), linetype='dashed')+
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
    scale_x_continuous()+
    scale_y_continuous()+
    # Add confidence bands
    theme_minimal()
  return(my_plot)
}


# Apply the function to the sequence 3:10
results <- lapply(c(1:n) , plot_irf_mini_panel_compare)

# Arrange the plots in a grid
plots_grid <- do.call("grid.arrange", c(results, nrow = 2))

final_plot <- plot_grid(plots_grid, custom_legend , ncol = 1, rel_heights = c(1, 0.1))

title <- ggdraw() + 
  draw_label(TeX('$Q_{elec}$ shock IRF'), fontface = 'bold', x = 0.5, hjust = 0.5)

# Combine the title and the final plot with legend
final_plot_with_title <- plot_grid(title, final_plot, ncol = 1, rel_heights = c(0.1, 1))

# Display the final plot
print(final_plot_with_title)







