vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))
x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]
vl = vars::VAR(x, p, type = "none")
coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)


# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute impulse response functions (IRFs)
irf_model <- irf(vl, impulse=NULL, response=NULL, n.ahead=19)

irf_main <- irf_model$irf
irf_lower_bound <- irf_model$Lower
irf_upper_bound <- irf_model$Upper


irf_lower_bound <- lapply(seq_along(irf_lower_bound), function(i) {
  irf_lower_bound[[i]] / irf_main[[i]][1, i]
})
irf_upper_bound <- lapply(seq_along(irf_upper_bound), function(i) {
  irf_upper_bound[[i]] / irf_main[[i]][1, i]
})
# Remplacer les IRFs non normalisées dans le modèle par les normalisées
irf_main <- lapply(seq_along(irf_main), function(i) {
  irf_main[[i]] / irf_main[[i]][1, i]
})

library(abind)


# Combiner les matrices en un tableau 20x4x4
irf_main <- abind(irf_main, along = 3)
irf_lower_bound <- abind(irf_lower_bound, along = 3)
irf_upper_bound <- abind(irf_upper_bound, along = 3)



cholesky_lambda_log <-  array(NA, dim = c(irhor, n, 2,  2)) 
dimnames(cholesky_lambda_log ) <- list(
   NULL, NULL, c("lower", "upper"), c("lambda_01", "lambda_02")
)

cholesky_lambda_log[,,1,] <- irf_lower_bound[,,1:2]
cholesky_lambda_log[,,2,] <- irf_upper_bound[,,1:2]

save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output/plot_irfs'
setwd(save_rds_path)
saveRDS(irf_main, "irf_main.rds")
saveRDS(cholesky_lambda_log, "cholesky_lambda_log.rds")



library(latex2exp)

names <- c('$P_{elec}$','$Q_{elec}$', "IPI", '$NE Index$')

n <- length(names)
plot_irf_mini_panel_tot_cholesky <- function(i){
  #i=1
  #main_prediction <- lambda_0_log[, i, 1]
  main_prediction <- irf_main[,i,1]
  
  time_horizon <- 1:20
  
  strong_band_inf <-
    cholesky_lambda_log[,i,"lower","lambda_01"]
  strong_band_sup <- 
    cholesky_lambda_log[,i,"upper","lambda_01"]
  
  # Plot the responses for each variable
  
  my_plot <- ggplot()+
    
    geom_ribbon(aes(x=time_horizon, ymin = strong_band_inf,
                    ymax = strong_band_sup), fill = "darkblue", alpha = 0.5) +
    
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
results <- lapply(c(1:n) , plot_irf_mini_panel_tot_cholesky)

# Arrange the plots in a grid
plots_grid <- do.call("grid.arrange", c(results, nrow = 2))

#final_plot <- plot_grid(plots_grid, custom_legend , ncol = 1, rel_heights = c(1, 0.1))

title <- ggdraw() + 
  draw_label(TeX('$P_{elec}$ shock IRF'), fontface = 'bold', x = 0.5, hjust = 0.5)

# Combine the title and the final plot with legend
final_plot_with_title <- plot_grid(title, plots_grid, ncol = 1, rel_heights = c(0.1, 1))

# Display the final plot
print(final_plot_with_title)








