library(latex2exp)
library(cowplot)
library(gridExtra)

#Take care of the legend

# Exemple simulé si tu n’as pas défini `simul_data`
set.seed(123)
simul_data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  `Confidence.Bands` = rep(c("90%", "68%"), each = 50)
  ,
  `Linetype` = rep(c("Main Estimate", "Bootstrap Median"), 50)
)
names_supply <- c("Pelec"   ,    "Qelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 
names_demand <- c(   "Qelec"   , "Pelec"   ,    "IPI"    ,     "CPI"    ,     "Policy rate", "Pgas") 

names <- c("Pelec", "Qelec", "IPI", "CPI", "Policy rate", "Pgas")
# Définir le mapping manuel pour les types de traits
linetype <- c("Bootstrap Median"= "dashed", "Main Estimate" = "solid")

# Plot avec légende uniquement
plot_legend <- ggplot(data = simul_data, aes(x = x)) +
  geom_histogram(aes(fill = `Confidence.Bands`), bins = 30, alpha = 0.6) +
  geom_line(aes(y = y, linetype = `Linetype`)) +
  scale_fill_manual(
    values = c("90%" = "blue", "68%" = "lightblue"),
    drop = FALSE
  ) +
  scale_linetype_manual(
    values = linetype,
    name = "Linetype",
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

# Extraire la légende personnalisée
custom_legend <- cowplot::get_legend(plot_legend)

plot_legend_msw <- ggplot(data = simul_data, aes(x = x)) +
  geom_histogram(aes(fill = `Confidence.Bands`), bins = 30, alpha = 0.6) +
  scale_fill_manual(
    values = c("90%" = "blue", "68%" = "lightblue"),
    drop = FALSE
  ) +

  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  )

# Extraire la légende personnalisée
custom_legend_msw <- cowplot::get_legend(plot_legend_msw)

#custom_legend <- get_plot_component(plot_legend, "guide-box", return_all = TRUE)[[3]]

#custom_legend[[3]]
#custom_legend


make_bandwith_obj <- function(irf_data, my_confi_level, my_type, template_dim){
  
  # 1) filtrer et passer en large
  wide_tbl <- irf_data %>% 
    filter(confi_level == my_confi_level&
             type        == my_type) %>%
    dplyr::select(variable, horizon, value) %>% 
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(horizon) %>% 
    dplyr::select(-horizon)
  
  # 2) extraire les deux éléments (array 3D)
  mat12 <- apply(wide_tbl, c(1, 2), function(x) x)   # list matrix
  # On transforme en array 3D [horizon × variable × 2]
  #arr <- array(NA, dim = c(dim(mat12), 1))
  
  # # Extraire structure à 2 valeurs
  # # mat1 = premier élément, mat2 = second élément
  # mat1 <- apply(wide_tbl, c(1, 2), function(x) x[[1]])
  #mat2 <- apply(wide_tbl, c(1, 2), function(x) x[[2]])
  
  # # 3) reconstruire new_obj avec même structure que bandwith_68
  # new_obj <- array(NA, dim = template_dim)
  # 
  # new_obj[,,1] <- mat1[1,1:50,]
  # new_obj[,,2] <- mat1[2,1:50,]
  
  #return( mat12[1:50,])
  return( mat12)
}
quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_probs_50 <- c(0.5, 0.5)
compute_quantile_bands <- function(boot_array, probs) {
  # boot_array: horizon+1 x nvar x nsim
  apply(boot_array, c(1, 2), function(x) quantile(x, probs = probs, na.rm = TRUE))
}

plot_irf_msw <- function(i, shock, band_lower_68,
                         band_upper_68,band_lower_90, band_upper_90,
                         names = names,main_IV_irf= main_IV_irf,
                         irhor = irhor) {
  #trick I just put 0.5 twice
  
  # main_chol <- lambda_0_chol[,i,index]
  # #Normalize IRF
  # main_chol <- as.numeric(main_chol/lambda_0_chol[1,index,index])
  
  #dim(lambda_0_median)
  time_horizon <- 1:(irhor+1)
  
  band_lower_68 <- band_lower_68[, i]
  band_upper_68 <- band_upper_68[, i]
  band_lower_90 <- band_lower_90 [, i]
  band_upper_90 <- band_upper_90[, i]
  
  main_IV <- unlist(main_IV_irf[,i])
  # length(time_horizon)
  # length(band_lower_68)
  ggplot() +
    geom_ribbon(aes(x = time_horizon, ymin = band_lower_90, ymax = band_upper_90), fill = "blue", alpha = 0.4) +
    geom_ribbon(aes(x = time_horizon, ymin = band_lower_68, ymax = band_upper_68), fill = "lightblue", alpha = 0.6) +
    #geom_line(aes(x = time_horizon, y = main_chol), color = "darkblue", linetype = 'dashed') +
    geom_line(aes(x = time_horizon, y = main_IV), color = "darkblue", linetype = "solid") +
    #geom_line(aes(x = time_horizon, y = median_50), color = "darkblue", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
    labs(
      title = TeX(names[i]),
      x = "Time",
      y = "Unit Points"
    ) +
    theme(legend.text = element_text(size = 15),      # taille du texte
          legend.title = element_text(size = 16),     # taille du titre
          legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
          axis.title.x = element_text(size = 14),   # taille du titre axe x
          axis.title.y = element_text(size = 14),   # taille du titre axe y
          axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
          axis.text.y  = element_text(size = 12)    # taille des ticks y
    )
}

plot_full_irf <- function(VAR_data, shock, plot_irf) {
  
  irf_data <- VAR_data$irfs
  
  if (shock == 1) {
    my_names <- names_supply
    #my_names <- names_demand
    #normaliser par spot price log
    #présenter comme un choc en prix
    # irf_data <- irf_data %>%
    #   group_by(confi_type, confi_level) %>%
    #   mutate(
    #     value = value / value[
    #       horizon == 1 &
    #         grepl("spot_price_log", variable) &
    #         type == "point"
    #     ]
    #   ) %>%
    #   ungroup()
    
  } else {
    my_names <- names_demand
  }

  irf_data_cum <- irf_data%>%
    filter(confi_type == "msw" )#| confi_type == "msw_cum")
  
  lower_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
  upper_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))
  
  lower_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
  upper_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))
  
  main_IV_irf <- irf_data_cum %>% 
    filter(confi_level == 0.68 & type == "point") %>%
    dplyr::select(variable, horizon, value) %>% 
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>% 
    dplyr::select(-horizon)
 
  vars_wedge <- grep("wedge", unique(irf_data_cum$variable), value = TRUE)
  if (length(vars_wedge)>=1){
    q <- n-1
  }else{
    q<- n
  }
# i=7
  plots <- lapply(seq_len(q), function(i) {
    plot_irf_msw(
      i = i,
      shock = shock,
      band_lower_68 = lower_68,
      band_upper_68 = upper_68,
      band_lower_90 = lower_90,
      band_upper_90 = upper_90,
      names = my_names,
      main_IV_irf= main_IV_irf,
      irhor = irhor
    )
  })
  if(shock == 1){
    # Inverser la première et la deuxième vignette
    plots[c(1,2)] <- plots[c(2,1)]
  }
  # Generate plots
  grid_plots <- grid.arrange(grobs = plots, nrow = 2)
  
  legend_plot <- cowplot::plot_grid(grid_plots, custom_legend_msw , ncol = 1, rel_heights = c(1, 0.1))
  
  # Title and final composition
  title <- ggdraw() + draw_label(paste0("Shock: ", ifelse(shock == 1, "Supply", "Demand")),
                                 , fontface = 'bold', x = 0.5, hjust = 0.5)
  final_plot <- cowplot::plot_grid(title, legend_plot, ncol = 1, rel_heights = c(0.1, 1))
  
  return(final_plot)
}


# bootIRFs = bootIRFs_double_original

plot_full_irf_mbb <- function(bootIRFs, shock, plot_irf, lambda_0, double = FALSE) {
  

  if (shock == 1) {
    my_names <-  names_supply
  } else{
    my_names <- names_demand 
  }
  # med <- median(bootIRFs[1, 2, ])
  # if (shock == 1) {
  #   
  #   
  #   #normaliser par spot price log
  #   #présenter comme un choc en prix
  #   bootIRFs<- sweep(
  #     bootIRFs,
  #     MARGIN = 3,
  #     STATS = median(bootIRFs[1, 2, ]),
  #     FUN = "/"
  #   )
  #   lambda_0 = lambda_0/med
  # } 
  plots <- lapply(seq_len(n), function(i) {
    plot_irf_mbb(
      i = i,
      bootIRFs,
      names = my_names,
      irhor = irhor, 
      shock = shock,
      lambda_0 = lambda_0,
      double = double
    )
  })
  if(shock == 1){
    # Inverser la première et la deuxième vignette
    plots[c(1,2)] <- plots[c(2,1)]
  }
  # Generate plots
  grid_plots <- grid.arrange(grobs = plots, nrow = 2)
  
  legend_plot <- cowplot::plot_grid(grid_plots, custom_legend , ncol = 1, rel_heights = c(1, 0.1))
  
  # Title and final composition
  title <- ggdraw() + draw_label(paste0("Shock: ", ifelse(shock == 1, "Supply", "Demand")),
                                 , fontface = 'bold', x = 0.5, hjust = 0.5)
  final_plot <- cowplot::plot_grid(title, legend_plot, ncol = 1, rel_heights = c(0.1, 1))
  
  return(final_plot)
}


plot_irf_mbb <- function(
    i = i,
    bootIRFs,
    names = names,
    irhor = irhor,
    shock = shock,
    lambda_0 = lambda_0,
    double = FALSE
) {
  IRF_90 <- compute_quantile_bands(bootIRFs, quantile_probs_90)
  IRF_68 <- compute_quantile_bands(bootIRFs, quantile_probs_68)
  IRF_50 <- compute_quantile_bands(bootIRFs, quantile_probs_50)[1,,]
  if (double == TRUE){
    main_IV <- lambda_0[, i]
  }else{
    main_IV <- lambda_0[, i,1]
  }
  
  median_50 <- IRF_50[, i]
  # main_chol <- lambda_0_chol[,i,index]
  # #Normalize IRF
  # main_chol <- as.numeric(main_chol/lambda_0_chol[1,index,index])
  
  #dim(lambda_0_median)
  time_horizon <- 1:irhor
  lower_68 <- IRF_68[1, ,i]
  upper_68 <- IRF_68[2, ,i]
  lower_90 <- IRF_90[1, ,i]
  upper_90 <- IRF_90[2, ,i]
  
  ggplot() +
    geom_ribbon(aes(x = time_horizon, ymin = lower_90, ymax = upper_90), fill = "blue", alpha = 0.4) +
    geom_ribbon(aes(x = time_horizon, ymin = lower_68, ymax = upper_68), fill = "lightblue", alpha = 0.6) +
    #geom_line(aes(x = time_horizon, y = main_chol), color = "darkblue", linetype = 'dashed') +
    geom_line(aes(x = time_horizon, y = main_IV), color = "darkblue", linetype = "solid") +
    geom_line(aes(x = time_horizon, y = median_50), color = "darkblue", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
    labs(
      title = TeX(names[i]),
      x = "Time",
      y = "Unit Points"
    ) +
    theme(legend.text = element_text(size = 15),      # taille du texte
          legend.title = element_text(size = 16),     # taille du titre
          legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
          axis.title.x = element_text(size = 14),   # taille du titre axe x
          axis.title.y = element_text(size = 14),   # taille du titre axe y
          axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
          axis.text.y  = element_text(size = 12)    # taille des ticks y
    )
}


plot_irf_mbb_double <- function(
    i = i,
    bootIRFs,
    names = names,
    irhor = irhor,
    shock = shock,
    lambda_0 = lambda_0
) {
  IRF_90 <- compute_quantile_bands_dim3(bootIRFs, quantile_probs_90)
  IRF_68 <- compute_quantile_bands_dim3(bootIRFs, quantile_probs_68)
  IRF_50 <- compute_quantile_bands_dim3(bootIRFs, quantile_probs_50)[1,,,]
  main_IV <- lambda_0[, i, shock]
  
  median_50 <- IRF_50[, i, shock]
  # main_chol <- lambda_0_chol[,i,index]
  # #Normalize IRF
  # main_chol <- as.numeric(main_chol/lambda_0_chol[1,index,index])
  
  #dim(lambda_0_median)
  time_horizon <- 1:irhor
  lower_68 <- IRF_68[1, ,i, shock]
  upper_68 <- IRF_68[2, ,i, shock]
  lower_90 <- IRF_90[1, ,i, shock]
  upper_90 <- IRF_90[2, ,i, shock]
  
  ggplot() +
    geom_ribbon(aes(x = time_horizon, ymin = lower_90, ymax = upper_90), fill = "blue", alpha = 0.4) +
    geom_ribbon(aes(x = time_horizon, ymin = lower_68, ymax = upper_68), fill = "lightblue", alpha = 0.6) +
    #geom_line(aes(x = time_horizon, y = main_chol), color = "darkblue", linetype = 'dashed') +
    geom_line(aes(x = time_horizon, y = main_IV), color = "darkblue", linetype = "solid") +
    geom_line(aes(x = time_horizon, y = median_50), color = "darkblue", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
    labs(
      title = TeX(names[i]),
      x = "Time",
      y = "Unit Points"
    ) +
    theme(legend.text = element_text(size = 15),      # taille du texte
          legend.title = element_text(size = 16),     # taille du titre
          legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
          axis.title.x = element_text(size = 14),   # taille du titre axe x
          axis.title.y = element_text(size = 14),   # taille du titre axe y
          axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
          axis.text.y  = element_text(size = 12)    # taille des ticks y
    )
}


# irf_list <- list_irfs
# dim(irf_list)
# irf_list[[1]]
# irf_list[[1]][1:20, i]

compare_irf <- function(irf_list, variable_names, shock = "price") {
  n_irf <- length(irf_list)
  names(irf_list)
  if (shock == "supply"){
    shock_label = "Supply shock"
  }else if (shock == "demand"){
    shock_label = "Demand shock"
  }else{
    return("Invalid shock type. Please use 'price' or 'quantity'.")
  }
  
  #solution <-gen_benchmark
  plot_irf <- function(i=1) {
    #i=3
    #trick I just put 0.5 twice
    #main_IV <- lambda_0[, i, "lower"]
    # Combine all IRFs into one data frame
    #irf_list[[4]][1:20, i, 1]
    time_horizon <- c(1:20)
    # j =2
    df_all <- lapply(seq_along(irf_list), function(j) {
      data.frame(
        time = time_horizon,
        value = irf_list[[j]][1:20, i],
        model = names(irf_list)[j]
      )
    }) %>% bind_rows()
    
    ggplot(df_all, aes(x = time, y = value, color = model)) +
      geom_line(linetype = "solid") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = TeX(variable_names[i]),
        x = "Time",
        y = "Unit Points"
      ) +
      scale_color_manual(
        values = c("Cholesky" ="blue" ,   "IV MR"="red" ,      "IV SD" = "green" ,     "Joint IV SD"= "darkgreen"),
        drop = FALSE
      ) +
      guides(color = "none")+
      theme(plot.title = element_text(size = 16))
    
  }

  # Generate plots
  plots_list <- lapply(1:n,plot_irf)
  grid_plots <- do.call("grid.arrange", c(plots_list, nrow = 2))
  
  legend_plot <- plot_grid(grid_plots, custom_legend , ncol = 1, rel_heights = c(1, 0.1))
  return(legend_plot)
  
}


compare_irf_bis <- function(irf_list, variable_names, shock = "price") {
  n_irf <- length(irf_list)
  names(irf_list)
  if (shock == "supply"){
    shock_label = "Supply shock"
  }else if (shock == "demand"){
    shock_label = "Demand shock"
  }else{
    return("Invalid shock type. Please use 'price' or 'quantity'.")
  }
  
  #solution <-gen_benchmark
  plot_irf <- function(i=1) {
    #i=3
    #trick I just put 0.5 twice
    #main_IV <- lambda_0[, i, "lower"]
    # Combine all IRFs into one data frame
    #irf_list[[4]][1:20, i, 1]
    time_horizon <- c(1:20)
    # j =2
    df_all <- lapply(seq_along(irf_list), function(j) {
      data.frame(
        time = time_horizon,
        value = irf_list[[j]][1:20, i],
        model = names(irf_list)[j]
      )
    }) %>% bind_rows()
    
    ggplot(df_all, aes(x = time, y = value, color = model)) +
      geom_line(linetype = "solid") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = TeX(variable_names[i]),
        x = "Time",
        y = "Unit Points"
      ) +
      scale_color_manual(
        values = c("Raw" ="blue" ,   "Residualized"="red" ,      "PCA" = "green"),
        drop = FALSE
      ) +
      guides(color = "none")+
      theme(plot.title = element_text(size = 16))
    
  }
  
  # Generate plots
  plots_list <- lapply(1:n,plot_irf)
  grid_plots <- do.call("grid.arrange", c(plots_list, nrow = 2))
  
  legend_plot <- plot_grid(grid_plots, custom_legend , ncol = 1, rel_heights = c(1, 0.1))
  return(legend_plot)
  
}
