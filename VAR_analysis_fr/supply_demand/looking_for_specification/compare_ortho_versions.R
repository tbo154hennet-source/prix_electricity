
plot_irf <- function(i=1) {
  #trick I just put 0.5 twice
  
  # main_chol <- lambda_0_chol[,i,index]
  # #Normalize IRF
  # main_chol <- as.numeric(main_chol/lambda_0_chol[1,index,index])
  
  #dim(lambda_0_median)
  time_horizon <- 1:(irhor+1)
  
  band_lower_68 <- lower_68[, i]
  band_upper_68 <- upper_68[, i]
  band_lower_90 <- lower_90 [, i]
  band_upper_90 <- upper_90[, i]
  main_IV_irf <- irf_data_cum %>% 
    filter(confi_level == 0.68 & type        == "point") %>%
    dplyr::select(variable, horizon, value) %>% 
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>% 
    dplyr::select(-horizon)
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
      #title = TeX(variable_names[i]),
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

VAR_supply_test <-SVARIV(ydata = x, z = IV_data_log_esp$delta_s_tot,p,
                    confidence,NWlags,
                    norm,scale,irhor,instrument_name="supply")
VAR_demand_test <-SVARIV(ydata = x, z = IV_data_log_esp$delta_d_tot,p,
                    confidence,NWlags,
                    norm,scale,irhor,instrument_name="supply")



irf_data <- VAR_supply_test$irfs
irf_data_cum <- irf_data%>%
  filter(confi_type == "msw" )#| confi_type == "msw_cum")

lower_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
upper_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))

lower_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
upper_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))

plots_list <- lapply(1:n, plot_irf)
grid_plots <- do.call("grid.arrange", c(plots_list, nrow = 2))
grid_plots 


irf_data <- VAR_demand_test$irfs
irf_data_cum <- irf_data%>%
  filter(confi_type == "msw" )#| confi_type == "msw_cum")

lower_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
upper_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))

lower_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
upper_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))

plots_list <- lapply(1:n, plot_irf)
grid_plots <- do.call("grid.arrange", c(plots_list, nrow = 2))









IV_data_log_esp$delta_s_orth_bis <- resid(lm(delta_s_tot ~delta_d_tot, data = IV_data_log_esp))
IV_data_log_esp$delta_d_orth_bis <- resid(lm(delta_d_tot ~delta_s_tot, data = IV_data_log_esp))

IV_data_log_esp %>% 
  dplyr::select(delta_d_orth_bis, delta_d_orth,
               delta_s_orth_bis, delta_s_orth
               ) %>% 
  cor()

VAR_supply_test <-SVARIV(ydata = x, z = IV_data_log_esp$delta_s_orth_bis,p,
                         confidence,NWlags,
                         norm,scale,irhor,instrument_name="supply")
VAR_demand_test <-SVARIV(ydata = x, z = IV_data_log_esp$delta_d_orth_bis,p,
                         confidence,NWlags,
                         norm,scale,irhor,instrument_name="supply")





X <- scale(cbind(s = IV_data_log_esp$delta_s_tot , d = IV_data_log_esp$delta_d_tot), center = TRUE, scale = FALSE)

pca <- prcomp(X)

# Valeurs propres (variances) des composantes principales
eigenvalues <- pca$sdev^2

# Variance expliquée par chaque composante
variance_explained <- eigenvalues / sum(eigenvalues)
variance_explained


# Reconstituer s et d à partir des composantes principales
PC1 <- pca$x[, 1]
PC2 <- pca$x[, 2]
loadings <- pca$rotation

# Reconstruction
IV_data_log_esp$delta_supply <- pca$x[, 1]
IV_data_log_esp$delta_demand <- pca$x[, 2]



VAR_supply_pc <-SVARIV(ydata = x, z = IV_data_log_esp$delta_supply,p,
                         confidence,NWlags,
                         norm,scale,irhor,instrument_name="supply")
VAR_demand_pc <-SVARIV(ydata = x, z = IV_data_log_esp$delta_demand,p,
                         confidence,NWlags,
                         norm,scale,irhor,instrument_name="supply")




