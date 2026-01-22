plot_hd_meaned <- function(series){
  
  
  decomp_tot_data <- data.frame(
    time= as.Date(dates_formatted),
    historical_decomp_matrix[,series,])
  # Plot the decomposition
  colnames(decomp_tot_data)[-1] <- names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  # Reshaper les données en format long pour ggplot2
  long_data <- pivot_longer(decomp_tot_data, 
                            cols = -c(time, index),  names_to = "category",
                            values_to = "value")
  
  
  trend_data <- long_data %>%
    filter(category == "$HICP$") %>%
    mutate(
      category = "trend",
      value = index * mean_vector[series]
    )
  
  # Combiner avec les autres catégories
  long_data <- bind_rows(long_data, trend_data)
  
  # S'assurer que 'category' a l'ordre des niveaux correspondant à 'colors_shock'
  #long_data$category <- factor(long_data$category, levels = names)
  long_data$category <- factor(long_data$category, levels = rev(names))
  
  
  
  # Tracer des barres empilées avec ggplot2
  my_plot <- ggplot()+
    geom_bar(data= long_data, aes(x = time, y = value, fill = category),
             stat = "identity", position = "stack", alpha = alpha_chosen) +  # Barres empilées
    scale_fill_manual(
      values = rev(colors_shock)[(length(colors_shock)-(dim(decomp_tot_data)[2]-1)+1):length(colors_shock)],
      labels = TeX(names))+
    #scale_fill_manual(values = c("blue", "red", "green"), labels = c("Supply", "Demand", "Other")) +
    theme_minimal()
  
  # comparison <- lag(as.vector(vars_demeaned[5:96,series]), 20)
  # comparison[1:20] <- rep(vars_demeaned[4,series], 10)
  
  #y_diff <- vars_demeaned[5:nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  y_diff <- vars[(p+1):nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  
  data <- data.frame(
    x = as.Date(dates_formatted),
    y = cumsum(y_diff),
    #ytest = apply(historical_decomp_matrix[,series,, drop = FALSE], 1, sum) 
    ytest = apply(historical_decomp_matrix[,series,, drop = FALSE], 1, sum) #, # Add mean vector to the test series,
    #ytrend = + c(1:T)*mean_vector[series]
    
  )
  
  my_plot <- my_plot + 
    geom_line(data, mapping = aes(x = x, y = y), linetype = "dashed")  # Add dashed line+
  #geom_line(data, mapping = aes(x = x, y = ytest), linetype = "dashed", color='grey') +
  #geom_line(data, mapping = aes(x = x, y = ytrend), linetype = "dashed", color='darkred') 
  return(my_plot)
}



plot_hd_demeaned <- function(series){
  
  
  decomp_tot_data <- data.frame(
    time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    historical_decomp_matrix[,series,])
  # Plot the decomposition
  colnames(decomp_tot_data)[-1] <- names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  # Reshaper les données en format long pour ggplot2
  long_data <- pivot_longer(decomp_tot_data, 
                            cols = -c(time, index),  names_to = "category",
                            values_to = "value")
  
  
  
  #transform the HICP category
  # long_data <- long_data %>% 
  #   mutate(value = ifelse(category == "$HICP$", value + index*mean_vector[series], value))
  # 
  # S'assurer que 'category' a l'ordre des niveaux correspondant à 'colors_shock'
  #long_data$category <- factor(long_data$category, levels = names)
  long_data$category <- factor(long_data$category, levels = rev(names))
  
  
  
  # Tracer des barres empilées avec ggplot2
  my_plot <- ggplot()+
    geom_bar(data= long_data  %>% filter(time < as.Date("2023-01-01")), aes(x = time, y = value, fill = category),
             stat = "identity", position = "stack", alpha = alpha_chosen) +  # Barres empilées
    scale_fill_manual(
      values = rev(colors_shock),#[(length(colors_shock)-(dim(decomp_tot_data)[2]-1)+1):length(colors_shock)],
      labels = TeX(names)
      )+
    #scale_fill_manual(values = c("blue", "red", "green"), labels = c("Supply", "Demand", "Other")) +
    theme_minimal()
  
  # comparison <- lag(as.vector(vars_demeaned[5:96,series]), 20)
  # comparison[1:20] <- rep(vars_demeaned[4,series], 10)
  
  #y_diff <- vars_demeaned[5:nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  y_diff <- vars[(p+1):nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  data <- data.frame(
    x = as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    y = cumsum(y_diff),
    #ytest = apply(historical_decomp_matrix[,series,, drop = FALSE], 1, sum) 
    ytest = apply(historical_decomp_matrix[,series,, drop = FALSE], 1, sum) #, # Add mean vector to the test series,
    # ytrend = + c(1:T)*mean_vector[series]
    
  )
  
  my_plot <- my_plot + 
    #geom_line(data, mapping = aes(x = x, y = y), linetype = "dashed")  # Add dashed line+
    geom_line(data[1:(nrow(data)-p),], mapping = aes(x = x, y = ytest), linetype = "dashed", color='grey')+
    theme(legend.text = element_text(size = 15),      # taille du texte
          legend.title = element_text(size = 16),     # taille du titre
          legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
          axis.title.x = element_text(size = 14),   # taille du titre axe x
          axis.title.y = element_text(size = 14),   # taille du titre axe y
          axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
          axis.text.y  = element_text(size = 12)    # taille des ticks y
    )
  #geom_line(data, mapping = aes(x = x, y = ytrend), linetype = "dashed", color='darkred') 
  return(my_plot)
}


plot_hd_inflation_glissante_12 <- function(series){
  
  
  decomp_tot_data <- data.frame(
    time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    historical_decomp_matrix[,series,])
  # Plot the decomposition
  colnames(decomp_tot_data)[-1] <- names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  
  k <- 12  # horizon 12 mois
  
  pi_ann_series <- (IV_data_log_esp$HICP_log[(k+1):(T+p)] - IV_data_log_esp$HICP_log[1:((T+p)-k)]) 
  
  # Colonnes de chocs/contributions (tout sauf time et index)
  comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))
  
  decomp_infl_12m <- decomp_tot_data %>%
    arrange(time) %>%
    # Différence à 12 mois de chaque contribution
    mutate(across(all_of(comp_cols),
                  ~ . - dplyr::lag(., k),
                  .names = "{.col}")) %>%
    # Supprimer les 12 premières lignes (NA)
    filter(!is.na(.data[[comp_cols[1]]])) 
  # Reshaper les données en format long pour ggplot2
  long_data <- pivot_longer(decomp_infl_12m , 
                            cols = -c(time, index),  names_to = "category",
                            values_to = "value")
  
  # S'assurer que 'category' a l'ordre des niveaux correspondant à 'rev(colors_shock)'
  #long_data$category <- factor(long_data$category, levels = names)
  long_data$category <- factor(long_data$category, levels = rev(names))
  
  
  
  # Tracer des barres empilées avec ggplot2
  my_plot <- ggplot()+
    geom_bar(data= long_data %>% filter(time < as.Date("2024-09-01")), aes(x = time, y =value, fill = category),
             stat = "identity", position = "stack", alpha = alpha_chosen) +  # Barres empilées
    scale_fill_manual(
      values = rev(colors_shock)[(length(colors_shock)-(dim(decomp_tot_data)[2]-1)+1):length(colors_shock)],
      labels = TeX(names))+
    #scale_fill_manual(values = c("blue", "red", "green"), labels = c("Supply", "Demand", "Other")) +
    theme_minimal()
  
  
  my_plot <- my_plot+
    # geom_line(aes(x = as.Date(dates_formatted[-(1:p)]) , y =  pi_ann_series))+
    theme(legend.text = element_text(size = 15),      # taille du texte
          legend.title = element_text(size = 16),     # taille du titre
          legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
          axis.title.x = element_text(size = 14),   # taille du titre axe x
          axis.title.y = element_text(size = 14),   # taille du titre axe y
          axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
          axis.text.y  = element_text(size = 12)    # taille des ticks y
    )
  #geom_line(data, mapping = aes(x = x, y = y), linetype = "dashed")  # Add dashed line+
  #geom_line(data[(p+1):(nrow(data)-p),], mapping = aes(x = x, y = rolling_avg_contrib), linetype = "dashed", color='grey')
  #geom_line(data, mapping = aes(x = x, y = ytrend), linetype = "dashed", color='darkred') 
  return(my_plot)
}


plot_hd_inflation_glissante_12_compare <- function(series_1=1, series_2=5){
  
  

  decomp_tot_data <- data.frame(
    time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    historical_decomp_matrix[,series_2,])
  # Plot the decomposition
  colnames(decomp_tot_data)[-1] <- names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  
  k <- 12  # horizon 12 mois
  
  pi_ann_series <- (IV_data_log_esp$HICP_log[(k+1):(T+p)] - IV_data_log_esp$HICP_log[1:((T+p)-k)]) 
  
  # Colonnes de chocs/contributions (tout sauf time et index)
  comp_cols <- setdiff(names(decomp_tot_data), c("time", "index"))
  
  decomp_infl_12m <- decomp_tot_data %>%
    arrange(time) %>%
    # Différence à 12 mois de chaque contribution
    mutate(across(all_of(comp_cols),
                  ~ . - dplyr::lag(., k),
                  .names = "{.col}")) %>%
    # Supprimer les 12 premières lignes (NA)
    filter(!is.na(.data[[comp_cols[1]]])) 
  # Reshaper les données en format long pour ggplot2
  long_data <- pivot_longer(decomp_infl_12m , 
                            cols = -c(time, index),  names_to = "category",
                            values_to = "value")
  
  # S'assurer que 'category' a l'ordre des niveaux correspondant à 'rev(colors_shock)'
  #long_data$category <- factor(long_data$category, levels = names)
  long_data$category <- factor(long_data$category, levels = rev(names))
  
  
  
  decomp_tot_data <- data.frame(
    time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    historical_decomp_matrix[,series_1,])
  # Plot the decomposition
  colnames(decomp_tot_data)[-1] <- names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  # Reshaper les données en format long pour ggplot2
  long_data_price <- pivot_longer(decomp_tot_data, 
                            cols = -c(time, index),  names_to = "category",
                            values_to = "value")
  

  long_data_price$category <- factor(long_data_price$category, levels = rev(names))
  
  long_data_tot <- rbind(long_data %>% 
                           mutate(variable = "inflation"),
                         long_data_price %>% 
                           mutate(variable = "price"))%>%
    mutate(fill_key = paste(category, variable))
  
  fill_values <- c(
    "Supply price"      = "darkblue",
    "Demand price"     = "blue",
    "Supply inflation" = "darkred",
    "Demand inflation" = "red"
  )
  fill_labels <- c(
    "Supply price"      = expression("Supply " * P[elec]),
    "Demand price"     = expression("Demand " * P[elec]),
    "Supply inflation" = expression("Supply " * pi),
    "Demand inflation" = expression("Demand " * pi)
  )
  long_data_tot$fill_key
  my_plot <- ggplot() +
    geom_bar(
      data = long_data_tot %>% filter(time < as.Date("2024-09-01")),
      aes(x = time, y = value, fill = fill_key),
      stat = "identity",
      position = "stack",
      alpha = alpha_chosen
    ) +
    scale_fill_manual(
      values = fill_values,
      labels = fill_labels,
      name   = NULL
    ) +
    theme_minimal()
  
  my_plot <- my_plot +
    theme(
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 16),
      legend.key.size = unit(1.5, "cm"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y  = element_text(size = 12)
    )
  
  
  
  return(my_plot)
}
