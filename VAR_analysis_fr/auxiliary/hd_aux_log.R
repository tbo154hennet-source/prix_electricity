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
    geom_bar(data= long_data  %>% filter(time < as.Date("2024-09-01")), aes(x = time, y = value, fill = category),
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
  # Reshaper les données en format long pour ggplot2
  long_data <- pivot_longer(decomp_tot_data, 
                            cols = -c(time, index),  names_to = "category",
                            values_to = "value")
  # I find a way to calculate trend only when 
  if (length(colnames(decomp_tot_data)[-1]) == length(colors_shock)){
    trend_data <- long_data %>%
      filter(category == "$HICP$") %>%
      mutate(
        category = "Trend",
        value = mean_vector[series]
      )
    
    # Combiner avec les autres catégories
    long_data <- bind_rows(long_data, trend_data)
  }
  
  # Exemple : calcul de la moyenne glissante sur les 12 dernières valeurs
  long_data <- long_data %>%
    group_by(category) %>%
    arrange(category, time) %>%  # assure-toi d'avoir une variable temporelle (ex: `time`)
    mutate(rolling_avg = slide_dbl(value, sum, .before = 11, .complete = TRUE))
  
  
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
  
  # comparison <- lag(as.vector(vars_demeaned[5:96,series]), 20)
  # comparison[1:20] <- rep(vars_demeaned[4,series], 10)
  
  #y_diff <- vars_demeaned[5:nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  y_diff <- vars[(p+1):nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  
  data <- data.frame(
    x = as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    y = cumsum(y_diff),
    #ytest = apply(historical_decomp_matrix[,series,, drop = FALSE], 1, sum) 
    ytest = apply(historical_decomp_matrix[,series,, drop = FALSE], 1, sum) + mean_vector[series]
    #ytrend = + c(1:T)*mean_vector[series]
  )%>%  # assure-toi d'avoir une variable temporelle (ex: `time`)
    mutate(rolling_avg_contrib = slide_dbl(ytest, sum, .before = 11, .complete = TRUE))
  
  my_plot <- my_plot+
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


plot_hd_inflation_glissante_12_compare <- function(series){
  
  
  decomp_tot_data <- data.frame(
    time= as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    historical_decomp_matrix1[,series,])
  # Plot the decomposition
  
  
  colnames(decomp_tot_data)[-1] <- names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  # Reshaper les données en format long pour ggplot2
  long_data_chol <- pivot_longer(decomp_tot_data, 
                                 cols = -c(time, index),  names_to = "category",
                                 values_to = "value")
  
  # Exemple : calcul de la moyenne glissante sur les 12 dernières valeurs
  long_data_chol <- long_data_chol %>%
    group_by(category) %>%
    arrange(category, time) %>%  # assure-toi d'avoir une variable temporelle (ex: `time`)
    mutate(rolling_avg = slide_dbl(value, sum, .before = 11, .complete = TRUE),
           Method = "Cholesky") %>% 
    filter(category == "Supply" | category == "Demand")
  
  
  # S'assurer que 'category' a l'ordre des niveaux correspondant à 'rev(colors_shock)'
  #long_data$category <- factor(long_data$category, levels = names)
  long_data_chol$category <- factor(long_data_chol$category, levels = rev(names))
  
  
  
  decomp_tot_data <- data.frame(
    time=as.Date(dates_formatted[(p+1):length(dates_formatted)]),
    historical_decomp_matrix2[,series,])
  # Plot the decomposition
  colnames(decomp_tot_data)[-1] <-names
  decomp_tot_data <- decomp_tot_data %>%
    mutate(index = row_number())
  # Reshaper les données en format long pour ggplot2
  long_data_iv <- pivot_longer(decomp_tot_data, 
                               cols = -c(time, index),  names_to = "category",
                               values_to = "value")
  
  
  # Exemple : calcul de la moyenne glissante sur les 12 dernières valeurs
  long_data_iv <- long_data_iv %>%
    group_by(category) %>%
    arrange(category, time) %>%  # assure-toi d'avoir une variable temporelle (ex: `time`)
    mutate(rolling_avg = slide_dbl(value, sum, .before = 11, .complete = TRUE),
           Method = "IV")%>% 
    filter(category == "Supply" | category == "Demand") 
  
  
  # S'assurer que 'category' a l'ordre des niveaux correspondant à 'rev(colors_shock)'
  #long_data$category <- factor(long_data$category, levels = names)
  long_data_iv$category <- factor(long_data_iv$category, levels = rev(names))
  
  
  
  # 2. Plot separately but still trigger alpha legend
  ne_hd_compare <- ggplot() +
    # Cholesky layer (alpha = 1)
    geom_bar(data = long_data_chol, aes(x = time, y = rolling_avg, fill = category, alpha = Method),
             stat = "identity", position = "stack") +
    
    # IV layer (alpha = 0.4)
    geom_bar(data = long_data_iv, aes(x = time, y = rolling_avg, fill = category, alpha = Method),
             stat = "identity", position = "stack") +
    
    # 3. Manual alpha scale
    scale_alpha_manual(
      values = c("Cholesky" = 0.4, "IV" = 1),
      name = "Identification"
    ) +
    
    # 4. Manual fill scale with LaTeX labels
    scale_fill_manual(
      values = c( "red", "green"),
      labels = c( "Supply", "Demand"),
      name = "Shock"
    ) +
    
    guides(
      alpha = guide_legend(order = 1),
      fill = guide_legend(order = 2)
    )+
    labs(title = "Comparing HD for HICP",
         x = "Time",
         y = "Contribution") +
    theme(legend.text = element_text(size = 15),      # taille du texte
          legend.title = element_text(size = 16),     # taille du titre
          legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
          axis.title.x = element_text(size = 14),   # taille du titre axe x
          axis.title.y = element_text(size = 14),   # taille du titre axe y
          axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
          axis.text.y  = element_text(size = 12)    # taille des ticks y
    )
  return(ne_hd_compare)
}
