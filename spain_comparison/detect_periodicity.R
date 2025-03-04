
series <- IV_data_detrended$abs_gas_shock


detect_periodicity <- function(series, threshold_acf = 0.5, threshold_spectrum = 0.05) {
  # Vérifier si la série est suffisante (au moins 24 mois pour éviter les faux positifs)
  if (length(series) < 24) return(FALSE)
  
  # 1. Test d'Auto-corrélation
  acf_values <- acf(series, plot = FALSE)$acf
  acf_12 <- acf_values[13]  # Récupère l'autocorrélation au lag 12
  
  # 2. Analyse Spectrale (Périodogramme)
  spec <- spectrum(series, plot = FALSE)
  freq_annual <- 1/12
  peak_freq <- spec$freq[which.max(spec$spec)]  # Fréquence dominante
  
  # Vérification des critères
  is_annual <- (acf_12 > threshold_acf) & (abs(peak_freq - freq_annual) < threshold_spectrum)
  
  return(is_annual)
}


periodicity_test_col <- setdiff(colnames(IV_data),
                                c("month" , "year" ,                     
                                  "datetime", "plot_date",
                                  "X" ,"Date.x" , "Date.y" ))


remove_trend <- function(series) {
  time_index <- 1:length(series)  # Créer un vecteur de temps
  model <- lm(series ~ time_index, na.action = na.omit)  # Ajuster une régression linéaire
  detrended_series <- resid(model)  # Récupérer les résidus (série sans tendance)
  return(detrended_series)
}

IV_data_detrended <- IV_data %>%
  dplyr::select(periodicity_test_col) %>%
  mutate(across(everything(), remove_trend))



annual_series <- IV_data %>%
  dplyr::select(periodicity_test_col) %>%
  mutate(across(everything(), remove_trend)) %>%  # Suppression de la tendance
  summarise(across(everything(), ~ detect_periodicity(.))) %>%
  pivot_longer(everything(), names_to = "series", values_to = "is_annual") %>%
  filter(is_annual == TRUE)


print(annual_series)








detect_general_periodicity <- function(series) {
  spec <- spectrum(series, plot = TRUE)  # Compute the periodogram
  dominant_freq <- spec$freq[which.max(spec$spec)]  # Get the most dominant frequency
  estimated_period <- 1 / dominant_freq  # Convert frequency to period
  return(estimated_period)
}

# Apply to your data
dominant_periodicity <- IV_data_detrended %>%
  dplyr::select(periodicity_test_col) %>%
  dplyr::select(-c("abs_gas_shock", "rel_gas_shock")) %>%
  summarise(across(everything(), detect_general_periodicity)) %>% 
  pivot_longer(everything(), names_to = "series", values_to = "dominant_period") %>%
  filter(as.numeric(dominant_period) <= 13)

# if the dominant periodicity is infra-annual
#select as needed to be deseasonnalized




ggplot(IV_data_detrended) +
  geom_line(aes( x= IV_data$datetime, y =IV_data$NE_Index - mean(IV_data$NE_Index) , color = "Original")) +
  geom_line(aes( x= IV_data$datetime, y =IV_data_detrended$NE_Index , color = "Detrended")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()


ggplot(IV_data_detrended) +
  geom_line(aes( x= IV_data$datetime, y =IV_data$abs_shock - mean(IV_data$abs_shock) , color = "Original")) +
  geom_line(aes( x= IV_data$datetime, y =IV_data_detrended$abs_shock , color = "Detrended")) +
  labs(title = "Non-Energy Index - Original and Seasonally Adjusted",
       x = "Date", y = "Index") +
  scale_color_manual(values = c("Original" = "blue", "Detrended" = "red")) +
  theme_minimal()



