# Modèle VAR avec p = 4
model <- vars::VAR(x, p, type = "none")

# Extraire les coefficients
coefficients <- coef(model)
Phi <- data.frame(
  coefficients[[1]][,1],  # Coefficients pour y_{t-1}
  coefficients[[2]][,1],  # Coefficients pour y_{t-2}
  coefficients[[3]][,1],  # Coefficients pour y_{t-3}
  coefficients[[4]][,1]   # Coefficients pour y_{t-4}
)
colnames(Phi) <- colnames(x)

# Extraire les résidus
residuals <- residuals(model)

# Initialiser la série reconstruite
T <- nrow(x)
n <- ncol(x)
reconstructed_series <- matrix(NA, nrow =T, ncol = n)
colnames(reconstructed_series) <- colnames(x)

# Copiez les 4 premières observations (valeurs initiales)
reconstructed_series[1:4, ] <- x[1:4, ]

# Reconstruction de y_t pour t = 5 à T
for (t in 5:T) {
  #t=5
  # Contribution de la constante
  y_t <- rep(0,4)
  
  # Contribution des lags
  for (lag in 1:4) {
    # Convertir la ligne en vecteur colonne (si nécessaire)
    lagged_values <- as.vector(reconstructed_series[t - lag, ])
    contrib_lag <- as.vector( t(Phi[(1+ 4*(lag-1)):((4*(lag-1))+p),]) %*% reconstructed_series[t - lag, ] )
    #print(contrib_lag)
    # Calculer la contribution pour ce lag
    y_t <- y_t + contrib_lag
  }
  
  # Ajouter les résidus
  reconstructed_series[t, ] <- y_t + residuals[(t-4), ]
}

# Convertir en data.frame si besoin
reconstructed_series <- as.data.frame(reconstructed_series)

# Vérifier les différences

difference <- as.numeric(x) - reconstructed_series

# Vérification de l'erreur globale
max_difference <- max(abs(difference), na.rm = TRUE)
print(max_difference)  # Devrait être proche de 0




reconstructed_series_bis <- forecasted_data
difference <- as.numeric(x) - reconstructed_series_bis

# Vérification de l'erreur globale
max_difference <- max(abs(difference), na.rm = TRUE)
print(max_difference)  # 

# Initialiser 'errors' avec les mêmes dimensions que 'res'
zero_errors <- matrix(0, nrow = dim(res)[1], ncol = dim(res)[2])

reconstruct_derive_without_errors <- iterate_var_bis(initial_obs = x[1:4, ],
                                                     errors = zero_errors,
                                                     var_model = model)




