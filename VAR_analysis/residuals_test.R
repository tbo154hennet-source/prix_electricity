test2 <- rbind(vars[1:p,],
               iterate_var_bis(vars[1:p,], res, vl))
test <- iterate_var_ter(vars[c(1:p),], res, vl)

library(vars)
# Étape 2 : Extraction des valeurs observées et des prédictions
vars <- as.matrix(Canada)  # Variables dépendantes
class(vars)
fitted_values <- fitted(vl)  # Valeurs prédites
residuals_var <- residuals(vl)  # Résidus

# Étape 3 : Calcul des variances
var_total <- apply(vars, 2, var)  # Variance totale par variable
var_explained <- apply(fitted_values, 2, var)  # Variance expliquée par variable
var_residuals <- apply(residuals_var, 2, var)  # Variance résiduelle par variable

# Étape 4 : Proportions de variance
r_squared <- var_explained / var_total  # Part de variance expliquée
residual_ratio <- var_residuals / var_total  # Part de variance résiduelle

# Résultats
results <- data.frame(
  Variable = colnames(vars),
  Variance_Totale = var_total,
  Variance_Expliquée = var_explained,
  Variance_Résiduelle = var_residuals,
  R2 = r_squared,
  Proportion_Résiduelle = residual_ratio
)

print(results)

# Conversion des résidus en format long
residuals_long <- melt(as.data.frame(residuals_var), variable.name = "Variable", value.name = "Résidu")

# Graphique avec ggplot2
ggplot(residuals_long, aes(x = Résidu)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "white") +
  geom_density(color = "red", size = 1) +
  facet_wrap(~ Variable, scales = "free") +  # Une facette par variable
  theme_minimal() +
  labs(title = "Distribution des Résidus", x = "Valeur des Résidus", y = "Densité")










