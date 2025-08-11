set.seed(0)

# Paramètres du modèle
K <- 5    # Nombre d'instruments
N <- 2    # Variables endogènes
Nx <- 2   # Variables exogènes

beta <- rep(1, N)  # Paramètres structurels

DL <- 0.08 * diag(N)
QL <- diag(N)

L0 <- qr.Q(qr(matrix(rnorm(K * K), K, K)))[, 1:N]
Pi <- sqrt(K) * QL %*% sqrt(DL) %*% t(L0)  # Paramètres de première étape
Pi <- t(Pi)

By <- rnorm(Nx + 1)         # Coefs sur X dans l'équation structurelle
BY <- matrix(rnorm((Nx + 1) * N), Nx + 1, N)  # Coefs sur X dans la première étape

# Générer les données
T <- 200
u <- rnorm(T)
v <- matrix(rnorm(T * N), T, N)

CONST <- rep(1, T)
X <- cbind(matrix(rnorm(T * Nx), T, Nx), CONST)
Z <- matrix(rnorm(T * K), T, K)
Y <- Z %*% Pi + X %*% BY + v
y <- Y %*% beta + X %*% By + u


result <- gweakivtest(y, Y, X, Z)
print(result)


result_1 <- gweakivtest(y, Y, X, Z, cov_type = "NW")#, target = 1)
print(result_1)





headers_proxies_esp = c(
  "temp_excess_instrument",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  #"PC1_supply",
  #"diff_predicted_unavailabilities",
  #"diff_cbf",
  #"cap_gap_diff",
  "renewable_day_ahead_forecast_esp_log_diff"
)

headers_proxies_esp = c(
  "delta_s_total",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  #"PC1_supply",
  #"diff_predicted_unavailabilities",
  #"diff_cbf",
  #"cap_gap_diff",
  "delta_d_total"
)
p=4
Z_test <- IV_data_log_diff_esp[c(-1:-p), c(headers_proxies_esp)]

Y_test <- res[, 1:k]
y_test <- res[, (k+1)]
#Z_test <- proxies

X_test <- res[, (k+2):n]

result <- gweakivtest(y = y_test, Y = Y_test, X = X_test, Z = Z_test)
print(result)


Y_test <- res[, 1:k]
y_test <- res[, (k+2)]
#Z_test <- proxies

X_test <- res[, c((k+1),n)]

result <- gweakivtest(y = y_test, Y = Y_test, X = X_test, Z = Z_test)
print(result)