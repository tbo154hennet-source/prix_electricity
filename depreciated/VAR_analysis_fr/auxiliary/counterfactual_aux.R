
## ---- 1. Construire F et G à partir des coefficients VAR (AL) ----
build_companion_from_AL <- function(AL, p) {
  # AL est la concaténation des coefficients (n x n*p)
  n <- nrow(AL)
  # F : (np x np)
  F <- matrix(0, n*p, n*p)
  F[1:n, 1:(n*p)] <- AL
  if (p > 1) {
    F[(n+1):(n*p), 1:(n*(p-1))] <- diag(n*(p-1))
  }
  G <- rbind(diag(n), matrix(0, n*(p-1), n))
  list(F = F, G = G)
}




## ---- 2. Récupérer un choc unique (policy shock) ----
recover_single_shock_IV <- function(y_obs, y_lags, AL, B, p) {
  # y_obs : vecteur (n x 1), l'observation à T+1
  # y_lags : matrice (n x p), les p retards observés (col1 = y_T, col2 = y_{T-1}, etc.)
  n <- nrow(B)
  
  # prédiction sans choc
  y_pred <- t(AL) %*% as.vector(y_lags)
  
  delta <- as.numeric(y_obs - y_pred)
  
  # résolution B * eps = delta
  
  #eps <- ginv(B) %*% delta
  
  #closest multiple of B that looks like delta
  #eps_policy <- as.numeric(crossprod(B, delta) / crossprod(B, B))
  eps_policy <- delta/B[1]
    
  delta_hat <- B * eps_policy
  as.numeric(eps_policy)
}




## ---- Récupérer chocs sur H périodes, uniquement le premier choc structurel ----
recover_shocks_horizon_IV <- function(y_obs_seq, y_init_lags, AL, B, p) {
  # y_obs_seq : matrice (n x H), observations aux temps T+1 ... T+H
  # y_init_lags : matrice (n x p), retards au temps T
  n <- nrow(B); H <- ncol(y_obs_seq)
  
  comp <- build_companion_from_AL(AL, p)
  F <- comp$F; G <- comp$G
  
  # état initial X_T
  X0 <- as.numeric(c(y_init_lags[,1], if (p > 1) as.numeric(y_init_lags[,2:p]) else numeric(0)))
  
  # Matrice M (nH x H)
  M <- matrix(0, n*H, H)
  Fpow <- diag(n*p)
  for (h in 1:H) {
    effect <- (Fpow %*% G %*% B)[1:n, , drop=FALSE]
    M[((h-1)*n+1):(h*n), h] <- effect[,1]  # première colonne = policy shock
    Fpow <- F %*% Fpow
  }
  
  # prédictions sans choc
  preds <- matrix(0, n, H)
  Xcur <- X0
  for (h in 1:H) {
    ypred <- (F %*% Xcur)[1:n]
    preds[,h] <- ypred
    Xcur <- F %*% Xcur
  }
  
  # écart observation - prédiction
  RHS <- as.numeric(y_obs_seq - preds)
  
  # Résolution en moindres carrés
  epsvec <- qr.solve(M, RHS)
  as.numeric(epsvec)  # vecteur longueur H des policy shocks
}







simulate_var_with_shocks <- function(y_init_lags, AL_fixed, B, eps_seq = NULL, nhor = 20) {
  comp <- build_companion(AL_fixed, p)
  F <- comp$F; G <- comp$G
  n <- nrow(B)
  # état initial X_T
  Xcur <- as.numeric(c(y_init_lags[,1], if (p>1) as.numeric(y_init_lags[,2:p]) else numeric(0)))
  out <- matrix(NA, n, nhor)
  for (t in 1:nhor) {
    eps_t <- if (!is.null(eps_seq) && t <= length(eps_seq)) {
      e <- numeric(n); e[1] <- eps_seq[t]; e   # si tu n'imposes que 1ère composante
    } else {
      e <- numeric(n)  # résidu nul -> pas de choc
    }
    u_t <- B %*% e
    Xcur <- F %*% Xcur + G %*% u_t
    out[, t] <- Xcur[1:n]
  }
  out
}



simulate_var_with_policy <- function(AL, theta_0, y_init, residuals_VAR, policy_shock, p, h) {
  
  n <- ncol(y_init)  # nombre de variables
  y_sim <- matrix(NA, nrow = h + p, ncol = n)
  y_sim[1:p, ] <- y_init  # initialiser les p premières observations
  
  # transformer AL en matrice si nécessaire
  AL_mat <- do.call(cbind, AL)
  
  for(t in (p + 1):(h + p)) {
    # construire le vecteur des lags
    y_lags <- as.vector(t(y_sim[(t-1):(t-p), ]))
    
    # prédiction VAR
    y_pred <- t(AL_mat) %*% y_lags
    
    # ajouter le résidu original si disponible
    if(t - p <= nrow(residuals_VAR)) {
      eps_t <- residuals_VAR[t - p, ]
    } else {
      eps_t <- rep(0, n)
    }
    
    # ajouter le choc de politique à la période souhaitée
    if(t == p + 1) {
      # choc injecté seulement à la première période
      eps_t <- eps_t + policy_shock
    }
    
    # appliquer B * eps
    y_sim[t, ] <- y_pred + theta_0 %*% eps_t
  }
  
  return(y_sim[(p+1):(h+p), ])  # ne garder que la simulation
}

# Exemple : injecter eps_Tplus1 (reconstruit) puis zeros
eps_seq <- c(eps_Tplus1, rep(0, 19))  # eps_Tplus1 obtenu par récupération précédemment
sim_path <- simulate_var_with_shocks(y_init_lags, AL_fixed, theta_0_log_diff, eps_seq, nhor = 20)