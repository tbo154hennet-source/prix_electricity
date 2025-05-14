sign_restrict_estimation <- function(x, y, p, proxies, irhor, controls = NULL) {
  # x: endogenous variables
  # y: dependent variables
  # p: lag order
  # proxies: instruments or proxies
  # irhor: impulse response horizon (unused here, but kept for potential future use)
  # controls: optional exogenous variables
  
  # proxies <- proxies_boot
  # x <- xnew
  # y <- ynew
  
  # Fit the VAR model
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  
  k  = ncol(proxies)
  n = ncol(y)
  coef = coef(vl)
  bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
  res = residuals(vl)
  
  T = nrow(y)
  Sigma_eta <- cov(res) 
  
  #suivre identification simplifiee de olea and al
  gamma <- t( t(proxies[c(-1:-p),]) %*% res)
  
  
  #calculate sigma_aa
  Sigma_a <- gamma %*%  ginv(t(gamma) %*% ginv(Sigma_eta) %*% gamma) %*% t(gamma)
  
  
  # Fonction pour obtenir c à partir d'un angle theta
  c_0 = c(0,1)
  # Fonction pour obtenir c à partir d'un angle theta
  c_from_angle <- function(lambda, vector) {
    rotation_matrix <- matrix(c(cos(lambda), -sin(lambda), sin(lambda), cos(lambda)), nrow = 2)
    return(rotation_matrix %*% vector)
  }
  # Exploration de l'intervalle [-pi, pi]
  angles <- seq(-pi, pi, length.out = 100)
  c_candidates <- sapply(angles, function(lambda) c_from_angle(lambda, c_0))
  
  
  
  length(c_candidates)
  #check of cs
  for (c in c_candidates){
    print(t(c) %*% theta_01[1:2,1])
  }
  
  
  #generate random c vector
  c <- c_candidates[,60]
  
  matrix_1c <- rbind( cbind(c(1, 0), c) , zero_matrix)
  theta_01 <- gamma %*% solve(t(matrix_1c) %*% gamma) %*% c(1, 0)
  
  theta_02 <- gamma %*% solve(t(cbind(c(1, 0), solve(Sigma_eta) %*% theta_01)) %*% gamma) %*% c(1, 0)
  theta_02 <- theta_02/theta_02[2]
  
  #
  
  
  t(c) %*% theta_01[1:2,1]
  #il faut c'theta_02 != 0
  #check de fin
  if (t(c) %*% theta_02[1:2,1] > eps){
    #reject
    
  }
  
  
  
  
  #vérifier si conforme aux sign restrictions
  #         supply/prix, demand
  # supply/prix (  +,  +)
  # demand      (  -,  +)
  
  if (theta_01[2]>0){
    #reject
  }
  if (theta_02[1]<0){
    #reject
  }
  
  
  
  theta_0 <- cbind(theta_01, theta_02)
  
  
  
  phi <- matrix(c(1,0, -theta_0[1,2][[1]], 1), nrow = 2, ncol = 2) %*% t(matrix_12)  %*% gamma
  
  
  
  return(list(theta_0= theta_0,  gamma=  gamma,
              phi = phi, coef = coef(vl)))
  
}