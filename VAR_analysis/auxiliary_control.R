library(vars)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(arrow)
library(estimatr)
library(stargazer)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)

first_var_estimation <- function(x, y, p, proxies, irhor, controls = NULL) {
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
  

  
  theta_01 <- gamma  %*% ginv(t(matrix_12) %*% gamma ) %*% c(1,0)
  
  matrix_12_bis <- rbind( diag(2) , zero_matrix)
  matrix_12_bis[,1] <-  ginv(Sigma_eta) %*% theta_01
  
  theta_02 <- gamma  %*% ginv( t(matrix_12_bis)  %*% gamma ) %*% c(0, 1)
  
  
  theta_0 <- cbind(theta_01, theta_02)
  
  
  
  phi <- matrix(c(1,0, -theta_0[1,2][[1]], 1), nrow = 2, ncol = 2) %*% t(matrix_12)  %*% gamma
  
  
  
  return(list(theta_0= theta_0,  gamma=  gamma,
              phi = phi, coef = coef(vl)))
  
}

# standard bootstrapping as presented by Lütkepohl, Helmut
# Fonction pour itérer le modèle VAR
iterate_var <- function(initial_obs, errors, var_model, controls = NULL) {
  
  # vbis = vars::VAR(x, p, type = "none")
  # initial_obs <- vars[1:p,]
  # errors <- res2*rr + mean(res)
  # var_model <- vl
  #vars[1:p,], res2*rr + mean(res), vl
  
  m <- nrow(errors)
  # Matrice pour stocker les valeurs simulées
  forecasted_data <- matrix(NA, nrow = m, ncol = n)
  colnames(forecasted_data) <- colnames(initial_obs)
  # Initialiser avec les observations initiales
  nbre <- nrow(var_model$datamat)
  var_model$datamat[nbre, ] <- var_model$datamat[1, ]
  # quoiqu'anormal, on observe qu'en changeant le lag 4 
  #  il ya une legere difference de prediction
  for (t in 1:m) {
    #t=1
    if (is.null(controls)) {
      forecasts <- predict(var_model, n.ahead = 1)#, dumvar = exogen)
      
    } else {
      exogen = data.frame(controls[p+t])
      colnames(exogen) <- c('gas_price_log_diff')
      forecasts <- predict(var_model, n.ahead = 1, dumvar = exogen)
    }
    
    # Calculer les valeurs prédites

    
    values_forecasted <- as.matrix(as.numeric(lapply(1:n, function(x) forecasts$fcst[[x]][1])))
    values_forecasted <- values_forecasted + errors[t, ]
    
    
    forecasted_data[t, ] <- values_forecasted
    
    
    # Mettre à jour les observations courantes
    
    nbre <- nrow(var_model$datamat)
    
    var_model$datamat[nbre + 1, from:to] <- var_model$datamat[nbre, 1:to_bis]
    var_model$datamat[nbre +1, 1:n] <- values_forecasted
  }
  
  return(forecasted_data)
}

set_sigmaT <- function(conf, SigmaTSigmaTp) {
  if (conf$orderFirst=="supply") {
    s1 <- sqrt(SigmaTSigmaTp[1,1])
    a  <- SigmaTSigmaTp[2,1]/s1
    s2 <- sqrt(SigmaTSigmaTp[2,2]-(a^2))
    SigmaT <- rbind(c(s1, 0), c(a ,s2))
  } else if (conf$orderFirst=="demand") {
    s2 <- sqrt(SigmaTSigmaTp[2,2])
    b <- SigmaTSigmaTp[1,2]/s2
    s1 <- sqrt(SigmaTSigmaTp[1,1]-(b^2))
    SigmaT <- rbind(c(s1, b), c(0 ,s2))
    
  } else {
    stop("conf$first has to be supply or demand")
  }
  return(SigmaT)
}

# Function to replace each element with the difference between itself and the last value
inflation_diff <- function(mat, j) {
  for (i in nrow(mat):2) {
    mat[i,j] <- 12*(mat[i,j] - mat[i-1,j]) # Subtract the last value from each element in the column
  }
  return(mat)
}

irf_computation <- function(b11iSig, b21iSig, SigmaTSigmaTp, conf){ #ordering
  # s1 <- sqrt(SigmaTSigmaTp[1,1])
  # a  <- SigmaTSigmaTp[2,1]/s1
  # s2 <- sqrt(SigmaTSigmaTp[2,2]-(a^2))
  # SigmaT <- rbind(c(s1, 0), c(a ,s2))
  #conf<-my_conf
  
  SigmaT <-  set_sigmaT(conf, SigmaTSigmaTp)
  b1 <- rbind(b11iSig, b21iSig) %*% SigmaT
  irs <- matrix(0, nrow=(irhor+p), ncol=ncol(vars)) 
  
  j <- if (conf$shock=="supply") { 2 } else if (conf$shock=="demand") { 1 }
  
  # attention input a one unit shock means here a one log point shock,
  # that is one order of magnitude change, insane response
  # and overall insane confidence bands
  n = ncol(y)
  irs[p+1,] <- b1[,j]/b1[j,j]
  
  for (tt in 2:irhor) {
    h <- (p+tt-1):tt
    lvars <- as.vector(t(irs[h,]))  
    irs[p+tt,] = t(lvars) %*% bet[1:(p*n),]     
  }
  response <- irs[c(-1:-p),]
  # inflation response
  # the second and third column are log index responses
  # that I transform in annualized inflation responses
  
  
  return(response)
  
}

get_lambda_0 <-function(C, theta_0){
  k <- 2
  n <- ncol(y)
  # on essaye d'abord de repliquer a l'identique stock and watson
  lambda_0 <- array(NA, dim = c(irhor, n, k))
  
  for (h in c(1:2)){
    for (i in c(1:n)){
      e_i <- rep(0, n)
      
      # Mettre 1 à la position j
      e_i[i] <- 1
      # Appliquer la fonction pour extraire les éléments
      lambda_0[, i, h] <- as.numeric(lapply(1:20, function(x) {
        return(t(e_i) %*% C[,,x] %*% theta_0[,h]  )    }))
    }
  }
  return(lambda_0)
}






# Function to assign significance stars
get_significance_stars <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}


MARep <- function(AL, p, hori) {
  
  # Reshape AL into a 3-D array
  n <- nrow(AL)
  
  
  # Reshape AL into a 3D array [n, n, p]
  vecAL <- array(AL, dim = c(n, n, p))
  
  
  # Initialize vecALrevT, which will store reversed and transposed coefficients
  vecALrevT <- array(0, dim = c(n, n, hori))
  
  # Populate vecALrevT based on horizon
  for (i in 1:hori) {
    if (i < (hori - p + 1)) {
      vecALrevT[, , i] <- matrix(0, nrow = n, ncol = n)
      
    } else {
      vecALrevT[, , i] <- t(vecAL[, , (hori - i + 1)])
    }
  }
  
  # Reshape vecALrevT into a 2-D matrix [n, n*hori]
  vecALrevT <- matrix(vecALrevT, n, n * hori)
  
  # Initialize MA coefficient matrix
  C <- matrix(0, n, n * hori)
  
  # Start with the first MA coefficient (identity matrix)
  C[, 1:n] <- vecAL[, , 1]
  
  # Compute the remaining MA coefficients
  for (i in 1:(hori - 1)) {
    # Create matrix with [eye(n), C] for current horizon
    temp <- cbind(diag(n), C[, 1:(n * i)])
    
    # Update the next set of MA coefficients
    C[, (n * i + 1):(n * (i + 1))] <- temp %*% t(vecALrevT[, (hori * n - n * (i + 1) + 1):(hori * n)])
  }
  
  C_3D <- array(C, dim = c(n, n, hori-1))
  
  # Step 4: Reshape Caux into a 3D array of dimensions [VAR.n, VAR.n, VAR.irhor]
  inverse <- array(0, dim = c(n, n, hori))
  inverse[,,1] <- diag(n)
  inverse[,,2:hori] <- C_3D
  
  
  return(inverse)
}

iterate_var_bis <- function(initial_obs, errors, var_coef, controls =NULL) {
  
  # vbis = vars::VAR(x, p, type = "none")
  # initial_obs <- vars[1:p,]
  # errors <- centered_U_boot
  # var_coef <- vl
  #iterate_var(vars[1:p,], centered_U_boot, vl)
  #iterate_var_bis(vars[1:p,], centered_U_boot, vl, controls)
  m <- nrow(errors)
  # Matrice pour stocker les valeurs simulées
  forecasted_data <- matrix(NA, nrow = m, ncol = n)
  colnames(forecasted_data) <- colnames(initial_obs)
  # Initialiser avec les observations initiales
  nbre <- nrow(var_coef$datamat)
  var_coef$datamat[nbre, ] <- var_coef$datamat[1, ]
  # quoiqu'anormal, on observe qu'en changeant le lag 4 
  #  il ya une legere difference de prediction
  for (t in 1:m) {
    # Calculer les valeurs prédites
    if (is.null(controls)) {
      forecasts <- predict(var_coef, n.ahead = 1)#, dumvar = exogen)
      
    } else {
      exogen = data.frame(controls[p+t])
      colnames(exogen) <- c('exo1')
      forecasts <- predict(var_coef, n.ahead = 1, dumvar = exogen)
    }
    
    values_forecasted <- as.matrix(as.numeric(lapply(1:n, function(x) forecasts$fcst[[x]][1])))
    values_forecasted <- values_forecasted + errors[t, ]
    
    
    forecasted_data[t, ] <- values_forecasted
    
    
    # Mettre à jour les observations courantes
    
    nbre <- nrow(var_coef$datamat)
    
    var_coef$datamat[nbre + 1, (from-1):to] <- var_coef$datamat[nbre, 1:(to_bis+1)]
    var_coef$datamat[nbre +1, 1:n] <- values_forecasted
  }
  
  return(forecasted_data)
}


