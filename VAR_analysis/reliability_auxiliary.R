
reliability_comparing <- function(vars,p, proxies, irhor, instruments){
  
  p = 4     
  irhor = 20
  x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
  y =vars[-1:-p, ]
  T = nrow(y)
  n <- ncol(y)
  #nbre de variables instrumentees
  k <-2
  #nbre d'instruments
  l <- ncol(proxies)
  # Create a 3x2 matrix of zeros
  zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
  #my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
  matrix_12 <- rbind( diag(k) , zero_matrix)
  
  vl = vars::VAR(x, p, type = "none")
  coef =coef(vl)
  bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
  res = residuals(vl)
  Sigma_eta <- cov(res) 
  
  T = nrow(y)
  
  Z <- window(proxies, start = start(proxies) + c(0, p))
  
  # Defining the projector on the instruments
  PZ <- Z %*% solve(t(Z)%*%Z) %*% t(Z)
  
  if (nrow(PZ)== ncol(PZ)){
    MZ <- diag(nrow(PZ)) -PZ
  }
  # βˆ2SLS =  (t(X)PZ X)^−1 t(X)PZy
  
  X <- res[,1:k]
  Y <- res[,(k+1):n]
  
  
  # first solution utilisant le projete
  
  b21ib11_TSLS <- ginv(t(X) %*% PZ %*% X) %*% t(X) %*% PZ %*% Y
  b21ib11 <- t(b21ib11_TSLS)
  Sig11 <- Sigma_eta[(1:k),(1:k)]
  Sig21 <- Sigma_eta[(k+1):n,1:k]
  Sig22 <- Sigma_eta[(k+1):n,(k+1):n]
  ZZp   <- b21ib11 %*% Sig11 %*% t(b21ib11) - (Sig21 %*% t(b21ib11) + b21ib11 %*% t(Sig21)) + Sig22
  b12b12p <- t(Sig21- b21ib11%*%Sig11) %*% (ginv(ZZp) %*% (Sig21- b21ib11%*%Sig11))
  b11b11p <- Sig11 - b12b12p
  res_TSLS <- Y - X %*% b21ib11_TSLS
  

  #reliability Mertens and Ravn
  
  proxies_postlag <- tryCatch(
    {
      # Code that might fail
      proxies[c(-1:-p),]
    },
    
    error = function(e) {
      # Handle errors
      return(proxies[c(-1:-p)])  # Return a value in case of error
    }
  )
  mbis <- ginv(proxies_postlag)
  
  ## Calculate Sigmm
  Sigmm <- mbis  %*% t(mbis)/ T
  # a priori ED compte les observations non-nulles
  #  dans le contexte des tax shocks, plein d'obs a 0
  # chez nous le choc est toujours non-nul
  
  # Calculate ED
  #ED <- diag(k) * sum(rowSums(mbis != 0))/T/n_ins
  
  # Calculate mu1
  sig_mu1 <- mbis %*% res[, 1:k] / T
  
  # Calculate PhiPhip
  PhiPhip <- sig_mu1 %*% solve(b11b11p) %*% t(sig_mu1)
  
  # Calculate RM
  RM <- solve(Sigmm) %*% PhiPhip #%*% solve(ED)
  
  # Calculate eigenvalues and sort them
  RMeigs <- sort(eigen(RM)$values, decreasing = TRUE)
  
  reliability_measure <- Re(RMeigs)
  
  
  
  
  
  data <- data.frame(cbind(res[,1:k], proxies_postlag ))
  colnames(data) <- c('res_gen_tot_log_diff', 'res_spot_price_log_diff',
                      instruments)
  
  # Construct the formula
  formula_price <- as.formula(paste('res_spot_price_log_diff', "~",
                                    paste(instruments, collapse = "+")))
  # Construct the formula
  formula_demand <- as.formula(paste('res_gen_tot_log_diff', "~",
                                     paste(instruments, collapse = "+")))
  
  m_price <- lm( formula_price, data = data)
  
  
  m_demand <- lm(formula_demand, data = data)
  
  # Get all the statistical coefficients
  coef_price <- summary(m_price)$coefficients
  # Convert to a data frame for easier subsetting
  coef_price_df <- as.data.frame(coef_price)
  # Subset the data frame to keep only significant coefficients at the 10 percent level
  coef_price_df <- coef_price_df[coef_price_df[["Pr(>|t|)"]] < 0.10, ]
  # Apply the function to the p-values and create new row names
  significance_stars <- sapply(coef_price_df[, "Pr(>|t|)"], get_significance_stars)
  
  price_names <- paste(rownames(coef_price_df), significance_stars)
  
  # the R^2 in the regression 
  fstat_price <- as.numeric(summary(m_price)$fstat[1])# Get all the statistical coefficients
  
  
  coef_gen <- summary(m_demand)$coefficients
  # Convert to a data frame for easier subsetting
  coef_gen_df <- as.data.frame(coef_gen)
  # Subset the data frame to keep only significant coefficients at the 10 percent level
  coef_gen_df <- coef_gen_df[coef_gen_df[["Pr(>|t|)"]] < 0.10, ]
  
  # Apply the function to the p-values and create new row names
  significance_stars <- sapply(coef_gen_df[, "Pr(>|t|)"], get_significance_stars)
  
  gen_names <- paste(rownames(coef_gen_df), significance_stars)
  # the R^2 in the regression 
  fstat_gen <-as.numeric(summary(m_demand)$fstat[1])
  
  
  # Number of observations
  n <- nrow(Z)
  
  # Step 1: Compute Z'Z, Z'X1, and X1'X1
  ZtZ <- t(Z) %*% Z
  ZtX <- t(Z) %*% X
  X1tX1 <- t(X) %*% X
  
  # Step 2: Compute the key matrix
  matrix_C <- solve(ZtZ) %*% ZtX %*% solve(X1tX1) %*% t(ZtX)
  
  # Step 3: Find the smallest eigenvalue
  lambda_min <-  min(Re(eigen(matrix_C)$values[1:k]))
  
  # Step 4: Compute the Cragg-Donald statistic
  cragg_donald_stat <- n * lambda_min
  
  
  
  return(list(reliability_measure = reliability_measure, GCraggDonald_stat = cragg_donald_stat,
              price_names= price_names, fstat_price = fstat_price,
              gen_names =gen_names, fstat_gen = fstat_gen))
  
}


format_data <- function(reliability_data, name){
  # Convert each list element to a single string with newline separators
  nbre_comp <- length(reliability_data$reliability_measure)
  
  # Handle empty reliability_measure
  RMeigs_str <- ifelse(nbre_comp == 0, "N/A", 
                       ifelse(nbre_comp == 1, 
                              round(reliability_data$reliability_measure, 2),
                              paste(round(reliability_data$reliability_measure[c(1, 2)], 2), collapse = "\n")))
  
  # Handle empty price_names (if needed, add additional checks here)
  price_names_str <- ifelse(length(reliability_data$price_names) == 0, "N/A", paste(reliability_data$price_names, collapse = "\n"))
  
  # Handle empty gen_names (if needed, add additional checks here)
  gen_names_str <- ifelse(length(reliability_data$gen_names) == 0, "N/A", paste(reliability_data$gen_names, collapse = "\n"))
  
  # Handle empty GCraggDonald_stat
  GCraggDonald_stat <- ifelse(length(reliability_data$GCraggDonald_stat) == 0, "N/A", round(reliability_data$GCraggDonald_stat, 2))
  
  # Handle empty fstat_price and fstat_gen
  fstat_price <- ifelse(length(reliability_data$fstat_price) == 0, "N/A", round(reliability_data$fstat_price, 2))
  fstat_gen <- ifelse(length(reliability_data$fstat_gen) == 0, "N/A", round(reliability_data$fstat_gen, 2))
  
  # Create a data frame with one row
  one_line_df <- data.frame(
    name,
    RMeigs_str,
    GCraggDonald_stat,
    price_names_str,
    gen_names_str,
    fstat_price,
    fstat_gen,
    stringsAsFactors = FALSE
  )
  
  return(one_line_df)
}

  
  # x <- x_bis
  # y <- y_bis
  # my_proxies <- my_proxies_bis
  # my_controls <- controls_bis
  
  
get_table <- function(vars,p,  proxies, title, headers_proxies){
    # Define the instruments
    my_proxies <- proxies
    instruments <- headers_proxies
    colnames(proxies)
    par(mar = c(2, 2, 2, 2)) 
    plot.new()
    
    
    reliability_tot <- reliability_comparing(
      vars,p, proxies, irhor, instruments)
    
    reliability_data <- format_data(reliability_tot,'All ins')
    
    for (instrument in instruments){
      # headers <- as.numeric(instrument_dict[instrument])
      
      reliability_unique <- reliability_comparing(
        vars,p,  my_proxies[,instrument], irhor, instrument)
      reliability_data <-  rbind(reliability_data,
                                 format_data(reliability_unique, instrument))
      
    }
    for (instrument in instruments){
      # Remove the current instrument from the list
      missing_instruments <- instruments[instruments != instrument]
      # headers <- as.numeric(instrument_dict[missing_instruments])
      # Remove the current instrument from the list
      reliability_missing <- reliability_comparing(
        vars,p, my_proxies[,missing_instruments], irhor,   missing_instruments)
      reliability_data <-  rbind(reliability_data, 
                                 format_data(reliability_missing, paste('without', instrument)))
      
    }
    
    # Define column headers with LaTeX expressions
    column_headers <- c("Model", "Reliab MR(2013)", "GCraggDonald stat",
                        "price", "gen",
                        "Fstat[price]", "Fstat[gen]")
    tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    
    colnames(reliability_data) <- column_headers
    # Create the table grob
    table_grob <- tableGrob(
      reliability_data, 
      rows = NULL,
      theme=tt
    )
    
    # Save as PNG
    png(filename = title, width = 1500, height =1000, res = 100)
    grid.draw(table_grob)
    dev.off()
}

