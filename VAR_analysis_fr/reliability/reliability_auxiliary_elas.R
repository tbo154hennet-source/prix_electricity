

source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/reliability'
setwd(source_path)
source("weak_instrument_test.R")


reliability_comparing <- function(vars,p, proxies, irhor, headers_proxies){
  
  p = 4     
  irhor = 20
  x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
  y =vars[-1:-p, ]
  T = nrow(y)
  n <- ncol(y)
  #nbre de variables instrumentees
  k <-2
  #nbre d'headers_proxies
  l <- ncol(proxies)
  # Create a 3x2 matrix of zeros
  zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
  #my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
  matrix_12 <- rbind( diag(k) , zero_matrix)
  
  vl <- vars::VAR(x, p, type = "none")
  coef =coef(vl)
  bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
  res = residuals(vl)
  Sigma_eta <- cov(res) 
  
  T = nrow(y)
  
  Z <- window(proxies, start = start(proxies) + c(0, p))
  
  # Defining the projector on the headers_proxies
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
                      headers_proxies)
  
  # Construct the formula
  formula_price <- as.formula(paste('res_spot_price_log_diff', "~",
                                    paste(headers_proxies, collapse = "+")))
  # Construct the formula
  formula_demand <- as.formula(paste('res_gen_tot_log_diff', "~",
                                     paste(headers_proxies, collapse = "+")))
  
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
  # proxies <- proxies_bis
  # my_controls <- controls_bis
  
  
get_table <- function(my_IV_data, headers_var, headers_proxies ,p , title){
    # Define the headers_proxies
    vars<- ts(my_IV_data[, c(headers_var)], frequency=12)
    proxies <- ts(my_IV_data[, c(headers_proxies)], frequency=12)
    
    par(mar = c(2, 2, 2, 2)) 
    plot.new()
    
    
    reliability_tot <- reliability_comparing(
      vars,p, proxies, irhor, headers_proxies)
    
    reliability_data <- format_data(reliability_tot,'All ins')
    
    for (instrument in headers_proxies){
      # headers <- as.numeric(instrument_dict[instrument])
      
      reliability_unique <- reliability_comparing(
        vars,p,  proxies[,instrument], irhor, instrument)
      reliability_data <-  rbind(reliability_data,
                                 format_data(reliability_unique, instrument))
      
    }
    for (instrument in headers_proxies){
      # Remove the current instrument from the list
      missing_headers_proxies <- headers_proxies[headers_proxies != instrument]
      # headers <- as.numeric(instrument_dict[missing_headers_proxies])
      # Remove the current instrument from the list
      reliability_missing <- reliability_comparing(
        vars,p, proxies[,missing_headers_proxies], irhor,   missing_headers_proxies)
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


run_iv_analysis_new <- function(my_IV_data, headers_var, headers_proxies,
                            start_time = c(2015, 1), end_time = c(2023, 12),
                            p = 4, irhor = 20, k = 2, dependent = "spot_price_log",
                            headers_controls = NULL, cov_type = "EHW") {
  
  vars<- ts(my_IV_data[, c(headers_var)], frequency=12)
  proxies <- ts(my_IV_data[, c(headers_proxies)], frequency=12)
  
  x <- ts(vars, start = start_time, end = end_time, frequency = 12)
  y <- vars[-1:-p, ]
  T <- nrow(y)
  n <- ncol(y)
  l <- ncol(proxies)
  
  # VAR estimation
  controls <- if (is.null(headers_controls)) {
    NULL
  } else {
    ts(my_IV_data[, headers_controls], frequency = 12)
  }
  
  vl <- if (is.null(controls)) {
    vars::VAR(x, p, type = "none")
  } else {
    vars::VAR(x, p, type = "none", exogen = controls)
  }
  
  
  coef <- coef(vl)
  bet <- do.call(cbind, lapply(1:n, function(i) coef[[i]][, 1]))
  res <- residuals(vl)
  Sigma_eta <- cov(res)
  
  # Instruments window
  Z <- window(proxies, start = start(proxies) + c(0, p))
  PZ <- Z %*% solve(t(Z) %*% Z) %*% t(Z)
  if (k==1){
    
    # OLS regression with residuals
    
    dependent_index <- match(dependent, colnames(res))
    if (is.na(dependent_index)) {
      stop("The dependent variable is not included in headers_var: ", dependent)
    }

    data <- data.frame(cbind(res[, dependent_index], Z))
    res_var <- paste0("res_",dependent)
    colnames(data) <- c(res_var, headers_proxies)
    # Create formula
    formula_str <- paste(res_var, "~", paste(headers_proxies, collapse = "+"))
    model <- lm(as.formula(formula_str), data = data)
    fstat <- as.numeric(summary(model)$fstatistic[1])
    
    # Run gweakivtest for each endogenous variable in res (excluding the dependent)
    results <- list()
    outcome_indices <- setdiff(seq_len(n), dependent_index)
    var_names <- colnames(res)[outcome_indices]
    
    for (i in seq_along(var_names)) {
      outcome_index <- outcome_indices[i]
      y_col <- res[, outcome_index]
      X_cols <- res[, setdiff(outcome_indices, outcome_index), drop = FALSE]
      result <- gweakivtest(
        y = y_col,
        Y = res[, dependent_index],
        X = X_cols,
        Z = Z,
        cov_type = cov_type
      )
      results[[paste0("result_", var_names[i])]] <- result
    }
    
    return(c(
      list(
        fstat = fstat,
        m_summary = model
      ),
      results
    ))
    
  }else{
    X <- res[, 1:k]
    Y <- res[, (k+1):n]
    
    # dim(PZ)
    # dim(X)
    # TSLS estimates
    b21ib11_TSLS <- ginv(t(X) %*% PZ %*% X) %*% t(X) %*% PZ %*% Y
    b21ib11 <- t(b21ib11_TSLS)
    Sig11 <- Sigma_eta[(1:k), (1:k)]
    Sig21 <- Sigma_eta[(k+1):n, 1:k]
    Sig22 <- Sigma_eta[(k+1):n, (k+1):n]
    
    ZZp <- b21ib11 %*% Sig11 %*% t(b21ib11) - (Sig21 %*% t(b21ib11) + b21ib11 %*% t(Sig21)) + Sig22
    b12b12p <- t(Sig21 - b21ib11 %*% Sig11) %*% ginv(ZZp) %*% (Sig21 - b21ib11 %*% Sig11)
    b11b11p <- Sig11 - b12b12p
    
    res_TSLS <- Y - X %*% b21ib11_TSLS
    
    # Reliability: Mertens-Ravn
    proxies_postlag <- proxies[-1:-p, ]
    mbis <- ginv(proxies_postlag)
    Sigmm <- mbis %*% t(mbis) / T
    sig_mu1 <- mbis %*% res[, 1:k] / T
    PhiPhip <- sig_mu1 %*% solve(b11b11p) %*% t(sig_mu1)
    RM <- solve(Sigmm) %*% PhiPhip
    RMeigs <- sort(eigen(RM)$values, decreasing = TRUE)
    reliability_measure <- Re(RMeigs)
    
    # Cragg-Donald statistic
    ZtZ <- t(Z) %*% Z
    ZtX <- t(Z) %*% X
    X1tX1 <- t(X) %*% X
    matrix_C <- solve(ZtZ) %*% ZtX %*% solve(X1tX1) %*% t(ZtX)
    lambda_min <- min(Re(eigen(matrix_C)$values[1:k]))
    cragg_donald_stat <- nrow(Z) * lambda_min
    
    #standard f-stat
    
    # Préparation des données
    data <- data.frame(cbind(res[, 1:k], Z))
    colnames(data) <- c(paste0('res_', headers_var[1:k]), headers_proxies)
    
    # Identifier les variables de prix et de demande (ex: "price" et "gen")
    #dep_var_price  <- headers_var[grepl("price", headers_var)][1]
    dep_var_price  <- colnames(data)[grepl("res_spot_price", colnames(data))][1]
    
    #dep_var_demand <- headers_var[grepl("gen", headers_var)][1]
    dep_var_demand <- colnames(data)[grepl("res_load_tot", colnames(data))][1]
    if (is.na(dep_var_demand)) {
      dep_var_demand <- colnames(data)[grepl("res_spot_gas", colnames(data))][1]
    }
    # Estimations OLS pour la first stage
    # form_price  <- as.formula(paste0("res_", dep_var_price, " ~ ", paste(headers_proxies, collapse = "+")))
    # form_demand <- as.formula(paste0("res_", dep_var_demand, " ~ ", paste(headers_proxies, collapse = "+")))
    
    form_price  <- as.formula(paste0( dep_var_price, " ~ ", paste(headers_proxies, collapse = "+")))
    form_demand <- as.formula(paste0(dep_var_demand, " ~ ", paste(headers_proxies, collapse = "+")))
    
    
    m_price  <- lm(form_price, data = data)
    m_demand <- lm(form_demand, data = data)
    
    # Statistiques F individuelles
    fstat_price <- as.numeric(summary(m_price)$fstatistic[1])
    fstat_gen   <- as.numeric(summary(m_demand)$fstatistic[1])
    

    # gweakivtest sur les résiduels restants, en excluant prix et demande
    excluded_vars <- c(dep_var_price, dep_var_demand)
    endog_vars <- setdiff(headers_var[(k + 1):(k + length(headers_var) - k)], excluded_vars)
    colnames(res) <- paste0("res_", colnames(res))
    gweakiv_results <- list()
    for (v in endog_vars) {
      print(v)
      y <- res[, paste0("res_", v)]
      x <- res[, paste0("res_", setdiff(endog_vars, v))]
      result <- gweakivtest(y = y, Y = X, X = x, Z = Z, cov_type = cov_type)
      gweakiv_results[[paste0("result_", toupper(v))]] <- result
    }
    
    # Assemblage du résultat
    return(c(
      list(
        reliability_measure = reliability_measure,
        cragg_donald_stat = cragg_donald_stat,
        fstat_price = fstat_price,
        m_price_summary = m_price,
        fstat_gen = fstat_gen,
        m_demand_summary = m_demand
      ),
      gweakiv_results
    ))
    
  }
}



get_table_compare <- function(headers_var,IV_data_log_esp, IV_data_log_fr,
                              demand_instruments, supply_instruments,
                              save_path, title){
    

  result_price_esp <- run_iv_analysis(IV_data_log_esp, headers_var, supply_instruments, p = 4,
                                      dependent = "spot_price_log", k = 1)
  result_price_fr <- run_iv_analysis(IV_data_log_fr, headers_var, supply_instruments, p = 4,
                                     dependent = "spot_price_log", k = 1)
  
  result_gen_esp <- run_iv_analysis(IV_data_log_esp, headers_var, demand_instruments, p = 4,
                                    dependent = "gen_tot_log", k = 1)
  result_gen_fr <- run_iv_analysis(IV_data_log_fr, headers_var, demand_instruments, p = 4,
                                   dependent = "gen_tot_log", k = 1)
  headers_proxies = c(
    supply_instruments, demand_instruments
  )

  result_esp <- run_iv_analysis(IV_data_log_esp, headers_var, headers_proxies, p = 4,
                                dependent = "", k = 2)

  result_fr <- run_iv_analysis(IV_data_log_fr, headers_var, headers_proxies, p = 4,
                               dependent = "", k = 2)
  # Rassembler les données dans un tableau unique
  summary_table <- tibble::tibble(
    Pays = c("France", "France", "France", "Espagne", "Espagne", "Espagne"),
    Modele = c("Prix (k=1)", "Demande (k=1)", "Joint (k=2)",
               "Prix (k=1)", "Demande (k=1)", "Joint (k=2)"),
    `F-statistic` = c(round(result_price_fr$fstat,2), round(result_gen_fr$fstat,2), 
                      paste(round(result_fr$fstat_price,2), round(result_fr$fstat_gen,2)),
                      round(result_price_esp$fstat,2), round(result_gen_esp$fstat,2), 
                      paste(round(result_esp$fstat_price,2), round(result_esp$fstat_gen,2)) ),
    `Cragg-Donald` = c(NA, NA, round(result_fr$cragg_donald_stat,2),
                       NA, NA, round(result_esp$cragg_donald_stat,2)),
    `Mertens and Ravn (2013)` = c(NA, NA, paste(round(result_fr$reliability_measure[1:2], 2), collapse = ", "),
                                  NA, NA, paste(round(result_esp$reliability_measure[1:2], 2), collapse = ", ")),
    
  )
  
  
  # Générer le tableau avec séparation France/Espagne
  iv_table <- summary_table %>%
    gt(groupname_col = "Pays") %>%
    tab_header(
      title = md("**IV Relevance**")
    ) %>%
    cols_label(
      Modele = "Modele",
      `F-statistic` = "F-stat",
      `Cragg-Donald` = "Cragg-Donald",
      `Mertens and Ravn (2013)` = "Mertens and Ravn (2013)"
    ) %>%
    fmt_missing(
      columns = everything(),
      missing_text = "-"
    ) %>%
    tab_options(row_group.as_column = TRUE) 
  
  setwd(save_path)
  # Sauvegarder en PNG
  gtsave(iv_table, filename = paste0(title,".html") )
    
}



