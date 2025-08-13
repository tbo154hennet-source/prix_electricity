
iterate_var_quat <- function(x, initial_obs, errors, var_model, controls =NULL){
  #var_model <- vl
  #errors <- res
  coefficients <- coef(var_model)
  
  # Number of variables and lags
  n <- ncol(x)          # number of endogenous variables
  T <- nrow(x)
  
  # Construct Phi: (n x (n*p)) matrix
  Phi <- matrix(NA, nrow = n, ncol = n * p)
  for (i in 1:n) {
    for (lag in 1:p) {
      Phi[i, ((lag - 1) * n + 1):(lag * n)] <- coefficients[[i]][(1 + (lag - 1) * n):(lag * n), 1]
    }
  }
  colnames(Phi) <- paste0(rep(colnames(x), times = p), "_L", rep(1:p, each = n))
  rownames(Phi) <- colnames(x)
  
  # Initialize the reconstructed series matrix
  reconstructed_series <- matrix(NA, nrow = T, ncol = n)
  colnames(reconstructed_series) <- colnames(x)
  
  # Insert initial values (from observed data or given initial_obs)
  reconstructed_series[1:p, ] <- initial_obs
  
  
  
  if (!is.null(controls)) {
    B <- sapply(coef, function(coef_list) {
      # Extract the rows corrfronding to controls
      ctrl_idx <- (n * p + 1):nrow(coef_list)
      coef_list[ctrl_idx, , drop = FALSE]
    }, simplify = "array")
  }
  # Extract the "Estimate" component across all regressions
  B_mat <- sapply(1:dim(B)[3], function(i) B[ , "Estimate", i])
  
  # Rename columns using the third dimension names
  colnames(B_mat) <- dimnames(B)[[3]]
  #t <- (p+1)
  #t <- T
  # In your for loop:
  for (t in (p + 1):T) {
    # Prepare lagged y
    x_lagged <- matrix(NA, nrow = n * p, ncol = 1)
    for (lag in 1:p) {
      x_lagged[((lag - 1) * n + 1):(lag * n), 1] <- reconstructed_series[t - lag, ]
    }
    
    # Compute Phi * lagged values
    base <- (Phi %*% x_lagged)[, 1]
    
    # Add contribution from controls if applicable
    #control_term <- if (!is.null(controls)) B * rep(controls[t, ], n) else 0
    
    x_t <- as.numeric(controls[t, ])
    
    # Multiply with B_mat
    control_term <- x_t %*% B_mat
    # Add error and controls
    reconstructed_series[t, ] <- base  + errors[t - p, ] + control_term
  }
  reconstructed_series <- as.data.frame(reconstructed_series)
  
  return(reconstructed_series)
}


headers_controls <- c("avg_spot_price_log_diff", "ffr_diff")

p = 12    
irhor = 40


proxies <- ts(IV_data_log_diff_fr[, c(headers_proxies_fr)], frequency=12)
proxies[is.na(proxies)] <- 0
vars<- ts(IV_data_log_diff_fr[, c(headers_var_gen_first)], frequency=12)

x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <-2
#nbre d'instruments
l <- ncol(proxies)
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)

matrix_12 <- rbind( diag(k) , zero_matrix)


controls <- ts(IV_data_log_diff_fr[, headers_controls] , start=c(2015,1), end=c(2023,12), frequency=12)

estimate_log_diff <- first_var_estimation(x,y,p, proxies, irhor, controls = controls)
theta_0 <- estimate_log_diff$theta_0
gamma <- estimate_log_diff$gamma
phi <- estimate_log_diff$phi
var_coef_log_diff <- estimate_log_diff$coef

AL <- do.call(cbind, lapply(var_coef_log_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log_diff)

library(latex2exp)
colors_shock <- c("purple", "orange", "lightblue", "yellow",  "blue", "red", "green")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$HICP$", "Gas", "ECB_rate", "Trend")
#custom legend part
vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))
x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]

vl = vars::VAR(x, p, type = "none", exogen = controls)
k  = ncol(proxies)

coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)

irf_cholesky <- irf(vl, impulse = colnames(x), rfronse = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

# Apply cumsum columnwise for each shock-rfronse matrix
cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))

#plot(irf_cholesky)
# save_rds_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
# setwd(save_rds_path)
# saveRDS(irf_cholesky, file = "irf_cholesky_fr.rds")



# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute mean
mean_shock <- mean(structural_shocks)

# Compute standard deviation
sd_shock <- sd(structural_shocks)

# Print results
cat("Mean of structural shocks:", mean_shock, "\n")
cat("Standard deviation of structural shocks:", sd_shock, "\n")



# Compute historical decomposition for each variable
# Initialize matrix to store historical decomposition
n_obs <- nrow(res)
n_vars <- ncol(res)
hd_cholesky <-array(NA, dim = c(n_obs,  n_vars, n_vars))
hd_cholesky_index <-array(NA, dim = c(n_obs,  n_vars, n_vars))
y_hat <-array(NA, dim = c(n_obs,   n_vars))
# class(irf_model$irf)
# length(irf_model$irf)


if (!is.null(controls)) {
  B <- sapply(coef, function(coef_list) {
    # Extract the rows corrfronding to controls
    ctrl_idx <- (n * p + 1):nrow(coef_list)
    coef_list[ctrl_idx, , drop = FALSE]
  }, simplify = "array")
  # Extract the "Estimate" component across all regressions
  B_mat <- sapply(1:dim(B)[3], function(i) B[ , "Estimate", i])
}


# Rename columns using the third dimension names
colnames(B_mat) <- dimnames(B)[[3]]


c <- ncol(controls)
hd_controls <- array(0, dim = c(T, n, c))  # T x number of endogenous vars

for (t in 1:T) {
  x_t <- as.numeric(controls[t, ])
  for (i in 1:c) {
    hd_controls[t,,i ] <- x_t[i] * B_mat[i, ]
  }
}
t <-1
j <-1
#j is the variable of interest
for (j in c(1:n)){
  for (shock in c(1:n)){
    for (t in c(1:T)){
      contribution <-0
      contribution_index <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- irf_cholesky$irf[[shock]][hori,j]*structural_shocks[shock_horizon,shock] 
        contrib_hori_index <- cum_irfs_all[[shock]][hori,j]*structural_shocks[shock_horizon,shock] 
        # contrib_hori <- irf_model$irf[[shock]][hori,j]*
        #   structural_shocks[shock_horizon,shock]
        # 
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          contribution_index <- contribution_index + contrib_hori_index
          
        }
      }
      hd_cholesky[t,j, shock ] <- contribution
      hd_cholesky_index[t,j, shock ] <- contribution_index
    }
    
  }
  
}

for (j in c(1:n)){
  for (t in c(1:T)){
    y_hat[t, j] <- sum(hd_cholesky[t, j, ]) + hd_controls[t, j, 1]+ hd_controls[t, j, 2]
  }
}

historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +c)))
historical_decomp_extended[,,1:n_vars] <- hd_cholesky
historical_decomp_extended[,,(n_vars +1):(n_vars +c)] <- hd_controls


colors_shock <- rev(c("purple" ,"orange", "lightblue", "yellow",  "blue", "red", "green"))
zero_errors <- matrix(0, nrow = dim(res)[1], ncol = dim(res)[2])


reconstruct_derive_without_errors_ter <- iterate_var_quat(x =x ,
                                                          initial_obs = x[1:p, ],
                                                          errors = zero_errors,
                                                          var_model = vl,
                                                          controls = controls
)
alpha_chosen = 1
#historical_decomp_matrix <- hd_cholesky
historical_decomp_matrix <- historical_decomp_extended
n_serie = 4
ne_hd_12_HICP <- plot_hd_inflation_glissante_12(series = n_serie)+
  labs(title=TeX(paste("Historical Decomposition of HICP")) ,
       x="Time", y="Contribution")
ne_hd_12_HICP


plot_hd_inflation_glissante_12 <- function(series){
  
  
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
    geom_bar(data= long_data, aes(x = time, y =rolling_avg, fill = category),
             stat = "identity", position = "stack", alpha = alpha_chosen) +  # Barres empilées
    scale_fill_manual(
      values = rev(colors_shock)[(length(colors_shock)-(dim(decomp_tot_data)[2]-1)+1):length(colors_shock)],
      labels = TeX(names))+
    #scale_fill_manual(values = c("blue", "red", "green"), labels = c("Supply", "Demand", "Other")) +
    theme_minimal()
  
  # comparison <- lag(as.vector(vars_demeaned[(p+1):96,series]), 20)
  # comparison[1:20] <- rep(vars_demeaned[4,series], 10)
  
  #y_diff <- vars_demeaned[(p+1):nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  y_diff <- vars[(p+1):nrow(vars_demeaned),series] - reconstruct_derive_without_errors_ter[,series][c(-1:-p)] 
  
  data <- data.frame(
    x = as.Date(dates_formatted),
    y = cumsum(y_diff),
    #ytest = apply(historical_decomp_matrix[,series ,], 1, sum) 
    ytest = apply(historical_decomp_matrix[,series ,], 1, sum) + mean_vector[series]
    #ytrend = + c(1:T)*mean_vector[series]
  )%>%  # assure-toi d'avoir une variable temporelle (ex: `time`)
    mutate(rolling_avg_contrib = slide_dbl(ytest, sum, .before = 11, .complete = TRUE))
  
  my_plot <- my_plot + 
    #geom_line(data, mapping = aes(x = x, y = y), linetype = "dashed")  # Add dashed line+
    geom_line(data, mapping = aes(x = x, y = rolling_avg_contrib), linetype = "dashed", color='grey')
  #geom_line(data, mapping = aes(x = x, y = ytrend), linetype = "dashed", color='darkred') 
  return(my_plot)
}
series = 4
historical_decomp_matrix[,series,6]
