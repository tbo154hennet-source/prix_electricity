

#For Spain


p = 12   
irhor = 40


proxies <- ts(IV_data_log_diff_fr[, c(headers_proxies_fr)], frequency=12)
proxies[is.na(proxies)] <- 0
vars<- ts(IV_data_log_diff_fr[, c(headers_var_gen_first_mp)], frequency=12)

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
colors_shock <- c( "orange", "purple", "yellow",  "blue", "red", "green")

names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$HICP$", "ECB_rate", "Gas")
#custom legend part
vars_demeaned <- as.data.frame(apply(vars, 2, function(x) x - mean(x)))
x = ts(vars_demeaned  , start=c(2015,1), end=c(2023,12), frequency=12)
y_demeaned = vars_demeaned[-1:-p, ]

vl = vars::VAR(x, p, type = "const", exogen = controls)
k  = ncol(proxies)

coef =coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
Sigma_eta = cov(res)

irf_cholesky <- irf(vl, impulse = colnames(x), response = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)

# Apply cumsum columnwise for each shock-response matrix
cum_irfs_all <- lapply(irf_cholesky$irf, function(mat) apply(mat, 2, cumsum))

#plot(irf_cholesky)
# save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
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
# nrow(irf_model$irf[[1]])
if (!is.null(controls)) {
  B <- sapply(coef, function(coef_list) {
    # Extract the rows corresponding to controls
    ctrl_idx <- (n * p + 1):nrow(coef_list)
    coef_list[ctrl_idx, , drop = FALSE]
  }, simplify = "array")
  B <- matrix(unlist(B), nrow = n, byrow = TRUE)[,1]
}


hd_controls <- array(0, dim = c(T, n))  # T x number of endogenous vars
for (t in 1:T) {
    hd_controls[t, ] <- B * rep(controls[t, ], n)
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
      y_hat[t, j] <- sum(hd_cholesky[t, j, ]) + hd_controls[t, j]
    }
}


#hd_pelec <- plot_hd(hd_cholesky, series = 1)
#print(hd_pelec)

# The 'historical_decomp' array contains the contributions of each shock to each variable at each time step



#calculate sigma_aa
Sigma_a <- gamma %*%  ginv(t(gamma) %*% ginv(Sigma_eta) %*% gamma) %*% t(gamma)

Sigma_theta <- t(theta_0) %*% theta_0
D <- solve(Sigma_theta)  %*% t(theta_0) %*%  Sigma_a %*% theta_0 %*% solve(Sigma_theta)

#almost diagonal matrix

right_term <- t(gamma) %*% ginv(Sigma_eta) %*% t(res)
left_matrix <- t(phi) %*% solve(D)

# with this strategy the system is undertemined,
#I impose additional assumptions

structural_shocks_IV <- solve(t(left_matrix) %*% left_matrix) %*% t(left_matrix) %*% right_term



#verification 
#Plot the shocks

data_IV <- as.data.frame(t(structural_shocks_IV))
colnames(data_IV) <- c( "quantity", "price")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$price, color = "price"))+
  geom_line(aes(x = dates_formatted, y = data_IV$quantity, color = "quantity")) +
  theme_minimal()

# Eigenvalue decomposition
# eig <- eigen(Sigma_a)
# Q <- eig$vectors  # Orthogonal eigenvectors (Q_0,1:r)
# D <- diag(eig$values)  # Diagonal matrix of eigenvalues (D_1:r,1:r)
# eps_t <- t(Q) %*% t(res)
# 
# 
# cov(t(eps_t)[,1:2], t(structural_shocks_IV))

#les deux mesures ne sont pas du tout correlées



#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)
historical_decomp_iv_tibo <-array(NA, dim = c(n_obs,  n_vars, k))

hori = 40 # Forecast horizon

n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_iv <- get_lambda_0(C, theta_0, irhor= hori)
apply(lambda_0_iv, c(2, 3), function(x) cumsum(x))

# lambda_0_iv <- copy(lambda_0)
# 
# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]

k<-2
#j is the variable of interest
for (j in c(1:n)){
  for (shock in c(1:k)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0_iv[hori,j, shock]*structural_shocks_IV[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo[t,j, shock] <- contribution
    }
  }
  
}



historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
historical_decomp_extended[,,1:n_vars] <- hd_cholesky
historical_decomp_extended[,,(n_vars +1)] <- hd_controls

historical_decomp_extended_index <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
historical_decomp_extended_index[,,1:n_vars] <- hd_cholesky_index
historical_decomp_extended_index[,,(n_vars +1)] <- hd_controls


save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo, "hd_iv_fr.rds")
saveRDS(hd_cholesky, "hd_chol_fr.rds")
saveRDS(historical_decomp_extended, "hd_chol_gas_fr.rds")




#Cumulative contribution to Inflation
compute_contributions_with_gas <- function(start_date, end_date, 
                                  dates_formatted, 
                                  IV_data_log_diff_fr, 
                                  hd_cholesky, 
                                  historical_decomp_iv_tibo, 
                                  series = 4) {
  
  # Generate the date sequence and convert to yearmon
  desired_period <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  desired_period_yearmon <- as.yearmon(desired_period)
  
  # Get matching indices
  matching_indices <- which(dates_formatted %in% desired_period_yearmon)
  
  # Total inflation during the period
  cumulative_inflation <- sum(IV_data_log_diff_fr$HICP_log_diff[p+matching_indices])
  
  # Helper function to compute contribution if column exists
  safe_contrib <- function(array, series, col_index, indices) {
    if (dim(array)[3] >= col_index) {
      return(sum(array[indices, series, col_index]) / cumulative_inflation)
    } else {
      return(NA_real_)
    }
  }
  # Compute all contributions (NA if not present)
  contribs <- list(
    Cholesky = list(
      "$P_{elec}$" = safe_contrib(hd_cholesky, series, 2, matching_indices),
      "$Q_{elec}$" = safe_contrib(hd_cholesky, series, 1, matching_indices),
      "$P_{gas}$"  = safe_contrib(hd_cholesky, series, 6, matching_indices),
      "Mon. Policy" = safe_contrib(hd_cholesky, series, 5, matching_indices)
    ),
    IV = list(
      "$P_{elec}$" = safe_contrib(historical_decomp_iv_tibo, series, 2, matching_indices),
      "$Q_{elec}$" = safe_contrib(historical_decomp_iv_tibo, series, 1, matching_indices),
      "$P_{gas}$"  = safe_contrib(historical_decomp_iv_tibo, series, 6, matching_indices),
      "Mon. Policy" = safe_contrib(historical_decomp_iv_tibo, series, 5, matching_indices)
    )
  )
  
  # Assemble tidy tibble (filtering out NAs)
  result <- bind_rows(
    lapply(names(contribs), function(id_type) {
      tibble(
        Identification = id_type,
        Contribution_Type = names(contribs[[id_type]]),
        Contribution = 100 * unlist(contribs[[id_type]])
      )
    })
  ) %>%
    filter(!is.na(Contribution))
  
  return(result)
}

contrib_table_with_gas <- compute_contributions_with_gas(
  start_date = "2021-07-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_fr = IV_data_log_diff_fr,
  hd_cholesky = historical_decomp_extended,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contrib_table_wide_with_gas <- contrib_table_with_gas %>%
  pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )




contribution_data_with_gas_2 <- compute_contributions(
  start_date = "2021-01-01",
  end_date = "2022-07-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_fr = IV_data_log_diff_fr,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide_with_gas_2 <- contribution_data_with_gas_2 %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )


contribution_data_with_gas_3 <- compute_contributions(
  start_date = "2021-01-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_fr = IV_data_log_diff_fr,
  hd_cholesky = hd_cholesky,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo,
  series = 4
)

contribution_table_wide_with_gas_3 <- contribution_data_with_gas_3 %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )




gt_table <- contrib_table_wide_with_gas %>%
  gt() %>%
  tab_header(
    title = md("**Cumulative Contribution to Inflation**"),
    subtitle = md("From 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  fmt_missing(
    columns = everything(),  # Or specify: c("$P_{elec}$", "$Q_{elec}$", ...)
    missing_text = "-"
  ) %>%
  cols_label(
    Identification = "Identification",
    "$P_{elec}$" = html("P<sub>elec</sub>"),
    "$Q_{elec}$" = html("Q<sub>elec</sub>"),
    "$P_{gas}$" = html("P<sub>gas</sub>"),
    "Mon. Policy" = "Mon Policy"
  )
gt_table




#unify in a single table





# Les types de contribution que tu veux voir, y compris gas et mon pol
all_types <- c("$P_{elec}$", "$Q_{elec}$", "$P_{gas}$", "Mon. Policy")

# Tous les Identification possibles
all_id <- unique(contribution_data$Identification)
# Créer toutes les combinaisons
full_contribution_table <- tidyr::expand_grid(
  Identification = all_id,
  Contribution_Type = all_types
) %>%
  # Left join pour compléter les données manquantes par NA
  left_join(contribution_data, by = c("Identification", "Contribution_Type")) %>%
  # Remplacer NA par NA (pour gt, on formate après)
  mutate(Contribution = ifelse(is.na(Contribution), NA_real_, Contribution))



# Ajouter une colonne pour distinguer les deux sous-tables
table1 <- full_contribution_table %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  ) %>%
  mutate(group = "Without Gas and Monetary Policy")




table2 <- contrib_table_wide_with_gas %>%
  mutate(group = "With Gas and Monetary Policy")
# Pour que les colonnes soient compatibles, ajouter les colonnes manquantes dans chaque table
cols_needed <- union(names(table1), names(table2))

table1 <- table1 %>% 
  mutate(across(setdiff(cols_needed, names(table1)), ~NA_real_))

table2 <- table2 %>% 
  mutate(across(setdiff(cols_needed, names(table2)), ~NA_real_))

# Reordonner colonnes pour qu'elles correspondent
table1 <- table1 %>% dplyr::select(all_of(cols_needed))
table2 <- table2 %>% dplyr::select(all_of(cols_needed))


# Concaténation
combined_table <- bind_rows(table1, table2) %>%
  # Pour faciliter la création de row_groups, on ajoute un id de ligne
  mutate(row_id = row_number())

# Créer GT
gt_tbl <- combined_table %>%
  dplyr::select(-row_id, -group) %>%  # supprimer colonne temporaire si inutile à afficher
  gt(rowname_col = "Identification") %>%   # "Identification" comme rowname si souhaité
  tab_header(
    title = md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  cols_label(
    `$P_{elec}$` = html("P<sub>elec</sub>"),
    `$Q_{elec}$` = html("Q<sub>elec</sub>"),
    `$P_{gas}$` = html("P<sub>gas</sub>"),
    `Mon. Policy` = "Mon Policy"
  ) %>%
  # Ajouter les row groups basés sur la variable "group"
  tab_row_group(
    label = "Controlling for Gas and Monetary Policy",
    rows = c(3:4)
  ) %>%
  tab_row_group(
    label = "",
    rows =  c(1:2)
  )

gt_tbl







