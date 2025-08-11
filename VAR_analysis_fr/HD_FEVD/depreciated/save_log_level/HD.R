

#For France
headers_var_gen_first <- c(
  "gen_tot_log",
  "spot_price_log",
  "IPI_log",
  "HICP_log"
)


p = 4     
irhor = 40


proxies <- ts(IV_data_log_esp[, c(headers_proxies)], frequency=12)
proxies[is.na(proxies)] <- 0
vars<- ts(IV_data_log_esp[, c(headers_var_gen_first)], frequency=12)

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


controls <- ts(IV_data_log_esp[, headers_controls_esp] , start=c(2015,1), end=c(2023,12), frequency=12)

estimate_log <- first_var_estimation(x,y,p, proxies, irhor, controls = controls)
theta_0 <- estimate_log$theta_0
gamma <- estimate_log$gamma
phi <- estimate_log$phi
var_coef_log <- estimate_log$coef

AL <- do.call(cbind, lapply(var_coef_log, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log)

library(latex2exp)
colors_shock <- c("blue", "red", "green", "yellow")
names <- c("$Q_{elec}$" ,"$P_{elec}$", "IPI",  "$HICP$")
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

irf_cholesky <- irf(vl, impulse = colnames(x), response = colnames(x),
                    n.ahead = (irhor - 1), ortho = TRUE, boot = TRUE)
#plot(irf_cholesky)
# save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds'
# setwd(save_rds_path)
# saveRDS(irf_cholesky, file = "irf_cholesky_esp.rds")



# Perform Cholesky decomposition
P <- chol(Sigma_eta)  # Cholesky decomposition of Σ_u

# Transform residuals into structural shocks using P
structural_shocks <-   res %*%  solve(P)


# Compute historical decomposition for each variable
# Initialize matrix to store historical decomposition
n_obs <- nrow(res)
n_vars <- ncol(res)
hd_cholesky <-array(NA, dim = c(n_obs,  n_vars, n_vars))

# class(irf_model$irf)
# length(irf_model$irf)
# nrow(irf_model$irf[[1]])

#j is the variable of interest
for (j in c(1:n)){
  for (shock in c(1:n)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- irf_cholesky$irf[[shock]][hori,j]*structural_shocks[shock_horizon,shock]
        # contrib_hori <- irf_model$irf[[shock]][hori,j]*
        #   structural_shocks[shock_horizon,shock]
        # 
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      hd_cholesky[t,j, shock ] <- contribution
    }
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


#Plot the shocks
data_IV <- as.data.frame(t(structural_shocks_IV))
colnames(data_IV) <- c("price", "quantity")

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


hori <- 40 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_iv <- get_lambda_0(C, theta_0, irhor= hori)
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

setwd(save_rds_path)
saveRDS(historical_decomp_iv_tibo, "hd_iv.rds")
saveRDS(hd_cholesky, "hd_chol.rds")


# hd_pelec <- plot_hd(historical_decomp_iv_tibo, series = 1)
# print(hd_pelec)





#On se demande à quel point il ne faut pas ajuster les structural shocks IV en renormalisant
#on regarde des summary stat

# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest



get_var_explained <- function(total_var, hd, var_idx, shock_idx){
  contrib <- hd[, var_idx, shock_idx]
  explained_var <- sum(contrib^2)
  percentage_explained <- 100 * explained_var / total_var
  return(percentage_explained)
}


library(flextable)

# Calcul identique à celui déjà fait

var_idx <- 5 # Index for "spot_price"

var_names <- c("gen_tot_log", "spot_price_log")
pretty_names <- c("Q_elec", "P_elec")  # can't use LaTeX in flextable

results_var <- matrix(NA, nrow = 2, ncol = 2)
rownames(results_var) <- c("Cholesky", "IV")
colnames(results_var) <- pretty_names

for (i in 1:2) {
  var_name <- var_names[i]
  shock_idx <- which(colnames(x) == var_name)
  total_var <- sum(apply(hd_cholesky[, var_idx, ], 2, function(x) sum(x^2)))
  results_var["Cholesky", i] <- get_var_explained(total_var, hd_cholesky, var_idx, shock_idx)
  results_var["IV", i] <- get_var_explained(total_var, historical_decomp_iv_tibo, var_idx, shock_idx)
}

# Convert to data frame
results_var_df <- as.data.frame(results_var)
results_var_df$Identification <- rownames(results_var_df)

library(gt)
# Formattage du tableau


gt_table <- results_var_df %>%
  gt(rowname_col = "Identification") %>%
  tab_header(
    title = "Historical Variance Explained (in %)"
  ) %>%
  cols_label(
    Q_elec = "Q_elec",
    P_elec = "P_elec"
  ) %>%
  fmt_number(
    columns = c(Q_elec, P_elec),
    decimals = 1
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  opt_table_lines(extent = "all")

gt_table

install.packages("webshot2")
library(webshot2)  #  si besoin
gtsave(gt_table, "variance_expliquee.png")



#je pense que ça revient à multiplier et diviser par la sd
# pas de correction à faire


