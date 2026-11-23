library(vars)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter_bis.R")

#install.packages("R.matlab")  # Install if not already installed
library(R.matlab)
library(dplyr)
library(tidyr)
library(readxl)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Kaenzig_replication/data")
# Load .mat file
data <- readMat("dataBaseM.mat")
data$sampleDates[1]

vars_kanzig <- ts(data$data, frequency = 12)
#proxies_kanzig <- ts(data$dataExo, frequency = 12)
kanzig_dates <- unlist(data$sampleDates)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Kaenzig_replication/instrument")
proxy_matlab <- readMat("OilSurprisesMLog.mat")
proxy_matlab$sampleDatesProxy[[1]][[1]][[1]]
proxy_matlab$sampleDatesProxy[[528]][[1]][[1]]

# Si nécessaire, convertir en data.frame
oil_proxies <- as.data.frame(proxy_matlab$oilProxiesWTIM)
oil_proxies_scaled <- scale(oil_proxies)
# Vérifier les données
pca_result <- prcomp(oil_proxies_scaled, center = TRUE, scale. = TRUE)
# PC1 : scores de la première composante pour chaque observation
PC1 <- pca_result$x[,1]


proxies_kanzig <- ts(PC1 , frequency=12)
proxies = proxies_kanzig

#vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp)], frequency=12)

p = 12
irhor = 50
x =ts(vars_kanzig  , start=c(1974,1), end=c(2017,12), frequency=12)
y =vars_kanzig[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- 1

vl <- if (is.null(controls)) {
  vars::VAR(x, p, type = "none")
} else {
  vars::VAR(x, p, type = "none", exogen = controls)
}

library(ggplot2)

# Dates de début et fin
start_date <- as.Date("1974-01-01")
end_date   <- as.Date("2017-12-01")

# Générer toutes les dates mensuelles
dates_monthly <- seq(from = start_date, to = end_date, by = "month")

# Vérifierdf <- data.frame(
date  = dates_monthly
price = vars_kanzig[,1]  # première colonne

df <- data.frame(
  date  = dates_monthly,
  price = vars_kanzig[,1]  # première colonne
)

ggplot(df, aes(x = date, y = price)) +
  geom_line(color = "blue") +
  labs(title = "Prix dans le temps", x = "Date", y = "Prix") +
  theme_minimal()

class(vars)
class(vars_kanzig)
class(proxies_kanzig)

estimate_log_diff <- first_var_estimation(x,y,p, proxies_kanzig, irhor)
theta_0_log_diff <- estimate_log_diff$theta_0
gamma_log_diff <- estimate_log_diff$gamma
phi_log_diff <- estimate_log_diff$phi
var_coef_log_diff <- estimate_log_diff$coef


AL <- do.call(cbind, lapply(var_coef_log_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log_diff)
p <- 12  # Number of lags in the VAR model
hori <- 50 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_log_diff <- get_lambda_0(C, theta_0_log_diff, irhor=50)
nboot = 100
block_size = 24

# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x, p, type = "none")
} else {
  vars::VAR(x, p, type = "none", exogen = controls)
}

#k  = ncol(proxies)
#n = ncol(y)

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)


theta_0_log_diff <- first_var_estimation_bis(y, res, p, proxies_kanzig, irhor, controls = NULL, elas = 0)$theta_0

bootIRFs <-gamma_bootstrap_mbb_corrected_final(x, nboot,  
                                         theta_0 = theta_0_log_diff , proxies = proxies_kanzig, controls=NULL, elas = 0,
                                         start_ts = start_date, end_ts =end_date, irhor = irhor)
# 90% confidence bands (5%-95%)
IRF_90 <- compute_quantile_bands(bootIRFs, quantile_probs_90)
IRF_68 <- compute_quantile_bands(bootIRFs, quantile_probs_68)
IRF_50 <- compute_quantile_bands(bootIRFs, quantile_probs_50)[1,,]


gamma_estimate_diff_MBB <- gamma_bootstrap_mbb_alternative_bis(
  x = x,
  nboot = nboot,
  vars = vars_kanzig,
  BlockSize = block_size ,
  theta_0 = theta_0_log_diff,
  proxies = proxies_kanzig,
  controls = NULL,
  start_ts = c(1974,1), 
  end_ts = c(2017,12)
)

list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                     "ncvg_lambda_01")
sample_matrices <- lapply(list_of_samples, function(name) {
  gamma_estimate_diff_MBB[[paste0("boot_", name)]]
})

get_strong_instruments_1var <- function(sample_matrices, quantile_probs){
  # Calcul des quantiles pour lambda_01 et lambda_02
  strong_instr_array <- array(NA, dim = c(2, irhor, n))
  dimnames(strong_instr_array) <- list(
    c("lower", "upper"), NULL, NULL
  )
  
  
  quantiles <- apply(sample_matrices[["ncvg_lambda_01"]],
                     c(1, 2), function(x) quantile(x, probs = quantile_probs))
  
  strong_instr_array[,,] <- array(quantiles, dim = c(irhor, n, 2))
  
  strong_instr_array <- aperm(strong_instr_array, perm = c(2, 3, 1))
  
  return(strong_instr_array)
}
names(sample_matrices) <- list_of_samples
quantile_probs_90 <- c(0.05, 0.95)
quantile_probs_68 <- c(0.16, 0.84)
quantile_prob_50 <- c(0.5, 0.5)
bandwith_90 <- get_strong_instruments_1var(sample_matrices, quantile_probs_90)
bandwith_68 <- get_strong_instruments_1var(sample_matrices, quantile_probs_68)
median_50 <- get_strong_instruments_1var(sample_matrices, quantile_prob_50)











library(tseries)   # pour adf.test et kpss.test
library(urca)      # pour PP test si nécessaire

stationarity_tests <- function(series) {
  # ADF
  adf_p <- tryCatch({
    adf.test(series)$p.value
  }, error = function(e) NA)
  
  # KPSS
  kpss_p <- tryCatch({
    kpss.test(series, null = "Level")$p.value[1]  # parfois vecteur
  }, error = function(e) NA)
  
  
  # PP test avec urca
  pp_p <- tryCatch({
    pp_test <- ur.pp(series, type = "Z-alpha", model = "trend")
    # Extraire le p-value approximatif via testreg si possible
    pp_test@teststat
  }, error = function(e) NA)
  
  data.frame(ADF_p = adf_p,
             KPSS_p = kpss_p,
             PP_p = pp_p)
}
series <- vars_kanzig[,1]
# Appliquer à toutes les colonnes
results <- lapply(as.data.frame(vars_kanzig), stationarity_tests)

# Compiler en table
stationarity_table <- bind_rows(results, .id = "Series")
stationarity_table$Series <- colnames(vars_kanzig)

stationarity_table


