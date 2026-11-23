library(vars)
source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")


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
cumulative_response <- apply(lambda_0_log_diff , c(2, 3), function(x) cumsum(x))
cumulative_response
