library(MASS)


source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/reliability'
setwd(source_path)
source("reliability_auxiliary.R")

source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis'
setwd(source_path)
source("auxiliary_control.R")

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')

#IV_data_log_esp$renewable_forecast_log <- IV_data_log_fr$renewable_forecast_log + IV_data_log_esp$renewable_forecast_log
colnames(IV_data_log_esp)

#"renewable_forecast_hors_esp_log",

headers_proxies = c(
  # "T_avg_log",
  # "cold_excess_instrument",
  # "heat_excess_instrument",
  # "wind_forecast_log",
  # "solar_forecast_log",
  #"cbf_es_log",
  #"renewable_forecast_log",
  #"renewable_forecast_hors_esp_weighted_log",
  "PC1_supply",
  "log_diff_renewable"
  # "NTC_ESP_FR_log",
  # "NTC_FR_ESP_log"
  #"nuclear_unavailabilities_log"
  # "thermal_unavailabilities_log",
  #"predicted_unavailabilities_log"
)

colnames(IV_data_log_esp)



proxies <- ts(IV_data_log_esp[, c(headers_proxies)], frequency=12)


headers_var = c("spot_price_log",
                "gen_tot_log",
                "IPI_log",
                # "ECB_rate_log",
                # "E_Index_log",
                # "Exp_pi_log",
                # "UnE_log" ,
                "NE_Index_log"
)
vars<- ts(IV_data_log_esp[, c(headers_var)], frequency=12)



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



data <- data.frame(cbind(res[,1], Z))
colnames(data) <- c('res_spot_price_log',headers_proxies)

data$iberic_exception <- ifelse(rownames(data) %in% 75:87, 1, 0)

#dates exception ibérique ("15-06-2022  " 01-03-2023
#on peut interargir avec cette période


# Construct the formula
formula_price <- as.formula(paste('res_spot_price_log', "~",
                                  paste(headers_proxies, collapse = "+")))

m_price_esp <- lm( formula_price, data = data)

# formula_price_int <- "res_spot_price_log ~ renewable_forecast_log + PC1_supply + NTC_ESP_FR_log*iberic_exception + 
#   NTC_FR_ESP_log*iberic_exception + predicted_unavailabilities_log"
# 
# m_price_esp <- lm( formula_price_int, data = data)
# 

# Get all the statistical coefficients
coef_price <- summary(m_price_esp)$coefficients
# Convert to a data frame for easier subsetting
coef_price_df <- as.data.frame(coef_price)
# Subset the data frame to keep only significant coefficients at the 10 percent level
coef_price_df <- coef_price_df[coef_price_df[["Pr(>|t|)"]] < 0.10, ]
# Apply the function to the p-values and create new row names
significance_stars <- sapply(coef_price_df[, "Pr(>|t|)"], get_significance_stars)

price_names <- paste(rownames(coef_price_df), significance_stars)

# the R^2 in the regression 
fstat_price <- as.numeric(summary(m_price_esp)$fstat[1])# Get all the statistical coefficients


print(fstat_price)

summary(m_price_esp)









