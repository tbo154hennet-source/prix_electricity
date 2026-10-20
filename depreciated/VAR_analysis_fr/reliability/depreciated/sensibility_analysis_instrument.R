
#contrast the estimates of theta 0 
#response on impact to 
# the different esets of instruments
proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_esp)], frequency=12)
proxies[is.na(proxies)] <- 0
p = 12 
irhor = 20
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

#controls = ts(IV_data_log_diff_esp[, headers_controls_esp], frequency = 12)
estimate_log_diff <- first_var_estimation(x,y,p, proxies, irhor)
var_coef_diff <- estimate_diff$coef

AL <- do.call(cbind, lapply(var_coef_diff, function(df) df[, 1]))
names(AL) <- names(var_coef_diff)

C <- MARep(t(AL), p, irhor)
lambda_0_diff <- get_lambda_0(C, theta_0_diff, irhor =irhor)



#draw proxies at random and using them as 
# one plot with all the different possibilities of final estimate



sensitivity_proxy <- function(headers_proxies, x,y,p){
  proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_esp)], frequency=12)
  proxies[is.na(proxies)] <- 0
  
  
}


colnames(IV_data_log_diff_esp)

headers_proxies_esp = c(
  "cold_excess_instrument",
  "heat_excess_instrument",
  "renewable_day_ahead_forecast_esp_log_diff",
  "diff_cbf",
  "cap_gap_diff",
  "predicted_unavail_log_diff"
)


for (proxy in headers_proxies_esp){
  proxies <- ts(IV_data_log_diff_esp[, proxy], frequency=12)
  proxies[is.na(proxies)] <- 0
  estimate_log_diff <- first_var_estimation(x,y,p, proxies, irhor)
  theta_0_log_diff <- estimate_log_diff$theta_0
  print(theta_0_log_diff)
}



#get the irf numbers for inflations long-term
#get the 











