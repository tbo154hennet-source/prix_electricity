#install.packages("R.matlab")  # Install if not already installed
library(R.matlab)

setwd("C:/Users/DBT3KD/Downloads/uncertainty_materials-main/Kaenzig_replication/data")
# Load .mat file
data <- readMat("dataBaseM.mat")
class(data)
length(data)
# View the structure
str(data)

vars_kanzig <- ts(data$data, frequency = 12)
#proxies_kanzig <- ts(data$dataExo, frequency = 12)
kanzig_dates <- unlist(data$sampleDates)

library(dplyr)
library(tidyr)
library(readxl)
setwd("C:/Users/DBT3KD/Desktop/uncertainty_shocks/DATA")
kanzig_serie <- read_excel("oilSupplyNewsShocks_2024M06.xlsx", sheet = "Monthly")
kanzig_serie <- kanzig_serie %>% 
  mutate(year = as.numeric(substr(Date, 1, 4)),  # Prend les 4 premiers caractères → "1975"
         month = as.numeric(substr(Date, 6, 7)) 
  )
proxies_kanzig <- kanzig_serie %>% 
  dplyr::select(Date, `Oil supply surprise series`) %>% 
  filter(Date %in% kanzig_dates) %>% 
  pull(`Oil supply surprise series`)


#il faut rajouter des zéros pour les douze premières valeurs de proxies
#qui ne seront pas utilisées
proxies_kanzig <- c(rep(0, 12), proxies_kanzig)


start_year = as.numeric(substr(kanzig_dates[1], 1, 4))
start_month = as.numeric(substr(kanzig_dates[1], 6, 7))
end_year = as.numeric(substr(kanzig_dates[length(kanzig_dates)], 1, 4))
end_month = as.numeric(substr(kanzig_dates[length(kanzig_dates)], 6, 7))


vars <- vars_kanzig
proxies <- proxies_kanzig

length(proxies_kanzig)
nrow(vars_kanzig)
length(kanzig_dates)

p = 12     
irhor = 20
x =ts(vars_kanzig , start=c(start_year,start_month), end=c(end_year,end_month), frequency=12)
y =vars_kanzig[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <-1
# Create a 3x2 matrix of zeros
zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
#my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)

matrix_12 <- rbind( diag(k) , zero_matrix)


estimate_log <- first_var_estimation(x,y,p, proxies_kanzig, irhor)
theta_0_log <- estimate_log$theta_0
gamma_log <- estimate_log$gamma
phi_log <- estimate_log$phi
var_coef_log <- estimate_log$coef

AL <- do.call(cbind, lapply(var_coef_log, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log)
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n <- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_log <- get_lambda_0(C, theta_0_log)



nboot<-100
gamma_estimate_log_GK <- gamma_bootstrap_GK_oil(nboot, vars = vars_kanzig, theta_0_log, proxies = proxies_kanzig,
                                                start=c(start_year,start_month), end=c(end_year,end_month) )


list_of_samples <- c("gamma", "psi", "ncvg_theta_0", "inv",
                     "ncvg_lambda_01", "ncvg_lambda_02")
sample_matrices_log_GK <- vector("list", length(list_of_samples))
cov_matrices <- vector("list", length(list_of_samples))
names(sample_matrices_log_GK) <- list_of_samples

# Extract and store bootstrapped samples
for (name in list_of_samples) {
  sample_matrices_log_GK[[name]] <- gamma_estimate_log_GK[[paste0("boot_", name)]]
}


save_mat_path <- 'C:/Users/DBT3KD/Desktop/uncertainty_shocks/uncertainty_materials_gitlab/VAR_analysis/output/sample_matrix'
setwd(save_mat_path)
saveRDS(sample_matrices_log_GK, "sample_matrices_log_GK_kanzig.rds")

dim(sample_matrices_log_GK$ncvg_lambda_01)
# Save bootstrapped samples
# Initialize a 3D array to store the quantiles for both lambda_01 and lambda_02
# Dimensions: (irhor, n, 2) for lower (10%) and upper (90%) quantiles



# Calculate quantiles and store them in the array
quantile_probs <- c(0.05, 0.95)
#print(class(sample_matrices_log_GK[[lambda_name]]))
strong_instruments_lambda_log_GK_90 <- apply(sample_matrices_log_GK$ncvg_lambda_01,
                                            c(1:2), function(x) quantile(x, probs = quantile_probs))


quantile_probs <- c(0.16, 0.84)
strong_instruments_lambda_log_GK_68 <- apply(sample_matrices_log_GK$ncvg_lambda_01,
                                             c(1:2), function(x) quantile(x, probs = quantile_probs))



library(latex2exp)


#names <- c('$P_{oil}$', '$Q_{oil}$', "$IPI_{world}$")
names <- c('$P_{oil}$', '$Q_{oil}$','$Stocks_{oil}$',
           "$IPI_{world}$", '$IPI_{US}$', '$CPI_{US}$')

n <- length(names)
plot_irf_mini_panel_chol <- function(i){
  #i=1
  main_prediction <- lambda_0_log[, i, 1]
  
  time_horizon <- 1:20
  strong_band_inf_GK_90 <-
    strong_instruments_lambda_log_GK_90[1,,i]
  strong_band_sup_GK_90 <- 
    strong_instruments_lambda_log_GK_90[2,,i]
  
  strong_band_inf_GK_68 <-
    strong_instruments_lambda_log_GK_68[1,,i]
  strong_band_sup_GK_68 <- 
    strong_instruments_lambda_log_GK_68[2,,i]
  
  # Plot the responses for each variable
  
  my_plot <- ggplot()+
    
    geom_ribbon(aes(x=time_horizon, ymin = strong_band_inf_GK_90,
                    ymax = strong_band_sup_GK_90), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x=time_horizon, ymin = strong_band_inf_GK_68,
                    ymax = strong_band_sup_GK_68), fill = "lightblue", alpha = 0.5) +
    
    
    # ggtitle(expression(paste(names[i]))) +
    labs(title = TeX(names[i]),
         x = "Time",
         y = "Unit Points") +
    geom_line(aes(x=time_horizon, y= main_prediction), color = "blue", linetype='dashed')+
    geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
    scale_x_continuous()+
    scale_y_continuous()+
    # Add confidence bands
    theme_minimal()
  return(my_plot)
}


# Apply the function to the sequence 3:10
results <- lapply(c(1:n) , plot_irf_mini_panel_chol)

# Arrange the plots in a grid
plots_grid <- do.call("grid.arrange", c(results, nrow = 2))





