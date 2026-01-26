library(vars)
source_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/auxiliary'
setwd(source_path)
source("auxiliary_control_bis.R")
source("bootstrap_auxiliary_control_ter.R")


save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff_elas_sd.parquet')

# correct for the Iberian exception
colnames(IV_data_log_diff_esp)
IV_data_log_diff_esp$observation_date <- as.Date(IV_data_log_diff_esp$observation_date)
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply),
    ffr_diff = ffr - lag(as.numeric(ffr)),
    ffr_diff = if_else(is.na(ffr_diff), 0, ffr_diff)
  ) 
# 
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    #ffr_diff = ffr - lag(as.numeric(ffr)),
    delta_s_tot_diff = delta_s_tot - lag(as.numeric(delta_s_tot))/1000,
    delta_d_tot_diff = delta_d_tot - lag(as.numeric(delta_d_tot))/1000,
    delta_s_orth_diff = delta_s_orth - lag(as.numeric(delta_s_orth))/1000,
    delta_d_orth_diff = delta_d_orth - lag(as.numeric(delta_d_orth))/1000,
    delta_s_tot_diff = if_else(is.na(delta_s_tot_diff), mean(delta_s_tot_diff, na.rm = TRUE), delta_s_tot_diff),
    delta_d_tot_diff = if_else(is.na(delta_d_tot_diff),  mean(delta_d_tot_diff, na.rm = TRUE), delta_d_tot_diff),
    delta_s_orth_diff = if_else(is.na(delta_s_orth_diff),  mean(delta_s_orth_diff, na.rm = TRUE), delta_s_orth_diff),
    delta_d_orth_diff = if_else(is.na(delta_d_orth_diff),  mean(delta_d_orth_diff, na.rm = TRUE), delta_d_orth_diff)
  )


#   
headers_proxies_supply = c(
  "delta_s_orth_diff"#,
  #"delta_d_tot_diff"
)
headers_proxies_demand = c(
  #"delta_s_tot_diff"#,
  "delta_d_orth_diff"
)
# headers_proxies_supply_demand = c(
#   "delta_s_tot_diff",#,
#   "delta_d_tot_diff"
# )
# headers_proxies_supply <- c("delta_s_orth_diff")
# headers_proxies_demand <- c("delta_d_orth_diff")

headers_var_spot_first <- c("spot_price_log_diff",
                            "gen_tot_log_diff",
                            "IPI_log_diff",
                            "HICP_log_diff")

headers_var_spot_first_mp = c(
  "spot_price_log_diff",
  "gen_tot_log_diff",
  "IPI_log_diff",
  "HICP_log_diff",
  "ffr_diff"
)

headers_var_gen_first <- c(
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff")

headers_var_gen_first_mp = c(
  
  "gen_tot_log_diff",
  "spot_price_log_diff",
  "IPI_log_diff",
  "HICP_log_diff",
  "ffr_diff"
)


# result_esp_sd_price <- run_iv_analysis(my_IV_data = IV_data_log_diff_esp, 
#                                        headers_var_spot_first, c("delta_s_tot_diff"), p =4, 
#                                        dependent = "spot_price_log_diff", k = 1)
# result_esp_sd_price$fstat
# print(summary(result_esp_sd_price$m_summary))
# result_esp_sd_price$result_qty$gmin_generalized
# result_esp_sd_price$result_IPI$gmin_generalized
# result_esp_sd_price$result_HICP$gmin_generalized



proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_supply)], frequency=12)
proxies[is.na(proxies)] <- 0

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp)], frequency=12)

p = 12
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- ncol(proxies)
# # Create a 3x2 matrix of zeros
# zero_matrix <- matrix(0, nrow = (n-k), ncol = 2)
# #my_diag <- matrix(c(1,0,0,1), nrow=2, ncol=2)
# 
# matrix_12 <- rbind( diag(k) , zero_matrix)
# 
# #controls = ts(IV_data_log_diff_esp[, headers_controls_esp], frequency = 12)
# #nbre de variables dans la VAR
# headers_controls = c("avg_spot_price_log_diff")



estimate_log_diff <- first_var_estimation(x,y,p, proxies, irhor)
theta_0_log_diff <- estimate_log_diff$theta_0
gamma_log_diff <- estimate_log_diff$gamma
phi_log_diff <- estimate_log_diff$phi
var_coef_log_diff <- estimate_log_diff$coef

AL <- do.call(cbind, lapply(var_coef_log_diff, function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(var_coef_log_diff)
p <- 12  # Number of lags in the VAR model
hori <- 20 # Forecast horizon
n<- ncol(y)
# Caux        = [eye(VAR.n),MARep(AL,VAR.p,VAR.irhor-1)]
# Step 1: Compute MA coefficients for VAR.p and VAR.irhor-1
C  <- MARep(t(AL), p, hori)


from <- (l+1)
to <- l*p
to_bis <- l*(p-1)

lambda_0_log_diff <- get_lambda_0(C, theta_0_log_diff)
cumulative_response <- apply(lambda_0_log_diff , c(2, 3), function(x) cumsum(x))
#cumulative_response /cumulative_response[1,2,1]

#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 1] # 0.0055844890
print(passthrough_esp)

