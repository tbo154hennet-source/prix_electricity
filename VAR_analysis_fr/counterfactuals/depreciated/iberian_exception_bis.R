library(MASS)



proxies <- ts(IV_data_log_diff_esp[, c(headers_proxies_supply)], frequency=12)
proxies[is.na(proxies)] <- 0

vars<- ts(IV_data_log_diff_esp[, c(headers_var_spot_first_mp)], frequency=12)

p = 12
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
## ---- 3. Exemple d’utilisation avec tes objets ----
# Suppose qu’on veut récupérer le choc à T+1
# y_Tplus1_obs : la vraie observation (n x 1)
# y_lags : matrice (n x p) avec les p retards au temps T
# AL : déjà construit (n x n*p)
# theta_0_log_diff : impact matrix (n x n)
vars_gas <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_var_spot_first_mp)),
  frequency = 12
)
x_gas =ts(vars_gas, start=c(2015,1), end=c(2022,5), frequency=12)
y_gas =vars_gas[-1:-p, ]
T_gas = nrow(y_gas)




vl_gas <- if (is.null(controls)) {
  vars::VAR(x_gas, p, type = "none")
} else {
  vars::VAR(x_gas, p, type = "none", exogen = controls)
}
AL <- do.call(cbind, lapply(coef(vl_gas) , function(df) df[, 1]))
# Optionally, set column names if needed
names(AL) <- names(coef(vl_gas))

dim(y)
# ex:
y_Tplus1_obs <- as.numeric(y[(T_gas+1), ])  # la première ligne de y après estimation
y_lags <- t(y[T_gas:(T_gas-p+1), ])                # matrice des p retards au temps T

shock_Tplus1 <- recover_single_shock_IV(
  y_obs  = y_Tplus1_obs ,    # observed at T+1
  y_lags = y_lags,           # ensure order [y_T, y_{T-1}, ...]
  AL     = AL,
  B      = theta_0_log_diff,
  p      = p
)

print(shock_Tplus1)  # vecteur des chocs structurels (1er = policy shock)




res_gas = residuals(vl_gas)

vars_gas_pred <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date <= as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_var_spot_first_mp)),
  frequency = 12
)

check_coeherence <- iterate_var_ter(x= x_gas_pred, initial_obs = vars_gas_pred[1:p,], 
                                  errors = rbind(res_gas, c(theta_0_log_diff *shock_Tplus1)), var_model = vl_gas)




#vérification que shock_Tplus1 permet de combler mon écart prédiction vs observed
# 
# controls = NULL
# # Fit the VAR model
# 

# 

# 
# 
# my_pred <-  iterate_var_ter(x= x_gas_pred, initial_obs = vars_gas_pred[1:p,], 
#                             errors = rbind(res_gas, rep(0,5)), var_model = vl_gas)



save_rds_path_hd <- "Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/structur_shock"
setwd(save_rds_path_hd)
structural_shocks_IV_spot <- read_parquet("structural_shocks_IV_spot.parquet" )

structural_shocks_IV_spot[(T_gas-1):(T_gas+3)]
summary(t(structural_shocks_IV_spot))
#one time shock that lowers electricity price to some value
#rejoue avec les structural shock identifiés
#on veut enlever le structural shock très négatif de la politique


counterfactual <- iterate_var_ter(x= x_gas_pred, initial_obs = vars_gas_pred[1:p,], 
                                  errors = rbind(res_gas, c(theta_0_log_diff *shock_Tplus1)), var_model = vl_gas)




#prediction prix de l'elec avec un prix du gas qui monte
#de combien
#on supppose économie reste la même
#on garde les valeurs observées de l'économie
#correspond à utiliser ne control
#en gardant les valeurs de l'économie observées


vars_gas <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_var_spot_first_mp)),
  frequency = 12
)



headers_controls = c("avg_spot_price_log_diff")
headers_controls_elec = c("spot_gas_capped_log_diff")

controls_gas_elec <- ts(
  IV_data_log_diff_esp %>%
    dplyr::select(all_of(headers_controls_elec)),
  frequency = 12
)

controls_gas <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(headers_controls)),
  frequency = 12
)
controls_futures <-  ts(
  IV_data_log_diff_esp %>%
    filter(observation_date >= as.Date("2022-06-01") & year <= 2022) %>%
    dplyr::select(all_of(headers_controls)),
  frequency = 12
)
controls_gas_tot <- ts(
  IV_data_log_diff_esp %>%
    dplyr::select(all_of(headers_controls)),
  frequency = 12
)


vars_tot <- ts(
  IV_data_log_diff_esp %>%
    filter(observation_date < as.Date("2022-06-01")) %>%
    dplyr::select(all_of(c(headers_var_spot_first_mp, headers_controls))),
  frequency = 12
)
x_tot =ts(vars_tot, start=c(2015,1), end=c(2022,5), frequency=12)
y_tot =vars_tot[-1:-p, ]
vl_tot <- vars::VAR(x_tot, p, type = "none")



vl_gas_elec <- if (is.null(controls_gas_elec)) {
  vars::VAR(x, p, type = "none")
} else {
  vars::VAR(x, p, type = "none", exogen = controls_gas_elec)
}
coef(vl_gas_elec)$spot_price_log_diff


vl_gas_count <- if (is.null(controls_gas)) {
  vars::VAR(x_gas, p, type = "none")
} else {
  vars::VAR(x_gas, p, type = "none", exogen = controls_gas)
}
coef(vl_gas_count)$spot_price_log_diff


x_gas_pred = ts(vars_gas_pred, start=c(2015,1), end=c(2022,6), frequency=12)
x_tot_pred = ts(vars_tot, start=c(2015,1), end=c(2022,6), frequency=12)

my_pred <-  iterate_var_ter(x= x_gas_pred, initial_obs = vars_gas_pred[1:p,],
      #errors = rbind(residuals(vl_gas_elec)[1:T_gas,], rep(0,5)), var_model = vl_gas_elec, controls = controls_futures )
      errors = residuals(vl_gas_elec), var_model = vl_gas_elec, controls = controls_gas_tot  )


my_pred_pre_crisis <-  iterate_var_ter(x= x_gas_pred, initial_obs = vars_gas_pred[1:p,],
                            errors = rbind(residuals(vl_gas_count) , rep(0,5)), var_model = vl_gas_count, controls = controls_gas_tot  )



#attention ici prendre juste le coeff sur l'elec
#car sinon affecte aussi potentiellement le reste de l'eco
#itérer un à un

coef(vl_tot)$spot_price_log_diff
coef(vl_gas_count)$spot_price_log_diff

my_pred_tot <-  iterate_var_ter(x= x_tot_pred, initial_obs = vars_tot[1:p,],
                            errors = rbind(residuals(vl_tot), rep(0,6)), var_model = vl_tot )
res_gas

dim(structural_shocks_IV_spot)

#calculer contribution gas to elec
#elec supply shock from gas
AL_tot <- do.call(cbind, lapply(coef(vl_tot), function(df) df[, 1]))
# Optionally, set column names if needed
names(AL_tot) <- names(coef(vl_tot))

indices <- (n + 1) * (1:p)
indices_bis <- (n-1) * (1:p)

gas_lags <- t(controls_gas_tot[(T_gas+p):(T_gas+1), ])
gas_contrib <- (t(AL_tot)[1,][indices]  %*% as.vector(gas_lags))[,1]


t(AL_tot)[1,][indices-5]
t(AL)[1,][indices_bis-4]

my_pred[(T_gas+1),]
my_pred_pre_crisis[(T_gas+1),]
my_pred_tot[(T_gas+1),]
y_Tplus1_obs


c(controls_gas, controls_futures)[(T_gas-2):(T_gas+2)]
cbind(y, c(controls_gas, controls_futures))[(T_gas-5):(T_gas+5),]
dim(vars)
length(c(controls_gas, controls_futures))



shock_Tplus1

counterfactual <- iterate_var_ter(x= x_gas_pred, initial_obs = vars_gas_pred[1:p,], 
                                  errors = rbind(res_gas, c(shock_Tplus1, rep(0,4))), var_model = vl_gas)




