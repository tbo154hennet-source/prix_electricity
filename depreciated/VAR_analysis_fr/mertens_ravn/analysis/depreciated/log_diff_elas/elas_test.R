library(AER)
library(stargazer)


#vérifier macro

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_delta <- read_parquet("price_indispo_merge_delta_supply_demand.parquet")


# Calculer la somme ligne par ligne des colonnes sélectionnées

instruments_supply <- paste0("delta_s_", instruments)
instruments_demand <- paste0("delta_d_", instruments)

# Construire les noms de colonnes
cols_supply<- paste0("delta_s_", instruments)
cols_demand<- paste0("delta_d_", instruments)

# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge_delta$delta_s_total <- rowSums(price_indispo_merge_delta[, cols_supply], na.rm = TRUE)
price_indispo_merge_delta$delta_d_total <- rowSums(price_indispo_merge_delta[, cols_demand], na.rm = TRUE)

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

#est ce que c'est une histoire de créneau, augmentation par créneau versus moyenne totale
#on s'attend à presque pas d'augmentation pour les créneaux sans contrainte
#par contre grosse augmentation pour les créneaux de pointe

monthly_price_indispo_merge_delta <- price_indispo_merge_delta %>%
  group_by(month, year) %>%
  summarise(
    delta_s_total = weighted_mean(delta_s_total, gen_tot),
    delta_d_total = weighted_mean(delta_d_total, gen_tot),
    price = weighted_mean(price, gen_tot),
    gen_tot = mean(gen_tot, na.rm = TRUE),
    spot_gas = mean(spot_gas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(observation_date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))

#ajouter contrôles de l'économie
save_var_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data <- read_parquet('IV_data_esp.parquet') 

col_com <- intersect(colnames(monthly_price_indispo_merge_delta), colnames(IV_data))

# price_indispo_merge_bronte <- left_join(price_indispo_merge_delta, renewable_forecasts %>% select(-col_com),

monthly_price_indispo_merge_delta <- monthly_price_indispo_merge_delta %>%
  left_join(IV_data %>% dplyr::select(-setdiff(col_com, "observation_date")), by = c("observation_date" = "observation_date"))



dep_var <- "log(price)"
endog_vars <- c("log(gen_tot)")
#endog_vars <- c("log(gen_tot_da)")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "IPI"  ,  "Spot_gas" , "HICP"
)


ols_formula_d <- as.formula(paste(
  paste(dep_var, "~",  paste(c(endog_vars, controls_vars), collapse = " + "))))  

m_ols_d <- ivreg(ols_formula_d , data = monthly_price_indispo_merge_delta)

ols_formula_s <- as.formula(paste(
  paste(endog_vars, "~",  paste(c(dep_var, controls_vars), collapse = " + "))))  

m_ols_s <- ivreg(ols_formula_s , data = monthly_price_indispo_merge_delta)

# Combine exogenous regressors and instruments
# Combine exogenous regressors and instruments
exog_instr_s <- "delta_s_total"


#iv_formula
iv_formula_supply_str <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(price)",exog_instr_d, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_s,exog_instr_d, controls_vars), collapse = " + ")),
  sep = " | "
))

exog_instr_d <- "delta_d_total"

#iv_formula
iv_formula_demand_str <- as.formula(paste(
  paste("log(price)", "~",  paste(c("log(gen_tot)",exog_instr_s, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))



m_iv_s <- ivreg(iv_formula_supply_str , data = monthly_price_indispo_merge_delta)
m_iv_d <- ivreg(iv_formula_demand_str , data = monthly_price_indispo_merge_delta)



reg_table_macro <- stargazer(
  m_ols_d,
  m_iv_d,
  m_ols_s,
  m_iv_s,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS Demand", "IV Demand", "OLS Supply",  'IV Supply'),#,
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot", "price" )
)



summary_iv_s <- summary(m_iv_s , diagnostics = TRUE)
fstat_val_s <- summary_iv_s$diagnostics["Weak instruments", "statistic"]

summary_iv_d <- summary(m_iv_d , diagnostics = TRUE)
fstat_val_d <- summary_iv_d$diagnostics["Weak instruments", "statistic"]

print(fstat_val_s)
print(fstat_val_d)


