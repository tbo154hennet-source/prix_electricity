library(AER)
library(stargazer)
library(knitr)
library(gt)
library(sandwich)
library(lmtest)





instruments <- c("Fore_Wind",
                 "Fore_Solar",
                 #"renewable_day_ahead_forecast",
                 #"Published.Unavailable.Nuclear.Capacity"
                 "cbf_es",
                 "cold_excess",
                 "heat_excess")
instruments_supply <- c("Fore_Wind",
                        "Fore_Solar",
                        #"renewable_day_ahead_forecast",
                        #"Published.Unavailable.Nuclear.Capacity"
                        "cbf_es",
                        "cold_excess_supply",
                        "heat_excess_supply")
names_instruments <- c(
  "Fore_Wind",
  "Fore_Solar",
  #"renewable_day_ahead_forecast",
  #"Published.Unavailable.Nuclear.Capacity",
  "cbf_es",
  "cold_excess_supply",
  "cold_excess_demand",
  "heat_excess_supply",
  "heat_excess_demand")

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo_no_unavail.parquet")
#price_indispo_merge_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo.parquet")


# setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
# price_indispo_merge_delta_exo <- read_parquet("price_indispo_merge_delta_supply_demand_exo.parquet") %>% 
#   rename(delta_s_total_exo = delta_s_total,
#          delta_d_total_exo = delta_d_total
#   )
# 
# price_indispo_merge_delta_test <- merge(price_indispo_merge_delta,
#                                         price_indispo_merge_delta_exo,
#                                         by = "datetime"
# )
# price_indispo_merge_delta_test %>% 
#   dplyr::select(delta_s_total,delta_s_total_exo,
#                 delta_d_total, delta_d_total_exo
#   ) %>% 
#   cor(use = "complete.obs")


colnames_with_capacity <- colnames(price_indispo_merge_delta)[grepl("Capacity", colnames(price_indispo_merge_delta))]
#get monthly avg of unavailabilities
capacity_fields <- c(colnames_with_capacity,
                     "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables")

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
save_path_data <- "Z:/prix_electricite_gitlab"
setwd(save_path_data)

renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')
col_com <- intersect(colnames(renewable_forecasts), colnames(price_indispo_merge_delta))

# price_indispo_merge_Bonte <- left_join(price_indispo_merge_delta, renewable_forecasts %>% select(-col_com),
#                                     by = c("datetime" = "__index_level_0__"))
price_indispo_merge_delta <- merge(price_indispo_merge_delta, renewable_forecasts %>% dplyr::select(-col_com),
                                   by.x = "datetime" , by.y= "__index_level_0__")
instruments_supply <- paste0("delta_s_", instruments)
instruments_demand <- paste0("delta_d_", instruments)

# Construire les noms de colonnes
cols_supply<- paste0("delta_s_", instruments)
cols_demand<- paste0("delta_d_", instruments)

#justification parce que fondamentalement supply shock
#réinterpréter en demand shock dans nos données

colnames(price_indispo_merge_delta)
# Calculer la somme ligne par ligne des colonnes sÃ©lectionnÃ©es
price_indispo_merge_delta$delta_s_total <- rowSums(price_indispo_merge_delta[, cols_supply], na.rm = TRUE)
price_indispo_merge_delta$delta_d_total <- rowSums(price_indispo_merge_delta[, cols_demand], na.rm = TRUE)


col_names <- c("weekday" , "creneau", names_instruments, "price", "datetime", "delivery_date", "spot_gas",
               colnames_with_capacity, "gen_tot_da","gen_tot_da_conso" , "gen_tot_da_net" ,
               "temperature_supply","precipitation_supply", "relative_humidity_supply" ,
               "intraday_forecast"  ,"unpredicted_renewables", "gen_tot_da"    ,                           
               "wind_solar_tot","price" ,"year"   ,"month"  )

price_indispo_merge_delta_clean <- price_indispo_merge_delta %>%
  dplyr::select(-log_price_diff, -price_diff) %>%       # sÃ©lectionne les colonnes dans col_names
  filter(complete.cases(.)) %>%           # garde seulement les lignes sans NA dans ces colonnes
  distinct()

dep_var <- "log(price+1)"
endog_vars <- c("log(gen_tot_da)")
#endog_vars <- c("log(gen_tot_da)")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
extended_controls <- c(controls_vars, "unpredicted_renewables",  "intraday_update_renewables" )


# Orthogonaliser delta_s_total par rapport à delta_d_total
price_indispo_merge_delta_clean$delta_s_orth <- resid(
  lm(delta_s_total ~ delta_d_total, data = price_indispo_merge_delta_clean)
)

# Orthogonaliser delta_d_total par rapport à delta_s_total
price_indispo_merge_delta_clean$delta_d_orth <- resid(
  lm(delta_d_total ~ delta_s_total, data = price_indispo_merge_delta_clean)
)



price_indispo_merge_delta_clean %>% 
  dplyr::select(delta_s_total, delta_d_total, delta_s_orth, delta_d_orth ) %>% 
  cor(use = "complete.obs")


ols_formula <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_vars), collapse = " + "))))  

m_ols <- ivreg(ols_formula , data = price_indispo_merge_delta_clean)

nw_se_ols  <- NeweyWest(m_ols , lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les résultats des coefficients avec SE NW
nw_coefs_ols  <- coeftest(m_ols , vcov. = nw_se_ols )


# Combine exogenous regressors and instruments

exog_instr_s <- "delta_s_total"
exog_instr_d <- "delta_d_total"


#iv_formula
iv_formula_supply_str <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_vars, exog_instr_d ), collapse = " + ")),
  paste(paste(c(exog_instr_s, exog_instr_d , controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c( "log(price+1)", controls_vars, exog_instr_s), collapse = " + ")),
  paste(paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s <- ivreg(iv_formula_supply_str , data = price_indispo_merge_delta_clean)
m_iv_d <- ivreg(iv_formula_demand_str , data = price_indispo_merge_delta_clean)

# --- Supply equation ---
# Newey-West standard errors, lag = 24 (ou autre selon fréquence des données)
nw_se_s <- NeweyWest(m_iv_s, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les résultats des coefficients avec SE NW
nw_coefs_s <- coeftest(m_iv_s, vcov. = nw_se_s)

# --- Demand equation ---
nw_se_d <- NeweyWest(m_iv_d, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d <- coeftest(m_iv_d, vcov. = nw_se_d)


controls_Bonte <- c(
  # "intraday_forecast" ,"unpredicted_renewables",#"cbf_es",
  "temperature_supply","precipitation_supply"  ,
  "relative_humidity_supply", "spot_gas", 
  "as.factor(year)" , "as.factor(month)",
  "as.factor(weekday)", "as.factor(creneau)"
)
# Build the formula as a string
# Build the formula as a string

#iv_formula

# Build the formula as a string
Bonte_formula_str <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_Bonte), collapse = " + ")),
  paste(paste(c("Fore_Wind", controls_Bonte), collapse = " + ")),
  sep = " | "
))
m_Bonte <- ivreg(Bonte_formula_str , data = price_indispo_merge_delta_clean)
nw_se_Bonte  <- NeweyWest(m_Bonte , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_Bonte  <- coeftest(m_Bonte , vcov. = nw_se_Bonte)


controls_demand <- c(setdiff(controls_Bonte, "temperature_supply"), "Fore_Wind", "Fore_Solar")

formula_demand_str <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_vars), collapse = " + ")),
  paste(paste(c("cold_excess_demand", "heat_excess_demand", controls_vars), collapse = " + ")),
  sep = " | "
))

m_demand <- ivreg(formula_demand_str , data = price_indispo_merge_delta_clean)

nw_se_demand  <- NeweyWest(m_demand , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_demand  <- coeftest(m_demand , vcov. = nw_se_Bonte)




# First-stage diagnostics
diag_iv_d <- summary(m_iv_d, diagnostics = TRUE)
diag_iv_s <- summary(m_iv_s, diagnostics = TRUE)
diag_iv_bronte <- summary(m_Bonte, diagnostics = TRUE)
diag_iv_demand <- summary(m_demand, diagnostics = TRUE)
# Extract weak instrument F-statistic
fstat_iv_d <- diag_iv_d$diagnostics["Weak instruments", "statistic"]
fstat_iv_s <- diag_iv_s$diagnostics["Weak instruments", "statistic"]
fstat_iv_bronte <- diag_iv_bronte$diagnostics["Weak instruments", "statistic"]
fstat_iv_demand <- diag_iv_demand$diagnostics["Weak instruments", "statistic"]

# Create LaTeX code with regression results
# reg_table <- stargazer(
#   m_ols,
#   m_iv_d,
#   m_iv_s,# m_iv_int,
#   m_Bonte ,
#   #m_renew_plant, 
#   type = "text",
#   title = "Regression Results",
#   column.labels = c("OLS ", "IV Supply",   'IV Demand', 'Bonte'),#,
#   add.lines = list(
#     c("First-stage F-stat",
#       "",                               # no F for OLS
#       round(fstat_iv_d, 2),             # IV demand
#       round(fstat_iv_s, 2),             # IV supply
#       round(fstat_iv_bronte, 2)                                # no F for Bonte
#     )),
#   #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
#   keep = c("gen_tot_da", "price" )
# )

reg_table <- stargazer(
  nw_coefs_ols,
  nw_coefs_d,
  nw_coefs_demand, 
  nw_coefs_s,# m_iv_int,
  nw_coefs_Bonte ,
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS ", "IV Supply", 'Bonte Supply',   'IV Demand', 'Bonte Demand'),#,
  add.lines = list(
    c("First-stage F-stat",
      "",                               # no F for OLS
      round(fstat_iv_d, 2),
      round(fstat_iv_demand, 2), # no F for Bonte,             # IV demand
      round(fstat_iv_s, 2),             # IV supply
      round(fstat_iv_bronte, 2)
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)
#summary(m_iv_s, diagnostics = TRUE)$diagnostics




# vcov_cluster_iv <- vcovCL(m_iv , type = "HC0", cluster = ~ aav2020)
# # Erreurs standard clusterisÃ©es
# cl_se_iv <- sqrt(diag(vcov_cluster_iv))

# Read the file
# tex_lines <- readLines("elasticity_table.tex")
# 
# # Keep only lines between \begin{tabular} and \end{tabular*}
# tabular_lines <- tex_lines[grep("\\\\begin\\{tabular", tex_lines):
#                              grep("\\\\end\\{tabular\\*\\}", tex_lines)]
# 
# getwd()
# # Write to a new file
# writeLines(tabular_lines, "elasticity_table.tex")



exog_instr_s <- "delta_s_orth"
exog_instr_d <- "delta_d_orth"

iv_formula_supply_str_ortho <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_vars ), collapse = " + ")),
  paste(paste(c(exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str_ortho <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c( "log(price+1)", controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d,  controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_ortho  <- ivreg(iv_formula_supply_str_ortho , data = price_indispo_merge_delta_clean)
m_iv_d_ortho <- ivreg(iv_formula_demand_str_ortho , data = price_indispo_merge_delta_clean)

nw_se_s_ortho <- NeweyWest(m_iv_s_ortho, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les résultats des coefficients avec SE NW
nw_coefs_s_ortho <- coeftest(m_iv_s_ortho, vcov. = nw_se_s_ortho)

# --- Demand equation ---
nw_se_d_ortho <- NeweyWest(m_iv_d_ortho, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_ortho <- coeftest(m_iv_d_ortho, vcov. = nw_se_d_ortho)

# First-stage diagnostics
diag_iv_d_ortho <- summary(m_iv_d_ortho, diagnostics = TRUE)
diag_iv_s_ortho <- summary(m_iv_s_ortho, diagnostics = TRUE)
# Extract weak instrument F-statistic
fstat_iv_d_ortho <- diag_iv_d_ortho$diagnostics["Weak instruments", "statistic"]
fstat_iv_s_ortho <- diag_iv_s_ortho$diagnostics["Weak instruments", "statistic"]

# Create LaTeX code with regression results
reg_table <- stargazer(
  nw_coefs_d,
  nw_coefs_d_ortho,
  nw_coefs_s,# m_iv_int,
  nw_coefs_s_ortho ,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c( "Supply", "Supply Ortho",  'Demand', 'Demand Ortho'),#,
  # add.lines = list(
  #   c("First-stage F-stat",
  #     "",                               # no F for OLS
  #     round(fstat_iv_d, 2),             # IV demand
  #     round(fstat_iv_s, 2),             # IV supply
  #     round(fstat_iv_bronte, 2)                                # no F for Bonte
  #   )),
  add.lines = list(
    c("First-stage F-stat",                               # no F for OLS
      round(fstat_iv_d, 2), 
      round(fstat_iv_d_ortho, 2),# IV demand
      round(fstat_iv_s, 2),             # IV supply
      round(fstat_iv_s_ortho, 2)                                # no F for Bonte
    )),
  keep = c("gen_tot_da", "price" )
)


# Extract estimate and standard error for 'gen_tot_da' from each model
extract_coef_se <- function(model, variable = "log(gen_tot_da)") {
  s <- summary(model)
  est <- s$coefficients[variable, "Estimate"]
  se  <- s$coefficients[variable, "Std. Error"]
  return(c(est = est, se = se))
}

# Supposons que nw_coefs_Bonte est un objet coeftest
# extraire estimate et SE pour une variable donnée
extract_coef_se_coeftest <- function(coeftest_obj, variable) {
  est <- coeftest_obj[variable, "Estimate"]
  se  <- coeftest_obj[variable, "Std. Error"]
  return(c(est = est, se = se))
}



# coef_se_ols <- extract_coef_se(m_ols, "log(price+1)")
# coef_se_iv_d    <- extract_coef_se(m_iv_d, "log(price+1)")
# coef_se_iv_s    <- extract_coef_se(m_iv_s, "log(price+1)")
# coef_se_Bonte_all    <- extract_coef_se(m_Bonte, "log(price+1)" )

coef_se_ols <- extract_coef_se_coeftest (nw_coefs_ols, "log(price + 1)")
coef_se_iv_d    <- extract_coef_se_coeftest (nw_coefs_d, "log(price + 1)")
coef_se_iv_s    <- extract_coef_se_coeftest (nw_coefs_s, "log(price + 1)")
coef_se_Bonte_all    <- extract_coef_se_coeftest (nw_coefs_Bonte, "log(price + 1)" )
coef_se_Bonte_demand    <- extract_coef_se_coeftest (nw_coefs_demand, "log(price + 1)" )
coef_se_iv_s_ortho    <- extract_coef_se_coeftest(nw_coefs_s_ortho, "log(price + 1)")
coef_se_iv_d_ortho    <- extract_coef_se_coeftest(nw_coefs_d_ortho, "log(price + 1)" )



df <- data.frame(
  Model = c("OLS", "IV Supply", 'IV Demand',  'Bonte et al (2015) Supply', 'Bonte et al (2015) Demand', "Supply Ortho", "Demand Ortho"),
  Estimate = c(coef_se_ols["est"], coef_se_iv_d["est"],
               coef_se_iv_s["est"],coef_se_Bonte_demand["est"],
               coef_se_Bonte_all["est"],  coef_se_iv_d_ortho["est"],
               coef_se_iv_s_ortho["est"]   ),
  StdError = c(coef_se_ols["se"], coef_se_iv_d["se"],
               coef_se_iv_s["se"],  coef_se_Bonte_demand["se"],
               coef_se_Bonte_all["se"], coef_se_iv_d_ortho["se"],
               coef_se_iv_s_ortho["se"]
  )
)


# Compute elasticity and SE using delta method
df$Elasticity <- df$Estimate
df$Elasticity_SE <- #df$StdError #/ (df$Estimate^2)
  paste0(
    round(df$Estimate, 3), " (", round(df$StdError, 3), ")"
  )

df_clean <- df%>%
  mutate(
    Instrument = case_when(
      grepl("OLS", Model) ~ "OLS",
      grepl("IV", Model) ~ "IV",
      grepl("Bonte", Model) ~ "Bonte et al (2015)",
      grepl("Ortho", Model) ~ "IV Ortho",
    ),
    Type = case_when(
      grepl("Supply", Model) ~ "Supply",
      grepl("Demand", Model) ~ "Demand"
    )
  )
#dédoubler la ligne OLS

df_new <- df_clean %>%
  # Séparer la ligne OLS en deux copies
  filter(Model == "OLS") %>%
  mutate(Type = "Supply") %>%
  bind_rows(
    df_clean %>% filter(Model == "OLS") %>% mutate(Type = "Demand")
  ) %>%
  # Ajouter les autres lignes non-OLS
  bind_rows(
    df_clean %>% filter(Model != "OLS")
  )

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
getwd()
write.csv(df_clean, "df_clean_da_no_unavail.csv")

# Build the formula as a string
Bonte_formula_str_lag <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_Bonte, "lag(Fore_Wind)"), collapse = " + ")),
  paste(paste(c("Fore_Wind", controls_Bonte, "lag(Fore_Wind)"), collapse = " + ")),
  sep = " | "
))
m_Bonte_lag <- ivreg(Bonte_formula_str_lag , data = price_indispo_merge_delta_clean)
nw_se_Bonte_lag  <- NeweyWest(m_Bonte_lag , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_Bonte_lag  <- coeftest(m_Bonte_lag , vcov. = nw_se_Bonte_lag)


# Créer une liste de 12 lags
lag_instruments <- paste0("lag(Fore_Wind, ", 1:12, ")")

Bonte_formula_str_lag_12 <- as.formula(paste(
  # équation structurelle
  paste("log(gen_tot_da)", "~",  
        paste(c("log(price+1)", controls_Bonte, lag_instruments), collapse = " + ")),
  # instruments
  paste(paste(c("Fore_Wind", controls_Bonte, lag_instruments), collapse = " + ")),
  sep = " | "
))

m_Bonte_lag_12  <- ivreg(Bonte_formula_str_lag_12  , data = price_indispo_merge_delta_clean)
nw_se_Bonte_lag_12   <- NeweyWest(m_Bonte_lag_12  , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_Bonte_lag_12   <- coeftest(m_Bonte_lag_12  , vcov. = nw_se_Bonte_lag)



diag_iv_bronte <- summary(m_Bonte, diagnostics = TRUE)
fstat_iv_bronte <- diag_iv_bronte$diagnostics["Weak instruments", "statistic"]
diag_iv_bronte_lag <- summary(m_Bonte_lag, diagnostics = TRUE)
fstat_iv_bronte_lag <- diag_iv_bronte_lag$diagnostics["Weak instruments", "statistic"]
diag_iv_bronte_lag_12 <- summary(m_Bonte_lag_12, diagnostics = TRUE)
fstat_iv_bronte_lag_12 <- diag_iv_bronte_lag_12$diagnostics["Weak instruments", "statistic"]



#Create LaTeX code with regression results
reg_table <- stargazer(
  m_Bonte ,
  m_Bonte_lag ,
  m_Bonte_lag_12 ,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_bronte, 2),             # IV demand
      round(fstat_iv_bronte_lag, 2),             # IV supply
      round(fstat_iv_bronte_lag_12, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)





# Build the formula as a string
demand_formula_str_lag <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_demand, "lag(cold_excess_supply)", "lag(heat_excess_supply)"), collapse = " + ")),
  paste(paste(c("cold_excess_supply","heat_excess_supply", controls_demand,"lag(cold_excess_supply)", "lag(heat_excess_supply)"), collapse = " + ")),
  sep = " | "
))
m_demand_lag <- ivreg(demand_formula_str_lag , data = price_indispo_merge_delta_clean)
nw_se_demand_lag  <- NeweyWest(m_demand_lag , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_demand_lag  <- coeftest(m_demand_lag , vcov. = nw_se_demand_lag)


# Créer une liste de 12 lags
lag_instruments <- c(paste0("lag(cold_excess_supply, ", 1:12, ")"), paste0("lag(heat_excess_supply, ", 1:12, ")"))

demand_formula_str_lag_12 <- as.formula(paste(
  # équation structurelle
  paste("log(gen_tot_da)", "~",  
        paste(c("log(price+1)", controls_demand, lag_instruments), collapse = " + ")),
  # instruments
  paste(paste(c("cold_excess_supply","heat_excess_supply", controls_demand, lag_instruments), collapse = " + ")),
  sep = " | "
))

m_demand_lag_12  <- ivreg(demand_formula_str_lag_12  , data = price_indispo_merge_delta_clean)
nw_se_demand_lag_12   <- NeweyWest(m_demand_lag_12  , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_demand_lag_12   <- coeftest(m_demand_lag_12  , vcov. = nw_se_demand_lag)



diag_iv_demand <- summary(m_demand, diagnostics = TRUE)
fstat_iv_demand <- diag_iv_demand$diagnostics["Weak instruments", "statistic"]
diag_iv_demand_lag <- summary(m_demand_lag, diagnostics = TRUE)
fstat_iv_demand_lag <- diag_iv_demand_lag$diagnostics["Weak instruments", "statistic"]
diag_iv_demand_lag_12 <- summary(m_demand_lag_12, diagnostics = TRUE)
fstat_iv_demand_lag_12 <- diag_iv_demand_lag_12$diagnostics["Weak instruments", "statistic"]




#Create LaTeX code with regression results
reg_table <- stargazer(
  m_demand ,
  m_demand_lag ,
  m_demand_lag_12 ,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_demand, 2),             # IV demand
      round(fstat_iv_demand_lag, 2),             # IV supply
      round(fstat_iv_demand_lag_12, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)




# Build lagged instrument
exog_instr_s_lag <- paste0("lag(", exog_instr_s, ")")
exog_instr_d_lag <- paste0("lag(", exog_instr_d, ")")

iv_formula_supply_lag <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars,exog_instr_d_lag, exog_instr_s_lag, exog_instr_d), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, exog_instr_d_lag, exog_instr_s_lag, exog_instr_d,controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_s_lag <- ivreg(iv_formula_supply_lag, data = price_indispo_merge_delta_clean)
nw_se_s_lag <- NeweyWest(m_iv_s_lag, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag <- coeftest(m_iv_s_lag, vcov. = nw_se_s_lag)



iv_formula_demand_lag <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars, exog_instr_s, exog_instr_d_lag, exog_instr_s_lag), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, exog_instr_d_lag, exog_instr_s, exog_instr_s_lag, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag <- ivreg(iv_formula_demand_lag, data = price_indispo_merge_delta_clean)
nw_se_d_lag <- NeweyWest(m_iv_d_lag, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag <- coeftest(m_iv_d_lag, vcov. = nw_se_d_lag)


lag_exog_s <- paste0("lag(", exog_instr_s, ", ", 1:12, ")")
lag_exog_d <- paste0("lag(", exog_instr_d, ", ", 1:12, ")")

# Example for supply with lagged exog_instr_s
iv_formula_supply_lag12 <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars,lag_exog_d, lag_exog_s, exog_instr_d), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, lag_exog_d, lag_exog_s, exog_instr_d,controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_lag12 <- ivreg(iv_formula_supply_lag12, data = price_indispo_merge_delta_clean)
nw_se_s_lag12 <- NeweyWest(m_iv_s_lag12, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag12 <- coeftest(m_iv_s_lag12, vcov. = nw_se_s_lag)

iv_formula_demand_lag12 <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars, exog_instr_s, lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, lag_exog_d, exog_instr_s, lag_exog_s, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag12 <- ivreg(iv_formula_demand_lag12, data = price_indispo_merge_delta_clean)
nw_se_d_lag12 <- NeweyWest(m_iv_d_lag12, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag12 <- coeftest(m_iv_d_lag12, vcov. = nw_se_d_lag)

## --- Supply baseline ---
diag_iv_s <- summary(m_iv_s, diagnostics = TRUE)
fstat_iv_s <- diag_iv_s$diagnostics["Weak instruments", "statistic"]

## --- Supply lag ---
diag_iv_s_lag <- summary(m_iv_s_lag, diagnostics = TRUE)
fstat_iv_s_lag <- diag_iv_s_lag$diagnostics["Weak instruments", "statistic"]

## --- Supply lag 12 ---
diag_iv_s_lag_12 <- summary(m_iv_s_lag12, diagnostics = TRUE)
fstat_iv_s_lag_12 <- diag_iv_s_lag_12$diagnostics["Weak instruments", "statistic"]


## --- Demand baseline ---
diag_iv_d <- summary(m_iv_d, diagnostics = TRUE)
fstat_iv_d <- diag_iv_d$diagnostics["Weak instruments", "statistic"]

## --- Demand lag ---
diag_iv_d_lag <- summary(m_iv_d_lag, diagnostics = TRUE)
fstat_iv_d_lag <- diag_iv_d_lag$diagnostics["Weak instruments", "statistic"]

## --- Demand lag 12 ---
diag_iv_d_lag_12 <- summary(m_iv_d_lag12, diagnostics = TRUE)
fstat_iv_d_lag_12 <- diag_iv_d_lag_12$diagnostics["Weak instruments", "statistic"]


#Create LaTeX code with regression results
reg_table <- stargazer(
  m_iv_d ,
  m_iv_d_lag ,
  m_iv_d_lag12,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_d, 2),             # IV demand
      round(fstat_iv_d_lag, 2),             # IV supply
      round(fstat_iv_d_lag_12, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)


#Create LaTeX code with regression results
reg_table <- stargazer(
  m_iv_s ,
  m_iv_s_lag ,
  m_iv_s_lag12,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_s, 2),             # IV demand
      round(fstat_iv_s_lag, 2),             # IV supply
      round(fstat_iv_s_lag_12, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)









#with orthogonalisation

exog_instr_s <- "delta_s_orth"
exog_instr_d <- "delta_d_orth"


#iv_formula
iv_formula_supply_str_orth <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_vars ), collapse = " + ")),
  paste(paste(c(exog_instr_s , controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str_orth <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c( "log(price+1)", controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_orth <- ivreg(iv_formula_supply_str_orth , data = price_indispo_merge_delta_clean)
m_iv_d_orth <- ivreg(iv_formula_demand_str_orth , data = price_indispo_merge_delta_clean)

# --- Supply equation ---
# Newey-West standard errors, lag = 24 (ou autre selon fréquence des données)
nw_se_s_orth <- NeweyWest(m_iv_s_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les résultats des coefficients avec SE NW
nw_coefs_s_orth <- coeftest(m_iv_s_orth, vcov. = nw_se_s)

# --- Demand equation ---
nw_se_d_orth <- NeweyWest(m_iv_d_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_orth <- coeftest(m_iv_d_orth, vcov. = nw_se_d)


# Build lagged instrument
exog_instr_s_lag <- paste0("lag(", exog_instr_s, ")")
exog_instr_d_lag <- paste0("lag(", exog_instr_d, ")")

iv_formula_supply_lag_orth <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars,exog_instr_d_lag, exog_instr_s_lag), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, exog_instr_d_lag, exog_instr_s_lag,controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_s_lag_orth <- ivreg(iv_formula_supply_lag_orth, data = price_indispo_merge_delta_clean)
nw_se_s_lag_orth <- NeweyWest(m_iv_s_lag_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag_orth <- coeftest(m_iv_s_lag_orth, vcov. = nw_se_s_lag)



iv_formula_demand_lag_orth <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars, exog_instr_d_lag, exog_instr_s_lag), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, exog_instr_d_lag, exog_instr_s_lag, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag_orth <- ivreg(iv_formula_demand_lag_orth, data = price_indispo_merge_delta_clean)
nw_se_d_lag_orth <- NeweyWest(m_iv_d_lag_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag_orth <- coeftest(m_iv_d_lag_orth, vcov. = nw_se_d_lag)


lag_exog_s <- paste0("lag(", exog_instr_s, ", ", 1:12, ")")
lag_exog_d <- paste0("lag(", exog_instr_d, ", ", 1:12, ")")

# Example for supply with lagged exog_instr_s
iv_formula_supply_lag12_orth <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars,lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, lag_exog_d, lag_exog_s,controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_lag12_orth <- ivreg(iv_formula_supply_lag12_orth, data = price_indispo_merge_delta_clean)
nw_se_s_lag12_orth <- NeweyWest(m_iv_s_lag12_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag12_orth <- coeftest(m_iv_s_lag12_orth, vcov. = nw_se_s_lag)

iv_formula_demand_lag12_orth <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_vars, lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, lag_exog_d, lag_exog_s, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag12_orth <- ivreg(iv_formula_demand_lag12_orth, data = price_indispo_merge_delta_clean)
nw_se_d_lag12_orth <- NeweyWest(m_iv_d_lag12_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag12_orth <- coeftest(m_iv_d_lag12_orth, vcov. = nw_se_d_lag)

## --- Supply baseline ---
diag_iv_s_orth <- summary(m_iv_s_orth, diagnostics = TRUE)
fstat_iv_s_orth <- diag_iv_s_orth$diagnostics["Weak instruments", "statistic"]

## --- Supply lag ---
diag_iv_s_lag_orth <- summary(m_iv_s_lag_orth, diagnostics = TRUE)
fstat_iv_s_lag_orth <- diag_iv_s_lag_orth$diagnostics["Weak instruments", "statistic"]

## --- Supply lag 12 ---
diag_iv_s_lag_12_orth <- summary(m_iv_s_lag12_orth, diagnostics = TRUE)
fstat_iv_s_lag_12_orth <- diag_iv_s_lag_12_orth$diagnostics["Weak instruments", "statistic"]


## --- Demand baseline ---
diag_iv_d_orth <- summary(m_iv_d_orth, diagnostics = TRUE)
fstat_iv_d_orth <- diag_iv_d_orth$diagnostics["Weak instruments", "statistic"]

## --- Demand lag ---
diag_iv_d_lag_orth <- summary(m_iv_d_lag_orth, diagnostics = TRUE)
fstat_iv_d_lag_orth<- diag_iv_d_lag_orth$diagnostics["Weak instruments", "statistic"]

## --- Demand lag 12 ---
diag_iv_d_lag_12_orth <- summary(m_iv_d_lag12_orth, diagnostics = TRUE)
fstat_iv_d_lag_12_orth <- diag_iv_d_lag_12_orth$diagnostics["Weak instruments", "statistic"]


#Create LaTeX code with regression results
reg_table <- stargazer(
  m_iv_d_orth ,
  m_iv_d_lag_orth ,
  m_iv_d_lag12_orth,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_d, 2),             # IV demand
      round(fstat_iv_d_lag, 2),             # IV supply
      round(fstat_iv_d_lag_12, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)


#Create LaTeX code with regression results
reg_table <- stargazer(
  m_iv_s ,
  m_iv_s_lag ,
  m_iv_s_lag12,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_s, 2),             # IV demand
      round(fstat_iv_s_lag, 2),             # IV supply
      round(fstat_iv_s_lag_12, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)











coef_se_ols <- extract_coef_se_coeftest (nw_coefs_ols, "log(price + 1)")
coef_se_iv_d    <- extract_coef_se_coeftest (nw_coefs_d_lag12, "log(price + 1)")
coef_se_iv_s    <- extract_coef_se_coeftest (nw_coefs_s_lag12, "log(price + 1)")
coef_se_Bonte_all    <- extract_coef_se_coeftest (nw_coefs_Bonte_lag_12, "log(price + 1)" )
coef_se_Bonte_demand    <- extract_coef_se_coeftest (nw_coefs_demand_lag_12, "log(price + 1)" )
# coef_se_iv_s_bis    <- extract_coef_se_coeftest(nw_coefs_s_bis_lag12, "log(price + 1)")
# coef_se_iv_d_bis    <- extract_coef_se_coeftest(nw_coefs_d_bis_lag12, "log(price + 1)" )
coef_se_iv_s_orth    <- extract_coef_se_coeftest(nw_coefs_s_lag12_orth , "log(price + 1)")
coef_se_iv_d_orth     <- extract_coef_se_coeftest(nw_coefs_d_lag12_orth , "log(price + 1)" )



df <- data.frame(
  Model = c("OLS", "IV Supply", 'IV Demand',  'Bonte et al (2015) Supply', 'Bonte et al (2015) Demand', "Supply Ortho", "Demand Ortho"),
  Estimate = c(coef_se_ols["est"], coef_se_iv_d["est"],
               coef_se_iv_s["est"],coef_se_Bonte_demand["est"],
               coef_se_Bonte_all["est"], 
               # coef_se_iv_d_bis["est"],coef_se_iv_s_bis["est"]   
               coef_se_iv_d_orth["est"],coef_se_iv_s_orth["est"]   
               
  ),
  StdError = c(coef_se_ols["se"], coef_se_iv_d["se"],
               coef_se_iv_s["se"],  coef_se_Bonte_demand["se"],
               coef_se_Bonte_all["se"], 
               # coef_se_iv_d_bis["se"],coef_se_iv_s_bis["se"],
               coef_se_iv_d_orth["se"],coef_se_iv_s_orth["se"] 
  )
)


# Compute elasticity and SE using delta method
df$Elasticity <- df$Estimate
df$Elasticity_SE <- #df$StdError #/ (df$Estimate^2)
  paste0(
    round(df$Estimate, 3), " (", round(df$StdError, 3), ")"
  )

df_clean <- df%>%
  mutate(
    Instrument = case_when(
      grepl("OLS", Model) ~ "OLS",
      grepl("IV", Model) ~ "IV",
      grepl("Bonte", Model) ~ "Bonte et al (2015)",
      grepl("Ortho", Model) ~ "IV Ortho",
    ),
    Type = case_when(
      grepl("Supply", Model) ~ "Supply",
      grepl("Demand", Model) ~ "Demand"
    )
  )
#dédoubler la ligne OLS
# 
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
getwd()
write.csv(df_clean, "df_clean_da_seminar_no_unavail.csv")






