# Build the formula as a string
Bonte_formula_str_lag <- as.formula(paste(
  paste("log(load_tot)", "~",  paste(c("log(price+1)", controls_Bonte, "lag(Fore_Wind)"), collapse = " + ")),
  paste(paste(c("Fore_Wind", controls_Bonte, "lag(Fore_Wind)"), collapse = " + ")),
  sep = " | "
))
m_Bonte_lag <- ivreg(Bonte_formula_str_lag , data = price_indispo_merge_delta_clean)
nw_se_Bonte_lag  <- NeweyWest(m_Bonte_lag , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_Bonte_lag  <- coeftest(m_Bonte_lag , vcov. = nw_se_Bonte_lag)


# Cr?er une liste de 12 lags
lag_instruments <- paste0("lag(Fore_Wind, ", 1:12, ")")

Bonte_formula_str_lag_12 <- as.formula(paste(
  # ?quation structurelle
  paste("log(load_tot)", "~",  
        paste(c("log(price+1)", controls_Bonte, lag_instruments), collapse = " + ")),
  # instruments
  paste(paste(c("Fore_Wind", controls_Bonte, lag_instruments), collapse = " + ")),
  sep = " | "
))

m_Bonte_lag_12  <- ivreg(Bonte_formula_str_lag_12  , data = price_indispo_merge_delta_clean)
nw_se_Bonte_lag_12   <- NeweyWest(m_Bonte_lag_12  , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_Bonte_lag_12   <- coeftest(m_Bonte_lag_12  , vcov. = nw_se_Bonte_lag_12)



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
  keep = c("load_tot", "price" )
)





# Build the formula as a string
demand_formula_str_lag <- as.formula(paste(
  paste("log(load_tot)", "~",  paste(c("log(price+1)", controls_demand, "lag(cold_excess_supply)", "lag(heat_excess_supply)"), collapse = " + ")),
  paste(paste(c("cold_excess_supply","heat_excess_supply", controls_demand,"lag(cold_excess_supply)", "lag(heat_excess_supply)"), collapse = " + ")),
  sep = " | "
))
m_demand_lag <- ivreg(demand_formula_str_lag , data = price_indispo_merge_delta_clean)
nw_se_demand_lag  <- NeweyWest(m_demand_lag , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_demand_lag  <- coeftest(m_demand_lag , vcov. = nw_se_demand_lag)


# Cr?er une liste de 12 lags
lag_instruments <- c(paste0("lag(cold_excess_supply, ", 1:12, ")"), paste0("lag(heat_excess_supply, ", 1:12, ")"))

demand_formula_str_lag_12 <- as.formula(paste(
  # ?quation structurelle
  paste("log(load_tot)", "~",  
        paste(c("log(price+1)", controls_demand, lag_instruments), collapse = " + ")),
  # instruments
  paste(paste(c("cold_excess_supply","heat_excess_supply", controls_demand, lag_instruments), collapse = " + ")),
  sep = " | "
))

m_demand_lag_12  <- ivreg(demand_formula_str_lag_12  , data = price_indispo_merge_delta_clean)
nw_se_demand_lag_12   <- NeweyWest(m_demand_lag_12  , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_demand_lag_12   <- coeftest(m_demand_lag_12  , vcov. = nw_se_demand_lag_12)



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
  keep = c("load_tot", "price" )
)




# Build lagged instrument
exog_instr_s_lag <- paste0("lag(", exog_instr_s, ")")
exog_instr_d_lag <- paste0("lag(", exog_instr_d, ")")

iv_formula_supply_lag <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars,exog_instr_d_lag, exog_instr_s_lag, exog_instr_d), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, exog_instr_d_lag, exog_instr_s_lag, exog_instr_d,controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_s_lag <- ivreg(iv_formula_supply_lag, data = price_indispo_merge_delta_clean)
nw_se_s_lag <- NeweyWest(m_iv_s_lag, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag <- coeftest(m_iv_s_lag, vcov. = nw_se_s_lag)



iv_formula_demand_lag <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars, exog_instr_s, exog_instr_d_lag, exog_instr_s_lag), collapse = " + ")),
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
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars,lag_exog_d, lag_exog_s, exog_instr_d), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, lag_exog_d, lag_exog_s, exog_instr_d, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_lag12 <- ivreg(iv_formula_supply_lag12, data = price_indispo_merge_delta_clean)
nw_se_s_lag12 <- NeweyWest(m_iv_s_lag12, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag12 <- coeftest(m_iv_s_lag12, vcov. = nw_se_s_lag12)

iv_formula_demand_lag12 <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars, exog_instr_s, lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, lag_exog_d, exog_instr_s, lag_exog_s, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag12 <- ivreg(iv_formula_demand_lag12, data = price_indispo_merge_delta_clean)
nw_se_d_lag12 <- NeweyWest(m_iv_d_lag12, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag12 <- coeftest(m_iv_d_lag12, vcov. = nw_se_d_lag12)

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
  keep = c("load_tot", "price" )
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
  keep = c("load_tot", "price" )
)









#with orthogonalisation

exog_instr_s <- "delta_s_orth"
exog_instr_d <- "delta_d_orth"


#iv_formula
iv_formula_supply_str_orth <- as.formula(paste(
  paste("log(load_tot)", "~",  paste(c("log(price+1)", controls_vars ), collapse = " + ")),
  paste(paste(c(exog_instr_s , controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str_orth <- as.formula(paste(
  paste("log(load_tot)", "~",  paste(c( "log(price+1)", controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_orth <- ivreg(iv_formula_supply_str_orth , data = price_indispo_merge_delta_clean)
m_iv_d_orth <- ivreg(iv_formula_demand_str_orth , data = price_indispo_merge_delta_clean)

# --- Supply equation ---
# Newey-West standard errors, lag = 24 (ou autre selon fr?quence des donn?es)
nw_se_s_orth <- NeweyWest(m_iv_s_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les r?sultats des coefficients avec SE NW
nw_coefs_s_orth <- coeftest(m_iv_s_orth, vcov. = nw_se_s_orth)

# --- Demand equation ---
nw_se_d_orth <- NeweyWest(m_iv_d_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_orth <- coeftest(m_iv_d_orth, vcov. = nw_se_d_orth)


# Build lagged instrument
exog_instr_s_lag <- paste0("lag(", exog_instr_s, ")")
exog_instr_d_lag <- paste0("lag(", exog_instr_d, ")")

iv_formula_supply_lag_orth <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars,exog_instr_d_lag, exog_instr_s_lag), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, exog_instr_d_lag, exog_instr_s_lag,controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_s_lag_orth <- ivreg(iv_formula_supply_lag_orth, data = price_indispo_merge_delta_clean)
nw_se_s_lag_orth <- NeweyWest(m_iv_s_lag_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag_orth <- coeftest(m_iv_s_lag_orth, vcov. = nw_se_s_lag_orth)



iv_formula_demand_lag_orth <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars, exog_instr_d_lag, exog_instr_s_lag), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, exog_instr_d_lag, exog_instr_s_lag, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag_orth <- ivreg(iv_formula_demand_lag_orth, data = price_indispo_merge_delta_clean)
nw_se_d_lag_orth <- NeweyWest(m_iv_d_lag_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag_orth <- coeftest(m_iv_d_lag_orth, vcov. = nw_se_d_lag_orth)


lag_exog_s <- paste0("lag(", exog_instr_s, ", ", 1:12, ")")
lag_exog_d <- paste0("lag(", exog_instr_d, ", ", 1:12, ")")

# Example for supply with lagged exog_instr_s
iv_formula_supply_lag12_orth <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars,lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, lag_exog_d, lag_exog_s,controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_lag12_orth <- ivreg(iv_formula_supply_lag12_orth, data = price_indispo_merge_delta_clean)
nw_se_s_lag12_orth <- NeweyWest(m_iv_s_lag12_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_s_lag12_orth <- coeftest(m_iv_s_lag12_orth, vcov. = nw_se_s_lag12_orth)

iv_formula_demand_lag12_orth <- as.formula(paste(
  # Structural equation
  paste("log(load_tot)", "~", paste(c("log(price+1)", controls_vars, lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, lag_exog_d, lag_exog_s, controls_vars), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag12_orth <- ivreg(iv_formula_demand_lag12_orth, data = price_indispo_merge_delta_clean)
nw_se_d_lag12_orth <- NeweyWest(m_iv_d_lag12_orth, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_coefs_d_lag12_orth <- coeftest(m_iv_d_lag12_orth, vcov. = nw_se_d_lag12_orth)

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
      round(fstat_iv_d_orth, 2),             # IV demand
      round(fstat_iv_d_lag_orth, 2),             # IV supply
      round(fstat_iv_d_lag_12_orth, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("load_tot", "price" )
)


#Create LaTeX code with regression results
reg_table <- stargazer(
  m_iv_s_orth ,
  m_iv_s_lag_orth ,
  m_iv_s_lag12_orth,
  #m_renew_plant,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV ", "IV Lag",   'IV Lag 12'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_s_orth, 2),             # IV demand
      round(fstat_iv_s_lag_orth, 2),             # IV supply
      round(fstat_iv_s_lag_12_orth, 2)                                # no F for Bonte
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("load_tot", "price" )
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
#d?doubler la ligne OLS

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
getwd()
write.csv(df_clean, "df_clean_da_seminar_no_unavail.csv")

#write.csv(df_clean, "df_clean_da_seminar.csv")




