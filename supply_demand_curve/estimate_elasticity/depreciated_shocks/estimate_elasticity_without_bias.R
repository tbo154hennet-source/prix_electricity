

#Dans un système de production avec une part importante de renouvelable, on ne peut pas considérer que
# l'intercept est constant


instruments_ni <- c(
                 #  "Fore_Wind",
                 # "Fore_Solar",
                 "Published.Predicted.Unavailable.Capacity",
                 #"renewable_day_ahead_forecast",
                 #"Published.Unavailable.Nuclear.Capacity"
                 "cbf_es",
                 "cold_excess",
                 "heat_excess"
)

# Construire les noms de colonnes
cols_supply_ni<- paste0("delta_s_", instruments_ni)
cols_demand_ni<- paste0("delta_d_", instruments_ni)

#justification parce que fondamentalement supply shock
#r?interpr?ter en demand shock dans nos donn?es

# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge_delta_clean$delta_s_total_ni <- rowSums(price_indispo_merge_delta_clean[, cols_supply_ni], na.rm = TRUE)
price_indispo_merge_delta_clean$delta_d_total_ni <- rowSums(price_indispo_merge_delta_clean[, cols_demand_ni], na.rm = TRUE)



# Orthogonaliser delta_s_total par rapport ? delta_d_total
price_indispo_merge_delta_clean$delta_s_orth_ni <- resid(
  lm(delta_s_total_ni ~ delta_d_total_ni, data = price_indispo_merge_delta_clean)
)

# Orthogonaliser delta_d_total par rapport ? delta_s_total
price_indispo_merge_delta_clean$delta_d_orth_ni <- resid(
  lm(delta_d_total_ni ~ delta_s_total_ni, data = price_indispo_merge_delta_clean)
)


#faire une robustesse sur les prix bas et les prix élevés
# où possiblement mes proxies sont mauvais car l'énergie limitante n'est pas thermique

price_indispo_merge_delta_clean %>% 
  select(delta_s_orth, delta_s_orth_ni,
         delta_d_orth, delta_d_orth_ni
         ) %>% 
  cor()


controls_var_ni <- c(controls_vars, "Fore_Wind", "Fore_Solar")

exog_instr_s <- "delta_s_orth_ni"
exog_instr_d <- "delta_d_orth_ni"

iv_formula_supply_str_ortho_ni <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c("log(price+1)", controls_var_ni ), collapse = " + ")),
  paste(paste(c(exog_instr_s, controls_var_ni), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str_ortho_ni <- as.formula(paste(
  paste("log(gen_tot_da)", "~",  paste(c( "log(price+1)", controls_var_ni), collapse = " + ")),
  paste(paste(c(exog_instr_d,  controls_var_ni), collapse = " + ")),
  sep = " | "
))


m_iv_s_ortho_ni  <- ivreg(iv_formula_supply_str_ortho_ni , data = price_indispo_merge_delta_clean)
m_iv_d_ortho_ni <- ivreg(iv_formula_demand_str_ortho_ni , data = price_indispo_merge_delta_clean)

nw_se_s_ortho_ni <- NeweyWest(m_iv_s_ortho_ni, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les r?sultats des coefficients avec SE NW
nw_s_ortho_ni <- coeftest(m_iv_s_ortho_ni, vcov. = nw_se_s_ortho_ni)

# --- Demand equation ---
nw_se_d_ortho_ni <- NeweyWest(m_iv_d_ortho_ni, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_d_ortho_ni <- coeftest(m_iv_d_ortho_ni, vcov. = nw_se_d_ortho_ni)

# First-stage diagnostics
diag_iv_d_ortho_ni <- summary(m_iv_d_ortho_ni, diagnostics = TRUE)
diag_iv_s_ortho_ni <- summary(m_iv_s_ortho_ni, diagnostics = TRUE)
# Extract weak instrument F-statistic
fstat_iv_d_ortho_ni <- diag_iv_d_ortho_ni$diagnostics["Weak instruments", "statistic"]
fstat_iv_s_ortho_ni <- diag_iv_s_ortho_ni$diagnostics["Weak instruments", "statistic"]




reg_table <- stargazer(
  nw_d_ortho,
  nw_d_ortho_ni,
  nw_s_ortho,# m_iv_int,
  nw_s_ortho_ni ,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV Supply", 'IV Supply Unbiased',   'IV Demand', 'IV Demand Unbiased'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_d_ortho, 2),             # IV supply
      round(fstat_iv_d_ortho_ni, 2),           # IV demand
      round(fstat_iv_s_ortho, 2),             # IV supply
      round(fstat_iv_s_ortho_ni, 2)
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)






lag_exog_s <- paste0("lag(", exog_instr_s, ", ", 1:12, ")")
lag_exog_d <- paste0("lag(", exog_instr_d, ", ", 1:12, ")")

# Example for supply with lagged exog_instr_s
iv_formula_supply_lag12_orth_ni <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_var_ni,lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_s instead of contemporaneous)
  paste(paste(c( exog_instr_s, lag_exog_d, lag_exog_s,controls_var_ni), collapse = " + ")),
  sep = " | "
))


m_iv_s_lag12_orth_ni <- ivreg(iv_formula_supply_lag12_orth_ni, data = price_indispo_merge_delta_clean)
nw_se_s_lag12_orth_ni <- NeweyWest(m_iv_s_lag12_orth_ni, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_s_lag12_orth_ni <- coeftest(m_iv_s_lag12_orth_ni, vcov. = nw_se_s_lag12_orth_ni)

iv_formula_demand_lag12_orth_ni <- as.formula(paste(
  # Structural equation
  paste("log(gen_tot_da)", "~", paste(c("log(price+1)", controls_var_ni, lag_exog_d, lag_exog_s), collapse = " + ")),
  # Instrument set (lagged exog_instr_d instead of contemporaneous)
  paste(paste(c(exog_instr_d, lag_exog_d, lag_exog_s, controls_var_ni), collapse = " + ")),
  sep = " | "
))

m_iv_d_lag12_orth_ni <- ivreg(iv_formula_demand_lag12_orth_ni, data = price_indispo_merge_delta_clean)
nw_se_d_lag12_orth_ni <- NeweyWest(m_iv_d_lag12_orth_ni, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_d_lag12_orth_ni <- coeftest(m_iv_d_lag12_orth_ni, vcov. = nw_se_d_lag12_orth_ni)


## --- Supply lag 12 ---
diag_iv_s_l12_orth_ni <- summary(m_iv_s_lag12_orth_ni, diagnostics = TRUE)
fstat_iv_s_l12_orth_ni <- diag_iv_s_l12_orth_ni$diagnostics["Weak instruments", "statistic"]


## --- Demand lag 12 ---
diag_iv_d_l12_orth_ni <- summary(m_iv_d_lag12_orth_ni, diagnostics = TRUE)
fstat_iv_d_l12_orth_ni <- diag_iv_d_l12_orth_ni$diagnostics["Weak instruments", "statistic"]



reg_table <- stargazer(
  nw_d_lag12_orth_ni ,
  nw_d_ortho_ni,
  nw_s_lag12_orth_ni ,# m_iv_int,
  nw_s_ortho_ni  ,
  type = "text",
  title = "Regression Results",
  column.labels = c("IV Supply  Unbiased", 'IV Supply',   'IV Demand  Unbiased', 'IV Demand'),#,
  add.lines = list(
    c("First-stage F-stat",
      round(fstat_iv_d_l12_orth_ni , 2),             # IV supply
      round(fstat_iv_d_ortho_ni, 2),           # IV demand
      round(fstat_iv_s_l12_orth_ni , 2),             # IV supply
      round(fstat_iv_s_ortho_ni, 2)
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot_da", "price" )
)


#faire des robustesses sur la gamme de prix en prenant des prix où mes variations sont limitantes

