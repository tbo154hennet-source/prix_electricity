
# Pour voir les équations de la first stage
summary(m_iv_d_bis, diagnostics = TRUE)



#iv_formula
iv_formula_demand_str_bis <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(price)",exog_instr_s, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_s,exog_instr_d, controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_supply_str_bis <- as.formula(paste(
  paste("log(price)", "~",  paste(c("log(gen_tot)",exog_instr_d, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_bis <- ivreg(iv_formula_supply_str_bis , data = price_indispo_merge_delta_clean)
m_iv_d_bis <- ivreg(iv_formula_demand_str_bis , data = price_indispo_merge_delta_clean)


# Create LaTeX code with regression results
reg_table <- stargazer(
  m_iv_d,
  m_iv_d_bis,
  m_iv_s,# m_iv_int,
  m_iv_s_bis,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c( "Qty Demand", 'Price Demand',"Price Supply", "Qty Supply"),#,
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot", "price" )
)



# Extract estimate and standard error for 'gen_tot' from each model
extract_coef_se <- function(model, variable = "log(gen_tot)") {
  s <- summary(model)
  est <- s$coefficients[variable, "Estimate"]
  se  <- s$coefficients[variable, "Std. Error"]
  return(c(est = est, se = se))
}



coef_se_iv_d    <- extract_coef_se(m_iv_d, "log(gen_tot)")
coef_se_iv_s    <- extract_coef_se(m_iv_s, "log(price)")
coef_se_iv_s_bis <- extract_coef_se(m_iv_s_bis, "log(gen_tot)")
coef_se_iv_d_bis <- extract_coef_se(m_iv_d_bis, "log(price)")




df <- data.frame(
  Model = c( "Qty Demand", 'Price Demand',"Price Supply", "Qty Supply"),
  Estimate = c( coef_se_iv_d["est"],coef_se_iv_d_bis["est"],
                coef_se_iv_s["est"],coef_se_iv_s_bis["est"]),
  StdError = c( coef_se_iv_d["se"], coef_se_iv_d_bis["se"],
                coef_se_iv_s["se"], coef_se_iv_s_bis["se"]
  )
)


# Compute elasticity and SE using delta method
df$Elasticity <- 1 / df$Estimate
df$Elasticity_SE <- df$StdError / (df$Estimate^2)

# Format string with SE in parentheses

#élasticité de l’offre
df$Elasticity_Qty <- paste0(
  round(df$Elasticity, 3), " (", round(df$Elasticity_SE, 3), ")"
)
#élasticité prix de la demande
df$Elasticity_Price <- paste0(
  round(df$Estimate, 3), " (", round(df$StdError, 3), ")"
)


#reate a named vector with the elasticity (SE) strings
df_tot <- rbind(df %>%
                  filter(grepl("Price", Model)) %>%
                  dplyr::select(Model, Elasticity_Price) %>%
                  rename(Elasticity_CI = Elasticity_Price),
                
                  df %>%
                  filter(grepl("Qty", Model))%>%
                  dplyr::select(Model, Elasticity_Qty) %>%
                  rename(Elasticity_CI = Elasticity_Qty))







#check for first stage


# Liste de tous tes modèles IV
iv_models <- list(
  m_iv_d = m_iv_d,
  m_iv_s = m_iv_s,
  # m_iv_int = m_iv_int,  # décommente si tu veux l'inclure
  m_iv_d_bis = m_iv_d_bis,
  m_iv_s_bis = m_iv_s_bis
)

# Fonction pour extraire le test de faibles instruments
extract_weak_iv_test <- function(model) {
  diag <- summary(model, diagnostics = TRUE)$diagnostics
  weak_inst_row <- diag["Weak instruments", , drop = FALSE]
  return(weak_inst_row)
}

# Appliquer à tous les modèles
weak_iv_tests <- lapply(iv_models, extract_weak_iv_test)

# Affichage clair
weak_iv_tests_df <- do.call(rbind, weak_iv_tests)
print(weak_iv_tests_df)
