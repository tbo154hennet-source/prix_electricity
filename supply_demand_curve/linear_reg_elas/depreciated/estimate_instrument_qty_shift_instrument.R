library(AER)
library(stargazer)
library(knitr)
library(gt)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_delta <- read_parquet("price_indispo_merge_delta.parquet")


# Construire les noms de colonnes
cols_to_sum <- paste0("delta_q_", instruments)

# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge_delta$delta_q_total <- rowSums(price_indispo_merge_delta[, cols_to_sum], na.rm = TRUE)





dep_var <- "log(price)"
endog_vars <- c("log(gen_tot)")
#endog_vars <- c("log(gen_tot_da)")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                     "as.factor(weekday)", "as.factor(creneau)"
                   )
extended_controls <- c(controls_vars, "unpredicted_renewables",  "intraday_update_renewables" )

# Combine exogenous regressors and instruments
exog_instr <- "delta_q_total"

ols_formula <- as.formula(paste(
  paste(dep_var, "~",  paste(c(endog_vars), collapse = " + "))))

ols_formula_str <- as.formula(paste(
  paste(dep_var, "~",  paste(c(endog_vars, controls_vars), collapse = " + "))))  

m_ols <- ivreg(ols_formula , data = price_indispo_merge_delta)

m_ols_c <- ivreg(ols_formula_str , data = price_indispo_merge_delta)

  
# Build the formula as a string
iv_formula<- as.formula(paste(
  paste(dep_var, "~",  paste(c(endog_vars), collapse = " + ")),
  paste(paste(exog_instr, collapse = " + ")),
  sep = " | "
))

#iv_formula

# Build the formula as a string
iv_formula_str <- as.formula(paste(
  paste(dep_var, "~",  paste(c(endog_vars, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv <- ivreg(iv_formula , data = price_indispo_merge_delta)

m_iv_c <- ivreg(iv_formula_str , data = price_indispo_merge_delta)



colnames(price_indispo_merge_delta)


bronte_style_formula <- as.formula(paste(
  paste(endog_vars, "~",  paste(dep_var), collapse = " + "),
  paste(paste(c(instruments_supply), collapse = " + ")),
  sep = " | "
))

# Build the formula as a string

bronte_style_formula_str <- as.formula(paste(
  paste(endog_vars, "~",  paste(c(dep_var, controls_vars), collapse = " + ")),
  paste(paste(c(instruments_supply, controls_vars), collapse = " + ")),
  sep = " | "
))

m_bronte_style <- ivreg(bronte_style_formula , data = price_indispo_merge_delta)

m_bronte_style_c <- ivreg(bronte_style_formula_str , data = price_indispo_merge_delta)


# Create LaTeX code with regression results
reg_table <- stargazer(
                      m_ols,
                      m_ols_c,
                       m_iv,# m_iv_int,
                       m_iv_c,
                       #m_renew_plant, 
                       type = "text",
                       title = "Regression Results",
                       column.labels = c("OLS", "OLS + Controls",  'IV', 'IV + Controls'),#,
                       #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
                       keep = c("gen_tot" )
)

# vcov_cluster_iv <- vcovCL(m_iv , type = "HC0", cluster = ~ aav2020)
# # Erreurs standard clusterisées
# cl_se_iv <- sqrt(diag(vcov_cluster_iv))

# Extract estimate and standard error for 'gen_tot' from each model
extract_coef_se <- function(model, variable = "log(gen_tot)") {
  s <- summary(model)
  est <- s$coefficients[variable, "Estimate"]
  se  <- s$coefficients[variable, "Std. Error"]
  return(c(est = est, se = se))
}

coef_se_ols   <- extract_coef_se(m_ols)
coef_se_ols_c <- extract_coef_se(m_ols_c)
coef_se_iv    <- extract_coef_se(m_iv)
coef_se_iv_c  <- extract_coef_se(m_iv_c)
coef_se_bronte_all    <- extract_coef_se(m_bronte_style, "log(price)" )
coef_se_bronte_all_c  <- extract_coef_se(m_bronte_style_c, "log(price)")

df <- data.frame(
  Model = c("OLS", "OLS + Controls", "IV", "IV + Controls", 
            "Bronte IV", "Bronte + Controls"),
  Estimate = c(coef_se_ols["est"], coef_se_ols_c["est"], 
               coef_se_iv["est"], coef_se_iv_c["est"],
              coef_se_bronte_all["est"], coef_se_bronte_all_c["est"] ),
  StdError = c(coef_se_ols["se"], coef_se_ols_c["se"], 
               coef_se_iv["se"], coef_se_iv_c["se"],
               coef_se_bronte_all["se"], coef_se_bronte_all_c["se"]
               )
)

# Compute elasticity and SE using delta method
df$Elasticity <- 1 / df$Estimate
df$Elasticity_SE <- df$StdError / (df$Estimate^2)

# Format string with SE in parentheses
df$Elasticity_CI <- paste0(
  round(df$Elasticity, 3), " (", round(df$Elasticity_SE, 3), ")"
)
df$Elasticity_Bronte <- paste0(
  round(df$Estimate, 3), " (", round(df$StdError, 3), ")"
)
# Create a named vector with the elasticity (SE) strings
df_tot <- rbind(df %>% filter(Model == "OLS + Controls" |Model == "OLS" |
                                Model =="IV"| Model =="IV + Controls")%>%
                    select(Model, Elasticity_CI),
                  df %>% 
                    filter(Model == "Bronte IV" |Model == "Bronte + Controls"
                           )%>%
                    select(Model, Elasticity_Bronte) %>% 
                  rename(Elasticity_CI = Elasticity_Bronte))

df_tot_table <- df_tot %>% 
  filter(Model == "OLS + Controls" | Model == "IV + Controls"| Model == "Bronte + Controls")



df_plot_table <-  rbind(df_tot_table %>% mutate(Instrument = "All"),
                        df_final_table %>% mutate(Instrument = "Wind")%>%
                          filter(Model != "OLS + Controls" & Model != "OLS"))


# Reorder for clarity (optional)
df_plot_table_wide <- df_plot_table %>%
  complete(Model, Instrument, fill = list(Elasticity_CI = "-")) %>% 
  pivot_wider(
    names_from = Instrument,
    values_from = Elasticity_CI
  )

gt_table <- df_plot_table_wide %>%
  gt() %>%
  cols_label(
    All = "All",
    Wind = "Wind"
  ) %>%
  tab_header(
    title = "Price Elasticity of Demand"
  )

save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table,
  filename = "elasticity_table.tex"
)

setwd()
cat(latex_table_horizontal, file = "elasticity_delta_method.tex")
