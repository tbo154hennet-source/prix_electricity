
library(AER)

#elasticity at the monthly level
#colnames(IV_data_log_diff_esp)
#colnames(IV_data_log_diff_fr)

#wind_forecast

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff_elas.parquet')

# correct for the Iberian exception
colnames(IV_data_log_diff_esp)
IV_data_log_diff_esp$observation_date <- as.Date(IV_data_log_diff_esp$observation_date)
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply,
    temp_excess_instrument = temp_excess_supply
  )
IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    ffr_diff = ffr - lag(as.numeric(ffr))
  )%>%
  fill(ffr_diff, diff_cbf, .direction = "up")



controls_vars <- c("Spot_gas",    
                   "as.factor(year)")

ols_formula_d <- as.formula(paste(
  paste("log(spot_price)", "~",  paste(c("log(gen_tot)", controls_vars), collapse = " + "))))  

m_ols_d <- ivreg(ols_formula_d , data = IV_data_log_diff_esp)

ols_formula_s <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(spot_price)", controls_vars), collapse = " + "))))  

m_ols_s <- ivreg(ols_formula_s , data = IV_data_log_diff_esp)

# Build the formula as a string
iv_supply <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(spot_price)", controls_vars), collapse = " + ")),
  paste(paste(c("delta_s_total", controls_vars), collapse = " + ")),
  sep = " | "
))

# iv_supply_da <- as.formula(paste(
#   paste("log(gen_tot_da)", "~",  paste(c("log(spot_price)", controls_vars), collapse = " + ")),
#   paste(paste(c("delta_s_total", controls_vars), collapse = " + ")),
#   sep = " | "
# ))



m_iv_s <- ivreg(iv_supply , data = IV_data_log_diff_esp)
# m_iv_s_da <- ivreg(iv_supply_da , data = IV_data_log_diff_esp)


# Build the formula as a string
iv_demand <- as.formula(paste(
  paste("log(spot_price)", "~",  paste(c("log(gen_tot)", controls_vars), collapse = " + ")),
  paste(paste(c("delta_d_total", controls_vars), collapse = " + ")),
  sep = " | "
))

# iv_demand_da <- as.formula(paste(
#   paste("log(gen_tot_da)", "~",  paste(c("log(spot_price)", controls_vars), collapse = " + ")),
#   paste(paste(c("delta_d_total", controls_vars), collapse = " + ")),
#   sep = " | "
# ))


m_iv_d <- ivreg(iv_demand , data = IV_data_log_diff_esp)
#m_iv_d_da <- ivreg(iv_demand_da , data = IV_data_log_diff_esp)

controls_Bonte <- c(
    # "intraday_forecast" ,"unpredicted_renewables",
  #"cbf_es",
    "temperature_supply","precipitation_supply"  ,
    "relative_humidity_supply", "Spot_gas", 
    "as.factor(year)" 
  )
# Build the formula as a string
# Build the formula as a string

#iv_formula

# Build the formula as a string
Bonte_formula_str <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(spot_price)", controls_Bonte), collapse = " + ")),
  paste(paste(c("wind_forecast", controls_Bonte), collapse = " + ")),
  sep = " | "
))
m_Bonte <- ivreg(Bonte_formula_str , data = IV_data_log_diff_esp)
summary(m_Bonte)

# Create LaTeX code with regression results
reg_table_renwables_bis <- stargazer(
  
  
  m_ols_d,
  m_iv_d,
  # m_iv_d_da,
  m_ols_s,
  m_iv_s,
  m_Bonte,
  # m_iv_s_da,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS Supply", "IV Supply", "OLS Demand",  'IV Demand', "IV Wind"),
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


coef_se_ols_d <- extract_coef_se(m_ols_d, "log(gen_tot)")
coef_se_iv_d    <- extract_coef_se(m_iv_d, "log(gen_tot)")
coef_se_ols_s <- extract_coef_se(m_ols_s, "log(spot_price)")
coef_se_iv_s    <- extract_coef_se(m_iv_s, "log(spot_price)")
coef_se_Bonte_all    <- extract_coef_se(m_Bonte, "log(spot_price)" )



df <- data.frame(
  Model = c("OLS Supply", "IV Supply", "OLS Demand", 'IV Demand', 'Wind Demand'),
  Estimate = c(coef_se_ols_d["est"], coef_se_iv_d["est"],
               coef_se_ols_s["est"],coef_se_iv_s["est"], coef_se_Bonte_all["est"]),
  StdError = c(coef_se_ols_d["se"], coef_se_iv_d["se"],
               coef_se_ols_s["se"],coef_se_iv_s["se"], coef_se_Bonte_all["se"]
  )
)


# Compute elasticity and SE using delta method
df$Elasticity <- 1 / df$Estimate
df$Elasticity_SE <- df$StdError / (df$Estimate^2)

# Format string with SE in parentheses

#élasticité de l’offre
df$Elasticity_Demand <- paste0(
  round(df$Elasticity, 3), " (", round(df$Elasticity_SE, 3), ")"
)
#élasticité prix de la demande
df$Elasticity_Supply <- paste0(
  round(df$Estimate, 3), " (", round(df$StdError, 3), ")"
)


#reate a named vector with the elasticity (SE) strings
df_tot <- rbind(df %>% filter(Model == "OLS Supply"  |Model == "IV Supply")%>%
                  dplyr::select(Model, Elasticity_Demand)%>%
                  rename(Elasticity_CI = Elasticity_Demand),
                df %>%
                  filter(Model == "OLS Demand" |
                           Model == 'IV Demand' |
                           Model == 'Wind Demand'
                  )%>%
                  dplyr::select(Model, Elasticity_Supply) %>%
                  rename(Elasticity_CI = Elasticity_Supply))

# df_tot <- df%>%
#           dplyr::select(Model, Elasticity_Demand) %>% 
#           rename(Elasticity_CI = Elasticity_Demand)

df_clean <- df_tot %>%
  mutate(
    Instrument = case_when(
      grepl("OLS", Model) ~ "OLS",
      grepl("IV", Model) ~ "IV",
      grepl("Wind", Model) ~ "Bonte et al (2015)"
    ),
    Type = case_when(
      grepl("Supply", Model) ~ "Supply",
      grepl("Demand", Model) ~ "Demand"
    )
  )

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
write.csv(df_clean, "df_clean_macro.csv")
# Passer en format large : lignes = Type, colonnes = Instrument
df_plot_table_wide <- df_clean %>%
  dplyr::select(Type, Instrument, Elasticity_CI) %>%
  complete(Type, Instrument, fill = list(Elasticity_CI = "-")) %>% 
  mutate(Instrument = factor(Instrument, levels = c("OLS", "Bonte et al (2015)", "IV"))) %>%
  pivot_wider(names_from = Instrument, values_from = Elasticity_CI)



gt_table <- df_plot_table_wide %>%
  gt() %>%
  cols_label(
    OLS = "OLS",
    IV = "IV",
    `Bonte et al (2015)` = "Bonte et al (2015)"
  ) %>%
  tab_header(
    title = "Elasticities of Supply and Demand"
  )

gt_table

save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table,
  filename = "macro_elasticity_table.tex"
)





# on fai la même chose pour la France avec le wind instrument

Bonte_formula_str_fr <- as.formula(paste(
  paste("log(gen_tot+1)", "~",  paste(c("log(spot_price+1)", "Spot_gas" , "as.factor(year)"), collapse = " + ")),
  paste(paste(c("wind_forecast",  "Spot_gas", "as.factor(year)" ), collapse = " + ")),
  sep = " | "
))

m_Bonte_fr <- ivreg(Bonte_formula_str_fr , data = IV_data_log_diff_fr[-1,]
                      )

summary(m_Bonte_fr)












