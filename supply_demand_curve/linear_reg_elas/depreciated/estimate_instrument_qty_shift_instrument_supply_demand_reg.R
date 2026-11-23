library(AER)
library(stargazer)
library(knitr)
library(gt)
library(sandwich)
library(lmtest)





instruments <- c("Fore_Wind",
                 "Fore_Solar",
                 "Published.Predicted.Unavailable.Capacity",
                 #"renewable_day_ahead_forecast",
                 #"Published.Unavailable.Nuclear.Capacity"
                 "cbf_es",
                 "cold_excess",
                 "heat_excess"
                 )
instruments_supply <- c("Fore_Wind",
                        "Fore_Solar",
                        "Published.Predicted.Unavailable.Capacity",
                        #"renewable_day_ahead_forecast",
                        #"Published.Unavailable.Nuclear.Capacity"
                       "cbf_es",
                        "cold_excess_supply",
                        "heat_excess_supply"
                       )
names_instruments <- c(
                      "Fore_Wind",
                      "Fore_Solar",
                      "Published.Predicted.Unavailable.Capacity",
                        #"renewable_day_ahead_forecast",
                       #"Published.Unavailable.Nuclear.Capacity",
                       "cbf_es",
                       "cold_excess_supply",
                       "cold_excess_demand",
                       "heat_excess_supply",
                       "heat_excess_demand")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo.parquet")
#price_indispo_merge_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo_no_unavail.parquet")


# setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
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
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab"
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
#r?interpr?ter en demand shock dans nos donn?es

colnames(price_indispo_merge_delta)
# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge_delta$delta_s_total <- rowSums(price_indispo_merge_delta[, cols_supply], na.rm = TRUE)
price_indispo_merge_delta$delta_d_total <- rowSums(price_indispo_merge_delta[, cols_demand], na.rm = TRUE)


col_names <- c("weekday" , "creneau", names_instruments, "price", "datetime", "delivery_date", "spot_gas",
               colnames_with_capacity, "load_tot","load_tot_conso" , "load_tot_net" ,
               "temperature_supply","precipitation_supply", "relative_humidity_supply" ,
               "intraday_forecast"  ,"unpredicted_renewables", "load_tot"    ,                           
               "wind_solar_tot","price" ,"year"   ,"month"  )

price_indispo_merge_delta_clean <- price_indispo_merge_delta %>%
  dplyr::select(-log_price_diff, -price_diff) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.)) %>%           # garde seulement les lignes sans NA dans ces colonnes
  distinct()

dep_var <- "price"
endog_vars <- c("load_tot")
#endog_vars <- c("load_tot")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
extended_controls <- c(controls_vars, "unpredicted_renewables",  "intraday_update_renewables" )


# Orthogonaliser delta_s_total par rapport ? delta_d_total
price_indispo_merge_delta_clean$delta_s_orth <- resid(
  lm(delta_s_total ~ delta_d_total, data = price_indispo_merge_delta_clean)
)

# Orthogonaliser delta_d_total par rapport ? delta_s_total
price_indispo_merge_delta_clean$delta_d_orth <- resid(
  lm(delta_d_total ~ delta_s_total, data = price_indispo_merge_delta_clean)
)



price_indispo_merge_delta_clean %>% 
  dplyr::select(delta_s_total, delta_d_total, delta_s_orth, delta_d_orth ) %>% 
  cor(use = "complete.obs")


ols_formula <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_vars), collapse = " + "))))  

m_ols <- ivreg(ols_formula , data = price_indispo_merge_delta_clean)

nw_se_ols  <- NeweyWest(m_ols , lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les r?sultats des coefficients avec SE NW
nw_ols  <- coeftest(m_ols , vcov. = nw_se_ols )


# Combine exogenous regressors and instruments

exog_instr_s <- "delta_s_total"
exog_instr_d <- "delta_d_total"


#iv_formula
iv_formula_supply_str <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_vars, exog_instr_d ), collapse = " + ")),
  paste(paste(c(exog_instr_s, exog_instr_d , controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str <- as.formula(paste(
  paste("load_tot", "~",  paste(c( "price", controls_vars, exog_instr_s), collapse = " + ")),
  paste(paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s <- ivreg(iv_formula_supply_str , data = price_indispo_merge_delta_clean)
m_iv_d <- ivreg(iv_formula_demand_str , data = price_indispo_merge_delta_clean)

# --- Supply equation ---
# Newey-West standard errors, lag = 24 (ou autre selon fr?quence des donn?es)
nw_se_s <- NeweyWest(m_iv_s, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les r?sultats des coefficients avec SE NW
nw_s <- coeftest(m_iv_s, vcov. = nw_se_s)

# --- Demand equation ---
nw_se_d <- NeweyWest(m_iv_d, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_d <- coeftest(m_iv_d, vcov. = nw_se_d)


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
  paste("load_tot", "~",  paste(c("price", controls_Bonte), collapse = " + ")),
  paste(paste(c("Fore_Wind", controls_Bonte), collapse = " + ")),
  sep = " | "
))
m_Bonte <- ivreg(Bonte_formula_str , data = price_indispo_merge_delta_clean)
nw_se_Bonte  <- NeweyWest(m_Bonte , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_Bonte  <- coeftest(m_Bonte , vcov. = nw_se_Bonte)


controls_demand <- c(setdiff(controls_Bonte, "temperature_supply"), "Fore_Wind", "Fore_Solar")

formula_demand_str <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_demand), collapse = " + ")),
  paste(paste(c("cold_excess_demand", "heat_excess_demand", controls_demand), collapse = " + ")),
  sep = " | "
))

m_demand <- ivreg(formula_demand_str , data = price_indispo_merge_delta_clean)

nw_se_demand  <- NeweyWest(m_demand , lag = 24, prewhite = FALSE, adjust = TRUE)
nw_demand  <- coeftest(m_demand , vcov. = nw_se_Bonte)




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


#summary(m_iv_s, diagnostics = TRUE)$diagnostics




# vcov_cluster_iv <- vcovCL(m_iv , type = "HC0", cluster = ~ aav2020)
# # Erreurs standard clusterisées
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
  paste("load_tot", "~",  paste(c("price", controls_vars ), collapse = " + ")),
  paste(paste(c(exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str_ortho <- as.formula(paste(
  paste("load_tot", "~",  paste(c( "price", controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d,  controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s_ortho  <- ivreg(iv_formula_supply_str_ortho , data = price_indispo_merge_delta_clean)
m_iv_d_ortho <- ivreg(iv_formula_demand_str_ortho , data = price_indispo_merge_delta_clean)

nw_se_s_ortho <- NeweyWest(m_iv_s_ortho, lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les r?sultats des coefficients avec SE NW
nw_s_ortho <- coeftest(m_iv_s_ortho, vcov. = nw_se_s_ortho)

# --- Demand equation ---
nw_se_d_ortho <- NeweyWest(m_iv_d_ortho, lag = 24, prewhite = FALSE, adjust = TRUE)
nw_d_ortho <- coeftest(m_iv_d_ortho, vcov. = nw_se_d_ortho)

# First-stage diagnostics
diag_iv_d_ortho <- summary(m_iv_d_ortho, diagnostics = TRUE)
diag_iv_s_ortho <- summary(m_iv_s_ortho, diagnostics = TRUE)
# Extract weak instrument F-statistic
fstat_iv_d_ortho <- diag_iv_d_ortho$diagnostics["Weak instruments", "statistic"]
fstat_iv_s_ortho <- diag_iv_s_ortho$diagnostics["Weak instruments", "statistic"]



reg_table <- stargazer(
  nw_ols,
  nw_d_ortho,
  nw_demand, 
  nw_s_ortho,# m_iv_int,
  nw_Bonte ,
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
  keep = c("load_tot", "price" )
)

# # Create LaTeX code with regression results
# reg_table <- stargazer(
#   nw_d,
#   nw_d_ortho,
#   nw_s,# m_iv_int,
#   nw_s_ortho ,
#   #m_renew_plant, 
#   type = "text",
#   title = "Regression Results",
#   column.labels = c( "Supply", "Supply Ortho",  'Demand', 'Demand Ortho'),#,
#   # add.lines = list(
#   #   c("First-stage F-stat",
#   #     "",                               # no F for OLS
#   #     round(fstat_iv_d, 2),             # IV demand
#   #     round(fstat_iv_s, 2),             # IV supply
#   #     round(fstat_iv_bronte, 2)                                # no F for Bonte
#   #   )),
#   add.lines = list(
#     c("First-stage F-stat",                               # no F for OLS
#       round(fstat_iv_d, 2), 
#       round(fstat_iv_d_ortho, 2),# IV demand
#       round(fstat_iv_s, 2),             # IV supply
#       round(fstat_iv_s_ortho, 2)                                # no F for Bonte
#     )),
#   keep = c("load_tot", "price" )
# )


# Extract estimate and standard error for 'load_tot' from each model
extract_coef_se <- function(model, variable = "load_tot") {
  s <- summary(model)
  est <- s$coefficients[variable, "Estimate"]
  se  <- s$coefficients[variable, "Std. Error"]
  return(c(est = est, se = se))
}

# Supposons que nw_Bonte est un objet coeftest
# extraire estimate et SE pour une variable donn?e
extract_coef_se_coeftest <- function(coeftest_obj, variable) {
  est <- coeftest_obj[variable, "Estimate"]
  se  <- coeftest_obj[variable, "Std. Error"]
  return(c(est = est, se = se))
}



# coef_se_ols <- extract_coef_se(m_ols, "price")
# coef_se_iv_d    <- extract_coef_se(m_iv_d, "price")
# coef_se_iv_s    <- extract_coef_se(m_iv_s, "price")
# coef_se_Bonte_all    <- extract_coef_se(m_Bonte, "price" )

coef_se_ols <- extract_coef_se_coeftest (nw_ols, "log(price + 1)")
coef_se_iv_d    <- extract_coef_se_coeftest (nw_d, "log(price + 1)")
coef_se_iv_s    <- extract_coef_se_coeftest (nw_s, "log(price + 1)")
coef_se_Bonte_all    <- extract_coef_se_coeftest (nw_Bonte, "log(price + 1)" )
coef_se_Bonte_demand    <- extract_coef_se_coeftest (nw_demand, "log(price + 1)" )
coef_se_iv_s_ortho    <- extract_coef_se_coeftest(nw_s_ortho, "log(price + 1)")
coef_se_iv_d_ortho    <- extract_coef_se_coeftest(nw_d_ortho, "log(price + 1)" )



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
#d?doubler la ligne OLS

df_new <- df_clean %>%
  # S?parer la ligne OLS en deux copies
  filter(Model == "OLS") %>%
  mutate(Type = "Supply") %>%
  bind_rows(
    df_clean %>% filter(Model == "OLS") %>% mutate(Type = "Demand")
  ) %>%
  # Ajouter les autres lignes non-OLS
  bind_rows(
    df_clean %>% filter(Model != "OLS")
  )

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
getwd()
write.csv(df_clean, "df_clean_da.csv")


# Passer en format large : lignes = Type, colonnes = Instrument
df_plot_table_wide <- df_new %>%
  dplyr::select(Type, Instrument, Elasticity_SE) %>%
  complete(Type, Instrument, fill = list(Elasticity_SE = "-")) %>% 
  mutate(Instrument = factor(Instrument, levels = c("OLS", "Bonte et al (2015)", "IV",  "IV Ortho"))) %>%
  pivot_wider(names_from = Instrument, values_from = Elasticity_SE)



gt_table <- df_plot_table_wide %>%
  gt() %>%
  cols_label(
    OLS = "OLS",
    IV = "IV",
    `IV Ortho` = "IV Ortho",
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
  filename = "elasticity_table.tex"
)



library(systemfit)

exog_instr_s <- "delta_s_total"
exog_instr_d <- "delta_d_total"

eq1 <- load_tot ~ price + spot_gas + as.factor(year) + as.factor(month) + 
  as.factor(weekday) + as.factor(creneau) + delta_d_total
eq2 <- load_tot ~ price + spot_gas + as.factor(year) + as.factor(month) + 
  as.factor(weekday) + as.factor(creneau) + delta_s_total
# Put equations in a list with names
system <- list(supply = eq1, demand = eq2)

# Instruments: for 3SLS we provide instrument matrix (here both Zs & Zd & W available)
# systemfit will detect endogenous = variables on left-of-tilde? To be explicit:syst
inst <- as.formula(
  paste("~", paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + "))
)

# Estimate with 3SLS (method = "3SLS")
fit_3sls <- systemfit(system, method = "3SLS", inst = inst, data = price_indispo_merge_delta_clean)

# Extraire coefficients
coefs <- coef(fit_3sls)

# Filtrer ceux contenant "price"
coefs_logprice <- coefs[grep("log\\(price\\)", names(coefs))]

# Afficher
cat("\nCoefficients contenant price :\n")
print(coefs_logprice)


# 
# setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
# price_indispo_merge_delta_tot <- read_parquet( "price_indispo_merge_delta_supply_demand_ortho.parquet")
# colnames(price_indispo_merge_delta_tot)
# 
# price_indispo_merge_delta_tot_clean <- price_indispo_merge_delta_tot %>%
#   dplyr::select(-log_price_diff, -price_diff) %>%       # sélectionne les colonnes dans col_names
#   filter(complete.cases(.)) %>%       # garde seulement les lignes sans NA dans ces colonnes
#   distinct()          # garde seulement les lignes sans NA dans ces colonnes
# 
# 
# price_indispo_merge_delta_tot_clean %>% 
#   dplyr::select(delta_s_total, delta_d_total, delta_s_ortho_total,delta_d_ortho_total ) %>% 
#   cor(use = "complete.obs")
# 
# exog_instr_s <- "delta_s_ortho_total"
# exog_instr_d <- "delta_d_ortho_total"
# 
# 
# #iv_formula
# iv_formula_supply_str_robust <- as.formula(paste(
#   paste("load_tot", "~",  paste(c("price", controls_vars, exog_instr_d ), collapse = " + ")),
#   paste(paste(c(exog_instr_s, exog_instr_d , controls_vars), collapse = " + ")),
#   sep = " | "
# ))
# 
# #iv_formula
# iv_formula_demand_str_robust <- as.formula(paste(
#   paste("load_tot", "~",  paste(c( "price", controls_vars, exog_instr_s), collapse = " + ")),
#   paste(paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + ")),
#   sep = " | "
# ))
# 
# 
# m_iv_s_robust <- ivreg(iv_formula_supply_str_robust , data = price_indispo_merge_delta_tot_clean)
# m_iv_d_robust <- ivreg(iv_formula_demand_str_robust , data = price_indispo_merge_delta_tot_clean)
# 
# 
# # Create LaTeX code with regression results
# reg_table <- stargazer(
#   #m_iv_d,
#   m_iv_d,
#   m_iv_d_robust,
#   #m_iv_s,
#   m_iv_s,
#   m_iv_s_robust,
#   type = "text",
#   title = "Regression Results",
#   column.labels = c("Demand Ortho", "Demand Robust",
#                     "Supply Ortho", "Supply Robust"),
#   keep = c("load_tot", "price")
# )
# 
