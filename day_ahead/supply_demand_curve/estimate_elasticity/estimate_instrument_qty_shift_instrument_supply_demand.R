library(AER)
library(stargazer)
library(knitr)
library(gt)


instruments <- c("renewable_day_ahead_forecast",
                 #"Published.Unavailable.Nuclear.Capacity"
                 "cbf_es",
                 "cold_excess",
                 "heat_excess",
                 "Published.Predicted.Unavailable.Capacity")
instruments_supply <- c("renewable_day_ahead_forecast",
                        #"Published.Unavailable.Nuclear.Capacity"
                       "cbf_es",
                        "cold_excess_supply",
                        "heat_excess_supply",
                        "Published.Predicted.Unavailable.Capacity")
names_instruments <- c("renewable_day_ahead_forecast",
                       #"Published.Unavailable.Nuclear.Capacity",
                       "cbf_es",
                       "cold_excess_supply",
                       "cold_excess_demand",
                       "heat_excess_supply",
                       "heat_excess_demand",
                       "Published.Predicted.Unavailable.Capacity")

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_delta <- read_parquet("price_indispo_merge_delta_supply_demand.parquet")


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

# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge_delta$delta_s_total <- rowSums(price_indispo_merge_delta[, cols_supply], na.rm = TRUE)
price_indispo_merge_delta$delta_d_total <- rowSums(price_indispo_merge_delta[, cols_demand], na.rm = TRUE)


col_names <- c("weekday" , "creneau", names_instruments, "price", "datetime", "delivery_date", "spot_gas",
               colnames_with_capacity, "gen_tot_da","gen_tot_da_conso" , "gen_tot_da_net" ,
               "temperature_supply","precipitation_supply", "relative_humidity_supply" ,
               "intraday_forecast"  ,"unpredicted_renewables", "gen_tot"    ,                           
               "wind_solar_tot","price" ,"year"   ,"month" , "price_diff" ,   "log_price_diff"  )

price_indispo_merge_delta_clean <- price_indispo_merge_delta %>%
  #dplyr::select(all_of(col_names)) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.))           # garde seulement les lignes sans NA dans ces colonnes


dep_var <- "log(price)"
endog_vars <- c("log(gen_tot)")
#endog_vars <- c("log(gen_tot_da)")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
extended_controls <- c(controls_vars, "unpredicted_renewables",  "intraday_update_renewables" )


ols_formula_d <- as.formula(paste(
  paste("log(price)", "~",  paste(c("log(gen_tot)", controls_vars), collapse = " + "))))  

m_ols_d <- ivreg(ols_formula_d , data = price_indispo_merge_delta_clean)

ols_formula_s <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(price)", controls_vars), collapse = " + "))))  

m_ols_s <- ivreg(ols_formula_s , data = price_indispo_merge_delta_clean)

# Combine exogenous regressors and instruments
exog_instr_s <- "delta_s_total"


exog_instr_d <- "delta_d_total"

#iv_formula
iv_formula_supply_str <- as.formula(paste(
  paste("log(gen_tot)", "~",  paste(c("log(price)",exog_instr_d, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_s,exog_instr_d, controls_vars), collapse = " + ")),
  sep = " | "
))

#iv_formula
iv_formula_demand_str <- as.formula(paste(
  paste("log(price)", "~",  paste(c("log(gen_tot)",exog_instr_s, controls_vars), collapse = " + ")),
  paste(paste(c(exog_instr_d, exog_instr_s, controls_vars), collapse = " + ")),
  sep = " | "
))


m_iv_s <- ivreg(iv_formula_supply_str , data = price_indispo_merge_delta_clean)
m_iv_d <- ivreg(iv_formula_demand_str , data = price_indispo_merge_delta_clean)


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
  paste("log(gen_tot)", "~",  paste(c("log(price)", controls_Bonte), collapse = " + ")),
  paste(paste(c(instruments_supply, controls_Bonte), collapse = " + ")),
  sep = " | "
))
m_Bonte <- ivreg(Bonte_formula_str , data = price_indispo_merge_delta_clean)


# Create LaTeX code with regression results
reg_table <- stargazer(
  m_ols_d,
  m_iv_d,
  m_ols_s,
  m_iv_s,# m_iv_int,
  m_Bonte ,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS Supply", "IV Supply", "OLS Demand",  'IV Demand', 'Bonte'),#,
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot", "price" )
)

#summary(m_iv_s, diagnostics = TRUE)$diagnostics

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


coef_se_ols_d <- extract_coef_se(m_ols_d, "log(gen_tot)")
coef_se_iv_d    <- extract_coef_se(m_iv_d, "log(gen_tot)")
coef_se_ols_s <- extract_coef_se(m_ols_s, "log(price)")
coef_se_iv_s    <- extract_coef_se(m_iv_s, "log(price)")
coef_se_Bonte_all    <- extract_coef_se(m_Bonte, "log(price)" )



df <- data.frame(
  Model = c("OLS Supply", "IV Supply", "OLS Demand", 'IV Demand', 'Bonte et al (2015) Demand'),
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
                           Model == 'IV Demand' |Model ==  'Bonte et al (2015) Demand'
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
      grepl("Bonte", Model) ~ "Bonte et al (2015)"
    ),
    Type = case_when(
      grepl("Supply", Model) ~ "Supply",
      grepl("Demand", Model) ~ "Demand"
    )
  )

getwd()
write.csv(df_clean, "df_clean_da.csv")


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

save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table,
  filename = "elasticity_table.tex"
)
# Read the file
tex_lines <- readLines("elasticity_table.tex")

# Keep only lines between \begin{tabular} and \end{tabular*}
tabular_lines <- tex_lines[grep("\\\\begin\\{tabular", tex_lines):
                             grep("\\\\end\\{tabular\\*\\}", tex_lines)]

getwd()
# Write to a new file
writeLines(tabular_lines, "elasticity_table.tex")


