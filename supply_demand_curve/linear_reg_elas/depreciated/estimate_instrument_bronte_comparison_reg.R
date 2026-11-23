
#get a version with wind speed


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
coef_supply_unmatched_fore_wind_sun <- readRDS("reg_shift_result_qty_supply_unmatched_sparse_fore_wind_sun_variation.rds")
coef_supply_unmatched_fore_wind_sun_data <- rbindlist(coef_supply_unmatched_fore_wind_sun)
#coef_supply_unmatched_data_ins <- coef_supply_unmatched_data[coef_supply_unmatched_data$variable %in% names_instruments,]
colnames(coef_supply_unmatched_fore_wind_sun_data)

coef_demand_unmatched_fore_wind_sun <- readRDS("reg_shift_result_qty_demand_unmatched_sparse_fore_wind_sun_variation.rds")
coef_demand_unmatched_fore_wind_sun_data <- rbindlist(coef_demand_unmatched_fore_wind_sun)
#coef_demand_unmatched_data_ins <- coef_demand_unmatched_data[coef_demand_unmatched_data$variable %in% names_instruments,]

coef_supply_unmatched_wind_sun <- readRDS("reg_shift_result_qty_supply_unmatched_sparse_wind_sun_variation.rds")
coef_supply_unmatched_wind_sun_data <- rbindlist(coef_supply_unmatched_wind_sun)
#coef_supply_unmatched_data_ins <- coef_supply_unmatched_data[coef_supply_unmatched_data$variable %in% names_instruments,]

coef_demand_unmatched_wind_sun <- readRDS("reg_shift_result_qty_demand_unmatched_sparse_wind_sun_variation.rds")
coef_demand_unmatched_wind_sun_data <- rbindlist(coef_demand_unmatched_wind_sun)
#coef_demand_unmatched_data_ins <- coef_demand_unmatched_data[coef_demand_unmatched_data$variable %in% names_instruments,]

coef_demand_combined <- rbind(coef_demand_unmatched_fore_wind_sun_data, coef_demand_unmatched_wind_sun_data)
coef_supply_combined <- rbind(coef_supply_unmatched_fore_wind_sun_data, coef_supply_unmatched_wind_sun_data)


instruments <- c("Fore_Solar",
                 "Fore_Wind",
                 "wind_speed_100m",
                 "global_tilted_irradiance")

instruments_supply <-instruments 
names_instruments <- instruments

instruments %in% colnames(price_indispo_merge)

colSums(is.na(price_indispo_merge[, instruments]))


generate_quantity_variation_complete <- function(row) {
  
  target_price <- as.numeric(row["price"])  # extraire le prix de la ligne
  # instruments doit être défini globalement (vecteur de longueur 5)
  # ex: instruments <- c("inst1", "inst2", "inst3", "inst4", "inst5")
  
  results <- data.frame(variable = instruments,
                        supply_variation = NA_real_,
                        demand_variation = NA_real_,
                        delta_q = NA_real_)
  #i =2
  for (i in seq_along(instruments)) {
    var_supply <- instruments[i]
    var_demand <- instruments[i]
    
    
    if (var_supply %in% coef_supply_combined$variable) {
      supply_var <- linear_interpolate(df= coef_supply_combined, 
                                       variable_name=var_supply, target_price)*as.numeric(row[var_supply])
    } else {
      supply_var <- NA_real_
    }
    if (var_demand %in% coef_demand_combined$variable) {
      demand_var <- linear_interpolate(coef_demand_combined, var_demand, target_price)*as.numeric(row[var_demand])
    } else {
      demand_var <- NA_real_
    }
    
    results$supply_variation[i] <- supply_var
    results$demand_variation[i] <- demand_var
    results$delta_q[i] <- supply_var + demand_var
  }
  
  return(c(results$supply_variation, results$demand_variation))  # vecteur de longueur 5
}

handlers(global = TRUE)
progressr::with_progress({
  p <- progressr::progressor(steps = nrow(price_indispo_merge))
  
  delta_q_list <- map2(seq_len(nrow(price_indispo_merge)), price_indispo_merge$price, function(i, price) {
    p()
    tryCatch({
      row <- price_indispo_merge[i, ]
      generate_quantity_variation_complete(row)
    }, error = function(e) {
      message(sprintf("Erreur à l'index %d (prix = %.2f) : %s", i, price, e$message))
      rep(NA_real_, length(instruments))
    })
  })
})

getwd()
#saveRDS(delta_q_list, "delta_q_list_supply_demand_renewable.rds")
delta_q_matrix <- do.call(rbind, delta_q_list)
colnames(delta_q_matrix) <- c(paste0("delta_s_", instruments), paste0("delta_d_", instruments))


# Ajouter au dataframe
price_indispo_merge_delta <- bind_cols(price_indispo_merge, as.data.frame(delta_q_matrix))
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
write_parquet(price_indispo_merge_delta, "price_indispo_merge_delta_supply_demand_renewable.parquet")




#using only wind as bronte did

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_bronte <- read_parquet("price_indispo_merge_delta_supply_demand_renewable.parquet")

colnames(price_indispo_merge_bronte)

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge <- read_parquet("price_indispo_merge_esp_NTC.parquet")


#add wind speed and Fore Wind
col_com <- intersect(colnames(price_indispo_merge_bronte), colnames(price_indispo_merge))


# price_indispo_merge_bronte_clean <- left_join(price_indispo_merge_bronte, price_indispo_merge,
#                                         by = col_com)

# Voir les doublons

colnames(price_indispo_merge_bronte)
# Build the formula as a string

controls_delta <- c(
  "delta_s_Fore_Solar" ,                       
  "delta_s_Fore_Wind" ,                        
  "delta_s_wind_speed_100m",                   
  "delta_s_global_tilted_irradiance",          
  "delta_d_Fore_Solar" ,                       
  "delta_d_Fore_Wind" ,                        
  "delta_d_wind_speed_100m" ,                  
  "delta_d_global_tilted_irradiance"
)
#iv_formula

# Build the formula as a string
# Build the formula as a string
# Orthogonaliser delta_d_Fore_Wind par rapport ? delta_d_wind_speed_100m
# Cr?er la variable directement avec NA quand il manque des valeurs

# Orthogonaliser delta_s_Fore_Wind par rapport ? delta_d_Fore_Wind
mod_s <- lm(delta_s_wind_speed_100m ~ delta_d_wind_speed_100m, 
            data = price_indispo_merge_bronte, na.action = na.exclude)

price_indispo_merge_bronte$delta_s_wind_speed_100m_ortho <- resid(mod_s)

# Orthogonaliser delta_d_wind_speed_100m par rapport ? delta_s_wind_speed_100m
mod_d <- lm(delta_d_wind_speed_100m ~ delta_s_wind_speed_100m, 
            data = price_indispo_merge_bronte, na.action = na.exclude)

price_indispo_merge_bronte$delta_d_wind_speed_100m_ortho <- resid(mod_d)


mod_s <- lm(delta_s_Fore_Wind ~ delta_d_Fore_Wind, 
            data = price_indispo_merge_bronte, na.action = na.exclude)

price_indispo_merge_bronte$delta_s_Fore_Wind_ortho <- resid(mod_s)

# Orthogonaliser delta_d_Fore_Wind par rapport ? delta_s_Fore_Wind
mod_d <- lm(delta_d_Fore_Wind ~ delta_s_Fore_Wind, 
            data = price_indispo_merge_bronte, na.action = na.exclude)

price_indispo_merge_bronte$delta_d_Fore_Wind_ortho <- resid(mod_d)


price_indispo_merge_bronte$year <- year(price_indispo_merge_bronte$delivery_date)
price_indispo_merge_bronte$month <- month(price_indispo_merge_bronte$delivery_date)
price_indispo_merge_bronte$weekday <- weekdays(price_indispo_merge_bronte$delivery_date)
price_indispo_merge_bronte$creneau <- hour(price_indispo_merge_bronte$datetime)
# Créer une version "nettoyee



controls_clean <- controls_vars[!grepl("^as\\.factor\\(", controls_vars)]

#colSums(is.na(price_indispo_merge_bronte[, controls_clean])) #check if there are NA in the instruments
extended_controls <- c( "cold_excess_supply"  , "heat_excess_supply","global_tilted_irradiance",
                       "temperature_supply"  , "precipitation_supply" ,    "relative_humidity_supply",
                       "Published.Unavailable.Capacity" , "Published.Predicted.Unavailable.Capacity", "cbf_es"
)

price_indispo_merge_bronte_clean <- price_indispo_merge_bronte %>%
  rename(renewable_day_ahead_forecast = day_ahead_forecast) %>% 
  dplyr::select(all_of(c(controls_delta, controls_clean, extended_controls, "year", "gen_tot","load_tot", "price", "spot_gas",
                         "month", "weekday", "creneau", "wind_speed_100m", "Fore_Wind", "intraday_Wind", "Wind Onshore"))) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.) )   %>% 
  filter(price >= 0)
colnames(price_indispo_merge_bronte)


#iv_formula

# sapply(price_indispo_merge_bronte_clean[, colnames(price_indispo_merge_bronte_clean)], function(x) sum(is.na(x)))
# sapply(price_indispo_merge_bronte_clean[, colnames(price_indispo_merge_bronte_clean)], function(x) sum(is.infinite(x)))

price_indispo_merge_bronte_clean %>% 
  mutate(update_wind = (intraday_Wind -Fore_Wind) ) %>% 
  select(wind_speed_100m, delta_s_wind_speed_100m,
         Fore_Wind, delta_s_Fore_Wind, intraday_Wind,
         update_wind
         ) %>%
  cor( use = "pairwise.complete.obs")

getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
write_parquet(price_indispo_merge_bronte_clean, "price_indispo_merge_bronte_clean.parquet")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_bronte_clean <- read_parquet( "price_indispo_merge_bronte_clean.parquet")




# Build the formula as a string
ols_formula_str <- as.formula(
  paste("load_tot", "~",  paste(c("price", controls_Bonte), collapse = " + ")),
)


m_ols_c <- ivreg(ols_formula_str , data = price_indispo_merge_bronte_clean)


# Build the formula as a string
bronte_formula_str <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_Bonte), collapse = " + ")),
  paste(paste(c("wind_speed_100m", controls_Bonte), collapse = " + ")),
  sep = " | "
))

m_bronte_c <- ivreg(bronte_formula_str , data = price_indispo_merge_bronte_clean)

# Build the formula as a string
bronte_formula_str_fore <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_Bonte), collapse = " + ")),
  paste(paste(c("Fore_Wind", controls_Bonte), collapse = " + ")),
  sep = " | "
))

m_bronte_fore <- ivreg(bronte_formula_str_fore , data = price_indispo_merge_bronte_clean)


# Build the formula as a string
iv_formula_str <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_Bonte), collapse = " + ")),
  paste(paste(c("delta_s_wind_speed_100m", controls_Bonte), collapse = " + ")),
  sep = " | "
))

m_iv_c <- ivreg(iv_formula_str , data = price_indispo_merge_bronte_clean)

# Build the formula as a string
iv_formula_str_fore <- as.formula(paste(
  paste("load_tot", "~",  paste(c("price", controls_Bonte), collapse = " + ")),
  paste(paste(c("delta_s_Fore_Wind", controls_Bonte), collapse = " + ")),
  sep = " | "
))

m_iv_fore <- ivreg(iv_formula_str_fore , data = price_indispo_merge_bronte_clean)


# Compute first-stage diagnostics for each model
diag_bronte_c     <- summary(m_bronte_c, diagnostics = TRUE)
diag_bronte_fore  <- summary(m_bronte_fore, diagnostics = TRUE)
diag_iv_c         <- summary(m_iv_c, diagnostics = TRUE)
diag_iv_fore      <- summary(m_iv_fore, diagnostics = TRUE)

# Extract weak instrument F-statistics
fstat_bronte_c    <- diag_bronte_c$diagnostics["Weak instruments", "statistic"]
fstat_bronte_fore <- diag_bronte_fore$diagnostics["Weak instruments", "statistic"]
fstat_iv_c        <- diag_iv_c$diagnostics["Weak instruments", "statistic"]
fstat_iv_fore     <- diag_iv_fore$diagnostics["Weak instruments", "statistic"]




# Create LaTeX code with regression results
reg_table_renwables_bis <- stargazer(
  m_ols_c,
  m_bronte_c,
  m_bronte_fore,
  m_iv_c,
  m_iv_fore,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS",  "Bonte Wind Speed", 'Bonte Windpower Forecast',
                    "IV Wind Speed", "IV Windpower Forecast"),#,
  add.lines = list(
    c("First-stage F-stat",
      "",                               # no F for OLS
      round(fstat_bronte_c , 2),
      round(fstat_bronte_fore, 2), # no F for Bonte,             # IV demand
      round(fstat_iv_c, 2),             # IV supply
      round(fstat_iv_fore, 2)
    )),
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot", "price" )
)


extract_coef_se <- function(model, variable = "load_tot") {
  s <- summary(model)
  est <- s$coefficients[variable, "Estimate"]
  se  <- s$coefficients[variable, "Std. Error"]
  return(c(est = est, se = se))
}

coef_se_bronte_c  <- extract_coef_se(model = m_bronte_c, variable="log(price + 1)")
coef_se_bronte_fore  <- extract_coef_se(m_bronte_fore, "log(price + 1)")
coef_se_iv_wind_c  <- extract_coef_se(m_iv_c, "log(price + 1)")
coef_se_iv_wind_fore  <- extract_coef_se(m_iv_fore, "log(price + 1)")


df_wind <- data.frame(
  Model = c("Bonte Wind Speed", 'Bonte Windpower Forecast',
            "IV Wind Speed", "IV Windpower Forecast"),
  Estimate = c(coef_se_bronte_c["est"], coef_se_bronte_fore["est"],
               coef_se_iv_wind_c["est"], coef_se_iv_wind_fore["est"]  ),
  StdError = c(coef_se_bronte_c["se"], coef_se_bronte_fore["se"],
               coef_se_iv_wind_c["se"], coef_se_iv_wind_fore["se"]  )
)


df_wind$Elasticity_CI <- paste0(
  round(df_wind$Estimate, 4), " (", round(df_wind$StdError, 4), ")"
)



df_wind_clean <- df_wind %>%
  mutate(
    Instrument = case_when(
      grepl("IV", Model) ~ "IV",
      grepl("Bonte", Model) ~ "Bonte et al (2015)"
    ),
    Type = case_when(
      grepl("Wind Speed", Model) ~ "Wind Speed",
      grepl("Windpower Forecast", Model) ~ "Windpower Forecast"
    )
  )

# Passer en format large : lignes = Type, colonnes = Instrument
df_wind_plot_table_wide <- df_wind_clean %>%
  dplyr::select(Type, Instrument, Elasticity_CI) %>%
  complete(Type, Instrument, fill = list(Elasticity_CI = "-")) %>% 
  mutate(Instrument = factor(Instrument, levels = c("Bonte et al (2015)", "IV"))) %>%
  pivot_wider(names_from = Instrument, values_from = Elasticity_CI)



gt_table <- df_wind_plot_table_wide %>%
  gt() %>%
  cols_label(
    IV = "IV",
    `Bonte et al (2015)` = "Bonte et al (2015)"
  ) %>%
  tab_header(
    title = "Elasticities of Supply"
  )

gt_table


save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
gtsave(
  gt_table,
  filename = "elasticity_table_wind.tex"
)

# Read the file
tex_lines <- readLines("elasticity_table_wind.tex")

# Keep only lines between \begin{tabular} and \end{tabular*}
tabular_lines <- tex_lines[grep("\\\\begin\\{tabular", tex_lines):
                             grep("\\\\end\\{tabular\\*\\}", tex_lines)]

# Write to a new file
writeLines(tabular_lines, "elasticity_table_wind.tex")


