library(purrr)
library(fixest)
output_dir <- file.path(Sys.getenv("REPLICATION_ROOT", "."), "supply_demand_curve/outputs/coef_reg_no_conditioning")
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")
colnames(price_indispo_merge_esp_npc)

price_indispo_merge_esp_npc <- 
  price_indispo_merge_esp_npc %>%
  distinct(datetime, .keep_all = TRUE)

general_factors <- c(
  "Fore_Wind",
  #ces controles me font perdre 25000 lignes, je prefere les enlever
  
  #"intraday_update_wind", "unpredicted_wind",
  "Fore_Solar", 
  #"unpredicted_sun","intraday_update_sun",
  "spot_gas",
  "Published.Predicted.Unavailable.Capacity",
  "Unpublished.Predicted.Unavailable.Capacity",
  "cbf_es"
)

general_factors_indispo <- c(
  setdiff(general_factors, c("Published.Predicted.Unavailable.Capacity",
                             "Unpublished.Predicted.Unavailable.Capacity")),
  "Published.Unavailable.Capacity",
  "Unpublished.Unavailable.Capacity",
  "Published.Unavailable.Nuclear.Capacity",
  "Unpublished.Unavailable.Nuclear.Capacity"
)
control_factors <- c("relative_humidity_2m_ss", "precipitation_ss",
                     "relative_humidity_2m_demand", "precipitation_demand",
                     "relative_humidity_2m_sth", "precipitation_sth"
)

# temp_factors <- c( "cold_excess_ss", "heat_excess_ss","cold_excess_demand", 
#                    "heat_excess_demand","cold_excess_sth","heat_excess_sth")
temp_factors <- c( "cold_excess_supply", "heat_excess_supply","cold_excess_demand", 
                   "heat_excess_demand")


my_factors <- c(general_factors, control_factors, temp_factors)



################### OLS

ols_m <- feols(log(load_tot) ~ log(price + 1) + year + month + creneau + spot_gas ,
               data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

my_controls <- paste( paste(c(my_factors), collapse = " + "), sep = " + ")

ols_formula <- as.formula(
  paste0("log(load_tot) ~ log(price + 1) + year + month + creneau + spot_gas +", my_controls))

#colSums(is.na(price_indispo_merge_esp_npc[, ..my_factors]))

ols_mc <- feols(ols_formula, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)


####################### IV using standard Instruments

iv_wind_controls <- paste0(setdiff(my_factors, "Fore_Wind"), collapse = " + ")
iv_wind_formula <-  as.formula(
  paste0("log(load_tot) ~  year + month + creneau + spot_gas +", iv_wind_controls, " | log(price + 1) ~ Fore_Wind "))

iv_wind_m <- feols(iv_wind_formula, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)


iv_temp_controls <- paste0(setdiff(my_factors, temp_factors ), collapse = " + ")
iv_temp_formula <-  as.formula(
  paste0("log(load_tot) ~  year + month + creneau + spot_gas +", iv_temp_controls, " | log(price + 1) ~ cold_excess_demand + heat_excess_demand"))

iv_temp_m <- feols(iv_temp_formula, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)


####################### IV using lag of standard instruments

# Suppose your instrument is in instr_vars_s (e.g. "wind" or "temp")
inst_name_s <- "Fore_Wind"
inst_name_d <- c("cold_excess_demand", "heat_excess_demand")
# Generate 12 lags
price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
  mutate(across(
    all_of(c(inst_name_s, inst_name_d)),
    list(
      lag1 = ~dplyr::lag(.x, 1)
    ),
    .names = "{.col}_{.fn}"
  )) 
colnames(price_indispo_merge_esp_npc)
# Build the formula dynamically
# Build the formula dynamically
lag_controls_s <- paste0(inst_name_s, "_lag", 1, collapse = " + ")

iv_formula_s <- as.formula(
  paste0(
    "log(load_tot) ~  year + month + creneau + spot_gas +",
    iv_wind_controls, " + ", lag_controls_s," | log(price + 1) ~ ", inst_name_s
  )
)


lag_controls_d <- paste0(inst_name_d, "_lag", 1, collapse = " + ")

iv_formula_d <- as.formula(
  paste0(
    "log(load_tot) ~  year + month + creneau + spot_gas +",
    iv_temp_controls, " + ", lag_controls_d," | log(price + 1) ~ ", paste0(inst_name_d, collapse = " + ")
  )
)


iv_wind_lag <- feols(iv_formula_s, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

iv_temp_lag <- feols(iv_formula_d, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

#fitstat(show_types = TRUE)
etable(ols_m,ols_mc,
       iv_wind_m ,iv_temp_m,
       iv_wind_lag, iv_temp_lag,
       keep = c("price"),
       fitstat = ~ ivf.stat +  r2+ n)


# Fonction pour extraire coef + IVF
extract_coef_ivf <- function(model, var_name = "fit_log(price + 1)", IV = TRUE) {
  # Si model est une liste (supply/demand), prendre le premier Ã©lÃ©ment
  if (is.list(model) && inherits(model[[1]], "fixest")) {
    mod <- model[[1]]
  } else if (inherits(model, "fixest")) {
    mod <- model
  } else {
    stop("Model must be a fixest object or a list of fixest objects")
  }
  
  # Coefficient
  coef_val <- mod$coefficients[var_name]
  se_val <-mod$se[var_name]
  if (IV){
    # IV-F statistic
    ivf_val <- fitstat(summary(mod, stage = 1), type = "ivf")
    
    # Retourne un tibble
    return(tibble(
      Model = deparse(substitute(model)),  # nom du modÃ¨le
      Coef = coef_val,
      Se = se_val,
      IVF = ivf_val$ivf1$stat
    ))
    
  }else{
    return(tibble(
      Model = deparse(substitute(model)),  # nom du modÃ¨le
      Coef = coef_val,
      Se = se_val,
      IVF = NA_real_
    ))
  }
  
}

# Liste combinÃ©e

all_models_ols <- list(ols_m,ols_mc)
all_models_standard <- list(#,
                         iv_wind_m ,iv_temp_m,
                         iv_wind_lag, iv_temp_lag)

# Extraire infos
summary_table_standard <- map_dfr(all_models_standard, ~ extract_coef_ivf(.x, "fit_log(price + 1)"))
#add the OLS 

summary_table_standard <- rbind(map_dfr(all_models_ols, ~ extract_coef_ivf(.x, "log(price + 1)", IV = FALSE)),
                                summary_table_standard)
summary_table_standard$Model <-  c(rep("OLS",2), rep(c("Supply","Demand"),2))
summary_table_standard$Lag <-c(rep(0,4), rep(1,2))
summary_table_standard


save_path <- file.path(Sys.getenv("REPLICATION_ROOT", "."), "supply_demand_curve/estimate_elasticity/outputs")
setwd(save_path)
write.csv(summary_table_standard, "elasticity_standard.csv")


