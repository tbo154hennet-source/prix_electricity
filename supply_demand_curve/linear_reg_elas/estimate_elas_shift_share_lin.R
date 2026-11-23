
output_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")
colnames(price_indispo_merge_esp_npc)
# price_indispo_merge_esp_npc  <- price_indispo_merge_esp_npc  %>%
#   left_join(wind_solar_openmeteo_data , by=c('datetime' = 'date'))
# colnames(price_indispo_merge_esp_npc)
# write_parquet(price_indispo_merge_esp_npc, "price_indispo_merge_esp_npc_all.parquet")
price_indispo_merge_esp_npc <- 
  price_indispo_merge_esp_npc %>%
  distinct(datetime, .keep_all = TRUE)



####################### IV using standard Instruments
inst_name_s <- "wind_speed_100m_supply"
inst_name_d <- c("cold_excess_demand", "heat_excess_demand")



iv_wind_controls <- paste0(setdiff(my_factors, "Fore_Wind"), collapse = " + ")
iv_wind_supply_formula <-  as.formula(
  paste0("load_tot ~  year + month + creneau + spot_gas +wind_speed_100m_demand +", iv_wind_controls, " | price ~ wind_speed_100m_supply "))
iv_wind_supply_m <- feols(iv_wind_supply_formula, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)


iv_temp_supply_controls_bis <- "heat_excess_supply + cold_excess_supply+"  
iv_temp_supply_controls <-setdiff(temp_factors, inst_name_d)
iv_temp_controls <- paste0(setdiff(my_factors, temp_factors ), collapse = " + ")
iv_temp_supply_formula <-  as.formula(
  paste0("load_tot ~  year + month + creneau + spot_gas +",iv_temp_controls, "+", paste0(iv_temp_supply_controls , collapse = " + "),
         " | price ~ cold_excess_demand + heat_excess_demand"))

iv_temp_supply_m <- feols(iv_temp_supply_formula, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)





# Generate 12 lags
price_indispo_merge_esp_npc <- price_indispo_merge_esp_npc %>%
  mutate(across(
    all_of(c(inst_name_s, inst_name_d, "wind_speed_100m_demand", iv_temp_supply_controls)),
    list(
      lag1 = ~dplyr::lag(.x, 1)
    ),
    .names = "{.col}_{.fn}"
  )) 
# Build the formula dynamically
# Build the formula dynamically
lag_controls_s <- paste0(c(inst_name_s, "wind_speed_100m_demand"), "_lag", 1, collapse = " + ")


iv_formula_s <- as.formula(
  paste0(
    "load_tot ~  year + month + creneau + spot_gas +",
    iv_wind_controls, " +wind_speed_100m_demand  + ", lag_controls_s," | price ~ ", inst_name_s
  )
)


lag_controls_d <- paste0(c(inst_name_d, iv_temp_supply_controls), "_lag", 1, collapse = " + ")

iv_formula_d <- as.formula(
  paste0(
    "load_tot ~  year + month + creneau + spot_gas +",
    iv_temp_controls, "+", paste0(iv_temp_supply_controls , collapse = " + "), " + ", lag_controls_d,
    " | price ~ ", paste0(inst_name_d, collapse = " + ")
  )
)


iv_wind_lag_supply <- feols(iv_formula_s, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)

iv_temp_lag_supply <- feols(iv_formula_d, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)


etable(
       iv_wind_supply_m ,iv_temp_supply_m ,
       iv_wind_lag_supply, iv_temp_lag_supply,
       keep = c("price"),
       fitstat = ~ ivf.stat +  r2+ n)


all_models_supply_lin <- list(#,
  iv_wind_supply_m ,iv_temp_supply_m ,
  iv_wind_lag_supply, iv_temp_lag_supply)

# Extraire infos
summary_table_supply_lin <- map_dfr(all_models_supply_lin, ~ extract_coef_ivf(.x, "fit_price"))
#add the OLS 

summary_table_supply_lin$Model <-   rep(c("Supply","Demand"),2)
summary_table_supply_lin$Lag <-c(rep(0,2), rep(1,2))
summary_table_supply_lin

save_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/estimate_elasticity/outputs"
setwd(save_path)
write.csv(summary_table_supply_lin, "elasticity_shift_share_lin.csv")
