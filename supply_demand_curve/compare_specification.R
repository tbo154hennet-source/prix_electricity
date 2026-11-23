

#On cherche à regarder si la transformation arc sin hyperbolique peut nous permettre de stabiliser les estimations d'élasticités
# pour prix proche de zéro ce qui est important c'est +1 euro
#loin c'est +1%

#The message I want to convey is that prices close to zero are a completely different regime
#In particular the standard instruments that we use are irrelevant when it comes to this price bracket



output_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all.parquet")
colnames(price_indispo_merge_esp_npc)

temp_factors <- c( "temperature_discomfort_ss"  , "temperature_discomfort_sth" ,
                   "temperature_discomfort_demand")
price_indispo_merge_esp_npc <- merge(price_indispo_merge_esp_npc,
                                     price_indispo_merge_esp %>% 
                                       dplyr::select("datetime", temp_factors),
                                     by = ("datetime"))

price_indispo_merge_esp_npc <- 
  price_indispo_merge_esp_npc %>%
  distinct(datetime, .keep_all = TRUE)

general_factors <- c(
  "Fore_Wind","Fore_Solar",
  # "intraday_update_wind", "unpredicted_wind",
  #  "unpredicted_sun","intraday_update_sun",
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
temp_factors <- c( "temperature_discomfort_ss"  , "temperature_discomfort_sth" ,
                   "temperature_discomfort_demand")

my_factors <- c(general_factors, control_factors, temp_factors)

colnames(price_indispo_merge_esp_npc)
####################### IV using standard Instruments

iv_wind_controls <- paste0(setdiff(my_factors, "Fore_Wind"), collapse = " + ")
iv_temp_controls <- paste0(setdiff(my_factors, temp_factors ), collapse = " + ")

iv_wind_formula_log <-  as.formula(
  paste0("log(load_tot+1) ~  year + month + creneau + weekday + weekday + spot_gas +", iv_wind_controls, " | log(price+1) ~ wind_speed_100m_supply "))

iv_temp_formula_log <-  as.formula(
  paste0("log(load_tot+1) ~  year + month + creneau + weekday + weekday + spot_gas +", iv_temp_controls, " | log(price+1) ~ temperature_discomfort_demand"
         ))

iv_wind_formula_ash <-  as.formula(
  paste0("log(load_tot) ~  year + month + creneau + weekday + weekday + spot_gas +", iv_wind_controls, " | asinh(price) ~ wind_speed_100m_supply "))

iv_temp_formula_ash <-  as.formula(
  paste0("log(load_tot) ~  year + month + creneau + weekday + weekday + spot_gas +", iv_temp_controls, " | asinh(price) ~ temperature_discomfort_demand"
  ))

iv_wind_formula_lin <-  as.formula(
  paste0("load_tot ~  year + month + creneau + weekday + weekday + spot_gas +", iv_wind_controls, " | price ~ wind_speed_100m_supply "))

iv_temp_formula_lin <-  as.formula(
  paste0("load_tot ~  year + month + creneau + weekday + weekday + spot_gas +", iv_temp_controls, " | price ~ temperature_discomfort_demand"
  ))

wind_m_log <- feols(iv_wind_formula_log, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
temp_m_log <- feols(iv_temp_formula_log, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)

wind_m_log_pc <- feols(iv_wind_formula_log, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
temp_m_log_pc <- feols(iv_temp_formula_log, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

wind_m_log_p10 <- feols(iv_wind_formula_log, data = price_indispo_merge_esp_npc %>% filter(price <=10), DK(24) ~ datetime)
temp_m_log_p10 <- feols(iv_temp_formula_log, data = price_indispo_merge_esp_npc %>% filter(price <=10), DK(24) ~ datetime)

# colSums(is.na(price_indispo_merge_esp_npc %>%
#           dplyr::select(factors)))


wind_m_ash <- feols(iv_wind_formula_ash, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
temp_m_ash <- feols(iv_temp_formula_ash, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)

wind_m_ash_pc <- feols(iv_wind_formula_ash, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
temp_m_ash_pc <- feols(iv_temp_formula_ash, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

wind_m_ash_p10 <- feols(iv_wind_formula_ash, data = price_indispo_merge_esp_npc %>% filter(price <=10), DK(24) ~ datetime)
temp_m_ash_p10 <- feols(iv_temp_formula_ash, data = price_indispo_merge_esp_npc %>% filter(price <=10), DK(24) ~ datetime)


wind_m_lin <- feols(iv_wind_formula_lin, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
temp_m_lin <- feols(iv_temp_formula_lin, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)

wind_m_lin_pc <- feols(iv_wind_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)
temp_m_lin_pc <- feols(iv_temp_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price >=10), DK(24) ~ datetime)

wind_m_lin_p10 <- feols(iv_wind_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price <=10), DK(24) ~ datetime)
temp_m_lin_p10 <- feols(iv_temp_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price <=10), DK(24) ~ datetime)


etable(wind_m_log ,wind_m_log_pc,wind_m_log_p10,
       temp_m_log, temp_m_log_pc,temp_m_log_p10,
        
       keep = c("price"),
       headers = c(
         "Supply – log",
         "Supply – log",
         "Supply – log",
         "Demand – log",
         "Demand – log",
         "Demand – log"
       ),
       fitstat = ~ ivf.stat +  r2+ n)



etable(wind_m_ash ,wind_m_ash_pc, wind_m_ash_p10,
       temp_m_ash, temp_m_ash_pc, temp_m_ash_p10,
       keep = c("price"),
       headers = c(
         "Supply – log",
         "Supply – log",
         "Supply – log",
         "Demand – log",
         "Demand – log",
         "Demand – log"
       ),
       fitstat = ~ ivf.stat +  r2+ n)



etable(wind_m_lin ,wind_m_lin_pc, wind_m_lin_p10,
       temp_m_lin, temp_m_lin_pc,temp_m_lin_p10,
       keep = c("price"),
       headers = c(
         "Supply – log",
         "Supply – log",
         "Supply – log",
         "Demand – log",
         "Demand – log",
         "Demand – log"
       ),
       fitstat = ~ ivf.stat +  r2+ n)





iv_supply_formula_log <-  as.formula(
  paste0("log(load_tot+1) ~  year + month + creneau + weekday + weekday + spot_gas + fit_demand_unmatched",  " | log(price+1) ~ fit_supply_unmatched "))

supply_m_log <- feols(iv_supply_formula_log, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
supply_m_log_pc <- feols(iv_supply_formula_log, data = price_indispo_merge_esp_npc %>% filter(price >=20), DK(24) ~ datetime)
supply_m_log_p10 <- feols(iv_supply_formula_log, data = price_indispo_merge_esp_npc %>% filter(price <=20), DK(24) ~ datetime)

etable(supply_m_log ,supply_m_log_pc, supply_m_log_p10,
       keep = c("price"),
       
       fitstat = ~ ivf.stat +  r2+ n)



iv_supply_formula_lin <-  as.formula(
  paste0("load_tot ~  year + month + creneau + weekday + weekday + spot_gas + fit_demand_unmatched",  " | price ~ fit_supply_unmatched "))

supply_m_lin <- feols(iv_supply_formula_lin, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
supply_m_lin_pc <- feols(iv_supply_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price >=20), DK(24) ~ datetime)
supply_m_lin_p10 <- feols(iv_supply_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price <=20), DK(24) ~ datetime)

etable(supply_m_lin ,supply_m_lin_pc, supply_m_lin_p10,
       keep = c("price"),
       
       fitstat = ~ ivf.stat +  r2+ n)


#An example how insrtuments are defenless in this price bracket


etable(wind_m_log_pc,wind_m_log_p10,
       wind_m_lin_pc,wind_m_lin_p10,
       supply_m_log_pc, supply_m_log_p10, 
       supply_m_lin_pc, supply_m_lin_p10,
       keep = c("price"),
       # headers = c(
       #   "Supply – log",
       #   "Supply – log",
       #   "Supply – log",
       #   "Demand – log",
       #   "Demand – log",
       #   "Demand – log"
       # ),
       fitstat = ~ ivf.stat +  r2+ n)









iv_demand_formula_log <-  as.formula(
  paste0("log(load_tot+1) ~  year + month + creneau + weekday + weekday + spot_gas + fit_supply_unmatched",  " | log(price+1) ~ fit_demand_unmatched "))

demand_m_log <- feols(iv_demand_formula_log, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
demand_m_log_pc <- feols(iv_demand_formula_log, data = price_indispo_merge_esp_npc %>% filter(price >=20), DK(24) ~ datetime)
demand_m_log_p10 <- feols(iv_demand_formula_log, data = price_indispo_merge_esp_npc %>% filter(price <=20), DK(24) ~ datetime)

etable(demand_m_log ,demand_m_log_pc, demand_m_log_p10,
       keep = c("price"),
       
       fitstat = ~ ivf.stat +  r2+ n)



iv_demand_formula_lin <-  as.formula(
  paste0("load_tot ~  year + month + creneau + weekday + weekday + spot_gas+ fit_supply_unmatched",  " | price ~ fit_demand_unmatched "))

demand_m_lin <- feols(iv_demand_formula_lin, data = price_indispo_merge_esp_npc, DK(24) ~ datetime)
demand_m_lin_pc <- feols(iv_demand_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price >=20), DK(24) ~ datetime)
demand_m_lin_p10 <- feols(iv_demand_formula_lin, data = price_indispo_merge_esp_npc %>% filter(price <=20), DK(24) ~ datetime)

etable(demand_m_lin ,demand_m_lin_pc, demand_m_lin_p10,
       keep = c("price"),
       
       fitstat = ~ ivf.stat +  r2+ n)


#An example how insrtuments are defenless in this price bracket


etable(temp_m_log_pc,temp_m_log_p10,
       temp_m_lin_pc,temp_m_lin_p10,
       demand_m_log_pc, demand_m_log_p10, 
       demand_m_lin_pc, demand_m_lin_p10,
       keep = c("price"),
       # headers = c(
       #   "demand – log",
       #   "demand – log",
       #   "demand – log",
       #   "Demand – log",
       #   "Demand – log",
       #   "Demand – log"
       # ),
       fitstat = ~ ivf.stat +  r2+ n)

all_models_lin <- list(#,
  temp_m_lin_pc,temp_m_lin_p10,
  demand_m_lin_pc, demand_m_lin_p10,
  wind_m_lin_pc,wind_m_lin_p10,
  supply_m_lin_pc, supply_m_lin_p10)

all_models_log <- list(#,
  temp_m_log_pc,temp_m_log_p10,
  demand_m_log_pc, demand_m_log_p10,
  wind_m_log_pc,wind_m_log_p10,
  supply_m_log_pc, supply_m_log_p10)

library(purrr)
summary_table_compare_log <- map_dfr(all_models_log, ~ extract_coef_ivf(.x, "fit_log(price + 1)"))
summary_table_compare_lin<- map_dfr(all_models_lin, ~ extract_coef_ivf(.x, "fit_price"))

#instrument resilience
#not for a single price range
vecotr_model <- c(rep("Temperature", 2), rep("Demand", 2),
                  rep("Wind", 2) , rep("Supply", 2) )
price_vector <- rep(c("p>10", "p<10"),4)
summary_table_compare_log$Model = vecotr_model
summary_table_compare_log$Price_Bracket <- price_vector 
summary_table_compare_lin$Model <- vecotr_model
summary_table_compare_lin$Price_Bracket <- price_vector 

summary_table_compare <- rbind(summary_table_compare_log %>% 
        mutate(Specification = "Log"),
      summary_table_compare_lin%>% 
        mutate(Specification = "Lin")
      )


