output_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all.parquet")
colnames(price_indispo_merge_esp_npc)

price_indispo_merge_esp_npc <- 
  price_indispo_merge_esp_npc %>%
  distinct(datetime, .keep_all = TRUE)


price_indispo_merge_esp_elas <- merge(price_indispo_merge_esp_npc,
                                      local_slopes_demand_lin %>% 
                                        dplyr::select(DATE, Hour, slope) %>% 
                                        rename(slope_demand = slope), 
                                      by.x = c("delivery_date", "hour"),
                                      by.y = c("DATE", "Hour"))

price_indispo_merge_esp_elas <- merge(price_indispo_merge_esp_elas,
                                      local_slopes_supply_lin %>% 
                                        dplyr::select(DATE, Hour, slope) %>% 
                                        rename(slope_supply = slope), 
                                      by.x = c("delivery_date", "hour"),
                                      by.y = c("DATE", "Hour"))


# Prix résidualisé

# Élasticité observée
eta_t_d <- price_indispo_merge_esp_elas$slope_demand
eta_t_s <- price_indispo_merge_esp_elas$slope_supply
eta_hat_d <- sum(price_indispo_merge_esp_elas$price**2 * eta_t_d, na.rm = TRUE) /
  sum(price_indispo_merge_esp_elas$price**2, na.rm = TRUE)

eta_hat_s <- sum(price_indispo_merge_esp_elas$price**2 * eta_t_s, na.rm = TRUE) /
  sum(price_indispo_merge_esp_elas$price**2, na.rm = TRUE)
eta_hat_d
eta_hat_s
