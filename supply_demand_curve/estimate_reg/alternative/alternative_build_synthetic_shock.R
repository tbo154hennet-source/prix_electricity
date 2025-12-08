instruments <- c("renewable_day_ahead_forecast",
                 "Published.Unavailable.Nuclear.Capacity",
                 "cbf_es",
                 "cold_excess",
                 "heat_excess",
                 "Published.Unavailable.Capacity")
instruments_tot <- c("Fore_Wind",
                     "Fore_Solar",
                     "Published.Predicted.Unavailable.Capacity",
                     "cbf_es",
                     "cold_excess",
                     "heat_excess")
instruments_supply <- c("renewable_day_ahead_forecast",
                        "Published.Unavailable.Nuclear.Capacity",
                        "cbf_es",
                        "cold_excess_supply",
                        "heat_excess_supply",
                        "Published.Unavailable.Capacity")
names_instruments <- c("renewable_day_ahead_forecast",
                       "Published.Unavailable.Nuclear.Capacity",
                       "cbf_es",
                       "cold_excess_supply",
                       "cold_excess_demand",
                       "heat_excess_supply",
                       "heat_excess_demand",
                       "Published.Unavailable.Capacity")


setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
coef_supply_unmatched_control <- readRDS("reg_shift_result_qty_supply_unmatched_control.rds")
coef_supply_unmatched_control <- rbindlist(coef_supply_unmatched_control)
unique(coef_supply_unmatched_control$variable)



coef_demand_unmatched_control <- readRDS("reg_shift_result_qty_demand_unmatched_control.rds")
coef_demand_unmatched_control <- rbindlist(coef_demand_unmatched_control)

# names(coef_demand_unmatched_control)[names(coef_demand_unmatched_control) == "nw_se"] <- "std_error"
# price_vec <- rep(price_grid_sparse, each = 14)
# coef_demand_unmatched_control$price_test <- price_vec




#if it's a data.frame
#check that it is orthogonal

row <- price_indispo_merge_esp[15000,]


generate_quantity_variation_complete <- function(row) {
  
  target_price <- as.numeric(row["price"])  # extraire le prix de la ligne
  # instruments doit être défini globalement (vecteur de longueur 5)
  # ex: instruments <- c("inst1", "inst2", "inst3", "inst4", "inst5")
  
  results <- data.frame(variable = instruments,
                        supply_variation = NA_real_,
                        demand_variation = NA_real_,
                        delta_q = NA_real_)
  #i =3
  
  for (i in seq_along(instruments)) {
    
    # var <- instruments_supply[i]
    # supply_var <- linear_interpolate(df= coef_supply_unmatched_control, 
    #                                  variable_name=var, target_price)*as.numeric(row[var])
    # demand_var <- linear_interpolate(coef_demand_unmatched_control, var, target_price)*as.numeric(row[var])
    # results$supply_variation[i] <- supply_var
    # results$demand_variation[i] <- demand_var
    # results$delta_q[i] <- supply_var + demand_var
    if (i == 4 | i == 5){
      var_supply <- paste0(instruments[i], "_supply")
      var_demand <- paste0(instruments[i], "_demand")
    } else {
      var_supply <- instruments[i]
      var_demand <- instruments[i]
    }
    
    if (var_supply %in% coef_supply_unmatched_control$variable) {
      supply_var <- linear_interpolate(df= coef_supply_unmatched_control, 
                                       variable_name=var_supply, target_price)*as.numeric(row[var_supply])
    } else {
      supply_var <- NA_real_
    }
    if (var_demand %in% coef_demand_unmatched_control$variable) {
      demand_var <- linear_interpolate(coef_demand_unmatched_control, var_demand, target_price)*as.numeric(row[var_demand])
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
  p <- progressr::progressor(steps = nrow(price_indispo_merge_esp))
  
  delta_q_list <- map2(seq_len(nrow(price_indispo_merge_esp)), price_indispo_merge_esp$price, function(i, price) {
    p()
    tryCatch({
      row <- price_indispo_merge_esp[i, ]
      generate_quantity_variation_complete(row)
    }, error = function(e) {
      message(sprintf("Erreur à l'index %d (prix = %.2f) : %s", i, price, e$message))
      rep(NA_real_, length(instruments))
    })
  })
})

getwd()
#saveRDS(delta_q_list, "delta_q_list_supply_demand_control.rds")
delta_q_list <- readRDS( "delta_q_list_supply_demand_exo_ortho.rds")
delta_q_matrix <- do.call(rbind, delta_q_list)
colnames(delta_q_matrix) <- c(paste0("delta_s_ortho_", instruments_tot ), paste0("delta_d_ortho_", instruments_tot ))
price_indispo_merge_esp_delta <- bind_cols(price_indispo_merge_esp, as.data.frame(delta_q_matrix))


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_esp_delta_sd  <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo.parquet")
colnames(price_indispo_merge_esp_delta_sd)
price_indispo_merge_esp_delta_tot <- merge(price_indispo_merge_esp_delta,
                                       price_indispo_merge_esp_delta_sd,
                                       by =intersect(names(price_indispo_merge_esp_delta),names(price_indispo_merge_esp_delta_sd))
                                       )
price_indispo_merge_esp_delta_tot <- price_indispo_merge_esp_delta_tot %>% 
  distinct(datetime, delivery_date, .keep_all = TRUE)

# Construire les noms de colonnes
cols_to_sum_supply <- paste0("delta_s_ortho_", instruments_tot )
cols_to_sum_demand <- paste0("delta_d_ortho_", instruments_tot )

#colnames(price_indispo_merge_esp_delta_tot)

# Calculer la somme ligne par ligne des colonnes sélectionnées

price_indispo_merge_esp_delta_tot$delta_s_ortho_total <- rowSums(price_indispo_merge_esp_delta_tot[, cols_to_sum_supply], na.rm = TRUE)
price_indispo_merge_esp_delta_tot$delta_d_ortho_total <- rowSums(price_indispo_merge_esp_delta_tot[, cols_to_sum_demand ], na.rm = TRUE)



write_parquet(price_indispo_merge_esp_delta_tot, "price_indispo_merge_esp_delta_supply_demand_exo_ortho.parquet")


price_indispo_merge_esp_delta_tot %>% 
  dplyr::select(delta_s_total, delta_d_total,
                delta_s_ortho_total , delta_d_ortho_total ) %>% 
  cor(use = "complete.obs")






