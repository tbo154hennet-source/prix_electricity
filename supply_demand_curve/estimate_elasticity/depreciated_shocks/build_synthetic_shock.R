library(purrr)
library(furrr)


instruments_tot <- c("Fore_Wind",
                     "Fore_Solar",
                 #"Published.Predicted.Unavailable.Capacity",
                 "cbf_es",
                 "cold_excess",
                 "heat_excess")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
#coef_supply_unmatched_indispo <-  readRDS("reg_shift_result_qty_supply_unmatched_sparse_exo.rds")

coef_supply_unmatched_indispo <-  readRDS("reg_shift_result_qty_supply_unmatched_sparse_exo_no_unavail.rds")
#coef_supply_unmatched_indispo <-  readRDS("reg_shift_result_qty_supply_unmatched_exo_ortho.rds")
coef_supply_unmatched_indispo_data <- rbindlist(coef_supply_unmatched_indispo)
class(coef_supply_unmatched_indispo)
unique(coef_supply_unmatched_indispo_data$variable)


#reg_shift_result_qty_demand_unmatched_sparse_fore_wind_sun_variation

# coef_demand_unmatched_indispo <-  readRDS("reg_shift_result_qty_demand_unmatched_sparse_exo.rds")
coef_demand_unmatched_indispo <-  readRDS("reg_shift_result_qty_demand_unmatched_sparse_exo_no_unavail.rds")

coef_demand_unmatched_indispo_data <- rbindlist(coef_demand_unmatched_indispo)
class(coef_demand_unmatched_indispo)
unique(coef_demand_unmatched_indispo_data$variable)




price_indispo_merge_esp <- as.data.frame(price_indispo_merge_esp)
row <- price_indispo_merge_esp[15000,]
class(price_indispo_merge_esp)
class(row)

excess_indexes <- which(grepl("excess", instruments_tot))


linear_interpolate <- function(df, variable_name, target_price) {
  # Subset data for the variable
  sub_df <- df[df$variable == variable_name, ]
  
  # Sort by price
  sub_df <- sub_df[order(sub_df$price), ]
  
  # Check if target price is outside range
  if (target_price <= min(sub_df$price)) {
    return(sub_df$estimate[which.min(sub_df$price)])
  }
  if (target_price >= max(sub_df$price)) {
    return(sub_df$estimate[which.max(sub_df$price)])
  }
  
  # Find indices of closest prices below and above target
  below_idx <- max(which(sub_df$price <= target_price))
  above_idx <- min(which(sub_df$price >= target_price))
  
  # Extract points for interpolation
  x0 <- sub_df$price[below_idx]
  x1 <- sub_df$price[above_idx]
  y0 <- sub_df$estimate[below_idx]
  y1 <- sub_df$estimate[above_idx]
  
  # Linear interpolation formula
  y <- y0 + (target_price - x0) * (y1 - y0) / (x1 - x0)
  
  return(y)
}


generate_quantity_variation_complete <- function(row) {
  
  target_price <- as.numeric(row["price"])  # extraire le prix de la ligne
  # instruments doit être défini globalement (vecteur de longueur 5)
  # ex: instruments <- c("inst1", "inst2", "inst3", "inst4", "inst5")
  
  results <- data.frame(variable = instruments_tot,
                        supply_variation = NA_real_,
                        demand_variation = NA_real_,
                        delta_q = NA_real_)
  #i =2
  for (i in seq_along(instruments_tot)) {
    if (i %in% excess_indexes){
      # var_supply <- paste0(instruments_tot[i], "_supply")
      # var_demand <- paste0(instruments_tot[i], "_demand")
      var_supply <- paste0(instruments_tot[i], "_supply")
      var_demand <- paste0(instruments_tot[i], "_supply")
    } else {
      var_supply <- instruments_tot[i]
      var_demand <- instruments_tot[i]
    }
    # var_supply <- instruments_tot[i]
    # var_demand <- instruments_tot[i]
    if (var_supply %in% coef_supply_unmatched_indispo_data$variable) {
      supply_var <- linear_interpolate(df= coef_supply_unmatched_indispo_data, 
                                       variable_name=var_supply, target_price)*as.numeric(row[var_supply])
    } else {
      supply_var <- NA_real_
    }
    if (var_demand %in% coef_demand_unmatched_indispo_data$variable) {
      demand_var <- linear_interpolate(coef_demand_unmatched_indispo_data, var_demand, target_price)*as.numeric(row[var_demand])
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
# progressr::with_progress({
#   p <- progressr::progressor(steps = nrow(price_indispo_merge_esp))
#   
#   delta_q_list <- map2(seq_len(nrow(price_indispo_merge_esp)), price_indispo_merge_esp$price, function(i, price) {
#     p()
#     tryCatch({
#       row <- price_indispo_merge_esp[i, ]
#       generate_quantity_variation_complete(row)
#     }, error = function(e) {
#       message(sprintf("Erreur à l'index %d (prix = %.2f) : %s", i, price, e$message))
#       rep(NA_real_, length(instruments_tot))
#     })
#   })
# })


# 2?????? Parallel map2 without progress
delta_q_list <- future_map2(
  .x = seq_len(nrow(price_indispo_merge_esp)),
  .y = price_indispo_merge_esp$price,
  .options = furrr_options(seed = TRUE),  # optional for reproducibility
  .f = function(i, price) {
    tryCatch({
      row <- price_indispo_merge_esp[i, ]
      generate_quantity_variation_complete(row)
    }, error = function(e) {
      message(sprintf("Error at index %d (price = %.2f): %s", i, price, e$message))
      rep(NA_real_, length(instruments_tot))
    })
  }
)


getwd()
saveRDS(delta_q_list, "delta_q_list_supply_demand_exo_no_unavail.rds")
delta_q_list <- readRDS( "delta_q_list_supply_demand_exo_no_unavail.rds")
#delta_q_list <- readRDS( "delta_q_list_supply_demand_exo_ortho.rds")
delta_q_matrix <- do.call(rbind, delta_q_list)
colnames(delta_q_matrix) <- c(paste0("delta_s_", instruments_tot), paste0("delta_d_", instruments_tot))



# Ajouter au dataframe
price_indispo_merge_esp_delta <- bind_cols(price_indispo_merge_esp, as.data.frame(delta_q_matrix))

cols_to_sum_supply <- paste0("delta_s_", instruments_tot)
cols_to_sum_demand <- paste0("delta_d_", instruments_tot)

price_indispo_merge_esp_delta$delta_s_total <- rowSums(price_indispo_merge_esp_delta[, cols_to_sum_supply], na.rm = TRUE)
price_indispo_merge_esp_delta$delta_d_total <- rowSums(price_indispo_merge_esp_delta[, cols_to_sum_demand ], na.rm = TRUE)



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
write_parquet(price_indispo_merge_esp_delta, "price_indispo_merge_esp_delta_supply_demand_exo_no_unavail.parquet")
# # 


price_indispo_merge_esp_delta %>% 
  dplyr::select(delta_s_total, delta_d_total ) %>% 
  cor(use = "complete.obs")












