

library(progress)
library(progressr)
library(data.table)
library(dplyr)
library(tidyr)  
library(arrow)
library(lubridate)
library(ggplot2)
library(purrr)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
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

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge <- read_parquet("price_indispo_merge_esp_NTC.parquet")
price_indispo_merge <- price_indispo_merge %>%
  fill(spot_gas, .direction = "up") %>% 
  rename(renewable_day_ahead_forecast = day_ahead_forecast)


colnames_with_capacity <- colnames(price_indispo_merge)[grepl("Capacity", colnames(price_indispo_merge))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables", "cbf_es"
    
  )
price_indispo_merge[colnames_with_capacity] <- lapply(price_indispo_merge[colnames_with_capacity], function(x) ifelse(is.na(x), 0, x))
#
price_indispo_merge <- price_indispo_merge %>% 
  mutate(across(all_of(capacity_fields), 
                ~ . / 1000)) %>% 
  mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
         Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
         Uneffective.Unavailable.Capacity = Uneffective.Unavailable.Capacity - Uneffective.Unavailable.Nuclear.Capacity,
         temperature_mismatch = temperature_demand - temperature_supply 
  ) 
colnames(price_indispo_merge)

col_names <- c("weekday" , "creneau", names_instruments, "price", "datetime", "delivery_date", "spot_gas",
               colnames_with_capacity, "load_tot","load_tot_conso" , "load_tot_net" ,
               "temperature_supply","precipitation_supply", "relative_humidity_supply" ,
               "intraday_forecast"  ,"unpredicted_renewables", "gen_tot"    ,                               
               "wind_solar_tot","price" ,"year"   ,"month" , "price_diff" ,   "log_price_diff"  )
price_indispo_merge$weekday <- weekdays(price_indispo_merge$delivery_date)
price_indispo_merge$creneau <- hour(price_indispo_merge$datetime)

price_indispo_merge <- price_indispo_merge %>%
  dplyr::select(all_of(col_names)) #%>%       # sélectionne les colonnes dans col_names
  #filter(complete.cases(.))           # garde seulement les lignes sans NA dans ces colonnes


price_indispo_merge <- price_indispo_merge[!rowSums(sapply(price_indispo_merge, is.infinite)), ]


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
coef_supply_unmatched_predicted_indispo <- readRDS("reg_shift_result_qty_supply_unmatched_sparse_predicted_indispo_variation.rds")
coef_supply_unmatched_predicted_indispo_data <- rbindlist(coef_supply_unmatched_predicted_indispo)
#coef_supply_unmatched_data_ins <- coef_supply_unmatched_data[coef_supply_unmatched_data$variable %in% names_instruments,]
colnames(coef_supply_unmatched_predicted_indispo_data)

coef_demand_unmatched_predicted_indispo <- readRDS("reg_shift_result_qty_demand_unmatched_sparse_predicted_indispo_variation.rds")
coef_demand_unmatched_predicted_indispo_data <- rbindlist(coef_demand_unmatched_predicted_indispo)
#coef_demand_unmatched_data_ins <- coef_demand_unmatched_data[coef_demand_unmatched_data$variable %in% names_instruments,]


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



row <- price_indispo_merge[1000,]


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
    if (i == 3 | i == 4){
      var_supply <- paste0(instruments[i], "_supply")
      var_demand <- paste0(instruments[i], "_demand")
    } else {
      var_supply <- instruments[i]
      var_demand <- instruments[i]
    }
    
    if (var_supply %in% coef_supply_unmatched_predicted_indispo_data$variable) {
      supply_var <- linear_interpolate(df= coef_supply_unmatched_predicted_indispo_data, 
                                       variable_name=var_supply, target_price)*as.numeric(row[var_supply])
    } else {
      supply_var <- NA_real_
    }
    if (var_demand %in% coef_demand_unmatched_predicted_indispo_data$variable) {
      demand_var <- linear_interpolate(coef_demand_unmatched_predicted_indispo_data, var_demand, target_price)*as.numeric(row[var_demand])
    } else {
      demand_var <- NA_real_
    }
    
    results$supply_variation[i] <- supply_var
    results$demand_variation[i] <- demand_var
    results$delta_q[i] <- supply_var + demand_var
  }
  
  return(c(results$supply_variation, results$demand_variation))  # vecteur de longueur 5
}


#price_indispo_merge



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
saveRDS(delta_q_list, "delta_q_list_supply_demand.rds")
delta_q_matrix <- do.call(rbind, delta_q_list)
colnames(delta_q_matrix) <- c(paste0("delta_s_", instruments), paste0("delta_d_", instruments))

# Ajouter au dataframe
price_indispo_merge_delta <- bind_cols(price_indispo_merge, as.data.frame(delta_q_matrix))
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
write_parquet(price_indispo_merge_delta, "price_indispo_merge_delta_supply_demand.parquet")


# Construire les noms de colonnes
cols_to_supply <- paste0("delta_s_", instruments)
cols_to_demand<- paste0("delta_d_", instruments)
# Calculer la somme ligne par ligne des colonnes sélectionnées
price_indispo_merge_delta$delta_s_total <- rowSums(price_indispo_merge_delta[, cols_to_supply], na.rm = TRUE)
price_indispo_merge_delta$delta_d_total <- rowSums(price_indispo_merge_delta[, cols_to_demand], na.rm = TRUE)

price_indispo_merge_delta %>% 
  dplyr::select(delta_s_total, delta_d_total) %>% 
  cor(use = "complete.obs")
