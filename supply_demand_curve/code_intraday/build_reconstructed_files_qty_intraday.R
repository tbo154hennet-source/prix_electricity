
library(future.apply)
library(progress)
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)


#cum_grid <- seq(0, 60000,length.out = 1000)
price_grid <- seq(-200, 1000, length.out = 1001)
# Filtrer les valeurs existantes dans l'intervalle [-50, 500]
# subset_grid <- price_grid[price_grid >= -50 & price_grid <= 500]
# 
# # Prendre 101 points régulièrement espacés
# # Créer 101 points réguliers dans la plage
# target_points <- seq(from = min(subset_grid), to = max(subset_grid), length.out = 101)
# 
# # Pour chaque target, trouver le point de price_grid le plus proche
# price_grid_subset <- sapply(target_points, function(tp) {
#   subset_grid[which.min(abs(subset_grid - tp))]
# })

reconstruct_stocked_curve <- function(type, matched){
  
  if (type == "demand"){
    setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
    curve_shift_data <- read_parquet( 'curve_shift_data_demand_qty_bis.parquet')
    curve_shift_data$AUCTION <- 0
    setwd("Z:/code_supply_demand/code_intraday")
    curve_shift_data_intraday <- read_parquet("curve_shift_data_demand_qty_intraday.parquet")
    
    curve_shift_data <- rbind(curve_shift_data, curve_shift_data_intraday)
    
  }else if(type == "supply"){
    setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
    curve_shift_data <- read_parquet( 'curve_shift_data_supply_qty_bis.parquet')
    curve_shift_data$AUCTION <- 0
    setwd("Z:/code_supply_demand/code_intraday")
    curve_shift_data_intraday <- read_parquet("curve_shift_data_supply_qty_intraday.parquet")
    
    curve_shift_data <- rbind(curve_shift_data, curve_shift_data_intraday)
  }else{
    return("Invalid type")
  }
  setDT(curve_shift_data)
  curve_shift_data <- curve_shift_data[DATE >= "02/03/2018" & DATE <= "31/12/2023"]
  if (matched == "matched"){
    curve_shift_data <- curve_shift_data[MATCHED == "C"]
  }else if (matched == "unmatched"){
    curve_shift_data <- curve_shift_data[MATCHED == "O"]
  }else{
    return("Invalid matching")
  }
  
  
  # Table avec la grille pour tous les groupes
  grid_dt <- unique(curve_shift_data[, .(DATE, Hour,AUCTION, OFFER_TYPE, MATCHED)])[
    , .(PRICE_test = price_grid), 
    by = .(DATE, Hour,AUCTION, OFFER_TYPE, MATCHED)
  ]
  # Faire un join non-Ã©qui : on cherche le dernier grid_point <= grid_point_test
  reconstruct_full_curve_shift <- curve_shift_data[grid_dt, 
                                                   on = .(DATE, Hour,AUCTION, OFFER_TYPE, MATCHED, PRICE <= PRICE_test),
                                                   mult = "last",
                                                   allow.cartesian = TRUE][
                                                     , Hour := as.numeric(Hour)][, DATE := as.Date(DATE, format = "%d/%m/%Y")][ order(DATE, Hour)]
  return(reconstruct_full_curve_shift)
  
}



setwd("Z:/code_supply_demand/code_intraday")
# G?n?re toutes les combinaisons possibles
combinations <- expand.grid(
  matched = c( "unmatched", "matched"),
  type = c("supply", "demand")
)
i = 1
getwd()
# Boucle sur chaque ligne
for (i in 1:nrow(combinations)) {
  current_type <- combinations$type[i]
  current_matched <- combinations$matched[i]
  print(current_matched)
  print(current_type)
  reconstruct_full_curve_shift <- reconstruct_stocked_curve(
    type = as.character(current_type), 
    matched = as.character(current_matched)
    )
  write_parquet(reconstruct_full_curve_shift, 
                paste0("reconstruct_full_curve_shift_qty_intraday_",as.character(current_type),
                       "_",as.character(current_matched), ".parquet"))
  rm(reconstruct_full_curve_shift)
  gc()
  # Tu peux mettre ici le code ? ex?cuter pour chaque combinaison
}





#do some checks


library(ggplot2)
list.files()
setwd("Z:/code_supply_demand/code_intraday")
reconstruct_full_curve_shift_supply <- read_parquet( 'reconstruct_full_curve_shift_qty_intraday_supply_unmatched.parquet')
reconstruct_full_curve_shift_demand <- read_parquet( 'reconstruct_full_curve_shift_qty_intraday_demand_unmatched.parquet')

rm(reconstruct_full_curve_shift)
gc()

# Group by Price level (you may want to round or bin prices for better aggregation)
summary_supply_curve <- reconstruct_full_curve_shift_supply[, .(
  median_qty = median(QTY, na.rm = TRUE),
  mean_qty = mean(QTY, na.rm = TRUE),
  lower_bound = quantile(QTY, probs = 0.005, na.rm = TRUE),
  upper_bound = quantile(QTY, probs = 0.995, na.rm = TRUE)
), by = PRICE]


# Group by Price level (you may want to round or bin prices for better aggregation)
summary_demand_curve <- reconstruct_full_curve_shift_demand[, .(
  median_qty = median(QTY, na.rm = TRUE),
  mean_qty = mean(QTY, na.rm = TRUE),
  lower_bound = quantile(QTY, probs = 0.005, na.rm = TRUE),
  upper_bound = quantile(QTY, probs = 0.995, na.rm = TRUE)
), by = PRICE]


ggplot() +
  geom_line(data = summary_supply_curve, aes(x = PRICE, y = median_qty, color = "supply")) +
  geom_ribbon(data = summary_supply_curve, aes(x = PRICE, ymin = lower_bound, ymax = upper_bound,
                                               fill = "supply"),alpha = 0.2) +
  geom_line(data = summary_demand_curve, aes(x = PRICE, y = median_qty, color = "demand")) +
  geom_ribbon(data = summary_demand_curve, aes(x = PRICE, ymin = lower_bound, ymax = upper_bound
                                               , fill = "demand"),alpha = 0.2) +
  labs(
    x = "Price",
    y = "Quantity",
    title = "Reconstructed Supply Demand Curve with 1% Confidence Bands"
  ) +
  coord_flip()+
  theme_minimal()
