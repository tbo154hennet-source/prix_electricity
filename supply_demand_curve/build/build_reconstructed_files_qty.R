
library(future.apply)
library(progress)
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)

reconstruct_stocked_curve <- function(type, matched){
  
  if (type == "demand"){
    curve_shift_data <- read_parquet( 'curve_shift_data_demand_qty_2026.parquet')
  }else if(type == "supply"){
    curve_shift_data <- read_parquet( 'curve_shift_data_supply_qty_2026.parquet')
  }else{
    return("Invalid type")
  }
  setDT(curve_shift_data)
  curve_shift_data <- curve_shift_data[DATE >= "02/03/2018" & DATE <= "31/12/2025"]
  if (matched == "matched"){
    curve_shift_data <- curve_shift_data[MATCHED == "C"]
  }else if (matched == "unmatched"){
    curve_shift_data <- curve_shift_data[MATCHED == "O"]
  }else{
    return("Invalid matching")
  }
  price_grid <- seq(-200, 1000, length.out = 1001)
  # Table avec la grille pour tous les groupes
  grid_dt <- unique(curve_shift_data[, .(DATE, Hour, OFFER_TYPE, MATCHED)])[
    , .(PRICE_test = price_grid), 
    by = .(DATE, Hour, OFFER_TYPE, MATCHED)
  ]
  # Faire un join non-équi : on cherche le dernier grid_point <= grid_point_test
  reconstruct_full_curve_shift <- curve_shift_data[grid_dt, 
                                                   on = .(DATE, Hour, OFFER_TYPE, MATCHED, PRICE <= PRICE_test),
                                                   mult = "last",
                                                   allow.cartesian = TRUE][
                                                     , Hour := Hour][, DATE := as.Date(DATE, format = "%d/%m/%Y")][ order(DATE, Hour)]
  return(reconstruct_full_curve_shift)
  
}



setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# G?n?re toutes les combinaisons possibles
combinations <- expand.grid(
  matched = c( "unmatched", "matched"),
  type = c("supply", "demand")
)
i = 2
getwd()
# Boucle sur chaque ligne
for (i in 1:nrow(combinations)) {
  
  current_type <- combinations$type[i]
  current_matched <- combinations$matched[i]
  print(current_type)
  print(current_matched)
  reconstruct_full_curve_shift <- reconstruct_stocked_curve(
    type = as.character(current_type), 
    matched = as.character(current_matched))
  
  reconstruct_full_curve_shift[, is_quarter := grepl("Q[1-4]$", Hour)]
  reconstruct_full_curve_shift[, hour := sub("Q[1-4]$", "", Hour)]
  reconstruct_full_curve_shift[, hour_num := as.integer(sub("^H", "", hour))]
  
  threshold_date <- reconstruct_full_curve_shift[is_quarter == FALSE, max(DATE)]
  
  dt_quarter <- reconstruct_full_curve_shift[
    is_quarter == TRUE
  ]
  
  dt_hourly <- reconstruct_full_curve_shift[
    is_quarter == FALSE
  ]
  
  dt_quarter_hourly <- dt_quarter[
    ,
    .(
      QTY = mean(QTY, na.rm = TRUE)
    ),
    by = .(DATE, hour, hour_num, OFFER_TYPE, MATCHED, PRICE)
  ]
  
  dt_final <- rbindlist(
    list(dt_hourly, dt_quarter_hourly),
    use.names = TRUE,
    fill = TRUE
  )
  tail(dt_final)
  write_parquet(dt_final, 
                paste0("reconstruct_full_curve_shift_qty_",as.character(current_type),
                       "_",as.character(current_matched), ".parquet"))
  # write_parquet(reconstruct_full_curve_shift, 
  #               paste0("reconstruct_full_curve_shift_qty_",as.character(current_type),
  #                      "_",as.character(current_matched), ".parquet"))
  rm(reconstruct_full_curve_shift)
  gc()
  # Tu peux mettre ici le code ? ex?cuter pour chaque combinaison
}


#pour la fin de période convertir les données à l'échelle du 1/4 d'heures en horaires



#do some checks


library(ggplot2)

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_supply <- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')
reconstruct_full_curve_shift_demand <- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')

rm(reconstruct_full_curve_shift)
gc()

# Group by Price level (you may want to round or bin prices for better aggregation)
summary_supply_curve <- reconstruct_full_curve_shift_supply[, .(
  median_qty = median(QTY, na.rm = TRUE),
  mean_qty = mean(QTY, na.rm = TRUE),
  lower_bound = quantile(QTY, probs = 0.025, na.rm = TRUE),
  upper_bound = quantile(QTY, probs = 0.975, na.rm = TRUE)
), by = PRICE]


# Group by Price level (you may want to round or bin prices for better aggregation)
summary_demand_curve <- reconstruct_full_curve_shift_demand[, .(
  median_qty = median(QTY, na.rm = TRUE),
  mean_qty = mean(QTY, na.rm = TRUE),
  lower_bound = quantile(QTY, probs = 0.025, na.rm = TRUE),
  upper_bound = quantile(QTY, probs = 0.975, na.rm = TRUE)
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
