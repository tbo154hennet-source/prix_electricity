

reconstruct_stocked_curve <- function(type, matched){
  
  if (type == "demand"){
    curve_shift_data <- read_parquet( 'curve_shift_data_demand_qty.parquet')
  }else if(type == "supply"){
    curve_shift_data <- read_parquet( 'curve_shift_data_supply_qty.parquet')
  }else{
    return("Ivalid type")
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
                                                     , Hour := as.numeric(Hour)][, DATE := as.Date(DATE, format = "%d/%m/%Y")][ order(DATE, Hour)]
  return(reconstruct_full_curve_shift)
  
}



setwd("Z:/code_supply_demand")
# G?n?re toutes les combinaisons possibles
combinations <- expand.grid(
  matched = c( "unmatched", "matched"),
  type = c("supply", "demand")
)
i = 1
getwd()
# Boucle sur chaque ligne
for (i in 2:nrow(combinations)) {
  current_type <- combinations$type[i]
  current_matched <- combinations$matched[i]
  reconstruct_full_curve_shift <- reconstruct_stocked_curve(
    type = as.character(current_type), 
    matched = as.character(current_matched))
  write_parquet(reconstruct_full_curve_shift, 
                paste0("reconstruct_full_curve_shift_qty_",as.character(current_type),
                       "_",as.character(current_matched), ".parquet"))
  rm(reconstruct_full_curve_shift)
  gc()
  # Tu peux mettre ici le code ? ex?cuter pour chaque combinaison
}







