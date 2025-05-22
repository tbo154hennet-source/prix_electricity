

reconstruct_stocked_curve <- function(type, matched){
  
  if (type == "demand"){
    curve_shift_data <- read_parquet( 'curve_shift_data_demand.parquet')
  }else if(type == "supply"){
    curve_shift_data <- read_parquet( 'curve_shift_data_supply.parquet')
  }else{
    return("Ivalid type")
  }
  setDT(curve_shift_data)
  curve_shift_data <- curve_shift_data[DATE >= "02/03/2018" & DATE <= "31/12/2023"]
  if (matched == "matched"){
    curve_shift_data <- curve_shift_data[MATCHED == "C"]
  }else if (type == "unmatched"){
    curve_shift_data <- curve_shift_data[MATCHED == "O"]
  }else{
    return("Invalid matching")
  }
  cum_grid <- seq(0, 60000,length.out = 1000)
  # Table avec la grille pour tous les groupes
  grid_dt <- unique(curve_shift_data[, .(DATE, Hour, OFFER_TYPE, MATCHED)])[
    , .(grid_point_test = cum_grid), 
    by = .(DATE, Hour, OFFER_TYPE, MATCHED)
  ]
  # Faire un join non-Ã©qui : on cherche le dernier grid_point <= grid_point_test
  reconstruct_full_curve_shift <- curve_shift_data[grid_dt, 
                                                   on = .(DATE, Hour, OFFER_TYPE, MATCHED, grid_point <= grid_point_test),
                                                   mult = "last",
                                                   allow.cartesian = TRUE][
                                                     , Hour := as.numeric(Hour)][, DATE := as.Date(DATE, format = "%d/%m/%Y")][ order(DATE, Hour)]
  return(reconstruct_full_curve_shift)
  
}



setwd('Z:/')
# Génère toutes les combinaisons possibles
combinations <- expand.grid(
  matched = c("matched", "unmatched"),
  type = c("supply", "demand")
)
i = 1
# Boucle sur chaque ligne
for (i in 1:nrow(combinations)) {
  current_type <- combinations$type[i]
  current_matched <- combinations$matched[i]
  reconstruct_full_curve_shift <- reconstruct_stocked_curve(as.character(current_type), as.character(current_matched))
  write_parquet(reconstruct_full_curve_shift, 
                paste0("reconstruct_full_curve_shift_",as.character(current_type),
                       "_",as.character(current_matched), ".parquet"))
  rm(reconstruct_full_curve_shift)
  gc()
  # Tu peux mettre ici le code à exécuter pour chaque combinaison
}







