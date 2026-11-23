

execute_code_shift <- function(curve){
  #2023-09-28 23:00:00
  csv_file2 <- read.csv2(curve, header = FALSE,
                        skip = 1)
  
  setDT(csv_file2)
  # 2. Utiliser la première ligne comme noms de colonnes
  setnames(csv_file2, as.character(csv_file2[1, ]))
  
  # 3. Filtrer les lignes pour 'Pais' == "ES" ou "MI"
  csv_file2 <- csv_file2[Pais == "ES" | Pais == "MI"]
  
  # 4. Supprimer la première ligne (devenue l'ancienne ligne d'en-tête) et ne garder que les colonnes 1:2 et 5:8
  csv_file2 <- csv_file2[-1, .SD, .SDcols = c(1:2, 5:8)]
  
  # 5. Renommer les colonnes
  setnames(csv_file2, c("Hour", "DATE", "OFFER_TYPE", "ENERGY", "PRICE", "MATCHED"))
  
  # 2. Nettoyage des colonnes et conversions
  csv_file2[, ENERGY := gsub("//.", "", ENERGY)]
  csv_file2[, ENERGY := as.numeric(gsub(",", ".", ENERGY))]
  csv_file2[, PRICE := as.numeric(gsub(",", ".", PRICE))]
  
  # 3. Calcul par groupe
  csv_file2 <- csv_file2[
    order(DATE, Hour, OFFER_TYPE, MATCHED, PRICE),  # tri nécessaire avant cumsum
    CUM_ENERGY := cumsum(ENERGY),
    by = .(DATE, Hour, OFFER_TYPE, MATCHED)
  ]
  # 3. Calcul pour chaque groupe
  result2 <- csv_file2[
    order(CUM_ENERGY), 
    {
      res <- lapply(cum_grid, function(g) {
        i <- which(CUM_ENERGY >= g)[1]
        if (!is.na(i)) PRICE[i] else NA_real_
      })
      data.table(grid_point = cum_grid, PRICE = unlist(res))
    },
    by = .(DATE, Hour, OFFER_TYPE, MATCHED)
  ]
  
  #cela fait beaucoup de points
  #je cherche à alléger ma structure de données
  #beaucoup de NA dans les matched, je les enlève
  result2 <- na.omit(result2)
  
  #fonction en escalier
  #superflus de stocker des points sur les marches horizontales
  # Supprimer les lignes où PRICE est identique au précédent (par groupe)
  result2 <- result2[
    ,
    .SD[c(TRUE, diff(PRICE) != 0)],
    by = .(DATE, Hour, OFFER_TYPE, MATCHED)
  ]
  
  #dans mon exemple permet de réduire d'un facteur 10
  # 96 000 -> drop na 51 000 -> 10 000
  
  
  # ✅ Ajouter correctement à la liste globale
  result_list[[length(result_list) + 1]] <<- result2
}

result_list <- list()

setwd(curve_path)


# Liste des fichiers dans le répertoire courant
my_files <- list.files()

# Filtrer les fichiers qui finissent par .1
files_ending_with_1 <- my_files[grep("\\.1$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers

file = my_files[1]

for (file in files_ending_with_1){
  index <- which(my_files == file)
  if (index %% 30 == 0){
    print(file)
  }
  setwd(curve_path)
  tryCatch(
    {
      execute_code_shift(file)
    },
    error = function(e) {
      cat("An error occurred:", conditionMessage(e), "/n")
      print(file) # Return NA or any other value you want to indicate an error
    }
  )
  
  
}

curve_shift_data <- rbindlist(result_list, use.names = TRUE, fill = TRUE)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/supply_demand_curve/outputs")
write_parquet(curve_shift_data, 'curve_shift_data_all.parquet')
