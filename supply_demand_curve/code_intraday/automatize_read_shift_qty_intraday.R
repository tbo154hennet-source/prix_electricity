
library(future.apply)
library(progress)
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)


execute_code_shift_qty <- function(curve, type){
  #2023-09-28 23:00:00
  getwd()
  csv_file2 <- read.csv2(curve, header = FALSE,
                         skip = 1)
  
  #rajouter numéro de l'enchère
  
  # Extract the part before `.1`
  auction_num <- sub(".*(\\d)\\.1$", "\\1", curve)
  
  #auction_num
  
  
  setDT(csv_file2)
  # 2. Utiliser la première ligne comme noms de colonnes
  setnames(csv_file2, as.character(csv_file2[1, ]))
  
  csv_file2[[ncol(csv_file2)]] <- auction_num 
  
  # 3. Filtrer les lignes pour 'Pais' == "ES" ou "MI"
  csv_file2 <- csv_file2[Pais == "ES" | Pais == "MI"]
  
  # 4. Supprimer la première ligne (devenue l'ancienne ligne d'en-tête) et ne garder que les colonnes 1:2 et 5:8
  csv_file2 <- csv_file2[-1, .SD, .SDcols = c(1:2, 5:9)]
  
  # 5. Renommer les colonnes
  setnames(csv_file2, c("Hour", "DATE", "OFFER_TYPE", "ENERGY", "PRICE", "MATCHED", "AUCTION"))
  
  # 2. Nettoyage des colonnes et conversions
  
  csv_file2[, ENERGY := as.numeric(gsub(",", ".", gsub("\\.", "",  ENERGY)))]
  csv_file2[, PRICE := as.numeric(gsub(",", ".", gsub("\\.", "",  PRICE)))]
  # csv_file2[, ENERGY := as.numeric(gsub(",", ".", ENERGY))]
  # csv_file2[, PRICE := as.numeric(gsub(",", ".", PRICE))]
  
  #on s'occupe uniquement des demand curve pour l'instant
  # 2. Filtrer OFFER_TYPE == 'V' venta
  # 2. Filtrer OFFER_TYPE == 'C' compra
  if (type =="demand"){
    csv_file2 <- csv_file2[OFFER_TYPE == "C"]
    #for demand desc(price)
    csv_file2 <- csv_file2[
      order(DATE, AUCTION, Hour, OFFER_TYPE, MATCHED, desc(PRICE)),  # tri nécessaire avant cumsum
      CUM_ENERGY := cumsum(ENERGY),
      by = .(DATE, Hour, OFFER_TYPE, MATCHED, AUCTION)
    ]
    result2 <- csv_file2[
      #order(CUM_ENERGY), 
      ,
      {
        res <- lapply(price_grid, function(p) {
          i <- which(PRICE <= p)[1]
          if (!is.na(i)) CUM_ENERGY[i] else NA_real_
        })
        data.table(PRICE = price_grid, QTY = unlist(res))
      },
      by = .(DATE, Hour, OFFER_TYPE, MATCHED, AUCTION)
    ]
    
  }else if (type =="supply"){
    csv_file2 <- csv_file2[OFFER_TYPE == "V"]
    #for supply asc(price)
    csv_file2 <- csv_file2[
      order(DATE, Hour, OFFER_TYPE, MATCHED, PRICE, AUCTION),  # tri nécessaire avant cumsum
      CUM_ENERGY := cumsum(ENERGY),
      by = .(DATE, Hour, OFFER_TYPE, MATCHED, AUCTION)
    ]
    result2 <- csv_file2[
      #order(CUM_ENERGY), 
      ,
      {
        res <- lapply(price_grid, function(p) {
          i <- which(PRICE >= p)[1]
          if (!is.na(i)) CUM_ENERGY[i] else NA_real_
        })
        data.table(PRICE = price_grid, QTY = unlist(res))
      },
      by = .(DATE, Hour, OFFER_TYPE, MATCHED, AUCTION)
    ]
  }else{
    return("Invalid type")
  }
  
  
  # 3. Calcul par groupe
  
  
  
  
  # g = 10000
  # CUM_ENERGY = csv_file2$CUM_ENERGY
  # 3. Calcul pour chaque groupe
  
  
  #cela fait beaucoup de points
  #je cherche à alléger ma structure de données
  #beaucoup de NA dans les matched, je les enlève
  result2 <- na.omit(result2)
  
  #fonction en escalier
  #superflus de stocker des points sur les marches horizontales
  # Supprimer les lignes où PRICE est identique au précédent (par groupe)
  result2 <- result2[
    ,
    .SD[c(TRUE, diff(QTY) != 0)],
    by = .(DATE, Hour, OFFER_TYPE, MATCHED)
  ]
  
  #dans mon exemple permet de réduire d'un facteur 10
  # 96 000 -> drop na 51 000 -> 10 000
  
  
  return(result2)
  # ✅ Ajouter correctement à la liste globale
  #result_list[[length(result_list) + 1]] <<- result2
}


# Nettoyer les fichiers ? traiter
output_dir <- "F:/intraday_supply_demand_curves"
setwd(output_dir)
# Liste des fichiers dans le répertoire courant
my_files <- list.files()
# Filtrer les fichiers qui finissent par .1
files_ending_with_1 <- my_files[grep("\\.1$", my_files)]

#cum_grid <- seq(0, 60000,length.out = 1000)
price_grid <- seq(-200, 1000, length.out = 1001)
# Filtrer les valeurs existantes dans l'intervalle [-50, 500]
# subset_grid <- price_grid[price_grid >= -50 & price_grid <= 500]
# 
# # Prendre 101 points r�guli�rement espac�s
# # Cr�er 101 points r�guliers dans la plage
# target_points <- seq(from = min(subset_grid), to = max(subset_grid), length.out = 101)
# 
# # Pour chaque target, trouver le point de price_grid le plus proche
# price_grid_subset <- sapply(target_points, function(tp) {
#   subset_grid[which.min(abs(subset_grid - tp))]
# })


#keep a grid compatible with the grid from day ahead market but with a smaller number of points



#on prend une grille distordue pour représenter avec meilleure précision
#la partie la plus utilisée de la grid
#ce n'est pas forcément là où l'équilibre se fait qu'il y a le plus de changements
#price_grid[match(price_grid_subset, price_grid)]

# files <- c("curva_pibc_2023053004.1", "curva_pibc_2023053007.1")
# sub(".*(\\d)\\.1$", "\\1", files)

# Fonction enveloppante pour la gestion des erreurs
safe_execute <- function(file, pb) {
  print(file)
  tryCatch(
    { result2 <-execute_code_shift_qty(file, type = "supply")  # Cette fonction doit modifier `result_list` globalement ??? ? corriger
    pb$tick()  # Mise ? jour de la barre de progression
    return(result2)           # Il faut que `execute_code_shift()` retourne result2 directement
    },
    error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
      pb$tick()  # Mise ? jour de la barre de progression m?me en cas d'erreur
      return(NULL)
    }
  )
}


# Nombre total de fichiers ? traiter
total_files <- length(files_ending_with_1)

# Cr?er la barre de progression
pb <- progress_bar$new(
  total = total_files,       # Total de fichiers
  format = "  [:bar] :percent (:current/:total) Elapsed time: :elapsed", 
  clear = FALSE,             # Garde la barre apr?s la fin
  width = 60                 # Largeur de la barre
)


# Choisis ton plan (ici multicore si possible)
future::plan(multisession)  # utilise tous les cours de ton CPU (multisession fonctionne sur Windows, multicore sur Linux/Mac)

#curve <- files_ending_with_1[1]
#test <- execute_code_shift_qty(curve, type = "demand")


# Appliquer la fonction avec barre de progression
result_list <- future_lapply(files_ending_with_1, safe_execute, pb = pb)

# Filtrer les r?sultats pour retirer les NULLs dus aux erreurs
result_list <- Filter(Negate(is.null), result_list)

#curve_shift_data_bis <- curve_shift_data
# Fusionner les r?sultats finaux
curve_shift_data <- rbindlist(result_list, use.names = TRUE, fill = TRUE)


setwd("Z:/code_supply_demand/code_intraday")
write_parquet(curve_shift_data, 'curve_shift_data_supply_qty_intraday.parquet')

setorder(curve_shift_data, DATE, Hour, OFFER_TYPE, MATCHED, PRICE)

setwd(curve_path)
file <- "curva_pbc_20230204.1"
curve <- "curva_pbc_20230204.1"

result2 <- execute_code_shift_qty(file, type ="supply")
result3 <- execute_code_shift_qty(file, type ="demand")

View(result2 %>% 
       mutate(Hour = as.numeric(Hour)) %>% 
       arrange(OFFER_TYPE, MATCHED, DATE, Hour, QQTY))
View(curve_shift_data %>% 
       filter(DATE =="04/02/2023" & OFFER_TYPE == "V" & MATCHED == "C"))

