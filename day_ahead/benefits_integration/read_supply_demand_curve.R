library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

Sys.setenv(lang = "en_US")


# pour fair une specification log log,
# normalisation par la puissance installee

# explorer differents types de normalisation
# pour tenir compte de l'intermittence
# normaliser par la production prevue
# pour donner un meilleur sens de la rarete
curve_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Supply_demand_curves'

# Fonction pour trouver l'intersection
find_intersection <- function(supply_curve, demand_curve) {
  intersection <- NULL
  
  # Pour chaque point dans la courbe d'offre
  for (i in 1:nrow(supply_curve)) {
    # Trouver le point de la courbe de demande le plus proche en cum_energy
    closest_idx <- which.min(abs(demand_curve$cum_energy - supply_curve$cum_energy[i]))
    
    # Vérifier si l'offre dépasse ou égale la demande à ce point
    if (supply_curve$PRICE[i] >= demand_curve$PRICE[closest_idx]) {
      intersection <- data.frame(
        cum_energy = supply_curve$cum_energy[i],
        price = supply_curve$PRICE[i]
      )
      break
    }
  }
  
  return(intersection)
}

execute_code <- function(curve){
   #2023-09-28 23:00:00
  csv_file <- read.csv2(curve, header = FALSE,
                        skip = 1)
  colnames(csv_file) <- as.character(csv_file[1, ]) 
  csv_file <- csv_file %>% 
    filter(Pais == "ES" | Pais == "MI")
  csv_file <- csv_file[-1, c(1:2, 5:8)]  # Supprimer la première ligne devenue superflue
  colnames(csv_file) <- c('Hour', 'DATE', 'OFFER_TYPE','ENERGY', 'PRICE', 'MATCHED' )
  
  for (my_hour in c(1:24)){
    plot_data <- csv_file %>% 
      filter(Hour == my_hour) %>%
      mutate(ENERGY = gsub("\\.", "", ENERGY)) %>% 
      mutate(ENERGY = as.numeric(gsub(",", ".", ENERGY)),
             PRICE = as.numeric(gsub(",", ".", PRICE)))
    
    # Filtrer les offres et demandes qui ont été "matched"
    matched_supply_curve <- plot_data %>%
      filter(OFFER_TYPE == 'V' & MATCHED == 'C') %>%
      arrange(PRICE) %>%
      mutate(cum_energy = cumsum(as.numeric(ENERGY)))
    
    matched_demand_curve <- plot_data %>%
      filter(OFFER_TYPE == 'C' & MATCHED == 'C') %>%
      arrange(desc(PRICE)) %>%
      mutate(cum_energy = cumsum(as.numeric(ENERGY)))

  matched_eq_price <- matched_supply_curve[nrow(matched_supply_curve), "PRICE"] 
  matched_eq_energy <- matched_supply_curve[nrow(matched_supply_curve), "cum_energy"]

  demand_curve_adj  <- plot_data %>%
    filter(OFFER_TYPE == 'C')%>%
    filter(MATCHED == 'C' | PRICE < matched_eq_price ) %>% 
    arrange(desc(PRICE)) %>%
    mutate(cum_energy = cumsum(as.numeric(ENERGY)))
  
  # Extraire la date (8 derniers chiffres avant le point)
  date_str <- sub(".*_(\\d{8})\\..*", "\\1", curve)
  time_part <- ifelse(my_hour >9, paste0(my_hour, ":00:00"), paste0("0", my_hour, ":00:00"))
  # Créer un objet POSIXct en combinant la date et l'heure
  my_datetime_slot <- as.POSIXct(paste(as.Date(date_str, format="%Y%m%d") , time_part), format="%Y-%m-%d %H:%M:%S", tz="UTC")

  CBF_ESP_FR <- price_merge_CBF %>% 
    filter(datetime == my_datetime_slot) %>%
    pull(CBF_ESP_FR)
  CBF_FR_ESP<- price_merge_CBF %>% 
    filter(datetime == my_datetime_slot) %>%
    pull(CBF_FR_ESP)
  CBF <- ifelse(CBF_ESP_FR == 0 ,  -CBF_FR_ESP, CBF_ESP_FR)
  
  cross_border_exchanges <- data.frame(Hour = my_hour, DATE = my_datetime_slot, OFFER_TYPE = "V",
                                       ENERGY = CBF, PRICE = -300, MATCHED = "O")
  
  new_supply_curve_adj <- plot_data %>%
    rbind(cross_border_exchanges) %>% 
    filter(OFFER_TYPE == 'V' ) %>%
    filter(MATCHED == 'C' | PRICE > matched_eq_price ) %>% 
    arrange(PRICE) %>%
    mutate(cum_energy = cumsum(as.numeric(ENERGY)))
  
  # Appliquer la fonction pour trouver l'intersection
  new_intersection_point_adj <- find_intersection(new_supply_curve_adj, demand_curve_adj)
  
  # Extraire les coordonnées du point d'équilibre
  new_equilibrium_energy <- round(new_intersection_point_adj$cum_energy, 0)
  new_equilibrium_price <- round(new_intersection_point_adj$price, 2)
  
  delta_price <- round(new_equilibrium_price - matched_eq_price, 2)
  delta_energy <- round(new_equilibrium_energy - matched_eq_energy, 0)
  
  my_row <- data.frame(DATE = my_datetime_slot, 
                       matched_eq_price = matched_eq_price, 
                       matched_eq_energy = matched_eq_energy,
                       new_equilibrium_price = new_equilibrium_price,
                       new_equilibrium_energy = new_equilibrium_energy
  )
  if (is.null(curve_data)){
    curve_data <<- my_row
  }else{
    curve_data <<- rbind(curve_data, my_row)
  }
}
  
}

curve_data <- NULL
setwd(curve_path)


my_files <- list.files()
# Obtenez la liste des fichiers .parquet
parquet_files <- my_files[grep("\\.parquet$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers
my_files <- setdiff(my_files, parquet_files)

file <- my_files[150]
length(my_files)
library(tibble)

for (file in my_files){
  index <- which(my_files == file)
  if (index %% 30 == 0){
    print(file)
  }
  setwd(curve_path)
  tryCatch(
    {
      execute_code(file)
    },
    error = function(e) {
      cat("An error occurred:", conditionMessage(e), "\n")
      print(file) # Return NA or any other value you want to indicate an error
    }
  )
    
    
}
setwd(curve_path)
write_parquet(curve_data, 'curve_data_all.parquet')

#some summary stats

curve_data <- curve_data %>% 
  mutate(delta_price = new_equilibrium_price - matched_eq_price,
         delta_energy = new_equilibrium_energy - matched_eq_energy,
         benefits = matched_eq_energy * delta_price,
         rebound_effect = matched_eq_price * delta_energy,
         net_benefits = benefits - rebound_effect
  )

summary(curve_data)

colnames(curve_data)

#attention à la période de l'exception ibérique
#export de l'énergie subventionnée
#subventionnée par le gouvernement espagnol

exception_start_date <- as.POSIXct("15-06-2022 00:00:00", 
                                   format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")
exception_end_date <- as.POSIXct("01-03-2023 00:00:00", 
                                 format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")
curve_data_exception <- curve_data %>% 
  filter(DATE >= exception_start_date & DATE <= exception_end_date)
curve_data_pre_exception <- curve_data %>% 
  filter(DATE < exception_start_date)

summary(curve_data_exception)
summary(curve_data_pre_exception)
