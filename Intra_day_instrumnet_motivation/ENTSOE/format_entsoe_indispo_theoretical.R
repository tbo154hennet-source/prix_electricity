library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE'
setwd(save_path_entsoe)
source("auxiliary_file.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut)

unexpected_indispo_ENTSOE_data  <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Unplanned outage')

#problem is i suppose that there is immediate transmission of the news
# from the start date
#because it was not published on REMIT
# but published on some national website 


register_auction_2_6 <- NULL

result_df <- apply(unexpected_indispo_ENTSOE_data, 1, pub_auction_window_theoretical)

# Convert the result to a data frame
result_df <- data.frame(t(result_df))




# all_indispo_ENTSOE_data <-
#   cbind(bout_indispo_ENTSOE_data, result_df)%>%
#   filter(businesstype=='Unplanned outage')

# Bind the result as new columns to the original data frame
all_indispo_ENTSOE_data <- 
  cbind(unexpected_indispo_ENTSOE_data, result_df)
# Get the number of columns in the data frame
num_cols <- ncol(all_indispo_ENTSOE_data)
names(all_indispo_ENTSOE_data)[(num_cols-6):num_cols] <- new_names_theoretical




# on s'occupe des doublons sur les marches differenciees hier et aujourd'hui

# Transformer le vecteur en une matrice de 32 lignes et 12 colonnes
auction_2_6 <- matrix(register_auction_2_6[[1]], nrow = num_cols)
# Convertir la matrice en data frame
auction_2_6 <- as.data.frame(t(auction_2_6))
colnames(auction_2_6) <- colnames(all_indispo_ENTSOE_data)

# Boucle pour ajuster les types de colonnes de auction_2_6
for (col in colnames(all_indispo_ENTSOE_data)) {
  # Vérifier si la colonne existe dans les deux dataframes
  if (col %in% colnames(auction_2_6)) {
    
    # Comparer uniquement le type principal
    if (class(all_indispo_ENTSOE_data[[col]])[1] != class(auction_2_6[[col]])[1]) {
      
      # Si la classe est POSIXct, utiliser as.POSIXct() pour la conversion
      if (class(all_indispo_ENTSOE_data[[col]])[1] == "POSIXct") {
        auction_2_6[[col]] <- as.POSIXct(auction_2_6[[col]], origin = "1970-01-01")
      }
      # Si la classe est numeric, utiliser as.numeric()
      else if (class(all_indispo_ENTSOE_data[[col]])[1] == "numeric") {
        auction_2_6[[col]] <- as.numeric(auction_2_6[[col]])
      }
      # Si la classe est character, utiliser as.character()
      else if (class(all_indispo_ENTSOE_data[[col]])[1] == "character") {
        auction_2_6[[col]] <- as.character(auction_2_6[[col]])
      }
      # Ajouter d'autres conditions ici pour d'autres types si nécessaire
    }
  }
}

auction_2_6$duration <- auction_2_6$datetime_fin- auction_2_6$datetime_debut


# Maintenant, vous pouvez combiner les deux dataframes
all_indispo_ENTSOE_data <- rbind(all_indispo_ENTSOE_data , auction_2_6)

all_indispo_ENTSOE_data <- all_indispo_ENTSOE_data  %>% 
  filter(auction_before!=auction_after)

# je decide d'enlever les evene;ents de moins d'une heure
all_indispo_ENTSOE_data <- all_indispo_ENTSOE_data %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

all_indispo_ENTSOE_data <- all_indispo_ENTSOE_data %>% 
  filter(duration >= 1)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

write_parquet(all_indispo_ENTSOE_data, 'theoretical_ENTSOE_indispo.parquet')



