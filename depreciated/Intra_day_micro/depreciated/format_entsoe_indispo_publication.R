library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE'
setwd(save_path_entsoe)
source("auxiliary_file_ter.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut)


# test <- bout_indispo_ENTSOE_data %>% 
#   filter(plant_type == "Nuclear" & datetime_debut > as.POSIXct("2018-01-01")) %>% 
#   arrange(datetime_pub)

unexpected_indispo_ENTSOE_data  <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Unplanned outage') %>% 
  ungroup()

colnames(unexpected_indispo_ENTSOE_data)

#on se concentre sur les publications qui ont été faites 
#à peu près en meêm temps que le début d'indispo
#sinon cela signifie que ce n'est potentiellement pas le premier message

# sample <- unexpected_indispo_ENTSOE_data %>% 
#   sample_n(10)



#je vais diviser mon échantillon entre

#pour mon event study
#pendant l'indispo (à l'heure près)


# ont été publiées en même temps que le début de l'indispo


# ont été publiées après la fin de l'indispo
# entre les deux

#on voudrait les auctions que cela aurait pu affecter
#si jamais l'information était fluide
# dans les fonctions suivantes, il ne faut donc aucune
# référence à la date de publication de l'indispo




publication_lapse <- unexpected_indispo_ENTSOE_data %>% 
  mutate(lapse = difftime(datetime_debut, datetime_pub, units = "hours"),
         year = year(datetime_debut))

# publication_lapse %>% 
#   filter(lapse <0 & year>2015)


publi_stat <- publication_lapse %>% 
  group_by(year) %>%
  summarise(lapse = mean(lapse, na.rm = TRUE))

library(ggplot2)
ggplot(publi_stat %>% filter(year>2015), aes(x = year, y = lapse)) +
  geom_line() +
  geom_point() +
  labs(title = "Publication delay for unplanned outages",
       x = "Year",
       y = "Average publication delay (hours)")

#pour avoir un effet le retard de publication ne peut pas être supérieur à 24h



result_df <- NULL

# sample <- unexpected_indispo_ENTSOE_data %>% 
#   sample_n(10)
# apply(sample, 1, pub_auction_window)

apply(unexpected_indispo_ENTSOE_data, 1, pub_auction_window)

# Convert the result to a data frame
result_df_bis <- data.frame(t(result_df))





# Bind the result as new columns to the original data frame
auction_ENTSOE_indispo <- 
  cbind(unexpected_indispo_ENTSOE_data, result_df_bis)
# Get the number of columns in the data frame
num_cols <- ncol(auction_ENTSOE_indispo)
names(auction_ENTSOE_indispo)[(num_cols-7):num_cols] <- new_names_pub




# add a dummy to check that startime is in the vicinity of ref_day
# and not a month before



# on s'occupe des doublons sur les marches differenciees hier et aujourd'hui

# Transformer le vecteur en une matrice de 32 lignes et 12 colonnes
auction_2_6 <- matrix(register_auction_2_6[[1]], nrow = num_cols)
# Convertir la matrice en data frame
auction_2_6 <- as.data.frame(t(auction_2_6))
colnames(auction_2_6) <- colnames(auction_ENTSOE_indispo)

# Boucle pour ajuster les types de colonnes de auction_2_6
for (col in colnames(auction_ENTSOE_indispo)) {
  # Vérifier si la colonne existe dans les deux dataframes
  if (col %in% colnames(auction_2_6)) {
    
    # Comparer uniquement le type principal
    if (class(auction_ENTSOE_indispo[[col]])[1] != class(auction_2_6[[col]])[1]) {
      
      # Si la classe est POSIXct, utiliser as.POSIXct() pour la conversion
      if (class(auction_ENTSOE_indispo[[col]])[1] == "POSIXct") {
        auction_2_6[[col]] <- as.POSIXct(auction_2_6[[col]], origin = "1970-01-01")
      }
      # Si la classe est numeric, utiliser as.numeric()
      else if (class(auction_ENTSOE_indispo[[col]])[1] == "numeric") {
        auction_2_6[[col]] <- as.numeric(auction_2_6[[col]])
      }
      # Si la classe est character, utiliser as.character()
      else if (class(auction_ENTSOE_indispo[[col]])[1] == "character") {
        auction_2_6[[col]] <- as.character(auction_2_6[[col]])
      }
      # Ajouter d'autres conditions ici pour d'autres types si nécessaire
    }
  }
}

auction_2_6$duration <- auction_2_6$datetime_fin- auction_2_6$datetime_debut

# Maintenant, vous pouvez combiner les deux dataframes
auction_ENTSOE_indispo <- rbind(auction_ENTSOE_indispo, auction_2_6)


actual_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)
colnames(actual_ENTSOE_indispo)



actual_ENTSOE_indispo %>% 
  group_by(end_before_pub) %>% 
  summarise(n = n())
  
# je decide d'enlever les evene;ents de moins d'une heure
actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  filter(duration >= 1)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)

write_parquet(actual_ENTSOE_indispo, 'actual_ENTSOE_indispo.parquet')


