library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'Z:/prix_electricite_gitlab/Intra_day_micro/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_quat.R")


save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE_unavailabilities/ES/formatted'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut)

unexpected_indispo_ENTSOE_data  <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Unplanned outage')

unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data[!(unexpected_indispo_ENTSOE_data$mrid
                                                                   %in% unexpected_indispo_ENTSOE_data$mrid[duplicated(unexpected_indispo_ENTSOE_data$mrid)]), ]


#problem is i suppose that there is immediate transmission of the news
# from the start date
#because it was not published on REMIT
# but published on some national website 

dim(unexpected_indispo_ENTSOE_data)
result_df <<- NULL
counter <- 0  # Initialiser un compteur

test <- apply(unexpected_indispo_ENTSOE_data, 1, function(row) {
  counter <<- counter + 1  # Incrémenter le compteur
  
  # Afficher un message toutes les 200 lignes
  if (counter %% 200 == 0) {
    message("Lignes traitées : ", counter)
  }
  
  # Appeler la fonction originale
  pub_auction_window_uneffective(row)
})

# Convert the result to a data frame
#result_df <- data.frame(t(result_df))


setwd("Z:/prix_electricite_gitlab/Intra_day_micro/outputs")
write_parquet(result_df, "result_df_uneffective.parquet")

setwd("Z:/prix_electricite_gitlab/Intra_day_micro/outputs")
result_df <- read_parquet("result_df_uneffective.parquet")

colnames(result_df)
colnames(unexpected_indispo_ENTSOE_data)

auction_ENTSOE_indispo <- result_df %>% 
  left_join(unexpected_indispo_ENTSOE_data,  by = "mrid")

# auction_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
#   mutate( dummy = ifelse( auction_before ==2 & auction_after ==3 & sprintf("%02d:%02d", hour(datetime_pub), minute_pub )< time_ref, 1, 0),
#           auction_before = ifelse( dummy, 1, auction_before),
#           auction_after = ifelse( dummy, 2, auction_after))


actual_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)

# je decide d'enlever les evene;ents de moins d'une heure
actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  filter(duration >= 1)



save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)
write_parquet(actual_ENTSOE_indispo, 'actual_ENTSOE_indispo_uneffective.parquet')


colnames(actual_ENTSOE_indispo)

actual_ENTSOE_indispo %>%
  mutate(end_before_pub = datetime_pub < datetime_fin) %>%
  group_by(end_before_pub) %>% 
  summarise(n = n())

actual_ENTSOE_indispo %>% 
  group_by(auction_before, auction_after) %>% 
  summarise(n = n())

