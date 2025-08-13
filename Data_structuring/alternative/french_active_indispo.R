
save_path_fr <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_path_fr)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet")

row <- bout_indispo_ENTSOE_data[1,]
#active french indispo
#correct indispos 

#long_format_indispo <- read_parquet('long_format_indispo.parquet')

# get long format for every hour of the day
# get the corresponding line with unavailability



long_format_indispo <- NULL
long_format <- function(row){
  datetime_debut <- as.POSIXct(row[['datetime_debut']], tz = "UTC")
  datetime_fin <- as.POSIXct(row[['datetime_fin']], tz = "UTC")
  # Arrondir la date de début à l'heure pleine suivante
  date_debut_arrondie <- floor_date(datetime_debut, unit = "hour")
  date_fin_arrondie <- ceiling_date(datetime_fin, unit = "hour")
  
  
  #Créer une séquence d'heures entre la date de début arrondie et la date de fin
  creneaux <- seq(from = date_debut_arrondie,
                to = date_fin_arrondie, by = "hour")

  # Convert the row to a dataframe
  row_df <- as.data.frame(t(row), stringsAsFactors = FALSE)
  
  # Iterate over each hour
  for (creneau in creneaux) {
    # Create a dataframe for the current hour
    line <- row_df %>% 
      mutate(creneau = hour(creneau),
             electricity_delivery_date = as.Date(creneau))
    
    # Append the line to the result
    long_format_indispo <<- rbind(long_format_indispo, line)
  }
}

sample <- bout_indispo_ENTSOE_data[1:100,]

#peut etre que j'ai beaucoup plus d'indispos 
#qu'avec les données espagnoles
#en tout cas je ne peux pas faire comme ça

# result_hours <- apply(sample, 1, long_format)


write_parquet(long_format_indispo, 'long_format_indispo_fr.parquet')

#test <- long_format_indispo %>% filter(electricity_delivery_date=='2021-09-25')

# 1st_affec_tim_period","last_affec_tim_period"
# "chevauche", in auction time

final_indispo <- long_format_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

#test <- final_indispo %>% filter(electricity_delivery_date=='2021-09-25')

write_parquet(final_indispo, 'final_indispo.parquet')
