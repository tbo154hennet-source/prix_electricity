
#un modele de prediction
#evaluer la production qui aurait été fournie
#dans un contre-factuel sans indispo

library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel')
generation_by_fuel <-  read_parquet('generation_by_fuel.parquet')
colnames(generation_by_fuel)
fields <- c("('Solar', 'Actual Aggregated')",
  "('Wind Onshore', 'Actual Aggregated')",
  "('Wind Offshore', 'Actual Aggregated')",
  "('Hydro Run-of-river and poundage', 'Actual Aggregated')")

generation_by_fuel <- generation_by_fuel %>% 
  rowwise() %>% 
  mutate(gen_tot = sum(c_across(-`__index_level_0__`), na.rm = TRUE),
         wind_solar_tot = sum(c_across(all_of(fields)), na.rm = TRUE)) %>% 
  select(`__index_level_0__`, gen_tot, wind_solar_tot, all_of(fields))

#using a small random forest model already implemented
# Fit a random forest model

generation_by_fuel$`__index_level_0__` <- 
  with_tz(generation_by_fuel$`__index_level_0__`, tzone ='Europe/Brussels')

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel')
write_parquet(generation_by_fuel, 'generation_by_fuel_format.parquet')

generation_by_fuel <- read_parquet('generation_by_fuel_format.parquet')



save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)
dismissed_long_format_indispo_non_published <- read_parquet('dismissed_long_format_indispo_non_published.parquet')
dismissed_long_format_nuclear_indispo_non_published <- read_parquet('dismissed_nuclear_long_format_indispo_non_published.parquet')


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant/esp')
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")

# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
generation_per_plant <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)

# Extract the first value between single quotes
names_extracted <- gsub("^\\('([^']+)'.*", "\\1", colnames(generation_per_plant))

colnames(generation_per_plant) <- names_extracted

library(lubridate)
prediction_data <- merge(generation_by_fuel, generation_per_plant, 
                            by='__index_level_0__') %>% 
  mutate(month= month(`__index_level_0__`),
         day_of_week = weekdays(`__index_level_0__`),
         creneau = hour(`__index_level_0__`) +1)


# Sort the columns by alphabetical order
generation_per_plant <- generation_per_plant[, order(colnames(generation_per_plant))]

# Get duplicated column names
duplicated_names <- names(generation_per_plant )[duplicated(names(generation_per_plant ))]

for (col in duplicated_names){
  # Get the indexes of the duplicated columns
  indexes <- which(colnames(generation_per_plant) == col)
  
  # Fusionner les colonnes avec le même nom en prenant les valeurs non-NA
  generation_per_plant[[indexes[1]]] <- coalesce(generation_per_plant[[indexes[1]]],
                                                      generation_per_plant[[indexes[2]]])
  # Remove the column by its index
  generation_per_plant <- generation_per_plant[, -indexes[2]]
}


get_predictions <- function(centrale){
  
  #centrale = "BESOS 3"
  # Prepare mini_prediction_data
  mini_prediction_data <- prediction_data[, c('__index_level_0__', 'month',
                                              'day_of_week', 'creneau',
                                              'wind_solar_tot', 'gen_tot', centrale)] %>%
    mutate(previous_day_prod = lag(!!sym(centrale), 24))  # Use !!sym to refer to the variable dynamically
  
  mini_prediction_data <- na.omit(mini_prediction_data)
    
  
  
  # Convert some columns to factors
  mini_prediction_data$day_of_week <- as.factor(mini_prediction_data$day_of_week)
  mini_prediction_data$month <- as.factor(mini_prediction_data$month)
  mini_prediction_data$creneau <- as.factor(mini_prediction_data$creneau)
  
  # Fit a random forest model
  formula_str <- paste0("`", centrale, "` ~ gen_tot + wind_solar_tot + previous_day_prod + day_of_week + month + creneau")
  rf_model_ranger <- ranger(
    formula = as.formula(formula_str),
    data = mini_prediction_data
  )
  
  #on predit pour toutes les dates où il y a une indisponibilité
  prediction_dates <- dismissed_long_format_indispo_non_published %>% 
    filter(production_resource_psr_name == !!centrale) %>% 
    distinct() %>% 
    mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau, units = "hours")
           )

  
  test_data  <- merge(mini_prediction_data, 
                      prediction_dates[, !(names(prediction_dates) %in% c('creneau', 'day_of_week'))],
                      by.x = c('__index_level_0__'),
                      by.y = c('datetime_slot') )
  
  test_data <- test_data[, c('__index_level_0__', 'month',
                             'wind_solar_tot', 'gen_tot','day_of_week','creneau','previous_day_prod',
                              centrale)] 
  # Make predictions
  predictions <- predict(rf_model_ranger, data=test_data)
  
  #on peut maintenant comparer les predictions avec les données réelles
  test_data <- cbind(test_data, predictions$predictions)
  test_data <- test_data %>% 
    mutate(deviation = !!sym(centrale) - predictions$predictions)
  
  
  return(test_data)
  
}
library(ranger)
centrales <- colnames(generation_per_plant %>% 
                        select(-c('__index_level_0__')))# Replace with your actual plant names

rf_predictions_data <- NULL
centrale <- centrales[1]
for (centrale in centrales){
  print(centrale)
  tryCatch({
    
    prediction_centrale <- get_predictions(centrale)%>% 
      mutate(centrale = !!centrale)
    colnames(prediction_centrale) <- c('__index_level_0__', 'month','wind_solar_tot','gen_tot',
                                       'day_of_week','creneau','previous_day_prod', 'centrale_prod', 
                                       'prediction_prod', 'predicted_real_unavailability', 'centrale')
    
    if (is.null(rf_predictions_data)){
      rf_predictions_data <- prediction_centrale 
    }else{
      rf_predictions_data <- rbind(rf_predictions_data, prediction_centrale)
    } 
  }, error = function(e) {
    print(paste("Error for", centrale))
  })
  
}


getwd()
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/rf_prediction'
setwd(save_path_entsoe_output)
write_parquet(rf_predictions_data, 'rf_theoretical_predictions_data_non_published_dismissed.parquet')


dismissed_long_format_indispo_non_published <- dismissed_long_format_indispo_non_published%>% 
           mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))



indispo_rf <- merge(dismissed_long_format_indispo_non_published,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
)


real_indispo_rf <- indispo_rf %>% 
  filter(predicted_real_unavailability < -50) %>%
  rename(datetime = datetime_slot)


#on fait un petit plot pour voir quelle est la relation entre les indispos annoncées
# et les predicted
library(ggplot2)


ggplot(data = real_indispo_rf %>% 
         mutate(Unavailable.Capacity = as.numeric(Unavailable.Capacity)),
       aes(x = -predicted_real_unavailability, y = Unavailable.Capacity)) +
  geom_point() +
  theme_minimal()


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/rf_prediction'
setwd(save_path_entsoe_output)
write_parquet(real_indispo_rf, 'dismissed_non_published_real_theoretical_indispo_rf.parquet')



final_real_indispo_rf <- real_indispo_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau, datetime, wind_solar_tot,
           gen_tot) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability)),
  ) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)
write_parquet(final_real_indispo_rf, 
              'final_not_publi_real_indispo_rf_dismissed.parquet')










