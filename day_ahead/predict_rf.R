library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)
library(data.table)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs")
filtered_indispo <- read_parquet("filtered_indispo.parquet")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant')
generation_per_plant <- read_parquet("generation_by_plant_esp.parquet")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp/gen_by_fuel')
generation_by_fuel <- read_parquet('generation_by_fuel_format.parquet')


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
                                              'day_of_week', 'wind_solar_tot', 'gen_tot', centrale)] %>%
    mutate(previous_day_prod = lag(!!sym(centrale), 24))  # Use !!sym to refer to the variable dynamically
  
  # Convert some columns to factors
  mini_prediction_data$day_of_week <- as.factor(mini_prediction_data$day_of_week)
  mini_prediction_data$month <- as.factor(mini_prediction_data$month)
  
  #on predit pour toutes les dates où il y a une indisponibilité
  prediction_dates <- filtered_indispo %>% 
    filter(production_resource_name == !!centrale) %>% 
    distinct()
  #retirer les périodes où indispo
  #pour l'entraînement
  training_data <- na.omit(mini_prediction_data) %>% 
    filter(!(`__index_level_0__` %in% prediction_dates$datetime_slot))
  
  
  # Fit a random forest model
  formula_str <- paste0("`", centrale, "` ~ gen_tot + wind_solar_tot + day_of_week + month")
  rf_model_ranger <- ranger(
    formula = as.formula(formula_str),
    data = training_data
  )
  
  test_data  <- merge(mini_prediction_data,
                      prediction_dates,
                      by.x = c('__index_level_0__'),
                      by.y = c('datetime_slot') )

  test_data <- test_data[, c('__index_level_0__', 'month',
                             'wind_solar_tot', 'gen_tot','day_of_week','previous_day_prod',
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
setDT(generation_per_plant)# Convertir en data.table

rf_predictions_data <- NULL
centrale <- centrales[1]
for (centrale in centrales){
  print(centrale)
  tryCatch({
    
    prediction_centrale <- get_predictions(centrale)%>% 
      mutate(centrale = !!centrale)
    colnames(prediction_centrale) <- c('__index_level_0__', 'month','wind_solar_tot','gen_tot',
                                       'day_of_week','previous_day_prod', 'centrale_prod', 
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
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/rf'
setwd(save_path_entsoe_output)
write_parquet(rf_predictions_data, 'rf_predictions_data.parquet')


indispo_rf <- merge(filtered_indispo,
                    rf_predictions_data,
                    by.x =  c('datetime_slot', 'production_resource_name'),
                    by.y = c('__index_level_0__', 'centrale')
                    )


real_indispo_rf <- indispo_rf %>% 
  filter(predicted_real_unavailability < -50) %>%
  rename(datetime = datetime_slot)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/rf'
setwd(save_path_entsoe_output)
write_parquet(real_indispo_rf, 'real_indispo_rf.parquet')
