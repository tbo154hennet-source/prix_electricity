library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)
library(data.table)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
filtered_indispo <- read_parquet("filtered_indispo_esp.parquet")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant')
generation_per_plant <- read_parquet("generation_by_plant_esp.parquet")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot')
generation_by_fuel <- read_parquet('generation_by_fuel_format_esp.parquet')


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



centrales <- colnames(generation_per_plant %>% 
                        select(-c('__index_level_0__')))# Replace with your actual plant names
indispo_centrales <- filtered_indispo %>% 
  distinct(production_resource_name) %>% 
  pull(production_resource_name)

active_centrales <- intersect(centrales, indispo_centrales)
centrale = active_centrales[1]

#estimate all models before predicting
for (centrale in active_centrales) {
  tryCatch({
    print(centrale)
    mini_prediction_data <- prediction_data[, c('__index_level_0__', 'month',
                                                'day_of_week', 'creneau',
                                                'wind_solar_tot', 'gen_tot', centrale)] %>%
      mutate(previous_day_prod = lag(!!sym(centrale), 24))  # Use !!sym to refer to the variable dynamically
    
    mini_prediction_data <- na.omit(mini_prediction_data)
    
    
    
    # Convert some columns to factors
    mini_prediction_data$day_of_week <- as.factor(mini_prediction_data$day_of_week)
    mini_prediction_data$month <- as.factor(mini_prediction_data$month)
    mini_prediction_data$creneau <- as.factor(mini_prediction_data$creneau)
    
    
    mini_prediction_data <- prediction_data[, c('__index_level_0__', 'month',
                                                'day_of_week', 'creneau',
                                                'wind_solar_tot', 'gen_tot', centrale)] %>%
      mutate(previous_day_prod = lag(!!sym(centrale), 24))  # Use !!sym to refer to the variable dynamically
    
    mini_prediction_data <- na.omit(mini_prediction_data)
    
    
    
    # Convert some columns to factors
    mini_prediction_data$day_of_week <- as.factor(mini_prediction_data$day_of_week)
    mini_prediction_data$month <- as.factor(mini_prediction_data$month)
    mini_prediction_data$creneau <- as.factor(mini_prediction_data$creneau)
    
    # Select relevant features
    features <- c("gen_tot", "wind_solar_tot", "day_of_week", "month", "creneau")
    
    #prepare training data
    # One-hot encoding (turn categorical variables into dummy variables)
    dummies <- model.matrix(~ day_of_week + month + creneau - 1, data = mini_prediction_data)
    
    # Combine encoded categorical variables with numerical features
    X <- as.matrix(cbind(mini_prediction_data[, c("gen_tot", "wind_solar_tot")], dummies))
    y <- mini_prediction_data[[centrale]]
    
    # Creax = # Create DMatrix (XGBoost format)
    dtrain <- xgb.DMatrix(data = X, label = y)
    
    # Train the model
    xgb_model <- xgboost(
      data = dtrain,
      objective = "reg:squarederror",  # Regression task
      nrounds = 50,                    # Small number of trees
      max_depth = 3,                    # Shallow trees for efficiency
      eta = 0.1,                        # Learning rate
      verbose = 0                        # Silent mode
    )
    
    models[[centrale]] <- xgb_model
  }, error = function(e) {
    print(paste("Error for", centrale))
  })
  
  
  
}


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/rf")
saveRDS(models, 'rf_models_xgb_esp.rds')

models <- readRDS('rf_models_xgb_esp.rds')



get_predictions <- function(centrale){
  
  #centrale = "BESOS 3"
  # Prepare mini_prediction_data
  mini_prediction_data <- prediction_data[, c('__index_level_0__', 'month',
                                              'day_of_week', 'wind_solar_tot','creneau',  'gen_tot', centrale)] %>%
    mutate(previous_day_prod = lag(!!sym(centrale), 24))  # Use !!sym to refer to the variable dynamically
  
  # Convert some columns to factors
  mini_prediction_data$day_of_week <- as.factor(mini_prediction_data$day_of_week)
  mini_prediction_data$month <- as.factor(mini_prediction_data$month)
  mini_prediction_data$creneau <- as.factor(mini_prediction_data$creneau)
  
  # Select relevant features
  features <- c("gen_tot", "wind_solar_tot", "previous_day_prod", "day_of_week", "month", "creneau")
  
  xgb_model <- models[[centrale]]
  
  
  
  # Prepare test data
  
  prediction_dates <- filtered_indispo %>% 
    filter(production_resource_name == !!centrale) %>% 
    distinct()

  test_data  <- merge(mini_prediction_data, 
                      prediction_dates,
                      by.x = c('__index_level_0__'),
                      by.y = c('datetime_slot') )
  colnames(test_data)
  test_data <- test_data[, c('__index_level_0__', 'month',
                             'wind_solar_tot', 'gen_tot','day_of_week','creneau',
                             centrale)]
  
  
  
  # One-hot encoding for new data
  new_dummies <- model.matrix(~ day_of_week + month + creneau - 1, data = test_data)
  
  # Prepare the feature matrix
  new_X <- cbind(test_data[, c("gen_tot", "wind_solar_tot")], new_dummies)
  new_X <- as.matrix(new_X)
  
  # Convert to DMatrix
  dtest <- xgb.DMatrix(data = new_X)
  
  # Predict
  predictions <- predict(xgb_model, dtest)
  
  # Add predictions to the dataset
  test_data$predicted_production <- predictions
  test_data <- test_data %>% 
    mutate(deviation = !!sym(centrale) - predicted_production)
  
  
  return(test_data)
  
}


setDT(generation_per_plant)# Convertir en data.table

rf_predictions_data <- NULL
centrale <- centrales[4]
for (centrale in active_centrales){
  print(centrale)
  tryCatch({
    
    prediction_centrale <- get_predictions(centrale)%>% 
      mutate(centrale = !!centrale)
    colnames(prediction_centrale) <- c('__index_level_0__', 'month','wind_solar_tot','gen_tot',
                                       'day_of_week', 'creneau', 'centrale_prod', 
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
write_parquet(rf_predictions_data, 'rf_predictions_data_xgb.parquet')

colnames(rf_predictions_data)
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
write_parquet(real_indispo_rf, 'real_indispo_rf_xgb.parquet')
