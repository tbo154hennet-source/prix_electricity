
#un modele de prediction
#evaluer la production qui aurait été fournie
#dans un contre-factuel sans indispo
library(xgboost)
library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)
library(ggplot2)



setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/esp')
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

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot')
write_parquet(generation_by_fuel, 'generation_by_fuel_format.parquet')

generation_by_fuel <- read_parquet('generation_by_fuel_format_esp.parquet')




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

#Get plants for which I can match unavailabilities


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)

dismissed_long_format_indispo <- read_parquet('dismissed_long_format_indispo.parquet')
dismissed_long_format_indispo_non_published <- read_parquet('dismissed_long_format_indispo_non_published.parquet')
dismissed_long_format_indispo_uneffective <- read_parquet('dismissed_uneffective_long_format_indispo.parquet')

long_format_indispo <- read_parquet('long_format_indispo.parquet')
long_format_indispo_non_published <- read_parquet('long_format_indispo_non_published.parquet')
long_format_indispo_uneffective <- read_parquet('long_format_indispo_uneffective.parquet')


# Add a dummy column to differentiate the sources
dismissed_long_format_indispo$source <- "dismissed_published"
dismissed_long_format_indispo_non_published$source <- "dismissed_non_published"
dismissed_long_format_indispo_uneffective$source <- "dismissed_uneffective"
long_format_indispo$source <- "published"
long_format_indispo_non_published$source <- "non_published"
long_format_indispo_uneffective$source <- "uneffective"


dismissed_long_format_indispo$chevauche <- 0
dismissed_long_format_indispo_non_published$chevauche <- 0
dismissed_long_format_indispo_uneffective$chevauche <- 0

colnames(dismissed_long_format_indispo)
colnames(dismissed_long_format_indispo_non_published)
colnames(dismissed_long_format_indispo_uneffective)
colnames(long_format_indispo)
colnames(long_format_indispo_non_published)
colnames(long_format_indispo_uneffective)

# Combine the two dataframes
all_indispo <- rbind(dismissed_long_format_indispo,
                     dismissed_long_format_indispo_non_published,
                     dismissed_long_format_indispo_uneffective)
all_indispo <- rbind(all_indispo, 
                     long_format_indispo, long_format_indispo_non_published, long_format_indispo_uneffective)



#estimate all the rf models only once

# Initialize an empty list to store models
models <- list()
colnames(prediction_data)

#enlever les .1


centrales <- colnames(generation_per_plant %>% 
                        select(-c('__index_level_0__')))# Replace with your actual plant names
indispo_centrales <- all_indispo %>% 
  distinct(production_resource_psr_name) %>% 
  pull(production_resource_psr_name)

active_centrales <- intersect(centrales, indispo_centrales)
#which(active_centrales == centrale)

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
    features <- c("gen_tot", "wind_solar_tot", "previous_day_prod", "day_of_week", "month", "creneau")
    
    #prepare training data
    # One-hot encoding (turn categorical variables into dummy variables)
    dummies <- model.matrix(~ day_of_week + month + creneau - 1, data = mini_prediction_data)
    
    # Combine encoded categorical variables with numerical features
    X <- as.matrix(cbind(mini_prediction_data[, c("gen_tot", "wind_solar_tot", "previous_day_prod")], dummies))
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


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs")
saveRDS(models, 'rf_models_xgb.rds')

models <- readRDS('rf_models_xgb.rds')


get_predictions <- function(centrale){
  
  #centrale = "BESOS 3"
  # Prepare mini_prediction_data
  
  
  #on predit pour toutes les dates où il y a une indisponibilité
  prediction_dates <- all_indispo %>% 
    filter(production_resource_psr_name == !!centrale) %>% 
    distinct() %>% 
    mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau, units = "hours")
           )

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
  features <- c("gen_tot", "wind_solar_tot", "previous_day_prod", "day_of_week", "month", "creneau")
  
  xgb_model <- models[[centrale]]
  
  
  test_data  <- merge(mini_prediction_data, 
                      prediction_dates[, !(names(prediction_dates) %in% c('creneau', 'day_of_week'))],
                      by.x = c('__index_level_0__'),
                      by.y = c('datetime_slot') )
  colnames(test_data)
  test_data <- test_data[, c('__index_level_0__', 'month',
                             'wind_solar_tot', 'gen_tot','day_of_week','creneau','previous_day_prod',
                              centrale)]
  
  
  
  # One-hot encoding for new data
  new_dummies <- model.matrix(~ day_of_week + month + creneau - 1, data = test_data)
  
  # Prepare the feature matrix
  new_X <- cbind(test_data[, c("gen_tot", "wind_solar_tot", "previous_day_prod")], new_dummies)
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


rf_predictions_data <- NULL
centrale <- centrales[5]
for (centrale in active_centrales){
  
  tryCatch({
    print(centrale)
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
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
write_parquet(rf_predictions_data, 'xgb_predictions_data_all.parquet')





dismissed_long_format_indispo <- dismissed_long_format_indispo%>% 
           mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))
dismissed_long_format_indispo_non_published <- dismissed_long_format_indispo_non_published%>% 
  mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))
dismissed_long_format_indispo_uneffective <- dismissed_long_format_indispo_uneffective%>%
  mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))
long_format_indispo <- long_format_indispo%>%
  mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))
long_format_indispo_non_published <- long_format_indispo_non_published%>%
  mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))
long_format_indispo_uneffective <- long_format_indispo_uneffective%>%
  mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau - 1 , units = "hours"))


colnames(dismissed_long_format_indispo_rf)

dismissed_long_format_indispo_rf <- merge(dismissed_long_format_indispo,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
       ) %>% 
  mutate(Available.Capacity = as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity)) %>%
  filter(
    #would have been an unavailability
    prediction_prod - Available.Capacity > 50
    &
      #but effectively dismissed
      centrale_prod - prediction_prod  > 50  
    
  ) %>%
  rename(datetime = datetime_slot)

dismissed_long_format_indispo_non_published_rf <- merge(dismissed_long_format_indispo_non_published,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
) %>% 
  mutate(Available.Capacity = as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity)) %>%
  filter(
    #would have been an unavailability
    prediction_prod - Available.Capacity > 50
    &
      #but effectively dismissed
      centrale_prod - prediction_prod  > 50  
    
  ) %>%
  rename(datetime = datetime_slot)

dismissed_long_format_indispo_uneffective_rf <- merge(dismissed_long_format_indispo_uneffective,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
) %>% 
  mutate(Available.Capacity = as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity)) %>%
  filter(
    #would have been an unavailability
    prediction_prod - Available.Capacity > 50
    &
      #but effectively dismissed
      centrale_prod - prediction_prod  > 50  
    
  ) %>%
  rename(datetime = datetime_slot)


long_format_indispo_rf <- merge(long_format_indispo,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
) %>% 
  filter(predicted_real_unavailability < -50) %>%
  rename(datetime = datetime_slot)

long_format_indispo_non_published_rf <- merge(long_format_indispo_non_published,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
) %>% 
  filter(predicted_real_unavailability < -50) %>%
  rename(datetime = datetime_slot)


long_format_indispo_uneffective_rf <- merge(long_format_indispo_uneffective,
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale', 'creneau'),
      by.x =  c('datetime_slot', 'production_resource_psr_name', 'creneau')
) %>% 
  filter(predicted_real_unavailability < -50) %>%
  rename(datetime = datetime_slot)


#on fait un petit plot pour voir quelle est la relation entre les indispos annoncées
# et les predicted



all_rf_indispo <- rbind(
  dismissed_long_format_indispo_rf %>% 
    select(-Available.Capacity),
  dismissed_long_format_indispo_non_published_rf %>% 
    select(-Available.Capacity),
  dismissed_long_format_indispo_uneffective_rf %>%
    select(-Available.Capacity),
  long_format_indispo_rf,
  long_format_indispo_non_published_rf,
  long_format_indispo_uneffective_rf
) %>% 
  mutate(supply_shock = ifelse(source == 'published' | source == 'non_published' | source == 'uneffective',
                               -predicted_real_unavailability,
                                as.numeric(Installed.Capacity) - as.numeric(Unavailable.Capacity) - prediction_prod ))

ggplot()+
  geom_point(data = all_rf_indispo  %>% 
               mutate(Unavailable.Capacity = as.numeric(Unavailable.Capacity)),
             aes(x = supply_shock, y = Unavailable.Capacity, color = source)) +
  theme_minimal()

getwd()
save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
write_parquet(all_rf_indispo, 'all_rf_indispo.parquet')


final_real_indispo_rf <- long_format_indispo_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()

final_not_publi_rf <- long_format_indispo_non_published_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()


final_uneffective_rf <- long_format_indispo_uneffective_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()


final_dismissed_rf <- dismissed_long_format_indispo_rf %>%
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Excess.Capacity = sum(prediction_prod - Available.Capacity),
            predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))
            ) %>%
  ungroup()

final_dismissed_not_publi_rf <- dismissed_long_format_indispo_non_published_rf  %>%
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Excess.Capacity = sum(prediction_prod - Available.Capacity),
            predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()


final_dismissed_uneffective_rf <- dismissed_long_format_indispo_uneffective_rf  %>%
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Excess.Capacity = sum(prediction_prod - Available.Capacity),
            predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability))) %>%
  ungroup()


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final'
setwd(save_path_entsoe_output)
getwd()
write_parquet(final_real_indispo_rf, 'final_real_indispo_rf.parquet')
write_parquet(final_not_publi_rf, 'final_not_publi_rf.parquet')
write_parquet(final_uneffective_rf, 'final_uneffective_rf.parquet')
write_parquet(final_dismissed_rf, 'final_dismissed_rf.parquet')
write_parquet(final_dismissed_not_publi_rf, 'final_dismissed_not_publi_rf.parquet')
write_parquet(final_dismissed_uneffective_rf, 'final_dismissed_uneffective_rf.parquet')






