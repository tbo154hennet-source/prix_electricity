
#un modele de prediction
#evaluer la production qui aurait été fournie
#dans un contre-factuel sans indispo

library(tidyr)
library(dplyr)
library(arrow)
library(lubridate)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp')
generation_by_fuel <-  read_parquet('generation_by_fuel.parquet')
colnames(generation_by_fuel)
fields <- c("('Solar', 'Actual Aggregated')",
  "('Wind Onshore', 'Actual Aggregated')",
  "('Wind Offshore', 'Actual Aggregated')",
  "('Hydro Run-of-river and poundage', 'Actual Aggregated')")

generation_by_fuel <- generation_by_fuel %>% 
  rowwise() %>% 
  mutate(gen_tot = sum(c_across(-`__index_level_0__`), na.rm = TRUE),
         gen_uncontrolled = sum(c_across(all_of(fields)), na.rm = TRUE)) %>% 
  select(`__index_level_0__`, gen_tot, gen_uncontrolled, all_of(fields))

#using a small random forest model already implemented
# Fit a random forest model

generation_by_fuel$`__index_level_0__` <- 
  with_tz(generation_by_fuel$`__index_level_0__`, tzone = "UTC")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/outputs'
setwd(save_path_esp)
write_parquet(generation_by_fuel, 'generation_by_fuel_format.parquet')

library(lubridate)
prediction_data <- merge(generation_by_fuel, generation_per_unit, 
                            by='__index_level_0__') %>% 
  mutate(month= month(`__index_level_0__`),
         day_of_week = weekdays(`__index_level_0__`),
         creneau = hour(`__index_level_0__`) +1)

centrales <- colnames(generation_per_unit %>% 
  select(-c('__index_level_0__')))# Replace with your actual plant names



get_predictions <- function(centrale){
  
  #centrale = "BESOS 3"
  # Prepare mini_prediction_data
  mini_prediction_data <- prediction_data[, c('__index_level_0__', 'month',
                                              'day_of_week', 'creneau',
                                              'gen_uncontrolled', 'gen_tot', centrale)] %>%
    mutate(previous_day_prod = lag(!!sym(centrale), 24))  # Use !!sym to refer to the variable dynamically
  
  mini_prediction_data <- na.omit(mini_prediction_data)
    
  
  
  # Convert some columns to factors
  mini_prediction_data$day_of_week <- as.factor(mini_prediction_data$day_of_week)
  mini_prediction_data$month <- as.factor(mini_prediction_data$month)
  mini_prediction_data$creneau <- as.factor(mini_prediction_data$creneau)
  
  # Fit a random forest model
  formula_str <- paste0("`", centrale, "` ~ gen_tot + gen_uncontrolled + previous_day_prod + day_of_week + month + creneau")
  rf_model_ranger <- ranger(
    formula = as.formula(formula_str),
    data = mini_prediction_data
  )
  
  #on predit pour toutes les dates où il y a une indisponibilité
  prediction_dates <- long_format_indispo_up %>% 
    filter(name == !!centrale) %>% 
    distinct()

  
  test_data  <- merge(mini_prediction_data, 
                      prediction_dates[, !(names(prediction_dates) %in% c('creneau', 'day_of_week'))],
                      by.x = c('__index_level_0__'),
                      by.y = c('datetime_slot') )
  
  test_data <- test_data[, c('__index_level_0__', 'month',
                             'gen_uncontrolled', 'gen_tot','day_of_week','creneau','previous_day_prod',
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

rf_predictions_data <- NULL
centrale <- centrales[1]
for (centrale in centrales){
  print(centrale)
  tryCatch({
    
    prediction_centrale <- get_predictions(centrale)%>% 
      mutate(centrale = !!centrale)
    colnames(prediction_centrale) <- c('__index_level_0__', 'month',
                                       'day_of_week','creneau','previous_day_prod',
                                       'gen_uncontrolled','gen_tot', 'centrale_prod', 
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


common_keys <- intersect(colnames(long_format_indispo_up),
          colnames(rf_predictions_data) )

colnames(long_format_indispo_up)
#test <- long_format_indispo_up %>% 
  #filter(electricity_delivery_date=='2021-09-25')
indispo_rf <- merge(long_format_indispo_up %>%  select(-common_keys),
      rf_predictions_data,
      by.y = c('__index_level_0__', 'centrale'),
      by.x =  c('datetime_slot', 'name')
)

real_indispo_rf <- indispo_rf %>% 
  filter(predicted_real_unavailability < -50) %>%
  rename(datetime = datetime_slot)


real_non_zero_previous_indispo <- real_non_zero_previous_indispo  %>% 
  filter(previous_day_prod > 0)%>%
  rename(datetime = datetime_slot)

setwd(save_path_esp)
write_parquet(real_indispo_rf, 'real_indispo_rf.parquet')
write_parquet(real_non_zero_previous_indispo, 'real_non_zero_previous_indispo.parquet')


final_real_indispo_rf <- real_indispo_rf %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau, datetime, gen_uncontrolled,
           gen_tot) %>% 
  summarise(predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability)),
  ) %>%
  ungroup()

final_non_zero_real_indispo_rf <- real_non_zero_previous_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau, datetime) %>% 
  summarise(non_zero_pred_real_unava = sum(as.numeric(non_zero_pred_real_unava)),
  ) %>%
  ungroup()

write_parquet(final_real_indispo_rf, 
              'final_real_indispo_rf.parquet')
write_parquet(final_non_zero_real_indispo_rf, 
              'final_non_zero_real_indispo_rf.parquet')

