
#indispo limitante quand le reste non-nul 
#est égal ou presque au reste disponible


#créer un fichier où l'indispo soit matcher avec sa production à l'heure dite
library(ranger)
centrales <- colnames(generation_per_plant %>% 
                        select(-c('__index_level_0__')))# Replace with your actual plant names
long_indispo_with_production <- NULL
centrale <- centrales[1]
for (centrale in centrales){
  tryCatch({
    #on predit pour toutes les dates où il y a une indisponibilité
    prediction_dates <- long_format_indispo %>% 
      filter(production_resource_psr_name == !!centrale) %>% 
      distinct() %>% 
      mutate(datetime_slot = as.POSIXct(electricity_delivery_date) + as.difftime(creneau, units = "hours")
      )
    generation_dates <- merge(prediction_dates, generation_per_plant,
          by.x = "datetime_slot", by.y = "__index_level_0__") %>% 
      select(c(centrale, colnames(prediction_dates))) %>% 
      rename("production" = centrale)
    
    if (is.null(long_indispo_with_production)){
      long_indispo_with_production <- generation_dates 
    }else{
      long_indispo_with_production <- rbind(long_indispo_with_production, generation_dates)
    } 
  }, error = function(e) {
    print(paste("Error for", centrale))
  })
  
}




