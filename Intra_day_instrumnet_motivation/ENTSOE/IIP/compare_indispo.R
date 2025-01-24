#compare the unavailabilities
#according to the IIP source 
#and those from the ENTSOE final unavailabilities file


#merge the two dataframes
#and compare the unavailabilities
#according to the IIP source
#and those from the ENTSOE final unavailabilities file


#if I merge post 2021, no very comparable
#because I don't have the same correction for dismissed messsages before






#compare pure indispo data
#with unexpected_indispo_data

unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data %>% 
  arrange(datetime_debut )
colnames(unexpected_indispo_ENTSOE_data)

class(unexpected_indispo_ENTSOE_data$datetime_pub)
start_date <- as.POSIXct("2021-01-01", format = "%Y-%m-%d", tz = "UTC")





study_unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data[,
                                                                       c("datetime_pub",                
                                                                         "Unavailable.Capacity",        
                                                                         "plant_type",                  
                                                                         "production_resource_id",      
                                                                         "production_resource_name",
                                                                         "datetime_debut",
                                                                         "datetime_fin"  ) ] %>% 
  filter(datetime_pub >= start_date )




study_nuclear_indispo_ENTSOE_data <- study_unexpected_indispo_ENTSOE_data %>% 
  filter(plant_type=="Nuclear")

