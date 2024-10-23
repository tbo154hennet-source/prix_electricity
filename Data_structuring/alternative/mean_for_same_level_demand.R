
# tentative de comparaison alternative
# benchmark moyenne pour un niveau de demande comparable


tot_generation_plant <- generation_per_unit %>% 
  select(-c(`__index_level_0__`))
demand <- rowSums(tot_generation_plant, na.rm = TRUE) 


# Ajoutez un identifiant d'observation pour plus de simplicité
demand_driven_data <- cbind(demand, generation_per_unit) %>%
  mutate(Observation = row_number())

# Fonction pour calculer la moyenne des niveaux de génération pour une observation donnée
get_mean_for_interval <- function(row_index, data, demand_interval = 30) {
  demand_value <- data$demand[row_index]
  
  # Filtrer les observations dont la demande est dans l'intervalle [demande - 30, demande + 30]
  filtered_data <- data %>%
    filter(demand >= (demand_value - demand_interval) & demand <= (demand_value + demand_interval))
  
  # Calculer la moyenne des colonnes de génération pour l'intervalle
  generation_means <- filtered_data %>%
    select(-demand, -Observation) %>%  # Exclure la colonne 'demand' et 'Observation'
    summarise(across(everything(), mean, na.rm = TRUE))  # Moyenne pour chaque centrale
  
  return(generation_means)
}

# Appliquez cette fonction à chaque observation
results <- map_dfr(1:nrow(demand_driven_data), ~get_mean_for_interval(.x, demand_driven_data))

#objective get the average of usual plant production
#for comparable levels of demand

common_columns <- names(demand_driven_data)
print(class(common_columns))
deviation_production <- demand_driven_data[!common_columns %in%
                                             c("Observation", "demand"
                                             )] - results

#colnames(results)[!colnames(results)  %in% colnames(demand_driven_data)]
#colnames(demand_driven_data)[!colnames(demand_driven_data)  %in% colnames(results)]


my_long_format_indispo_up <- merge(long_format_indispo_up, demand_data ,
                                   by.y= '__index_level_0__',
                                   by.x = 'datetime_slot')