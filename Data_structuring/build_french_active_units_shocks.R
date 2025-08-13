# we try to take into account that for most fossil fuel plants
# they probably would not have been used to their full capacity
# during the unavailability period

# first, build the production per unit total data

library(purrr)
library(dplyr)
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(tidyr)
library(lubridate)

Sys.setenv(lang = "en_US")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant/fr')


generation_per_unit <- NULL
my_files <- list.files()
# Obtenez la liste des fichiers .parquet
parquet_files <- my_files[grep("\\.parquet$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers
# my_files <- setdiff(my_files, parquet_files) 
for (file in parquet_files){
  # my_file <- read_parquet(file)
  if (is.null(generation_per_unit)){
    generation_per_unit <- read_parquet(file)
  }
  else{
    excel_file <- read_parquet(file)
    generation_per_unit <- bind_rows(generation_per_unit, excel_file)
  }
}



exact_names <- colnames(generation_per_unit)

clean_string <- function(input_string){
  
  # Supprimer les parenthèses
  cleaned_string <- gsub("\\(|\\)", "", input_string)
  # Enlever les guillemets simples
  cleaned_string <- gsub("'", "", cleaned_string)
  # Séparer la chaîne sur la virgule
  split_string <- strsplit(cleaned_string, ",")[[1]]
  
  # Prendre le premier morceau
  first_piece <- trimws(split_string[1])
  # Prendre le premier morceau
  second_piece <- trimws(split_string[2])
  # Afficher le résultat
  
  split_string_centrale <- strsplit(first_piece, " ")[[1]]
  # Prendre le premier morceau
  third_piece <- trimws(split_string_centrale[1])
  
  return(list(output1 = first_piece,
              output2 = second_piece, output_3 = third_piece))
}

# Apply the function to each element and return a dataframe
my_exact_names  <- map_df(exact_names, ~ clean_string(.x))
colnames(generation_per_unit) <- my_exact_names$output1
generation_per_unit$`__index_level_0__` <- 
  with_tz(generation_per_unit$`__index_level_0__`, tzone = "UTC")

View(data.frame(generation_per_unit$`__index_level_0__`))
min(generation_per_unit$`__index_level_0__`, tzone = "UTC")
max(generation_per_unit$`__index_level_0__`, tzone = "UTC", na.rm=TRUE)
save_path_fr <- "Z:/prix_electricite_gitlab/Data_structuring/output"
setwd(save_path_fr)

write_parquet(generation_per_unit, 'generation_per_unit_format.parquet')


generation_per_unit <- read_parquet('generation_per_unit_format.parquet')

print(class(my_exact_names))
print(colnames(my_exact_names))
View(my_exact_names %>% arrange(output1) %>% distinct())

# probleme les power plant sont referencees par un code
# qui varie dans le indispo_dataset

# mais par leur nom complet dans le production data
# il faut trouver un moyen de les associer



# j'ai besoin de long_format_indispo
# le format indispo heure par heure,
# ou je n'ai pas encore tout aggrege en sommant les indispos
# utiliser read_IPP_plant_suite
save_path_fr <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_path_fr)
bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet")

# on va chercher la prod pour cette unite
# a l'heure dite pour le journee d'indispo
# et pour le jour d'avant a la meme heure

# looking for the closest comparable day in the past
# if I control by wind and solar generation I could use a local average
# for instance the last four tuesdays

bout_indispo_ENTSOE_data$day_of_week <- weekdays(bout_indispo_ENTSOE_data$datetime_debut)

#gérer le cas AMFARD14 accoler
#gérer le cas DK6-TG1
my_exact_names$output_4 <- sub("-.*$", "", gsub("\\d+$", "", my_exact_names$output_3))



get_prod <- function(unit, datetime, day_of_week ){
  datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz= 'UTC')
  d_day_prod <- generation_per_unit %>% 
    select(unit, `__index_level_0__`) %>% 
    filter(`__index_level_0__`  == as.POSIXct(datetime))
  
  # find the most comparable day
  if (day_of_week=='lundi'){
    # Retrancher trois jour (24 heures)
    datetime_closest_day <- datetime - 3*24*60*60
  }else if (day_of_week=='samedi'){
    datetime_closest_day <- datetime - 6*24*60*60
  }else{
    datetime_closest_day <- datetime - 24*60*60
  }
  last_day_prod <- generation_per_unit %>% 
    select(unit, `__index_level_0__`) %>% 
    filter(`__index_level_0__`  == as.POSIXct(datetime_closest_day))
  # if no day before
  # return day after
  if (nrow(last_day_prod) == 0){
    last_day_prod <- NA
  }
  
  return(list(last_day_prod = last_day_prod[[1]],
              d_day_prod  = d_day_prod[[1]]))
}

View(generation_per_unit[, c("AIGLE 6", "__index_level_0__")])

my_true_indispo <- bout_indispo_ENTSOE_data %>%
  # my_test <- test %>%
  rowwise() %>%
  mutate(prod_results =  list(get_prod(production_resource_psr_name, 
                                       datetime_debut, day_of_week)),
         last_day_prod = prod_results[[1]],
         d_day_prod = prod_results[[2]]
         # ,sum_last_centrale_prod = prod_results[[3]],
         # sum_d_centrale_prod = prod_results[[4]]
         ) %>%
  select(-prod_results) %>%
  ungroup()

get_prod_centrale <- function(unit, centrale, datetime, day_of_week){
  print(unit)
  unit_names <- my_exact_names %>%
    filter(output_4==centrale) %>%
    select(output1)
  sum_last_centrale_prod <- 0
  sum_d_centrale_prod <- 0
  # Transforming the tibble column into a list
  unit_names <- unit_names %>% pull(output1) %>% as.list()
  for (unit_bis in unit_names){
    prod_results_unit = get_prod(unit_bis, datetime, day_of_week)
    if (unit_bis==unit){
      last_day_prod = prod_results_unit[[1]]
      d_day_prod = prod_results_unit[[2]]
    }
    sum_last_centrale_prod = sum_last_centrale_prod + prod_results_unit[[1]]
    sum_d_centrale_prod = sum_d_centrale_prod + prod_results_unit[[2]]
  }
  
  return(list(last_day_prod = last_day_prod[[1]],
              d_day_prod  = d_day_prod[[1]],
              sum_last_centrale_prod = sum_last_centrale_prod,
              sum_d_centrale_prod = sum_d_centrale_prod))
  
}

my_true_indispo2 <- bout_indispo_ENTSOE_data %>%
  # my_test <- test %>%
  rowwise() %>%
  mutate(prod_results =  list(get_prod_centrale(
    production_resource_psr_name,production_resource_name,
     datetime_debut, day_of_week)),
         last_day_prod = prod_results[[1]],
         d_day_prod = prod_results[[2]],
         sum_last_centrale_prod = prod_results[[3]],
         sum_d_centrale_prod = prod_results[[4]]) %>%
  select(-prod_results) %>%
  ungroup()



unit <- "REVIN 3"
datetime <- "2021-01-02 18:00:00"
centrale <- 'REVIN'
get_prod(unit, datetime, weekdays(as.POSIXct(datetime)))
prod_results <- get_prod_centrale(unit, centrale, datetime, weekdays(as.POSIXct(datetime)) )


prod_results_unit = get_prod(unit_bis, datetime, day_of_week)

# test <- non_duplicated_indispo_up %>% 
#   arrange(Affected.Asset.or.Unit)%>% 
#   distinct(as.numeric(Installed.Capacity)             
#                            ,Fuel.Type                      
#                            ,Affected.Asset.or.Unit
#                            , .keep_all = TRUE)
# View(test)

long_format_indispo_up <- merge(long_format_indispo_up, corres,
         by.x = c("Fuel.Type","Affected.Asset.or.Unit"),
           by.y= c("prod_type", "code" )) %>% 
      arrange(datetime_slot)

# on va chercher la prod pour cette unite
# a l'heure dite pour le journee d'indispo
# et pour le jour d'avant a la meme heure

# choix contestable


# looking for the closest comparable day in the past
# if I control by wind and solar generation I could use a local average
# for instance the last four tuesdays

long_format_indispo_up$day_of_week <- 
  weekdays(long_format_indispo_up$datetime_slot)


# unit <- "BESOS 3"
# datetime <- "2021-01-01 02:00:00"
# centrale <- 'BES'
# get_prod_centrale(unit, centrale, datetime, weekdays(datetime))
# get_prod(unit, datetime, 'Wednesday')



setwd('C:/Users/nchu/Desktop/DATA/ENTSOE_python_data/esp')


# write_parquet(my_true_indispo, 'true_indispo.parquet')



my_true_indispo <- read_parquet('true_indispo.parquet')


# consumption patterns at the weekly frequency
# exhibits some periodicity
# we have to control for whether it was a weekday or the weekend


# we want to compare with the most comparable
# caution the unit could be unavailable for several days following

weekday_diff <- function(last_day_prod, d_day_prod, 
   Unavailable.Capacity){
  Unavailable.Capacity <- as.numeric(Unavailable.Capacity)
  diff_power = last_day_prod - d_day_prod
  
  # positif
  real_unavailability = ifelse(
    diff_power> 0,
    diff_power, 0)
  # peut pas etre superieur a l'indispo
  real_unavailability = ifelse(
    diff_power> Unavailable.Capacity,
    Unavailable.Capacity, diff_power)
  return(real_unavailability)
}
# Remove rows with NA in datetime_slot
my_true_indispo <- my_true_indispo %>%
  filter(!is.na(last_day_prod))

my_true_indispo <- my_true_indispo %>% 
# my_test <- my_test %>% 
  rowwise() %>% 
  mutate(real_unavailability =  weekday_diff(
    last_day_prod, d_day_prod,  Unavailable.Capacity), 
    # sur les centrales a plusieurs reacteurs
    # regarder si ce n'est pas compense par les autres
     centrale_unavailability = weekday_diff(
       sum_last_centrale_prod, sum_d_centrale_prod,
       Unavailable.Capacity)
  )
 

updated_final_indispo <- my_true_indispo %>% 
  # seuil contestable pour enlever les variations parasites
  filter(real_unavailability > 50 )

# setwd('C:/Users/nchu/Desktop/DATA/ENTSOE_python_data/esp')
# 
# write_parquet(updated_final_indispo,
#               'short_indispo_2021_2022.parquet')
# 
# nuclear_indispo <- updated_final_indispo %>% 
#   filter(Fuel.Type == 'Nuclear')
# write_parquet(nuclear_indispo,
#               'nuclear_indispo.parquet')
# colnames(updated_final_indispo)
updated_final_indispo <- updated_final_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            theoretical_unavailability = sum(as.numeric(Unavailable.Capacity)),
            real_unavailability = sum(as.numeric(real_unavailability)),
            centrale_unavailability = sum(as.numeric(centrale_unavailability))) %>%
  ungroup()

setwd('C:/Users/nchu/Desktop/DATA/ENTSOE_python_data/esp')

write_parquet(updated_final_indispo,
              'updated_final_indispo.parquet')

# 
# 
# tentative de comparaison alternative
# benchmark moyenne pour un niveau de demande comparable


tot_generation_plant <- generation_per_unit %>% 
  select(-c(`__index_level_0__`))
demand <- rowSums(tot_generation_plant, na.rm = TRUE) 



# Ajoutez un identifiant d'observation pour plus de simplicité
demand_driven_data <- demand_driven_data %>%
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


my_long_format_indispo_up <- merge(long_format_indispo_up,demand_data ,
                                   by.y= '__index_level_0__',
                                   by.x = 'datetime_slot') 

#long_format_indispo_up %>% 
  #mutate(= get_prod_centrale(name,centrale, datetime_slot, day_of_week))



