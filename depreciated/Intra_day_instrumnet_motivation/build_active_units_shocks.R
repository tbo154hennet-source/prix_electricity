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

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant/esp')


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

# cas particulire n tilde
generation_per_unit <- generation_per_unit %>% 
  mutate(`ABOÑO 1`= ifelse(is.na(`ABOÑO 1`), `ABO¿O 1`, `ABOÑO 1`),
         `ABOÑO 2`= ifelse(is.na(`ABOÑO 2`), `ABO¿O 2`, `ABOÑO 2`)
  ) %>% 
  select(-`ABO¿O 1`, `ABO¿O 2`)

my_exact_names <- my_exact_names %>% 
  filter(output1!='ABO¿O 1' & output1!='ABO¿O 2')


# write_csv(my_exact_names, 'my_test.csv')


# probleme les power plant sont referencees par un code
# qui varie dans le indispo_dataset

# mais par leur nom complet dans le production data
# il faut trouver un moyen de les associer



# MANIERTE PLUS ARTISANALE
# en comparamt les codes excel et les noms deformes


corres <- read_csv('TABLE_CORRES.csv')

colnames(corres) <- c('name', 'prod_type', 'code')
corres <- corres %>% filter(!is.na(code))


# Replace '?' with spaces in the 'strings' column
corres$code <- gsub("\\?", " ", corres$code)
# Replace '??' with n tilde in the 'strings' column
corres$name <- gsub("\\?\\?", "Ñ", corres$name)

# j'ai besoin de long_format_indispo
# le format indispo heure par heure,
# ou je n'ai pas encore tout aggrege en sommant les indispos
# utiliser read_IPP_plant_suite

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/outputs'
setwd(save_path_esp)


long_format_indispo <- read_parquet('long_format_indispo.parquet')





long_format_indispo_up <- long_format_indispo %>% 
  # create date correponding to the beginning of the timeslot
  mutate(datetime_slot= as.POSIXct(
    paste(electricity_delivery_date, creneau-1),
    format="%Y-%m-%d %H", tz = 'UTC')) %>% 
  select(Fuel.Type, Installed.Capacity, Affected.Asset.or.Unit,
         datetime_slot, Unavailable.Capacity,creneau,
         first_affec_tim_period,last_affec_tim_period,
         auction_before, auction_after,
 Affected.Asset.or.Unit.EIC.code, electricity_delivery_date) %>% 
  arrange(Affected.Asset.or.Unit)
  
long_format_indispo_up <- long_format_indispo_up %>% 
  mutate(Affected.Asset.or.Unit= 
           ifelse(substr(Affected.Asset.or.Unit, 1, 2)=='CC', 
         substr(Affected.Asset.or.Unit,3, nchar(Affected.Asset.or.Unit)),
         Affected.Asset.or.Unit)) %>% 
  mutate(Affected.Asset.or.Unit= 
           ifelse(substr(Affected.Asset.or.Unit, 1, 1)=='C', 
                  substr(Affected.Asset.or.Unit,2, nchar(Affected.Asset.or.Unit)),
                  Affected.Asset.or.Unit))



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
  as.factor(weekdays(long_format_indispo_up$datetime_slot))


# unit <- "BESOS 3"
# datetime <- "2021-01-01 02:00:00"
# centrale <- 'BES'
# get_prod_centrale(unit, centrale, datetime, weekdays(datetime))
# get_prod(unit, datetime, 'Wednesday')

# true_indispo$day_of_week <- weekdays(true_indispo$datetime_slot)


get_prod <- function(unit, datetime, day_of_week ){
  datetime <- as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz= 'UTC')
  d_day_prod <- generation_per_unit %>% 
    select(unit, `__index_level_0__`) %>% 
    filter(`__index_level_0__`  == as.POSIXct(datetime))
  
  # find the most comparable day
  if (day_of_week=='Monday'){
    # Retrancher trois jour (24 heures)
    datetime_closest_day <- datetime - 3*24*60*60
  }else if (day_of_week=='Saturday'){
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

get_prod_centrale <- function(unit, centrale, datetime, day_of_week){
  
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

# # Apply the function to each element and return a dataframe
# my_exact_names  <- map_df(exact_names, ~ clean_string(.x))
# 
# my_data <- generation_per_unit %>%
#   select(unit, `__index_level_0__`)

my_exact_names$output_4 <- substr(my_exact_names$output_3, 1, 3)

long_format_indispo_up <- long_format_indispo_up %>%
  mutate(centrale = sapply(strsplit(name, " "), function(x) trimws(x[1])),
         centrale = substr(centrale, 1, 3))

#on peut faire mieux avec un mini modèle de prévision
#notamment pour tenir compte des renouvelables disponibles


# Remove rows with NA in datetime_slot
long_format_indispo_up <- long_format_indispo_up %>%
  filter(!is.na(datetime_slot))

setwd(save_path_esp)

write_parquet(long_format_indispo_up,'long_format_indispo_up.parquet')


my_true_indispo <- long_format_indispo_up %>%
# my_test <- test %>%
  rowwise() %>%
  mutate(prod_results =  list(get_prod_centrale(name,
             centrale, datetime_slot, day_of_week)),
         last_day_prod = prod_results[[1]],
         d_day_prod = prod_results[[2]],
         sum_last_centrale_prod = prod_results[[3]],
         sum_d_centrale_prod = prod_results[[4]]) %>%
  select(-prod_results) %>%
   ungroup()


# mutate(last_day_prod = get_prod_centrale(name, centrale, datetime_slot)[[1]],
#        d_day_prod = get_prod_centrale(name,centrale, datetime_slot)[[2]],
#        d_day_prod = get_prod_centrale(name,centrale, datetime_slot)[[2]],
#        sum_last_centrale_prod = get_prod_centrale(name,centrale, datetime_slot)[[3]],
#        sum_d_centrale_prod = get_prod_centrale(name,centrale, datetime_slot)[[4]]) %>%
# 



# write_parquet(my_true_indispo, 'true_indispo.parquet')


setwd(save_path_esp)
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
 

updated_non_duplicated_indispo <- my_true_indispo %>% 
  # seuil contestable pour enlever les variations parasites
  filter(real_unavailability > 50 ) %>% 
  distinct()

write_parquet(updated_non_duplicated_indispo,
              'updated_non_duplicated_indispo.parquet')

# colnames(updated_final_indispo)
updated_final_indispo <- updated_non_duplicated_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            theoretical_unavailability = sum(as.numeric(Unavailable.Capacity)),
            real_unavailability = sum(as.numeric(real_unavailability)),
            centrale_unavailability = sum(as.numeric(centrale_unavailability))) %>%
  ungroup()

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/outputs'
setwd(save_path_esp)


#spicifically nuclear

nuclear_indispo <- my_true_indispo %>%
  filter(Fuel.Type == 'Nuclear')%>% 
  # seuil contestable pour enlever les variations parasites
  filter(real_unavailability > 50 )

nuclear_final_indispo <- nuclear_indispo %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            theoretical_unavailability = sum(as.numeric(Unavailable.Capacity)),
            real_unavailability = sum(as.numeric(real_unavailability)),
            centrale_unavailability = sum(as.numeric(centrale_unavailability))) %>%
  ungroup()

write_parquet(nuclear_final_indispo,
              'nuclear_final_indispo.parquet')

write_parquet(updated_final_indispo,
              'updated_final_indispo.parquet')


