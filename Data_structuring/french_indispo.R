
library(arrow)
Sys.setenv(lang = "en_US")
read_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE'
setwd(read_path )

indispo_ENTSOE_data <- NULL
my_files <- list.files()
# Obtenez la liste des fichiers .parquet
#my_files <- my_files[grep("\\.csv$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers
# my_files <- setdiff(my_files, parquet_files)
for (file in my_files){
  excel_file <- read_parquet(file)
  print(file)
  if (is.null(indispo_ENTSOE_data)){
    indispo_ENTSOE_data <- excel_file
  }
  else{
    indispo_ENTSOE_data <- rbind(indispo_ENTSOE_data, excel_file)
  }
}  

indispo_ENTSOE_data <-unique(indispo_ENTSOE_data)

indispo_ENTSOE_data$`__index_level_0__` <- 
  with_tz(indispo_ENTSOE_data$`__index_level_0__`, tzone = "UTC")
indispo_ENTSOE_data$start <- 
  with_tz(indispo_ENTSOE_data$start, tzone = "UTC")
indispo_ENTSOE_data$end <- 
  with_tz(indispo_ENTSOE_data$end, tzone = "UTC")


my_columns <- c("avail_qty","businesstype","end" ,  "nominal_power",                       
                "mrid","nominal_power" ,"plant_type","production_resource_id",      
                "production_resource_name", "production_resource_psr_name",
                "revision", "start","__index_level_0__")

colnames(indispo_ENTSOE_data%>%
           select(my_columns) )

indispo_ENTSOE_data <- indispo_ENTSOE_data %>%
  select(my_columns) %>% 
  mutate(Unavailable.Capacity = nominal_power - as.numeric(avail_qty)) %>% 
  rename(datetime_pub = `__index_level_0__`,
    # create debut date
    datetime_debut = start,
    # create end date
    datetime_fin = end,
    Installed.Capacity = nominal_power)

#on vérfie qu'il n'y a qu'une seule ligne par indispo
#que les révisions ont déjà été traitées

#revision est nul et ne sert pas à grand chose

colnames(indispo_ENTSOE_data)

distinct_values <- indispo_ENTSOE_data %>%  distinct(businesstype)
#planned maintenance vs unplanned outage
#for long term, both could be equally interesting


# test2 <- indispo_ENTSOE_data %>% 
#   filter(production_resource_id == "17W100P100P0337A")
# 
# 
# sample <- test2[285:295,]
# View(sample[, c("mrid","datetime_debut", "datetime_fin", "datetime_pub")])

chevauchement <- indispo_ENTSOE_data %>% 
  group_by(mrid) %>% 
  arrange(datetime_debut, datetime_fin) %>%  # Trier par datetime_debut, puis datetime_fin
  summarise(
    # Stocker les valeurs triées de datetime_debut et datetime_fin dans des listes
    debuts = list(datetime_debut),
    fins = list(datetime_fin),
    # Nombre de dates dans le groupe
    n_dates = n(),
    #conditions = list(map(1:(n_dates-1), ~ print(fins[[1]][.x]))),  # Comparaisons successives
    conditions = list(map2(1:(n_dates-1), 2:n_dates,
                           ~ fins[[1]][.x] <= debuts[[1]][.y])),
  # Vérifier si toutes les conditions sont TRUE
  all_conditions_met = all(unlist(conditions)))  %>%
  ungroup() 


chevauchement <- chevauchement %>% 
  mutate(check = (all_conditions_met == TRUE | n_dates==1))
chevauchement %>% filter(check ==FALSE)
#tableau vide 
#assurance de non-chevauchement pour les différentes périodes



bout_indispo_ENTSOE_data <- indispo_ENTSOE_data %>% 
  group_by(businesstype, mrid, Installed.Capacity, datetime_pub,
           Unavailable.Capacity, plant_type, production_resource_id,      
          production_resource_name, production_resource_psr_name) %>% 
  summarise(datetime_debut = min(datetime_debut),
            datetime_fin = max(datetime_fin))


#test4 <- bout_indispo_ENTSOE_data %>% filter(mrid =="uOKJuRKj0o1N8nLBuxbBvw")
save_path_fr <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_path_fr)

write_parquet(bout_indispo_ENTSOE_data, "indispo_ENTSOE_data.parquet")





