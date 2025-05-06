

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/formatted_OMIE")

unavailabilities_data <- read_parquet("unavailabilities_tot_OMIE.parquet")
dismissed_data <- read_parquet("unavailabilities_dismissed_tot_OMIE.parquet")

abbreviations <- unique(unavailabilities_data$psr_resource_name)



setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_by_plant/esp')
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
generation_per_plant <- parquet_files %>%
  lapply(read_parquet) %>% # Lire chaque fichier parquet
  bind_rows() %>%  # Fusionner tous les dataframes
  arrange(`__index_level_0__`)

# Extract the first value between single quotes
centrale_names <- gsub("^\\('([^']+)'.*", "\\1", colnames(generation_per_plant))



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/formatted_OMIE")
write.csv(centrale_names, "centrale_names.csv", row.names = FALSE)


table <- unavailabilities_data %>% 
  group_by(psr_resource_name) %>% 
  summarize(count = n()) %>% 
  filter(count >10) %>% 
  arrange(psr_resource_name)
write.csv(table, "abbreviations.csv", row.names = FALSE)
getwd()


save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE_unavailabilities/ES/formatted'
setwd(save_path_esp)
bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet")
colnames(bout_indispo_ENTSOE_data)

plant_info <- bout_indispo_ENTSOE_data %>% 
  ungroup() %>% 
  dplyr::select(production_resource_name, plant_type, Installed.Capacity) %>% 
  unique() %>% 
  arrange(production_resource_name)

getwd()
write.csv(plant_info,"plant_info.csv")



#je les ai matcher à la main
#stat sur les indispos pour lesquelles j'ai la taille associée
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/formatted_OMIE")
centrale_matching <- read.csv("centrale_matching_plant.csv")
colnames(centrale_matching) <- c("centrale_name","psr_resource_name",
                                 "plant_type",  "Installed.Capacity")

matching_table <- merge(table, centrale_matching,
                        by = "psr_resource_name", all.x = TRUE) %>% 
  filter(!is.na(centrale_name) & !is.na(Installed.Capacity))
#avoir les infos Installed capacity + plant type
# à retrouver dans bout_indispo
colnames(unavailabilities_data)

unavailabilities_data_matched <- merge(matching_table,
                                       unavailabilities_data,
                         by = "psr_resource_name")

dismissed_unavailabilities_data_matched <- merge(matching_table,
                                       dismissed_data,
                                       by = "psr_resource_name")

print(dim(unavailabilities_data_matched)[1]/ dim(unavailabilities_data)[1])
#seuleemnt 30% bien matchées

print(dim(dismissed_unavailabilities_data_matched)[1]/ dim(dismissed_data)[1])
#seuleemnt 30% bien matchées


unique(unavailabilities_data_matched %>% 
         select(psr_resource_name, centrale_name))

View(dismissed_unavailabilities_data_matched %>% 
       filter(Available.Capacity_Delta!=0)%>% 
       arrange(psr_resource_name, datetime_pub))
View(unavailabilities_data_matched %>% 
       filter(Available.Capacity_Delta!=0) %>% 
       arrange(psr_resource_name, datetime_pub))

#je remarque certaines petites incohérences  dans les dismissed
# qui sont positives et recoupent les unavail
# je décide de ne retenir que les négatives



unavailabilities_data_matched %>% 
  group_by(is.na(Available.Capacity_Delta)) %>% 
  summarise(count = n())

unavailabilities_data_matched <- unavailabilities_data_matched %>% 
  filter(Available.Capacity_Delta > 0 | is.na(Available.Capacity_Delta)) %>%
  mutate(Unavailable.Capacity = ifelse(is.na(Available.Capacity_Delta),
                                       Installed.Capacity-Available.Capacity, Available.Capacity_Delta ))

dismissed_unavailabilities_data_matched <- dismissed_unavailabilities_data_matched %>% 
  filter(Available.Capacity_Delta < 0 | is.na(Available.Capacity_Delta)) %>%
  mutate(Unavailable.Capacity = ifelse(is.na(Available.Capacity_Delta),
                                       -(Installed.Capacity-Available.Capacity), Available.Capacity_Delta ))

all_matched_indispo <- rbind(unavailabilities_data_matched %>% 
                               select(-Unavailable.Capacity_Reason),
                                       dismissed_unavailabilities_data_matched)

getwd()

dim(all_matched_indispo)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/formatted_OMIE")
write_parquet(unavailabilities_data_matched, "unavailabilities_data_matched.parquet")
write_parquet(dismissed_unavailabilities_data_matched, "dismissed_unavailabilities_data_matched.parquet")
write_parquet(all_matched_indispo, "all_matched_indispo.parquet")


