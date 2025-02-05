save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)

long_format_indispo_theoretical <- read_parquet('long_format_indispo_theoretical.parquet')
long_format_indispo_nuclear_theoretical <- read_parquet('long_format_nuclear_indispo_theoretical.parquet')

long_format_indispo <- read_parquet('long_format_indispo.parquet')
long_format_indispo_nuclear <- read_parquet('long_format_nuclear_indispo.parquet')


#attention comme j'ai donné start_date pour les theoretical
#j'ai un petit contingent où la date de publication précède le début de l'évènement
#attention même pour les indispos publiées, 
#il y a des trous dans la raquette
# créneaux déjà indispos mais pas encore publiés


library(dplyr)
long_format_indispo_non_published <- long_format_indispo_theoretical %>% 
  anti_join(long_format_indispo, by = c("production_resource_name", "datetime_pub", 
                                        "datetime_debut", "datetime_fin", "auction_before", "creneau"))

long_format_indispo_pre_published <- long_format_indispo%>%
  anti_join(long_format_indispo_theoretical ,
            by = c("production_resource_name", "datetime_pub", 
                   "datetime_debut", "datetime_fin", "auction_before", "creneau"))


nrow(long_format_indispo_theoretical) + nrow(long_format_indispo_pre_published)
nrow(long_format_indispo_non_published) + nrow(long_format_indispo) 


final_not_publi_ENTSOE_indispo <- long_format_indispo_non_published %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/final'
setwd(save_path_entsoe_output)

write_parquet(final_not_publi_ENTSOE_indispo, 'final_not_publi_ENTSOE_indispo.parquet')





long_format_nuclear_indispo_non_published <- long_format_indispo_nuclear_theoretical %>% 
  anti_join(long_format_indispo_nuclear, by = c("production_resource_name", "datetime_pub", 
    "datetime_debut", "datetime_fin", "auction_before", "creneau"))

long_format_nuclear_indispo_pre_published <- long_format_indispo_nuclear%>%
  anti_join(long_format_indispo_nuclear_theoretical ,
          by = c("production_resource_name", "datetime_pub", 
                     "datetime_debut", "datetime_fin", "auction_before", "creneau"))


nrow(long_format_indispo_nuclear_theoretical) + nrow(long_format_nuclear_indispo_pre_published)
nrow(long_format_nuclear_indispo_non_published) + nrow(long_format_indispo_nuclear) 


final_not_publi_ENTSOE_nuclear_indispo <- long_format_nuclear_indispo_non_published %>% 
  group_by(electricity_delivery_date, auction_before,
           auction_after, creneau) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()


write_parquet(final_not_publi_ENTSOE_nuclear_indispo, 'final_not_publi_ENTSOE_nuclear_indispo.parquet')


save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs/long_format'
setwd(save_path_entsoe_output)
write_parquet(long_format_nuclear_indispo_non_published, "long_format_nuclear_indispo_non_published.parquet")
write_parquet(long_format_indispo_non_published, "long_format_indispo_non_published.parquet")




