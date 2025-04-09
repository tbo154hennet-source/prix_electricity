
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/actual_flows")
list.files()
# List all files in the directory that start with "ES_FR"
files_ex <- list.files( pattern = "^ES_FR", full.names = TRUE)
files_imp <- list.files( pattern = "^FR_ES", full.names = TRUE)

ex_data <- NULL
#file <- files_ex[1]
for (file in files_ex){
  print(file)
  
  parquet_file <- read_parquet(file)
  colnames(parquet_file) <- c("time", "CBF")

  if(is.null(ex_data)){
    ex_data <- parquet_file
  }else{
    ex_data  <- rbind(ex_data  ,parquet_file)
  }
}

imp_data <- NULL
#file <- files_ex[1]
for (file in files_imp){
  print(file)
  
  parquet_file <- read_parquet(file)
  colnames(parquet_file) <- c("time", "CBF")
  
  if(is.null(imp_data )){
    imp_data  <- parquet_file
  }else{
    imp_data   <- rbind(imp_data  ,parquet_file)
  }
} 

colnames(imp_data) <- c("time", "CBF_FR_ESP")
colnames(ex_data) <- c("time", "CBF_ESP_FR")

cross_border_flows <- merge(ex_data, imp_data, by = "time")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/NTC")

files_ex <- list.files( pattern = "^ES_FR", full.names = TRUE)
files_imp <- list.files( pattern = "^FR_ES", full.names = TRUE)

ex_data <- NULL
#file <- files_ex[1]
for (file in files_ex){
  print(file)
  
  parquet_file <- read_parquet(file)
  colnames(parquet_file) <- c("time", "NTC")
  
  if(is.null(ex_data)){
    ex_data <- parquet_file
  }else{
    ex_data  <- rbind(ex_data  ,parquet_file)
  }
}

imp_data <- NULL
#file <- files_ex[1]
for (file in files_imp){
  print(file)
  
  parquet_file <- read_parquet(file)
  colnames(parquet_file) <- c("time", "NTC")
  
  if(is.null(imp_data )){
    imp_data  <- parquet_file
  }else{
    imp_data   <- rbind(imp_data  ,parquet_file)
  }
} 

colnames(imp_data) <- c("time", "NTC_FR_ESP")
colnames(ex_data) <- c("time", "NTC_ESP_FR")

NTC <- merge(ex_data, imp_data, by = "time")
cross_border_flows <- merge(cross_border_flows, NTC, by = "time")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/scheduled_exchanges")

SCH_EX <- NULL
#file <- files_ex[1]
for (file in list.files()){
  print(file)
  
  excel_file <- read_excel(file, skip =6)
  colnames(excel_file) <- c("time", "SCH_EX_DA_FR_ES", "SCH_EX_TOT_FR_ES", "SCH_EX_DA_PT_ES", "SCH_EX_TOT_PT_ES", 
                            "SCH_EX_DA_ES_FR", "SCH_EX_TOT_ES_FR", "SCH_EX_DA_ES_PT", "SCH_EX_TOT_ES_PT")
  
  if(is.null(SCH_EX )){
    SCH_EX  <- excel_file
  }else{
    SCH_EX   <- rbind(SCH_EX   ,excel_file)
  }
}

final_SCH_EX  <- SCH_EX %>% 
  select(-c("SCH_EX_DA_PT_ES", "SCH_EX_TOT_PT_ES","SCH_EX_DA_ES_PT", "SCH_EX_TOT_ES_PT"))%>% 
  mutate(time = as.POSIXct(sub(" -.*", "", time), 
                           format = "%d/%m/%Y %H:%M:%S", tz= "Europe/Brussels"))


cross_border_flows <- merge(cross_border_flows, 
              final_SCH_EX,
             by = "time")
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows")

write_parquet(cross_border_flows, "fr_esp_cross_border_flows.parquet")






# pour l'estimation en niveau,
#on rajoute le spot gas price

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp.parquet")


save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
price_indispo_merge_fr <- read_parquet("price_indispo_merge_fr.parquet")


nrow(price_indispo_merge_fr)
nrow(price_indispo_merge_esp)

colnames(price_indispo_merge_fr)
colnames(price_indispo_merge_esp)

price_merge_CBF <- merge(price_indispo_merge_fr  %>% 
                           rename_with(~ paste0(., "_fr"), -c(datetime, year, month, day, delivery_date)),
                         price_indispo_merge_esp  %>% 
                           rename_with(~ paste0(., "_esp"), -c(datetime, year, month, day, delivery_date)),
                         by= c("datetime", "year", "month", "day", "delivery_date")
)

#exception ibérique
#Le 15 juin 2022, l’exception ibérique est entrée en vigueur, pour une durée initiale de 12 mois
#plafonne le prix du gaz utilisé dans la production d'électricité
#d'abord 55 euros
#puis extension un an plus tard jusqu'en décembre 2023 à 65 euros

# Si le dispositif a effectivement servi à amortir l'impact de la flambée des cours du gaz en 2022
# sur la note des consommateurs, il n'a de fait plus été activé depuis février 2023,
# lorsque les prix du marché gazier avaient retrouvé une certaine normalité.

class(price_merge_CBF$datetime)
exception_start_date <- as.POSIXct("15-06-2022 00:00:00", 
                                   format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")
exception_end_date <- as.POSIXct("01-03-2023 00:00:00", 
                                 format = "%d-%m-%Y %H:%M:%S", tz= "Europe/Brussels")
#exclure une valeur aberrante qui rend le graphique moins lisible

plot_data <- price_merge_CBF %>% 
  filter(price_fr < 1500)
library(ggplot2)
ggplot(plot_data)+
  geom_line(aes(x=datetime, y=price_fr, color = "France")) +
  geom_line(aes(x=datetime, y=price_esp, color = "Espagne")) +
  geom_vline(xintercept = exception_start_date, linetype = "dashed", color = "red") +
  geom_vline(xintercept = exception_end_date, linetype = "dashed", color = "red") +
  labs(title = "Day-ahead electricity prices in France and Spain",
       x = "Date", y = "Price (€/MWh)") +
  theme_minimal()


#regarder quelles sont les dates perdues dans le merge
#données 2024


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows")

cross_border_flows <- read_parquet("fr_esp_cross_border_flows.parquet")
colnames(cross_border_flows)


price_merge_CBF <- merge(price_merge_CBF, cross_border_flows, 
                         by.x ="datetime", by.y ="time")
getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

write_parquet(price_merge_CBF, "price_merge_CBF.parquet")



