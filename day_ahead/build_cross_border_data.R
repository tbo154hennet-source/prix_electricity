
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


