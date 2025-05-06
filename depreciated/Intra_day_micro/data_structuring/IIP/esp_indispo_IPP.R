
library(arrow)
library(purrr)
library(lubridate)
library(tidyverse)
library(tidyr)
Sys.setenv(lang = "en_US")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/IIP_indispo/raw_data'
setwd(save_path_esp)
my_files <-  list.files(pattern = "\\.csv$")

indispo_IIP_data <- NULL

for (file in my_files){
  excel_file <- read.csv(file)
  print(file)
  if (is.null(indispo_IIP_data)){
    indispo_IIP_data <- excel_file
  }
  else{
    indispo_IIP_data <- rbind(indispo_IIP_data, excel_file)
  }
}  

colnames(indispo_IIP_data) <- c("mrid", "docstatus","businesstype", 'type_event',
                                "__index_level_0__", "start" , "end", "qty_uom",
                              "unavail_qty",   "avail_qty" , "nominal_power" ,
                              "reason", "remark", "plant_type", "biddingzone_domain",
                              "production_resource_name" , "production_resource_id",
                              "company", "other1", "other2", "other3"
                                )

indispo_IIP_data %>% 
  group_by(docstatus, businesstype) %>% 
  summarise(n = n())

# Obtenez la liste des fichiers .parquet
#my_files <- my_files[grep("\\.csv$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers
# my_files <- setdiff(my_files, parquet_files)

#garder que les primary files


indispo_IIP_data$`__index_level_0__` <- 
  with_tz(indispo_IIP_data$`__index_level_0__`, tzone = 'Europe/Brussels')
indispo_IIP_data$start <- 
  with_tz(indispo_IIP_data$start, tzone = 'Europe/Brussels')
indispo_IIP_data$end <- 
  with_tz(indispo_IIP_data$end, tzone = 'Europe/Brussels')


my_columns <- c("avail_qty","biddingzone_domain","docstatus",  "businesstype","end" ,  "nominal_power",                       
                "mrid","nominal_power" ,"plant_type","production_resource_id",      
                "production_resource_name", "start","__index_level_0__")

colnames(indispo_IIP_data%>%
           select(my_columns) )

indispo_IIP_data <- indispo_IIP_data %>%
  select(my_columns) %>% 
  mutate(Unavailable.Capacity = nominal_power - as.numeric(avail_qty)) %>% 
  rename(datetime_pub = `__index_level_0__`,
    # create debut date
    datetime_debut = start,
    # create end date
    datetime_fin = end,
    Installed.Capacity = nominal_power)

indispo_IIP_data <-unique(indispo_IIP_data)


#attnetion l'identifiant du message n'est pas l'identifiant de l'indispo


#planned maintenance vs unplanned outage
#for long term, both could be equally interesting

indispo_IIP_data %>% 
  group_by(mrid) %>% 
  mutate(n_mrid = n()) %>% 
  group_by(n_mrid) %>%
  summarise(n = n())




bout_indispo_IIP_data <- indispo_IIP_data %>% 
  group_by(businesstype, docstatus, Installed.Capacity, datetime_pub, 
           Unavailable.Capacity, plant_type, production_resource_id,      
          production_resource_name, biddingzone_domain) %>% 
  summarise(datetime_debut = min(datetime_debut),
            datetime_fin = max(datetime_fin)) %>% 
  filter(Unavailable.Capacity > 100) %>% 
  arrange(datetime_pub)

unique(bout_indispo_IIP_data$docstatus)

bout_indispo_IIP_data %>% 
  group_by(docstatus) %>%
  summarise(n = n())




bout_indispo_IIP_data_bis <- bout_indispo_IIP_data %>% 
  filter(docstatus != "Inactive") %>% 
  group_by(Installed.Capacity, datetime_debut, datetime_fin, docstatus, 
           Unavailable.Capacity, plant_type, production_resource_id,      
           production_resource_name, biddingzone_domain) %>% 
  summarise(datetime_pub = min(datetime_pub),
            businesstype = if_else(any(businesstype == "Planned"), "Planned", "Unplanned"),
            docstatus = if_else(any(docstatus == "Dismissed"), "Dismissed", "Active"),
            .groups = "drop") %>% 
  ungroup() %>% 
  unique() %>% 
  arrange(datetime_pub)

bout_indispo_IIP_data_ter <-bout_indispo_IIP_data_bis %>% 
  mutate(publication_lag = as.numeric(datetime_pub - datetime_debut, units = "hours")) %>% 
  filter(publication_lag >=  -24 & publication_lag <= 24)


#press the button twice
#publication date in too close vicinity
#slight message adjustment

bout_indispo_IIP_data_quat <- bout_indispo_IIP_data_ter %>%
  group_by( datetime_debut, plant_type,     
           production_resource_name, biddingzone_domain) %>%
  arrange(datetime_pub) %>%  # Ensure chronological order within groups
  mutate(pub_diff = as.numeric(difftime(lead(datetime_pub), datetime_pub,  units = "hours")),
         n = n())  %>%
  filter(pub_diff > 1 | is.na(pub_diff)) %>%
  ungroup()



#si c'est une question de puissance
#je prends juste la plus grosse puissance proposée

bout_indispo_IIP_data_quint <- bout_indispo_IIP_data_quat %>%
  group_by( datetime_debut, datetime_fin, plant_type,     
            production_resource_name) %>%
  arrange(Unavailable.Capacity) %>% 
  slice_head(n = 1) %>%
  ungroup()


#j'enlève le cas où la deuxième publication est simplement une mise à jour de la première
# précision du temps exact à la fin de l'indispo 

bout_indispo_IIP_data_quint %>% 
  mutate(date = as.Date(datetime_debut)) %>% 
  group_by(date, production_resource_name) %>% 
  mutate(n_mrid = n()) %>%
  summarise(n_mrid = first(n_mrid)) %>% 
  ungroup() %>% 
  group_by(n_mrid) %>%
  summarise(n = n())

#keep only those with max 3 messages

bout_indispo_IIP_data_sixt <- bout_indispo_IIP_data_quint %>% 
  mutate(date = as.Date(datetime_debut)) %>% 
  group_by(date, production_resource_name) %>% 
  mutate(n_mrid = n()) %>%
  filter(n_mrid <= 2)
  

bout_indispo_IIP_data_sext <- bout_indispo_IIP_data_sixt %>%
  group_by(datetime_debut, plant_type, production_resource_name) %>%
  arrange(datetime_pub) %>%
  mutate(
    datetime_debut_new = pmin(datetime_fin,  lag(datetime_fin, default = first(datetime_debut))),
    datetime_fin_new = pmax(datetime_fin, lag(datetime_fin, default = first(datetime_fin))),
    Unavailable.Capacity_Delta = Unavailable.Capacity - lag(Unavailable.Capacity, default = 0)
  ) %>%
  ungroup() %>% 
  select(datetime_debut_new, datetime_fin_new, datetime_pub,
         plant_type, pub_diff, mrid,
         production_resource_name, Unavailable.Capacity_Delta,
         businesstype, docstatus, Installed.Capacity) %>%
  rename(datetime_debut = datetime_debut_new, datetime_fin = datetime_fin_new, Unavailable.Capacity = Unavailable.Capacity_Delta)


bout_indispo_IIP_data_huit <- bout_indispo_IIP_data_sext %>% 
  group_by(datetime_debut, production_resource_name) %>%
  mutate(n = n(),
         duration = as.numeric(difftime(datetime_fin, datetime_debut, units = "hours"))
         ) %>% 
  filter(n==1 & duration >1)

View(bout_indispo_IIP_data %>% 
       mutate(date = as.Date(datetime_debut)) %>% 
  filter(production_resource_name == "LITORAL1" & date == "2021-01-06") 
       )

unique(bout_indispo_IIP_data_huit$docstatus)

bout_indispo_IIP_data_huit %>% 
  group_by(docstatus, businesstype) %>%
  summarise(n = n())

final_indispo_IIP_data <- bout_indispo_IIP_data_huit %>% 
  filter(docstatus == "Active")
dismissed_indispo_IIP_data <- bout_indispo_IIP_data_huit %>% 
  filter(docstatus == "Dismissed")

unexpected_indispo_IIP_data <- final_indispo_IIP_data %>% 
  filter(businesstype == "Unplanned")

getwd()
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/IIP_indispo/formatted'
setwd(save_path_esp)
write_parquet(final_indispo_IIP_data, "indispo_IIP_data.parquet")
write_parquet(dismissed_indispo_IIP_data , "dismissed_indispo_IIP_data.parquet")



