library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(purrr)
library(stringr)
library(data.table)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/actual_flows/ES_FR")
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


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/NTC/ES_PT")

files_ex <- list.files( pattern = "^ES_PT", full.names = TRUE)
files_imp <- list.files( pattern = "^PT_ES", full.names = TRUE)

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

colnames(imp_data) <- c( "NTC_PT_ES","time")
colnames(ex_data) <- c( "NTC_ES_PT", "time")

NTC <- merge(ex_data, imp_data, by = "time")
cross_border_flows <- merge(cross_border_flows, NTC, by = "time")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/scheduled_exchanges/ES_neighbours")

#SCH_EX <- NULL
#file <- files_ex[1]
#name <- "BZN|GB(IFA) -> BZN|FR...22"
#name <- "BZN|DE-LU -> BZN|FR...22"
# Fonction pour générer les nouveaux noms
get_new_colname <- function(name, index) {
  # Extraire les pays (entre BZN|XX et BZN|YY)
  # Adjust the regex to capture only the first two letters before the hyphen
  # Adjust the regex to capture only the first two letters before any non-letter characters

  # Expression régulière pour capturer les codes pays (GB, FR, etc.)
  country_codes <- regmatches(name, gregexpr("(?<=BZN\\|)[A-Z]{2}", name, perl=TRUE))
  
  # Affichage des codes pays extraits
  from <- country_codes[[1]][1]
  to <- country_codes[[1]][2]
  
  suffix <- if (index %% 2 == 1) "DA" else "TOT"
  
  paste0("SCH_EX_", suffix, "_", from, "_", to)
}


excel_files <- list.files(pattern = "\\.xlsx$")
  
sch_ex_data_es <- excel_files %>%
    map(~ read_excel(.x, skip = 5) %>% slice(-1)) %>%  # Skip 5 lignes, puis enlever la première ligne restante
    bind_rows() %>%
    arrange(MTU)
  
  # Convert time column to POSIXct with timezone specified as "Europe/Brussels"
sch_ex_data_es <- sch_ex_data_es %>%
    mutate(time = str_extract(MTU, "^[^ -]+ [^ -]+") %>% dmy_hms(tz = "Europe/Brussels")) %>%
    arrange(time)
  

# Identify BZN and CTA columns
bzn_cols <- grep("^BZN\\|", names(sch_ex_data_es), value = TRUE)
cta_cols <- grep("^CTA\\|", names(sch_ex_data_es), value = TRUE)

# Replace NAs in BZN columns with values from CTA columns
for (i in seq_along(bzn_cols)) {
  sch_ex_data_es[[bzn_cols[i]]] <- ifelse(
    is.na(sch_ex_data_es[[bzn_cols[i]]]),
    sch_ex_data_es[[cta_cols[i]]],
    sch_ex_data_es[[bzn_cols[i]]]
  )
}

sch_ex_data_es <-sch_ex_data_es %>% 
  select(-cta_cols, -MTU) 
  
  

# Appliquer la fonction à tous les noms
new_colnames <- mapply(get_new_colname, bzn_cols, seq_along(bzn_cols))
colnames(sch_ex_data_es) <- c(new_colnames, "time")

cross_border_flows <- merge(cross_border_flows, 
                            sch_ex_data_es,
                              by = "time")
    

  
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows/scheduled_exchanges/FR_neighbours")

excel_files <- list.files(pattern = "\\.xlsx$")

sch_ex_data_fr <- excel_files %>%
  map(~ read_excel(.x, skip = 5) %>% slice(-1)) %>%  # Skip 5 lignes, puis enlever la première ligne restante
  bind_rows() %>%
  arrange(MTU)

# da_tot <- excel_files %>%
#   map(~ read_excel(.x, skip = 5) %>% slice(1))
# 
# da_tot_data <- da_tot[[1]]

# Convert time column to POSIXct with timezone specified as "Europe/Brussels"
sch_ex_data_fr <- sch_ex_data_fr %>%
  mutate(time = str_extract(MTU, "^[^ -]+ [^ -]+") %>% dmy_hms(tz = "Europe/Brussels")) %>%
  arrange(time) %>% 
  select(-MTU)


time_obj <- sch_ex_data_fr$time

my_cols <- colnames(sch_ex_data_fr)[-c(length(colnames(sch_ex_data_fr)))] 
new_colnames_fr <- mapply(get_new_colname, # Remove first and last column names
                       my_cols, seq_along(my_cols))


# Appliquer la fonction à tous les noms
colnames(sch_ex_data_fr) <- c(new_colnames_fr, "time")


# Step 1: Get the duplicated column names
col_groups <- split.default(sch_ex_data_fr, names(sch_ex_data_fr))

# Step 2: Row-wise sum for each group of columns
sch_summarised <- as.data.frame(lapply(col_groups, function(df_group) {
  # Convert to data frame and ensure all columns are numeric
  df_group <- as.data.frame(df_group)
  df_group[] <- lapply(df_group, function(x) as.numeric(as.character(x)))
  
  if (ncol(df_group) > 1) {
    rowSums(df_group, na.rm = TRUE)
  } else {
    df_group[[1]]  # only one column, just return it
  }
}))

sch_summarised$time <- sch_ex_data_fr$time


# test <- merge(sch_ex_data_es, sch_summarised,
#               by = "time")
# 
# colnames(test)
# 
# 
# View(test %>% 
#        select(SCH_EX_TOT_FR_ES.x, SCH_EX_DA_FR_ES.x,
#               SCH_EX_TOT_FR_ES.y, SCH_EX_DA_FR_ES.y,
#               time )
#        )

cross_border_flows <- merge(cross_border_flows, 
                            sch_summarised %>% 
                              select(c(setdiff(colnames(sch_summarised), colnames(sch_ex_data_es)), "time")),
                            by = "time")

colnames(cross_border_flows)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows")

write_parquet(cross_border_flows, "all_cross_border_flows.parquet")

#cross_border_flows <- read_parquet( "fr_esp_cross_border_flows.parquet")






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

#cross_border_flows <- read_parquet("fr_esp_cross_border_flows.parquet")
cross_border_flows <- read_parquet("all_cross_border_flows.parquet")
colnames(cross_border_flows)


price_merge_CBF_tot <- merge(price_merge_CBF, cross_border_flows, 
                         by.x ="datetime", by.y ="time")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")
my_ntc_data_wider <- read_parquet( "NTC_tot_data.parquet")
colnames(my_ntc_data_wider) <- c("start_date", paste0("NTC_", colnames(my_ntc_data_wider)[-1]))
colnames(my_ntc_data_wider)

price_merge_CBF_tot <- merge(price_merge_CBF_tot, my_ntc_data_wider, 
                             by.x ="datetime", by.y ="start_date")

getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

write_parquet(price_merge_CBF_tot, "price_merge_CBF_tot.parquet")

ntc_col <- grep("NTC", colnames(price_merge_CBF_tot), value = TRUE)



