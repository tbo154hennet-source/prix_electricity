library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
  
save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_unexpected_indispo_ENTSOE_data <- read_parquet("unexpected_indispo_ENTSOE_data.parquet")

colnames(bout_unexpected_indispo_ENTSOE_data)

bout_unexpected_indispo_ENTSOE_data$duration_in_days <- as.numeric(bout_unexpected_indispo_ENTSOE_data$duration, units = "days")

long_ENTSOE_unexpected_indispos <- bout_unexpected_indispo_ENTSOE_data %>% 
  filter(duration_in_days >10)

# on vérifie quelle part du temps total d'unexpected_indispo est conservé
ratio_unexpected_indispo <- sum(long_ENTSOE_unexpected_indispos$duration_in_days)/sum(bout_unexpected_indispo_ENTSOE_data$duration_in_days)

print(ratio_unexpected_indispo)
#encore 67% de la durée total d'unexpected_indispos

#row <- long_ENTSOE_unexpected_indispos[1,]
#index <- which(long_format_ENTSOE_unexpected_indispo$mrid == row[['mrid']])


#attention probleme de fesseheim qui ferme definitivement 
#encodée comme unexpected_indispo jusqu'en 2099
# soit pendant 29 000j gros bug

# View(long_ENTSOE_unexpected_indispos %>% 
#        filter(mrid == 'Ry5Sic3B7qqfGzZplJbVOg'))
# index <- which(long_ENTSOE_unexpected_indispos$mrid == 'Ry5Sic3B7qqfGzZplJbVOg')
#on change datetimefin jusqu'à horizon présent 2023_12
#long_ENTSOE_unexpected_indispos[index, 'datetime_fin'] <- present_date

  
#Attention il faut transposer dans l'exemple
#par rapport à la fonction industrielle
present_date <- as.Date("31-12-2023", format = "%d-%m-%Y")

#index <- which(long_ENTSOE_unexpected_indispos$mrid == "m7D7fgU1rTU1tq3cLwoUvQ")

long_format_ENTSOE_unexpected_indispo <- NULL
long_format <- function(row){
  #row= t(row)
  #indicateur d'avancement
  index <- which(long_ENTSOE_unexpected_indispos$mrid == row[['mrid']])

  if (index %% 100 == 0){
    print(index)
  }
  # Generate sequence of dates between start and end dates
  all_dates <- seq.Date(as.Date(row[['datetime_debut']]),
                        min(as.Date(row[['datetime_fin']]),present_date ), by = "day")
  
  #expand with all dates
  date_values <- tibble(unexpected_indispo_day = all_dates)
  
  # Convert the row to a dataframe
  row_df <- as.data.frame(t(row), stringsAsFactors = FALSE)
  
  expanded_df <- crossing(row_df , date_values)
  # Append the line to the result
  long_format_ENTSOE_unexpected_indispo <<- rbind(long_format_ENTSOE_unexpected_indispo, expanded_df)
  
}


# on pourra essayer d'ajuster
# pour les dates de debuts et de fin a quel point
# debut ou fin de mois
#sample <- long_ENTSOE_unexpected_indispos[1:3,]

result_hours <- apply(long_ENTSOE_unexpected_indispos, 1, long_format)

# savepath <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/safeplace"
# setwd(savepath)

write_parquet(long_format_ENTSOE_unexpected_indispo, 'unexpected_format_ENTSOE_unexpected_indispo_tot.parquet')
#on se propose comme niveau d'analyse la journee
# a savoir on calcule la somme des unexpected_indispos pour un jour donne
#result_hours <- apply(long_ENTSOE_unexpected_indispos, 1, long_format)


# long_format_ENTSOE_unexpected_indispo <- 
#   read_parquet('all_format_ENTSOE_unexpected_indispo_tot.parquet')

start_date <- as.Date("2015-01-01")
final_unexpected_indispo <- long_format_ENTSOE_unexpected_indispo %>% 
  group_by(unexpected_indispo_day) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()%>% 
  filter(unexpected_indispo_day > start_date)

write_parquet(final_unexpected_indispo, 'final_unexpected_spain_unexpected_indispo.parquet')



nuclear_unexpected_indispo <- long_format_ENTSOE_unexpected_indispo %>% 
  filter(plant_type== 'Nuclear')

final_nuclear_unexpected_indispo <- nuclear_unexpected_indispo%>% 
  group_by(unexpected_indispo_day) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()%>% 
  filter(unexpected_indispo_day > start_date)

write_parquet(final_nuclear_unexpected_indispo, 'final_unexpected_spain_nuclear_unexpected_indispo.parquet')

#y a t il une grande difference à faire l'intégralité du groupe

# short_ENTSOE_unexpected_indispos <- bout_unexpected_indispo_ENTSOE_data %>% 
#   filter(duration_in_days <=30)
# 
# result_hours <- apply(short_ENTSOE_unexpected_indispos, 1, long_format)
# 

#petit plot des unexpected_indispos théoriques
#avec long unexpected_indispo a priori mieux réalisées que annonces de court terme


monthly_unexpected_indispo <- final_unexpected_indispo %>% 
  mutate(month = month(unexpected_indispo_day),
         year = year(unexpected_indispo_day)) %>% 
  group_by(month, year) %>% 
  summarise(Installed.Capacity = mean(Installed.Capacity),
            Unavailable.Capacity = mean(Unavailable.Capacity)) %>%
  ungroup()

nuclear_monthly_unexpected_indispo <- final_nuclear_unexpected_indispo %>% 
  mutate(month = month(unexpected_indispo_day),
         year = year(unexpected_indispo_day)) %>% 
  group_by(month, year) %>% 
  summarise(Installed.Capacity = mean(Installed.Capacity),
            Unavailable.Capacity = mean(Unavailable.Capacity)) %>%
  ungroup()


write_parquet(monthly_unexpected_indispo, 'unexpected_monthly_indispo.parquet')
write_parquet(nuclear_monthly_unexpected_indispo, 'unexpected_nuclear_monthly_indispo.parquet')

