library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
  
save_path_fr <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_path_fr)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet")

colnames(bout_indispo_ENTSOE_data)

bout_indispo_ENTSOE_data$duration_in_days <- as.numeric(bout_indispo_ENTSOE_data$duration, units = "days")

long_ENTSOE_indispos <- bout_indispo_ENTSOE_data %>% 
  filter(duration_in_days >10)

# on vérifie quelle part du temps total d'indispo est conservé
ratio_indispo <- sum(long_ENTSOE_indispos$duration_in_days)/sum(bout_indispo_ENTSOE_data$duration_in_days)

#encore 84% de la durée total d'indispos

#row <- long_ENTSOE_indispos[1,]
#index <- which(long_format_ENTSOE_indispo$mrid == row[['mrid']])


#attention probleme de fesseheim qui ferme definitivement 
#encodée comme indispo jusqu'en 2099
# soit pendant 29 000j gros bug

# View(long_ENTSOE_indispos %>% 
#        filter(mrid == 'Ry5Sic3B7qqfGzZplJbVOg'))
# index <- which(long_ENTSOE_indispos$mrid == 'Ry5Sic3B7qqfGzZplJbVOg')
#on change datetimefin jusqu'à horizon présent 2023_12
#long_ENTSOE_indispos[index, 'datetime_fin'] <- present_date

  
#Attention il faut transposer dans l'exemple
#par rapport à la fonction industrielle
present_date <- as.Date("31-12-2023", format = "%d-%m-%Y")

#index <- which(long_ENTSOE_indispos$mrid == "m7D7fgU1rTU1tq3cLwoUvQ")

library(data.table)


long_format <- function(row){
  #row= t(row)
  #indicateur d'avancement
  index <- which(long_ENTSOE_indispos$mrid == row[['mrid']])

  if (index %% 100 == 0){
    print(index)
  }
  # Generate sequence of dates between start and end dates
  all_dates <- seq.Date(as.Date(row[['datetime_debut']]),
                        min(as.Date(row[['datetime_fin']]),present_date ), by = "day")
  
  
  # Expand with all dates
  date_values <- data.table(indispo_day = all_dates)
  
  # Ensure Unavailable.Capacity is repeated for each date
  expanded_df <- data.table(
    indispo_day = rep(date_values$indispo_day, each = length(row[['Unavailable.Capacity']])),
    Unavailable_Capacity = rep(row[['Unavailable.Capacity']], length(date_values$indispo_day))
  )
  
  # Convert Unavailable_Capacity to numeric if it isn't already
  expanded_df[, Unavailable_Capacity := as.numeric(Unavailable_Capacity)]
  # Append the line to the result
  #long_format_ENTSOE_indispo <<- rbind(long_format_ENTSOE_indispo, expanded_df)
  long_format_ENTSOE_indispo <<- rbindlist(list(long_format_ENTSOE_indispo, expanded_df), use.names = TRUE, fill = TRUE)
  
  long_format_ENTSOE_indispo <<- long_format_ENTSOE_indispo[, .(Tot_Unavailability = sum(.SD, na.rm = TRUE)), by = indispo_day, .SDcols = where(is.numeric)]
  
}

long_format_ENTSOE_indispo <- long_format_ENTSOE_indispo %>% 
  arrange(indispo_day)

ggplot() +
  geom_line(data = long_format_ENTSOE_indispo, aes(x = indispo_day, y = Tot_Unavailability)) +
  geom_line(data = final_indispo, aes(x = indispo_day, y = Unavailable.Capacity), color = "red") +
  labs(title = "Total Unavailability over time",
       x = "Date",
       y = "Total Unavailability (MW)")

long_format_ENTSOE_indispo <- NULL
# on pourra essayer d'ajuster
# pour les dates de debuts et de fin a quel point
# debut ou fin de mois
#row <- long_ENTSOE_indispos[1,]
#sample <- long_ENTSOE_indispos[1:3,]
setDT(long_ENTSOE_indispos)

result_hours <- apply(long_ENTSOE_indispos, 1, long_format)

# savepath <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/safeplace"
# setwd(savepath)
getwd()
write_parquet(long_format_ENTSOE_indispo, 'all_format_ENTSOE_indispo_tot.parquet')

long_format_ENTSOE_indispo_bis <- read_parquet('all_format_ENTSOE_indispo_tot.parquet')
#on se propose comme niveau d'analyse la journee
# a savoir on calcule la somme des indispos pour un jour donne
#result_hours <- apply(long_ENTSOE_indispos, 1, long_format)
long_format_ENTSOE_indispo <- 
  read_parquet('all_format_ENTSOE_indispo_tot.parquet')

start_date <- as.Date("2015-01-01")
final_indispo <- long_format_ENTSOE_indispo_bis %>% 
  group_by(indispo_day) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()%>% 
  filter(indispo_day > start_date)

write_parquet(final_indispo, 'final_french_indispo.parquet')



nuclear_indispo <- long_format_ENTSOE_indispo %>% 
  filter(plant_type== 'Nuclear')

final_nuclear_indispo <- nuclear_indispo%>% 
  group_by(indispo_day) %>% 
  summarise(Installed.Capacity = sum(as.numeric(Installed.Capacity)),
            Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity))) %>%
  ungroup()%>% 
  filter(indispo_day > start_date)

write_parquet(final_nuclear_indispo, 'final_french_nuclear_indispo.parquet')

#y a t il une grande difference à faire l'intégralité du groupe

# short_ENTSOE_indispos <- bout_indispo_ENTSOE_data %>% 
#   filter(duration_in_days <=30)
# 
# result_hours <- apply(short_ENTSOE_indispos, 1, long_format)
# 

#petit plot des indispos théoriques
#avec long indispo a priori mieux réalisées que annonces de court terme


monthly_indispo <- final_indispo %>% 
  mutate(month = month(indispo_day),
         year = year(indispo_day)) %>% 
  group_by(month, year) %>% 
  summarise(Installed.Capacity = mean(Installed.Capacity),
            Unavailable.Capacity = mean(Unavailable.Capacity)) %>%
  ungroup()

nuclear_monthly_indispo <- final_nuclear_indispo %>% 
  mutate(month = month(indispo_day),
         year = year(indispo_day)) %>% 
  group_by(month, year) %>% 
  summarise(Installed.Capacity = mean(Installed.Capacity),
            Unavailable.Capacity = mean(Unavailable.Capacity)) %>%
  ungroup()

write_parquet(monthly_indispo, 'monthly_indispo.parquet')
write_parquet(nuclear_monthly_indispo, 'nuclear_monthly_indispo.parquet')

