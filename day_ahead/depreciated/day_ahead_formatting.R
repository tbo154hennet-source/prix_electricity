library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_ter.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut) %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

bout_indispo_ENTSOE_data %>% 
  group_by(businesstype) %>% 
  summarise(n=n())


bout_indispo_ENTSOE_data <- 
  bout_indispo_ENTSOE_data[!duplicated(bout_indispo_ENTSOE_data$mrid), ]

#actual indispo ne retient qu'une petite part des indispos
#il faut me refaire un equivalent d'auxiliary


long_format_day_ahead_indispo <- NULL
row <- planned_maintenance[960,]

present_date <- as.Date("31-12-2024", format = "%d-%m-%Y")

planned_maintenance <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Planned maintenance')

unplanned_outage <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Unplanned outage')

#imposer une limite sur la durée de lindispo
# pour que ce ne soit pas une sortie du parc
library(data.table)

long_format <- function(row) {
  # Indicateur d'avancement
  index <- which(planned_maintenance$mrid == row[['mrid']])
  
  # tryCatch({
  #   if (length(index) > 0 && index %% 1000 == 0) {
  #     print(index)
  #   }
  # }, error = function(e) {
  #   # Print the index causing the error and the error message
  #   print(paste("Error at index:", toString(index)))
  #   print(paste("Error message:", e$message))
  # })
  
  if (index %% 1000 == 0) {
    print(index)
  }
  
  # Generate sequence of dates between start and end dates
  all_dates <- seq.Date(as.Date(row[['datetime_debut']]),
                        min(as.Date(row[['datetime_fin']]), present_date), by = "day")
  
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
  if (!exists("long_format_day_ahead_indispo")) {
    long_format_day_ahead_indispo <<- expanded_df
  } else {
    long_format_day_ahead_indispo <<- rbindlist(list(long_format_day_ahead_indispo, expanded_df), use.names = TRUE, fill = TRUE)
  }
}

# Convert planned_maintenance to data.table for faster operations
setDT(planned_maintenance)

# Apply the function
long_format_day_ahead_indispo <- NULL
# Apply the function
result_hours <-apply(planned_maintenance, 1, long_format)


#dplyr slow version
# long_format_planned_indispo <- long_format_day_ahead_indispo %>% 
#   group_by(indispo_day) %>%
#   summarise(Tot_Unavailability = rowSums(across(where(is.numeric)), na.rm = TRUE), .groups = "drop")

# Compute Tot_Unavailability and aggregate by indispo_day
long_format_planned_indispo <- long_format_day_ahead_indispo[, .(Tot_Unavailability = sum(.SD, na.rm = TRUE)), by = indispo_day, .SDcols = where(is.numeric)]




long_format_day_ahead_indispo <- NULL
setDT(unplanned_outage)
result_hours <- apply(unplanned_outage, 1, long_format)
long_format_unplanned_indispo <- long_format_day_ahead_indispo[, .(Tot_Unavailability = sum(.SD, na.rm = TRUE)), by = indispo_day, .SDcols = where(is.numeric)]


# Plot the results
ggplot() +
  geom_line(data= long_format_planned_indispo, aes(x=indispo_day, y=Tot_Unavailability, color = 'Planned Maintenance')) +
  geom_line(data= long_format_unplanned_indispo, aes(x=indispo_day, y=Tot_Unavailability, color = 'Unplanned outage')) +
  labs(title = "Total Unavailability Capacity by Day", x = "Day", y = "Total Unavailability Capacity") +
  theme_minimal()



indispo_day_ahead <- long_format_planned_indispo %>% 
  rename(Tot_Planned_Unavailability = Tot_Unavailability) %>% 
  full_join(long_format_unplanned_indispo %>% 
              rename(Tot_Unplanned_Unavailability = Tot_Unavailability), by = "indispo_day")
getwd()
write_parquet(indispo_day_ahead, "indispo_day_ahead_bis.parquet")






