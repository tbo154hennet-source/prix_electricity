
library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut)

colnames(bout_indispo_ENTSOE_data)

plant_list <- sort(unique(bout_indispo_ENTSOE_data$production_resource_name))

# Filter out names containing the inverse question mark
filtered_plant_list <- plant_list[!grepl("¿", plant_list)]




production_plant_list <- colnames(generation_per_unit)

# Load required package
library(stringdist)

# Create a distance matrix using Levenshtein distance
distance_matrix <- stringdistmatrix( production_plant_list, filtered_plant_list, method = "lv")

# Find the closest matches with a threshold
threshold <- 4  # Set maximum allowable distance
matches <- apply(distance_matrix, 1, function(row) {
  if (min(row) <= threshold) {
    closest_index <- which.min(row)
    filtered_plant_list[closest_index]
  } else {
    NA  # No match if distance exceeds the threshold
  }
})

# Combine the results
result <- data.frame(
  List1 = production_plant_list,
  Closest_Match = matches,
  stringsAsFactors = FALSE
)

getwd()
write.csv(result, "closest_matches.csv", row.names = FALSE)

correspondance <- read.csv("closest_matches_manual_corrected.csv")
# View results



unexpected_indispo_ENTSOE_data <- bout_indispo_ENTSOE_data %>% 
 filter(businesstype == "Unplanned outage" )




bout_indispo_ENTSOE_data[1,]
bout_indispo_ENTSOE_data[nrow(bout_indispo_ENTSOE_data),"datetime_fin"]
