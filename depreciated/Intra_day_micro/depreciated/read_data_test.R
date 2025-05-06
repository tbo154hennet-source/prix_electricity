library(arrow)
library(purrr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(data.table)
library(tibble)
library(readxl)

extract_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/OMIE_indispo"
zip_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/OMIE_indispo/zip"
setwd(zip_path)
list.files(zip_path)

# Get a list of all ZIP files in the directory
zip_files <- list.files(zip_path, pattern = "\\.zip$", full.names = TRUE)

# Loop through each ZIP file and extract it
for (zip_file in zip_files) {
  unzip(zip_file, exdir = extract_dir)
  message(paste("Extracted:", zip_file))
}

message("All files extracted successfully!")




setwd(extract_dir)
list.files(extract_dir)

get_clean_data <- function(file){
  # Read the file, skipping the first line
  data <- read.table(file, sep = ";", header = FALSE,
                     stringsAsFactors = FALSE, skip = 1, fill = TRUE)
  
  datetime_message <- as.POSIXct(paste(data[1, ]$V1, data[1, ]$V2, data[1, ]$V3, data[1, ]$V4, data[1, ]$V5, data[1, ]$V6,  sep = "-"), format = "%Y-%m-%d-%H-%M-%S")
  
  data <- data[c(-1, -nrow(data)),]
  # Combine the date and time columns (V2 to V7) into a DateTime column
  data$datetime_debut <- as.POSIXct(paste(data$V2, data$V3, data$V4, data$V5, data$V6, data$V7, sep = "-"), format = "%Y-%m-%d-%H-%M-%S")
  data$datetime_fin <- as.POSIXct(paste(data$V8, data$V9, data$V10, data$V11, data$V12, data$V13, sep = "-"), format = "%Y-%m-%d-%H-%M-%S")
  data$datetime_pub <- as.POSIXct(paste(data$V18, data$V19, data$V20, data$V21, data$V22,  sep = "-"), format = "%Y-%m-%d-%H-%M")
  
  # Drop the original columns V2 to V7 (which contain the individual components)
  data <- data[, !(names(data) %in% paste0("V",c(2:13, 17:23)))]
  
  data <- data %>%
    rename(
      mrid = V1,
      psr_resource_name = V14,
      Available.Capacity = V15,
      Unavailable.Capacity_Reason = V16
    )
  
  return(data)
}
# Read the file (assuming semicolon delimiters and possible headers)
current_unavailabilities <- data.table(get_clean_data("indisp_20180101.1"))


# Liste tous les fichiers et dossiers
files <- list.files(extract_dir)

# Extract numeric part after the last dot
extract_number <- function(filename) {
  
  # Remove everything after the last dot
  prefix <- sub("\\.[^.]*$", "", sub(".*\\_", "", filename))
  # Extract part after last dot and convert to numeric
  suffix <- as.numeric(sub(".*\\.", "", filename))  
  suffix3 <- sprintf("%03d", suffix) 
  
  return(paste0(prefix, suffix3))
}

order_numbers <- sort(extract_number(files))

# Sort files numerically
files_sorted <- files[order(extract_number(files))]
files_sorted <- files_sorted[c(-1, -length(files_sorted))]


# pour les trous de données
# checker que les messages sont bien consécutifs


format_indispo_bis <- function(all_unavailabilities) {
  setDT(all_unavailabilities)  # Convert to data.table for speed
  
  all_unavailabilities[, new_datetime_debut := pmax(datetime_debut, shift(datetime_debut, type = "lag")), by = psr_resource_name]
  all_unavailabilities[, new_datetime_fin := pmin(datetime_fin, shift(datetime_fin, type = "lag")), by = psr_resource_name]
  all_unavailabilities[, Available.Capacity_Delta := shift(Available.Capacity, type = "lag") - Available.Capacity, by = psr_resource_name]
  
  intersections <- all_unavailabilities[Available.Capacity_Delta != 0, .(
    mrid, psr_resource_name, datetime_debut = new_datetime_debut, datetime_fin = new_datetime_fin, Available.Capacity_Delta, Available.Capacity, field = "Available.Capacity_Delta"
  )]
  
  # Create summarized availability data
  new_before_unavailabilities <- all_unavailabilities[, .(
    datetime_debut = last(datetime_debut),
    datetime_fin = min(datetime_debut[-.N], na.rm = TRUE),
    Available.Capacity = last(Available.Capacity),
    mrid = last(mrid),
    Unavailable.Capacity_Reason = last(Unavailable.Capacity_Reason),
    datetime_pub = last(datetime_pub)
  ), by = psr_resource_name][datetime_debut < datetime_fin]
  
  new_after_unavailabilities <- all_unavailabilities[, .(
    datetime_debut = max(datetime_fin[-.N], na.rm = TRUE),
    datetime_fin = last(datetime_fin),
    Available.Capacity = last(Available.Capacity),
    mrid = last(mrid),
    Unavailable.Capacity_Reason = last(Unavailable.Capacity_Reason),
    datetime_pub = last(datetime_pub)
  ), by = psr_resource_name][datetime_debut < datetime_fin]
  
  # Combine results efficiently
  formatted_indispo <- rbindlist(list(
    intersections, 
    new_after_unavailabilities[, .(mrid, psr_resource_name, datetime_debut, datetime_fin, Available.Capacity, field = "Available.Capacity")],
    new_before_unavailabilities[, .(mrid, psr_resource_name, datetime_debut, datetime_fin, Available.Capacity, field = "Available.Capacity")]
  ),use.names = TRUE, fill = TRUE)
  
  return(formatted_indispo)
}

#sample_files <- files_sorted[1:100]

# Read the file (assuming semicolon delimiters and possible headers)
current_unavailabilities <- data.table(get_clean_data("indisp_20210216.40"))
# Initialize lists with preallocated size
result_list <- vector("list", length(files_sorted))
result_dismissed_indispo <- vector("list", length(files_sorted))

which(order_numbers=="20210216040")
i=38878
# Create the sequence of indices
files_list <- seq_along(files_sorted)[-length(files_sorted)]  # All indices except the last

#end of first group
# Extract elements starting from index 34323, 54907
files_list <- files_list[54907:length(files_list)]
i <- files_list[38878]
# Process all files
for (i in files_list) {#c(10)) { #
  if(i %% 500 == 0) {
    message(paste("Processing file", i, "of", length(files_sorted)))
  }
  file <- files_sorted[i]
  new_current_unavailabilities <- data.table(get_clean_data(file))
  
  # Check if we need to start fresh
  if (i < length(order_numbers) & as.numeric(order_numbers[i+1]) - as.numeric(order_numbers[i]) > 1000) {
    current_unavailabilities <- new_current_unavailabilities
  } else {
    new_unavailabilities <- fsetdiff(new_current_unavailabilities, current_unavailabilities)
    current_datetime <- new_unavailabilities[1, datetime_pub]
    if (nrow(new_unavailabilities) == 0) {
      next
    } else {
      old_unavailabilities <- fsetdiff(current_unavailabilities, new_current_unavailabilities)[
        datetime_fin >= current_datetime
      ]
      
      if (nrow(old_unavailabilities) == 0) {
        new_unavailabilities[, field := "Available.Capacity"]
        result_list[[i]] <- new_unavailabilities[
          , message_order := order_numbers[i+1]
        ]
      } else {
        all_unavailabilities <- rbindlist(list(
          new_unavailabilities, 
          current_unavailabilities[psr_resource_name %in% new_unavailabilities$psr_resource_name]
        ))[order(datetime_pub)]
        
        formatted_indispo_bis <- format_indispo_bis(all_unavailabilities)
        
        all_unavailabilities_bis <- rbindlist(list(
          old_unavailabilities, 
          new_current_unavailabilities[psr_resource_name %in% old_unavailabilities$psr_resource_name]
        ))[order(-datetime_pub)]
        
        dismissed_indispo_bis <- format_indispo_bis(all_unavailabilities_bis)
        
        result_list[[i]] <- formatted_indispo_bis[
          , message_order := order_numbers[i+1]
        ]
        result_dismissed_indispo[[i]] <- dismissed_indispo_bis[
          , message_order := order_numbers[i+1]
        ]
      }
      current_unavailabilities <- new_current_unavailabilities
    }
  }
}


# Bind results efficiently using data.table’s rbindlist()
Unavailabilities_data_bis <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
Unavailabilities_data_dismissed_bis <- rbindlist(result_dismissed_indispo, use.names = TRUE, fill = TRUE)

getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/formatted_OMIE")
write_parquet( Unavailabilities_data_bis   ,"unavailabilities_OMIE_part3")
write_parquet(Unavailabilities_data_dismissed_bis    ,"unavailabilities_dismissed_OMIE_part3")


tot_files <- list.files()
unavailabilities_files <- list.files(pattern = "unavailabilities_OMIE")
dismissed_files <- list.files(pattern = "unavailabilities_dismissed_OMIE")

# Read all parquet files in the directory
unavailabilities_data <- map_dfr(unavailabilities_files, ~ read_parquet(.x))
dismissed_data <- map_dfr(dismissed_files, ~ read_parquet(.x))

write_parquet(unavailabilities_data, "unavailabilities_OMIE_tot.parquet")
write_parquet(dismissed_data, "unavailabilities_dismissed_OMIE_tot.parquet")

# Read the parquet files

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/formatted_OMIE")

unavailabilities_data <- read_parquet("unavailabilities_tot_OMIE.parquet")
dismissed_data <- read_parquet("unavailabilities_dismissed_tot_OMIE.parquet")
pub_dates_df <- read_parquet("publication_dates.parquet")

colnames(unavailabilities_data)

unavailabilities_data <- unavailabilities_data %>%
  left_join(pub_dates_df, by = c("message_order" = "file")) %>%
  filter(!is.na(mrid)) %>% 
  mutate(datetime_pub = date) %>%
  filter(datetime_debut < datetime_fin) %>% 
  select(-date) %>% 
  unique()


dismissed_data <- dismissed_data %>%
  left_join(pub_dates_df, by = c("message_order" = "file")) %>%
  filter(!is.na(mrid)) %>%
  mutate(datetime_pub = date) %>%
  filter(datetime_debut < datetime_fin) %>% 
  select(-date)%>% 
  unique()

#il y a quelques erreurs de noms de fichier
#où l'on se rend compte en le lisant que la date de publication est différente

#produit typiquement la même indispo
#avec deux dates de publi très proches

unavailabilities_data <- unavailabilities_data %>% 
  group_by(datetime_debut, datetime_fin, mrid) %>% 
  arrange(datetime_pub) %>% 
  slice_head(n = 1) %>% 
  ungroup()

dismissed_data <- dismissed_data %>% 
  group_by(datetime_debut, datetime_fin, mrid) %>% 
  arrange(datetime_pub) %>% 
  slice_head(n = 1) %>% 
  ungroup()

colnames(unavailabilities_data)
colnames(dismissed_data)



write_parquet(unavailabilities_data, "unavailabilities_OMIE_tot.parquet")
write_parquet(dismissed_data, "unavailabilities_dismissed_OMIE_tot.parquet")
