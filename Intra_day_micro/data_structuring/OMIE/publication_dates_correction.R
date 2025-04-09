
#files_sorted <- files_sorted[1:100]

# Read the file (assuming semicolon delimiters and possible headers)
current_unavailabilities <- data.table(get_clean_data("indisp_20180101.1"))
# Initialize lists with preallocated size


# Create the sequence of indices
files_list <- seq_along(files_sorted)[-length(files_sorted)]  # All indices except the last

get_publication_date <- function(file){
  data <- read.table(file, sep = ";", header = FALSE,
                     stringsAsFactors = FALSE, skip = 1, fill = TRUE)
  
  datetime_message <- as.POSIXct(paste(data[1, ]$V1, data[1, ]$V2, data[1, ]$V3, data[1, ]$V4, data[1, ]$V5, data[1, ]$V6,  sep = "-"), format = "%Y-%m-%d-%H-%M-%S")
  return(datetime_message)
}



publication_dates <- vector("list", length(files_list))

for (idx in seq_along(files_list)) {
  file <- files_sorted[idx]
  
  # Print progress every 1000 files
  if (idx %% 1000 == 0) {
    message(paste("Processing file", idx, "of", length(files_list)))
  }
  
  # Extract publication date
  publication_dates[[idx]] <- tryCatch({
    get_publication_date(file)
  }, error = function(e) {
    warning(paste("Error in file:", file, "-", e$message))
    NA
  })
}


pub_dates_df <- data.frame(file = order_numbers[2:length(files_sorted)], date = as.POSIXct(unlist(publication_dates)))
getwd()
write_parquet(pub_dates_df, "publication_dates.parquet")


