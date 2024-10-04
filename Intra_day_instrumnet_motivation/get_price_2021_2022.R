library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

Sys.setenv(lang = "en_US")


# pour fair une specification log log,
# normalisation par la puissance installee

# explorer differents types de normalisation
# pour tenir compte de l'intermittence
# normaliser par la production prevue
# pour donner un meilleur sens de la rarete
auction_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_repo_gitlab/DATA/intra_day_prices/OMIE-auction-market-data'


execute_code <- function(auction){
  auction_number <- substr(auction,
                           23, 23)
  auction_date_plus_one <- substr(auction,
                                  14, 21)
  
  csv_file <- read.csv2(auction, header = FALSE,
                        skip = 1)
  csv_file <- csv_file[, 1:5]
  headers <- c('year', 'month', 'day', 'creneau', 'price')
  colnames(csv_file) <- headers
  csv_file$auction <- auction_number
  csv_file$auction_date_plus_one  <- auction_date_plus_one 
  csv_file <- csv_file %>% 
    mutate(
      datetime = as.POSIXct(
      paste(year, month, day, creneau-1), format="%Y %m %d %H", tz='UTC'),
      delivery_date = as.Date(datetime, format="%Y%m%d"),
      # Convert to Date object and reformat to %Y-%m-%d
      
      auction_date_plus_one = as.Date(auction_date_plus_one, format="%Y%m%d"),
                  
      
      # there is  problem with 21-24
      # there would be indexed in the same way
      # we create a fictious auction number 7
      
       auction = ifelse(delivery_date!=auction_date_plus_one, 7, auction)
      )
  csv_file <- head(csv_file, -1)
  
  if (is.null(price_data)){
    price_data <<- csv_file
  } else {
    price_data <<- rbind(price_data, csv_file)
  }
}

price_data <- NULL
setwd(auction_path)

my_files <- list.files()
# Obtenez la liste des fichiers .parquet
parquet_files <- my_files[grep("\\.parquet$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers
my_files <- setdiff(my_files, parquet_files)

for (file in my_files[4:5]){
  setwd(auction_path)
  setwd(file)
  print(file)
  auction_files <- list.files()
  for (auction in auction_files){
    tryCatch(
      {
        execute_code(auction)
      },
      error = function(e) {
        cat("An error occurred:", conditionMessage(e), "\n")
        print(auction) # Return NA or any other value you want to indicate an error
      }
    )
    
    
  }
}
setwd(auction_path)
write_parquet(price_data, 'auction_price_data_21_22.parquet')

# NB
# valeur manquante pour 2021082405.1


