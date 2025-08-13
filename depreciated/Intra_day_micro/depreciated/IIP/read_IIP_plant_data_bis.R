
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

Sys.setenv(lang = "en_US")
read_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/IIP_indispo'
setwd(read_path )

indispo_IIP_data <- NULL
my_files <- list.files()
# Obtenez la liste des fichiers .parquet
my_files <- my_files[grep("\\.csv$", my_files)]

# Supprimez les fichiers .parquet de la liste des fichiers
# my_files <- setdiff(my_files, parquet_files)


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

indispo_IIP_data <-unique(indispo_IIP_data)

# write_parquet(indispo_IIP_data, 'indispo_IIP_data.parquet')


# get list of units by owner/ market participant

plant_ownership_spain <- indispo_IIP_data %>% 
  select(Market.Participant.code, Market.Participant, Installed.Capacity,            
         Fuel.Type, Bidding.Zone, 
         Affected.Asset.or.Unit, Affected.Asset.or.Unit.EIC.code)

plant_ownership_spain <- unique(plant_ownership_spain)
# write_parquet(plant_ownership_spain, 'plant_ownership_spain.parquet')



colnames(indispo_IIP_data)

indispo_IIP_data <- indispo_IIP_data %>% select(-c("Type.of.Event",
    "Unit.of.Measurement","Bidding.Zone",
    "Interval.Start..V2.schema.only.","Interval.Stop..V2.schema.only." ))

# explications RTE
# Downtime status are classified as:
#   Active : Default status for unavailability that is in progress, or scheduled.
# Inactive : Status for a new publication or an update of a past unavailability.
# Dismissed : Publication status for a downtime that is no longer valid.

# active_IIP_indispo <- indispo_IIP_data %>% 
#   filter(Event.Status=='Active')

# doesn't work well
# manual check

indispo_IIP_data  <- indispo_IIP_data  %>% 
  mutate(
    # create publication date
    `Publication.date.time` = as.POSIXct( paste0(
      substr(indispo_IIP_data$`Publication.date.time`,1, 10),
      ' ',
      substr(indispo_IIP_data$`Publication.date.time`,12, 19))),
    # create debut date
    `Event.Start` = as.POSIXct( paste0(
      substr(indispo_IIP_data$`Event.Start`,1, 10),
      ' ',
      substr(indispo_IIP_data$`Event.Start`,12, 19))),
    # create end date
    `Event.Stop` = as.POSIXct( paste0(
      substr(indispo_IIP_data$`Event.Stop`,1, 10),
      ' ',
      substr(indispo_IIP_data$`Event.Stop`,12, 19))),
    # duree de la coupure
    duration = difftime(`Event.Stop`, `Event.Start`, units = "days")
)


# we want to disentangle the different version of the sane message
# build event identifier by retrieving the three letters at end
# 
# Event.ID = substr(Message.ID,18,25)

save_path <- 'Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE'

setwd(save_path)
write_parquet(indispo_IIP_data , 'indispo_IIP_data.parquet')











