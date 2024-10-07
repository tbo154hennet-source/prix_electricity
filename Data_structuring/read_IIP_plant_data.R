
library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)

Sys.setenv(lang = "en_US")

setwd('C:/Users/nchu/Desktop/DATA/power-plant/IIP_indispo')

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

active_IIP_indispo <- indispo_IIP_data %>% 
  filter(Event.Status=='Active')

# doesn't work well
# manual check

up_indispo_IIP_data  <- indispo_IIP_data  %>% 
  mutate(
    # create publication date
    datetime_pub = as.POSIXct( paste0(
      substr(indispo_IIP_data$`Publication.date.time`,1, 10),
      ' ',
      substr(indispo_IIP_data$`Publication.date.time`,12, 19))),
    # create debut date
    datetime_debut = as.POSIXct( paste0(
      substr(indispo_IIP_data$`Event.Start`,1, 10),
      ' ',
      substr(indispo_IIP_data$`Event.Start`,12, 19))),
    # create end date
    datetime_fin = as.POSIXct( paste0(
      substr(indispo_IIP_data$`Event.Stop`,1, 10),
      ' ',
      substr(indispo_IIP_data$`Event.Stop`,12, 19))),
    # duree de la coupure
    duration = difftime(datetime_fin, datetime_debut, units = "days"),
    # duree entre l'annonce et la mise en oeuvre
    start_lag = difftime(datetime_debut, datetime_pub, units = "days"),
    #  dummy est ce que c'est une annonce a court terme
    # typiquement un incident technique ou une prolongation de quelques jours
    real_time_announcement = ifelse(start_lag<1, 1, 0),
    # duree entre l'annonce et la fin
    end_lag = difftime(datetime_fin, datetime_pub, units = "days"),
    #  dummy est ce que c'est une annonce apres la fin
    lag_time_end = ifelse(end_lag<0, 1, 0),
    term = ifelse(duration <= 5, 'short', 'long'),
)


# we want to disentangle the different version of the sane message
# build event identifier by retrieving the three letters at end
# 
# Event.ID = substr(Message.ID,18,25)

up_active_IIP_indispo <- up_indispo_IIP_data %>% 
  filter(lag_time_end == 0)

up_active_IIP_indispo %>%
  group_by(Event.Status) %>% 
  summarise(count= n())

# il y a encore quelques indispos au statut inactive ou dismissed
# dans le doute on les enleve

up_very_active_IIP_indispo <- up_active_IIP_indispo %>% 
  filter(Event.Status=='Active')


# supposing for all planned outages there could be another canal
# to learn from unavailabilites


active_unplanned_IIP_indispo <- up_very_active_IIP_indispo  %>% 
  filter( Type.of.Unavailability=='Unplanned')


# look at which announcements could destabilize the market



# Apply the function to each row of the data frame using sapply


new_names <- c(
  # some checks to see if the publication is in relevant time window
  'in_pub_window',
  # last auction before news shock
  'auction_before', 
  # first auction after news shock
  'auction_after', 
  # dummy to indicate that happens during auction bidding period
  'in_auction_time',
  # 1st affected time period
  # nota bene indexing is as such that 
  # the hour from 00:00 to 00:59 is time period 1
  # and consequently from 23:00 to 23:59 is time period 24
  'first_affec_tim_period',
  # last affected time period
  'last_affec_tim_period',
  # dummy to indicate if
  'end_before_pub',
  # ref_day below
  'electricity_delivery_date')

# we try to select short term unavailabilities
# that will potentially have an effect on the auction
# publication between D-1 15h and D 9h

get_auction_numbers <-function(ref_day, start_datetime,
                               end_datetime,pub_datetime){
  # fonction to obtain the two auction numers between which
  # the news shock happen
  
  # print(pub_datetime)
  # print(start_datetime)
  # print(end_datetime)
  # 
  # on remarquera que l'on a mis 0 marges
  # trader process l'information immediatement
  previous_day <- as.character(as.Date(ref_day)-1)
  start_time_2 <- as.POSIXct(paste(previous_day, "17:00:00"), tz = "UTC")
  end_time_2 <- as.POSIXct(paste(previous_day, "17:50:00"), tz = "UTC")
  start_time_3 <- as.POSIXct(paste(previous_day, "21:00:00"), tz = "UTC")
  end_time_3 <- as.POSIXct(paste(previous_day, "21:50:00"), tz = "UTC")
  start_time_4 <- as.POSIXct(paste(ref_day, "01:00:00"), tz = "UTC")
  end_time_4 <- as.POSIXct(paste(ref_day, "01:50:00"), tz = "UTC")
  start_time_5 <- as.POSIXct(paste(ref_day, "04:00:00"), tz = "UTC")
  end_time_5 <- as.POSIXct(paste(ref_day, "04:50:00"), tz = "UTC")
  start_time_6 <- as.POSIXct(paste(ref_day, "09:00:00"), tz = "UTC")
  end_time_6 <- as.POSIXct(paste(ref_day, "09:50:00"), tz = "UTC")
  
  # il faut encore que pour la premiere enchere post news
  # on trade le creneau horaire impacte par l'evenement
  
  first_bid_time_2 <- as.POSIXct(paste(previous_day, "20:00:00"), tz = "UTC")
  first_bid_time_3  <- as.POSIXct(paste(ref_day, "00:00:00"), tz = "UTC")
  first_bid_time_4 <- as.POSIXct(paste(ref_day, "05:00:00"), tz = "UTC")
  first_bid_time_5 <- as.POSIXct(paste(ref_day, "08:00:00"), tz = "UTC")
  first_bid_time_6 <- as.POSIXct(paste(ref_day, "13:00:00"), tz = "UTC")
  
  
  
  in_auction_time <- 0
  auction_before <- 0
  auction_after <- 0
  # just because I can't send back a NULL value
  first_bid_time <- NA
  
  if (pub_datetime< end_time_2){
    if (start_time_2 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 1
    
    # check que la end date est suffisamment loin pour affecter
    # au moins le prix de l'enchere suivante
    if ( first_bid_time_2 > end_datetime || end_datetime <  end_time_2){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_2
    }

  }else if (pub_datetime < end_time_3){
    if (start_time_3 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 2
    if ( first_bid_time_3 > end_datetime || end_datetime <  end_time_3){
      auction_after <- auction_before
    }else{
      first_bid_time <- first_bid_time_3
      auction_after <- auction_before+1
    }
    
  }else if (pub_datetime < end_time_4){
    if (start_time_4 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 3
    if ( first_bid_time_4 > end_datetime || end_datetime <  end_time_4){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_4
    }
    
  }else if (pub_datetime < end_time_5){
    if (start_time_5 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 4
    if (first_bid_time_5 > end_datetime || end_datetime <  end_time_5){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_5
    }
    
  }else{
    auction_before <- 5
    if (first_bid_time_6 > end_datetime || end_datetime <  end_time_6){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_6
    }
  }

  return(c(auction_before, auction_after,
           in_auction_time, hour(max(first_bid_time, start_datetime))+1))
  
}


verify_window <-function(pub_datetime,start_datetime,
                         end_datetime, ref_day, dummy_2_6){
  
  # ref_day <- ref_day_2
  # special pour auction 2_6 qui ne passe pas dans get auctions
  # for auction 2_6 check that the start time is effective before
  # the last hour of the day for which bidding is open
  # that is before midnight
  
  if (dummy_2_6){
    first_bid_time_2 <- as.POSIXct(paste(ref_day, "20:00:00"), tz = "UTC")
    last_bid_time <- as.POSIXct(paste(ref_day, "23:59:00"), tz = "UTC")
    
    if (first_bid_time_2 > end_datetime || start_datetime > last_bid_time){
      return(0)
    }  
  }

  # verify that the window of publication and the start time are correct
  # return a single corresponding dummy
  previous_day <- as.character(as.Date(ref_day)-1)
  # publi post D-1 9h
  # Combine the date part with the desired time
  end_time_1 <- as.POSIXct(paste(previous_day, "15:00:00"), tz = "UTC")
  duration = difftime(pub_datetime, end_time_1,  units = "hours")
  
  
  # publication pre D 9 h 
  start_time_6 <- as.POSIXct(paste(ref_day, "17:50:00"), tz = "UTC")
  duration_2 = difftime( start_time_6, pub_datetime , units = "hours")
  
  
  # start before 23:59h D
  # Convert back to POSIXct
  new_datetime_str <- paste(ref_day, "23:59:00", tz = "UTC")
  duration_3 = difftime(new_datetime_str, start_datetime, units = "hours")
  
  pub_window <- ifelse(duration>0 & duration_2 >0 & duration_3 >0, 1, 0 )
  return(pub_window)
}

plages_horaires <- function(pub_datetime,start_datetime, end_datetime, ref_day){
  # add a dummy to check whether the interruption 
  # is not to short to do anything
  # typically ends before the first new auction
  
  # start_datetime = as.POSIXct(as.character(start_datetime$datetime_debut),  tz = "UTC")
  # pub_datetime = as.POSIXct(as.character(pub_datetime$datetime_pub),  tz = "UTC")
  # end_datetime = as.POSIXct(as.character(end_datetime$datetime_fin),  tz = "UTC")

  # 
  # le premier creneau affecte c'est celui apres strat time
  # pour peu qu'on soit informe
  

  token <- 'Normal: publi before end of incident'
  # affecte le marche, si on est au courant
  # avant minuit
  # avant la cloture de l'evenement
  new_datetime_str <- as.POSIXct(paste(ref_day, "23:59:00"), tz = "UTC")
  if (end_datetime< pub_datetime){
    token <- 'Lagged : publi after end of incident'
  }else{
    max_time <- min(end_datetime, new_datetime_str)
    # max_time <- as.POSIXct('2021-02-25 00:30:00')
    last_hour <- as.numeric(format(max_time, "%H"))
    minutes <- as.numeric(format(max_time, "%M"))
    seconds <- as.numeric(format(max_time, "%S"))
    
    # Adjust the hour only if minutes or seconds are not zero
    if (minutes > 0 || seconds > 0) {
      last_hour <- last_hour + 1
    }
  }
  
  return(c(last_hour, token))
}



pub_auction_window <- function(row){
  
  # extrait les informations
  # publication post D 15h 
  pub_datetime <- row['datetime_pub']
  
  pub_datetime <- as.POSIXct(pub_datetime,  tz = "UTC")
  # 
  # Given POSIXct date-time
  start_datetime <- row['datetime_debut']
  start_datetime <- as.POSIXct(start_datetime,  tz = "UTC")
  
  end_datetime<- row['datetime_fin']
  end_datetime <- as.POSIXct(end_datetime,  tz = "UTC")
  # 
  # look at the different cases depending on the time of pub
  # which market day could it have an impact on
  
  date_pub <- format(pub_datetime, "%Y-%m-%d")
  # Convert the date part to a Date object
  date_obj <- as.Date(date_pub)

  end_time_1 <- as.POSIXct(paste(date_pub, "15:00:00"),  tz = "UTC")

  end_time_2 <- as.POSIXct(paste(date_pub, "17:50:00"), tz = "UTC")

  end_time_6 <- as.POSIXct(paste(date_pub, "09:50:00"), tz = "UTC")
  # earlier than 9 o'clock
  # impact on the last auctions of publication day
  # with electricity delivery today
  
  if (pub_datetime < end_time_6){
    # ref_day is defined as the day of elec delivery
    ref_day <- date_pub
  } else if (pub_datetime > end_time_2 ){
    # ref_day is defined as the day of elec delivery
    ref_day <- as.character(date_obj + 1)
  } else{
    
    # on dedouble ce news shock parce qu'il
    # a des effets potentiels sur 2 marches distincts
    # le marche pour une livraison aujourd'hui
    # le marche pour livraison demain
    # ces shocks sont correles et meritent un traitement statistique differencie
    
    # keep the normal looking to the following day
    ref_day <- as.character(date_obj + 1)
    
    # handle separately the possibility to affect
    # prices for today
    # revealed by auctions 2 and 6
    ref_day_2 <- date_pub
    
    # more details on the two functions in description or below in main script
    window_dummy <- verify_window(pub_datetime, start_datetime,
                                  end_datetime, ref_day_2, 1)
    
    hours_affected <- plages_horaires(pub_datetime,start_datetime, 
                                      end_datetime, ref_day_2)
  
    auctions <- c(6, 2,
     # in auction time check for auction 6
      as.POSIXct(paste(ref_day_2, "17:50:00"), tz = "UTC")< pub_datetime,
     21)
    
    
    new_times <- c(window_dummy, auctions, hours_affected, ref_day_2)
    names(new_times) <- new_names
    for (name in names(new_times)) {
      row[[name]] <- new_times[[name]]
    }
    
    row_dfa <- list(row)
    register_auction_2_6 <<- ifelse(is.null(register_auction_2_6),
         as.data.frame(row_dfa) ,
          rbind(register_auction_2_6, as.data.frame(row_dfa)))

  }
  
  
  
  window_dummy <- verify_window(pub_datetime,start_datetime,
                                end_datetime, ref_day, 0)
  
  auctions <- get_auction_numbers(ref_day, start_datetime,
                                  end_datetime, pub_datetime)
  
  # what are the times subject to auctions
  # that could be affected bounded by start and end
  hours_affected <- plages_horaires(pub_datetime,start_datetime, 
                  end_datetime, ref_day)
  


  result <- c(window_dummy, auctions, hours_affected, ref_day)
  return(result)
  
}  


# test <- active_unplanned_IIP_indispo[1:100, ]
# result_df <- apply(test, 1, pub_auction_window)
# row <- test[4,]

# add a dummy to check that startime is in the vicinity of ref_day
# and not a month before


register_auction_2_6 <- NULL

result_df <- apply(active_unplanned_IIP_indispo, 1, pub_auction_window)

# Convert the result to a data frame
result_df <- data.frame(t(result_df))

# Bind the result as new columns to the original data frame
final_active_unplanned_IIP_indispo <- 
  cbind(active_unplanned_IIP_indispo, result_df)
# Get the number of columns in the data frame
num_cols <- ncol(final_active_unplanned_IIP_indispo)
names(final_active_unplanned_IIP_indispo)[(num_cols-7):num_cols] <- new_names




# on s'occupe des doublons sur les marches differenciees hier et aujourd'hui

# Transformer le vecteur en une matrice de 32 lignes et 12 colonnes
auction_2_6 <- matrix(register_auction_2_6[[1]], nrow = 33)
# Convertir la matrice en data frame
auction_2_6 <- as.data.frame(t(auction_2_6))
colnames(auction_2_6) <- colnames(final_active_unplanned_IIP_indispo)

final_active_unplanned_IIP_indispo <-rbind(final_active_unplanned_IIP_indispo,
                                           auction_2_6)


final_active_unplanned_IIP_indispo <- final_active_unplanned_IIP_indispo %>% 
  filter(in_pub_window==1)

pure_indispo_data <- final_active_unplanned_IIP_indispo  %>% 
  filter(auction_before!=auction_after)

write_parquet(pure_indispo_data, 'pure_indispo_data.parquet')





