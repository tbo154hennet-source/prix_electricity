


# Apply the function to each row of the data frame using sapply


new_names_pub <- c(
  #unique identifier
  'mrid',
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
  #ref_day <- ref_day_2
  # print(pub_datetime)
  # print(start_datetime)
  # print(end_datetime)
  # 
  #ref_day <- ref_day_2
  # on remarquera que l'on a mis 0 marges
  # trader process l'information immediatement
  previous_day <- as.character(as.Date(ref_day)-1)
  
  
  pre_bid_time <- as.POSIXct(paste(previous_day, "12:00:00"), tz='Europe/Brussels')
  start_time_1 <- as.POSIXct(paste(previous_day, "14:00:00"), tz='Europe/Brussels')
  end_time_1 <- as.POSIXct(paste(previous_day, "15:00:00"), tz='Europe/Brussels')
  start_time_2 <- as.POSIXct(paste(previous_day, "17:00:00"), tz='Europe/Brussels')
  end_time_2 <- as.POSIXct(paste(previous_day, "17:50:00"), tz='Europe/Brussels')
  start_time_3 <- as.POSIXct(paste(previous_day, "21:00:00"), tz='Europe/Brussels')
  end_time_3 <- as.POSIXct(paste(previous_day, "21:50:00"), tz='Europe/Brussels')
  start_time_4 <- as.POSIXct(paste(ref_day, "01:00:00"), tz='Europe/Brussels')
  end_time_4 <- as.POSIXct(paste(ref_day, "01:50:00"), tz='Europe/Brussels')
  start_time_5 <- as.POSIXct(paste(ref_day, "04:00:00"), tz='Europe/Brussels')
  end_time_5 <- as.POSIXct(paste(ref_day, "04:50:00"), tz='Europe/Brussels')
  start_time_6 <- as.POSIXct(paste(ref_day, "09:00:00"), tz='Europe/Brussels')
  end_time_6 <- as.POSIXct(paste(ref_day, "09:50:00"), tz='Europe/Brussels')
  start_time_7 <- as.POSIXct(paste(ref_day, "17:00:00"), tz='Europe/Brussels')
  end_time_7 <- as.POSIXct(paste(ref_day, "17:50:00"), tz='Europe/Brussels')
  # il faut encore que pour la premiere enchere post news
  # on trade le creneau horaire impacte par l'evenement
  
  first_bid_time_1 <- as.POSIXct(paste(ref_day, "00:00:00"), tz='Europe/Brussels')
  #first_bid_time_1 = first_bid_time_2
  first_bid_time_2 <- as.POSIXct(paste(ref_day, "00:00:00"), tz='Europe/Brussels')
  #first_bid_time_1 = first_bid_time_3
  first_bid_time_3  <- as.POSIXct(paste(ref_day, "00:00:00"), tz='Europe/Brussels')
  first_bid_time_4 <- as.POSIXct(paste(ref_day, "05:00:00"), tz='Europe/Brussels')
  first_bid_time_5 <- as.POSIXct(paste(ref_day, "08:00:00"), tz='Europe/Brussels')
  first_bid_time_6 <- as.POSIXct(paste(ref_day, "13:00:00"), tz='Europe/Brussels')
  first_bid_time_7 <- as.POSIXct(paste(ref_day, "21:00:00"), tz='Europe/Brussels')
  
  
  
  in_auction_time <- 0
  auction_before <- 0
  auction_after <- 0
  # just because I can't send back a NULL value
  first_bid_time <- NA
  
  if (pub_datetime< end_time_1 & start_datetime < end_time_1 & pub_datetime > pre_bid_time ){
    
    if (start_time_1 < pub_datetime){
      in_auction_time <- 1
    }
    auction_after <- 1
    
    # check que la end date est suffisamment loin pour affecter
    # au moins le prix de l'enchere suivante
    if ( first_bid_time_1 > end_datetime || end_datetime <  end_time_1){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_1
    }
    
  }else if (pub_datetime < end_time_2 & start_datetime < end_time_2 & pub_datetime > pre_bid_time){
    if (start_time_2 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 1
    if ( first_bid_time_2 > end_datetime || end_datetime <  end_time_2){
      auction_after <- auction_before
    }else{
      first_bid_time <- first_bid_time_2
      auction_after <- auction_before+1
    }
    
  }else if (pub_datetime < end_time_3 & start_datetime < end_time_3 & pub_datetime > pre_bid_time){
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
    
  }else if (pub_datetime < end_time_4 & start_datetime < end_time_4 & pub_datetime > pre_bid_time){
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
    
  }else if (pub_datetime < end_time_5 & start_datetime < end_time_5 & pub_datetime > pre_bid_time){
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
    
  }else if(pub_datetime < end_time_6 & start_datetime < end_time_6 & pub_datetime > pre_bid_time){
    if (start_time_6 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 5
    if (first_bid_time_6 > end_datetime || end_datetime <  end_time_6){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_6
    }
  }else if(pub_datetime < end_time_7 & start_datetime < end_time_7 & pub_datetime > pre_bid_time){
      auction_before <- 6
      if (first_bid_time_7 > end_datetime || end_datetime <  end_time_7){
        auction_after <- auction_before
      }else{
        auction_after <- auction_before+1
        first_bid_time <- first_bid_time_7
      }
  }
  first_affect_time_period <- ifelse(!is.na(first_bid_time), hour(max(first_bid_time, start_datetime))+1, NA)
  return(c(auction_before, auction_after,
           in_auction_time, first_affect_time_period))
  
}


verify_window <-function(pub_datetime,start_datetime,
                         end_datetime, ref_day, dummy_2_6){
  
  # ref_day <- ref_day_2
  # special pour auction 2_6 qui ne passe pas dans get auctions
  # for auction 2_6 check that the start time is effective before
  # the last hour of the day for which bidding is open
  # that is before midnight
  
  if (dummy_2_6){
    first_bid_time_2 <- as.POSIXct(paste(ref_day, "20:00:00"), tz='Europe/Brussels')
    last_bid_time <- as.POSIXct(paste(ref_day, "23:59:00"), tz='Europe/Brussels')
    
    if (first_bid_time_2 > end_datetime || start_datetime > last_bid_time){
      return(0)
    }  
  }
  
  # verify that the window of publication and the start time are correct
  # return a single corresponding dummy
  previous_day <- as.character(as.Date(ref_day)-1)
  # publi post D-1 9h
  # Combine the date part with the desired time
  end_time_1 <- as.POSIXct(paste(previous_day, "15:00:00"), tz='Europe/Brussels')
  duration = difftime(pub_datetime, end_time_1,  units = "hours")
  
  
  # publication pre D 9 h 
  start_time_6 <- as.POSIXct(paste(ref_day, "17:50:00"), tz='Europe/Brussels')
  duration_2 = difftime( start_time_6, pub_datetime , units = "hours")
  
  
  # start before 23:59h D
  # Convert back to POSIXct
  new_datetime_str <- paste(ref_day, "23:59:00", tz='Europe/Brussels')
  duration_3 = difftime(new_datetime_str, start_datetime, units = "hours")
  
  pub_window <- ifelse(duration>0 & duration_2 >0 & duration_3 >0, 1, 0 )
  return(pub_window)
}

plages_horaires <- function(pub_datetime,start_datetime, end_datetime, ref_day){
  # add a dummy to check whether the interruption 
  # is not to short to do anything
  # typically ends before the first new auction
  
  # start_datetime = as.POSIXct(as.character(start_datetime$datetime_debut),  tz = 'Europe/Brussels')
  # pub_datetime = as.POSIXct(as.character(pub_datetime$datetime_pub),  tz = 'Europe/Brussels')
  # end_datetime = as.POSIXct(as.character(end_datetime$datetime_fin),  tz = 'Europe/Brussels')
  
  
  # 
  # le premier creneau affecte c'est celui apres strat time
  # pour peu qu'on soit informe
  
  
  token <- 'Normal: publi before end of incident'
  # affecte le marche, si on est au courant
  # avant minuit
  # avant la cloture de l'evenement
  new_datetime_str <- as.POSIXct(paste(ref_day, "23:59:00"), tz='Europe/Brussels')
  if (end_datetime< pub_datetime){
    token <- 'Lagged : publi after end of incident'
    last_hour <- as.numeric(format(end_datetime, "%H"))
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
  # Given POSIXct date-time
  start_datetime <- row['datetime_debut']
  end_datetime<- row['datetime_fin']
  mrid <- row['mrid']
  
  # pub_datetime <- pub_datetime[[1]]
  # start_datetime <- start_datetime[[1]]
  # end_datetime <- end_datetime[[1]]

  pub_datetime <- as.POSIXct(pub_datetime,  tz = 'Europe/Brussels')
  start_datetime <- as.POSIXct(start_datetime,  tz = 'Europe/Brussels')
  end_datetime <- as.POSIXct(end_datetime,  tz = 'Europe/Brussels')
  # 
  # look at the different cases depending on the time of pub
  # which market day could it have an impact on
  date_pub <- format(pub_datetime, "%Y-%m-%d")
  # Convert the date part to a Date object
  date_obj <- as.Date(date_pub)
  
  #end time pour next day impact
  end_time_next_day <- as.POSIXct(paste(date_pub, "12:00:00"),  tz = 'Europe/Brussels')
  #end time pour same day impact
  end_time_same_day <- as.POSIXct(paste(date_pub, "17:50:00"), tz='Europe/Brussels')
  
  #end_time_6 <- as.POSIXct(paste(date_pub, "09:50:00"), tz='Europe/Brussels')
  # earlier than 9 o'clock
  # impact on the last auctions of publication day
  # with electricity delivery today

  ref_day <- date_pub
  if (pub_datetime < end_time_same_day){
    
    status <- "Published"
    
    # ref_day is defined as the day of elec delivery
  
    auctions <- get_auction_numbers(ref_day, start_datetime,
                                    end_datetime, pub_datetime)
    
    # what are the times subject to auctions
    # that could be affected bounded by start and end
    hours_affected <- plages_horaires(pub_datetime,start_datetime, 
                                      end_datetime, ref_day)
    
    
    
    result <- data.frame(t(c(mrid, auctions, hours_affected, ref_day)))
    colnames(result) <- new_names_pub
    if (!exists("result_df") || is.null(result_df)) {
      result_df <<- result
    } else {
      result_df <<- bind_rows(result_df, result)
    }
    
    
  }else{
    
    status <- "Unpublished"
    
    # ref_day is defined as the day of elec delivery
    
    auctions <- get_auction_numbers(ref_day, start_datetime,
                                    end_datetime, pub_datetime)
    
    hours_affected <- plages_horaires(pub_datetime,start_datetime, 
                                      end_datetime, ref_day)
    
    result <- data.frame(t(c(mrid, auctions, hours_affected, ref_day)))
    colnames(result) <- new_names_pub
    if (!exists("result_df") || is.null(result_df)) {
      result_df <<- result
    } else {
      result_df <<- bind_rows(result_df, result)
    }
    
  }
  
  # keep the normal looking to the following day
  ref_day_2 <- as.character(date_obj + 1)
  # ref_day is defined as the day of elec delivery
  
  if (pub_datetime > end_time_next_day){
    
    status <- "Published"
    
    # ref_day is defined as the day of elec delivery
    
    hours_affected_2 <- plages_horaires(pub_datetime,start_datetime, 
                                        end_datetime, ref_day_2)
    auctions_2 <- get_auction_numbers(ref_day_2, start_datetime,
                                      end_datetime, pub_datetime)
    
    result_2 <- data.frame(t(c(mrid, auctions_2, hours_affected_2, ref_day_2)))
    colnames(result_2) <- new_names_pub
    if (!exists("result_df") || is.null(result_df)) {
      result_df <<- result_2
    } else {
      result_df <<- bind_rows(result_df, result_2)
    }
    
    
  }else{
    
    status <- "Unpublished"

    
    
    hours_affected_2 <- plages_horaires(pub_datetime,start_datetime, 
                                        end_datetime, ref_day_2)
    auctions_2 <- get_auction_numbers(ref_day_2, start_datetime,
                                      end_datetime, pub_datetime)
    
    result_2 <- data.frame(t(c(mrid, auctions_2, hours_affected_2, ref_day_2)))
    colnames(result_2) <- new_names_pub
    if (!exists("result_df") || is.null(result_df)) {
      result_df <<- result_2
    } else {
      result_df <<- bind_rows(result_df, result_2)
    }
    
  }
  
  
}  



new_names_theoretical <- c(
  #unique identifier
  'mrid',
  # last auction before unavailability
  'auction_before', 
  # first auction after unavailability
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
  # ref_day below
  'electricity_delivery_date')


get_auction_numbers_theoretical <-function(ref_day, start_datetime,
                                           end_datetime){
  # fonction to obtain the two auction numers between which
  # the news shock happen
  
  
  # print(start_datetime)
  # print(end_datetime)
  # 
  # on remarquera que l'on a mis 0 marges
  # trader process l'information immediatement
  previous_day <- as.character(as.Date(ref_day)-1)
  
  
  pre_bid_time <- as.POSIXct(paste(previous_day, "12:00:00"), tz='Europe/Brussels')
  start_time_1 <- as.POSIXct(paste(previous_day, "14:00:00"), tz='Europe/Brussels')
  end_time_1 <- as.POSIXct(paste(previous_day, "15:00:00"), tz='Europe/Brussels')
  start_time_2 <- as.POSIXct(paste(previous_day, "17:00:00"), tz='Europe/Brussels')
  end_time_2 <- as.POSIXct(paste(previous_day, "17:50:00"), tz='Europe/Brussels')
  start_time_3 <- as.POSIXct(paste(previous_day, "21:00:00"), tz='Europe/Brussels')
  end_time_3 <- as.POSIXct(paste(previous_day, "21:50:00"), tz='Europe/Brussels')
  start_time_4 <- as.POSIXct(paste(ref_day, "01:00:00"), tz='Europe/Brussels')
  end_time_4 <- as.POSIXct(paste(ref_day, "01:50:00"), tz='Europe/Brussels')
  start_time_5 <- as.POSIXct(paste(ref_day, "04:00:00"), tz='Europe/Brussels')
  end_time_5 <- as.POSIXct(paste(ref_day, "04:50:00"), tz='Europe/Brussels')
  start_time_6 <- as.POSIXct(paste(ref_day, "09:00:00"), tz='Europe/Brussels')
  end_time_6 <- as.POSIXct(paste(ref_day, "09:50:00"), tz='Europe/Brussels')
  
  start_time_7 <- as.POSIXct(paste(ref_day, "17:00:00"), tz='Europe/Brussels')
  end_time_7 <- as.POSIXct(paste(ref_day, "17:50:00"), tz='Europe/Brussels')
  # il faut encore que pour la premiere enchere post news
  # on trade le creneau horaire impacte par l'evenement
  
  first_bid_time_1 <- as.POSIXct(paste(ref_day, "00:00:00"), tz='Europe/Brussels')
  #firdt_bid_time_1 = first_bid_time_2
  first_bid_time_2 <- as.POSIXct(paste(ref_day, "00:00:00"), tz='Europe/Brussels')
  #first_bid_time_1 = first_bid_time_3
  first_bid_time_3  <- as.POSIXct(paste(ref_day, "00:00:00"), tz='Europe/Brussels')
  first_bid_time_4 <- as.POSIXct(paste(ref_day, "05:00:00"), tz='Europe/Brussels')
  first_bid_time_5 <- as.POSIXct(paste(ref_day, "08:00:00"), tz='Europe/Brussels')
  first_bid_time_6 <- as.POSIXct(paste(ref_day, "13:00:00"), tz='Europe/Brussels')
  first_bid_time_7 <- as.POSIXct(paste(ref_day, "21:00:00"), tz='Europe/Brussels')
  
  in_auction_time <- 0
  auction_before <- 0
  auction_after <- 0
  # just because I can't send back a NULL value
  first_bid_time <- NA
  
  if (start_datetime< end_time_1 & start_datetime > pre_bid_time){
    
    if (start_time_1 < start_datetime){
      in_auction_time <- 1
    }
    auction_after <- 1
    
    # check que la end date est suffisamment loin pour affecter
    # au moins le prix de l'enchere suivante
    if ( first_bid_time_1 > end_datetime || end_datetime <  end_time_1){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_1
    }
    
  }else if (start_datetime < end_time_2 & start_datetime > pre_bid_time){
    if (start_time_2 < start_datetime){
      in_auction_time <- 1
    }
    auction_before <- 1
    if ( first_bid_time_2 > end_datetime || end_datetime <  end_time_2){
      auction_after <- auction_before
    }else{
      first_bid_time <- first_bid_time_2
      auction_after <- auction_before+1
    }
    
  }else if (start_datetime < end_time_3 & start_datetime > pre_bid_time){
    if (start_time_3 < start_datetime){
      in_auction_time <- 1
    }
    auction_before <- 2
    if ( first_bid_time_3 > end_datetime || end_datetime <  end_time_3){
      auction_after <- auction_before
    }else{
      first_bid_time <- first_bid_time_3
      auction_after <- auction_before+1
    }
    
  }else if (start_datetime < end_time_4 & start_datetime > pre_bid_time){
    if (start_time_4 < start_datetime){
      in_auction_time <- 1
    }
    auction_before <- 3
    if ( first_bid_time_4 > end_datetime || end_datetime <  end_time_4){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_4
    }
    
  }else if (start_datetime < end_time_5 & start_datetime > pre_bid_time){
    if (start_time_5 < start_datetime){
      in_auction_time <- 1
    }
    auction_before <- 4
    if (first_bid_time_5 > end_datetime || end_datetime <  end_time_5){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_5
    }
    
  }else if(start_datetime < end_time_6 & start_datetime > pre_bid_time){
    if (start_time_6 < start_datetime){
      in_auction_time <- 1
    }
    auction_before <- 5
    if (first_bid_time_6 > end_datetime || end_datetime <  end_time_6){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_6
    }
  }else if(start_datetime < end_time_7 & start_datetime > pre_bid_time){
    auction_before <- 6
    if (first_bid_time_7 > end_datetime || end_datetime <  end_time_7){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      first_bid_time <- first_bid_time_7
    }
  }
  first_affect_time_period <- ifelse(!is.na(first_bid_time), hour(max(first_bid_time, start_datetime))+1, NA)
  return(c(auction_before, auction_after,
           in_auction_time, first_affect_time_period))
  
}




verify_window_theoretical <-function(start_datetime,
                                     end_datetime, ref_day, dummy_2_6){
  
  # ref_day <- ref_day_2
  # special pour auction 2_6 qui ne passe pas dans get auctions
  # for auction 2_6 check that the start time is effective before
  # the last hour of the day for which bidding is open
  # that is before midnight
  
  if (dummy_2_6){
    first_bid_time_2 <- as.POSIXct(paste(ref_day, "20:00:00"), tz='Europe/Brussels')
    last_bid_time <- as.POSIXct(paste(ref_day, "23:59:00"), tz='Europe/Brussels')
    
    if (first_bid_time_2 > end_datetime || start_datetime > last_bid_time){
      return(0)
    }  
  }
  
  # verify that the window of publication and the start time are correct
  # return a single corresponding dummy
  previous_day <- as.character((ref_day)-1)
  # publi post D-1 9h
  # Combine the date part with the desired time
  end_time_1 <- as.POSIXct(paste(previous_day, "15:00:00"), tz='Europe/Brussels')
  duration = difftime(start_datetime, end_time_1,  units = "hours")
  
  
  # publication pre D 9 h 
  start_time_6 <- as.POSIXct(paste(ref_day, "17:50:00"), tz='Europe/Brussels')
  duration_2 = difftime( start_time_6, start_datetime , units = "hours")
  
  
  # start before 23:59h D
  # Convert back to POSIXct
  new_datetime_str <- paste(ref_day, "23:59:00", tz='Europe/Brussels')
  duration_3 = difftime(new_datetime_str, start_datetime, units = "hours")
  
  pub_window <- ifelse(duration>0 & duration_2 >0 & duration_3 >0, 1, 0 )
  return(pub_window)
}



plages_horaires_theoretical <- function(start_datetime, end_datetime, ref_day){
  # add a dummy to check whether the interruption 
  # is not to short to do anything
  # typically ends before the first new auction
  
  # start_datetime = as.POSIXct(as.character(start_datetime$datetime_debut),  tz = 'Europe/Brussels')
  # pub_datetime = as.POSIXct(as.character(pub_datetime$datetime_pub),  tz = 'Europe/Brussels')
  # end_datetime = as.POSIXct(as.character(end_datetime$datetime_fin),  tz = 'Europe/Brussels')
  
  # 
  # le premier creneau affecte c'est celui apres strat time
  # pour peu qu'on soit informe
  new_datetime_str <- paste(ref_day, "23:59:00", tz='Europe/Brussels')
  max_time <- min(end_datetime, new_datetime_str)
  # max_time <- as.POSIXct('2021-02-25 00:30:00')
  last_hour <- as.numeric(format(max_time, "%H"))
  minutes <- as.numeric(format(max_time, "%M"))
  seconds <- as.numeric(format(max_time, "%S"))
  
  # Adjust the hour only if minutes or seconds are not zero
  if (minutes > 0 || seconds > 0) {
    last_hour <- last_hour + 1
  }
  return(last_hour)
}



pub_auction_window_theoretical <- function(row){
  
  #extrait les informations
  # publication post D 15h 
  pub_datetime <- row['datetime_pub']
  start_datetime <- row['datetime_debut']
  end_datetime<- row['datetime_fin']
  mrid <- row['mrid']
  
  pub_datetime <- as.POSIXct(pub_datetime,  tz = 'Europe/Brussels')
  start_datetime <- as.POSIXct(start_datetime,  tz = 'Europe/Brussels')
  end_datetime <- as.POSIXct(end_datetime,  tz = 'Europe/Brussels')
  
  
  
  # look at the different cases depending on the time of pub
  # which market day could it have an impact on
  
  date_pub <- format(pub_datetime, "%Y-%m-%d")
  # Convert the date part to a Date object
  date_obj <- as.Date(date_pub)
  
  #end time pour next day impact
  end_time_next_day <- as.POSIXct(paste(date_pub, "12:00:00"),  tz = 'Europe/Brussels')
  #end time pour same day impact
  end_time_same_day <- as.POSIXct(paste(date_pub, "17:50:00"), tz='Europe/Brussels')
  
  #end_time_6 <- as.POSIXct(paste(date_pub, "09:50:00"), tz='Europe/Brussels')
  # earlier than 9 o'clock
  # impact on the last auctions of publication day
  # with electricity delivery today
  
  if (start_datetime < end_time_same_day){
    # ref_day is defined as the day of elec delivery
    status <- "Published"
    ref_day <- date_pub
    
    auctions <- get_auction_numbers_theoretical(ref_day, start_datetime,
                                    end_datetime)
    
    # what are the times subject to auctions
    # that could be affected bounded by start and end
    hours_affected <- plages_horaires_theoretical(start_datetime, 
                                      end_datetime, ref_day)
    
    
    
    result <- data.frame(t(c(mrid, auctions, hours_affected, ref_day)))
    colnames(result) <- new_names_theoretical
    if (!exists("result_df") || is.null(result_df)) {
      result_df <<- result
    } else {
      result_df <<- bind_rows(result_df, result)
    }
    
    
  }
  
  
  
  if (start_datetime > end_time_next_day ){
    # ref_day is defined as the day of elec delivery
    
    # on dedouble ce news shock parce qu'il
    # a des effets potentiels sur 2 marches distincts
    # le marche pour une livraison aujourd'hui
    # le marche pour livraison demain
    # ces shocks sont correles et meritent un traitement statistique differencie
    
    # keep the normal looking to the following day
    ref_day_2 <- as.character(date_obj + 1)
    
    # more details on the two functions in description or below in main script
    
    
    hours_affected_2 <- plages_horaires_theoretical(start_datetime, 
                                        end_datetime, ref_day_2)
    auctions_2 <- get_auction_numbers_theoretical(ref_day_2, start_datetime,
                                      end_datetime)
    
    result_2 <- data.frame(t(c(mrid, auctions_2, hours_affected_2, ref_day_2)))
    colnames(result_2) <- new_names_theoretical
    if (!exists("result_df") || is.null(result_df)) {
      result_df <<- result_2
    } else {
      result_df <<- bind_rows(result_df, result_2)
    }
    
  }
  
}  



