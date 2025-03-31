


# Apply the function to each row of the data frame using sapply


uneffective_names_pub <- c(
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
  # ref_day below
  'electricity_delivery_date')

# we try to select short term unavailabilities
# that will potentially have an effect on the auction
# publication between D-1 15h and D 9h

get_auction_numbers_uneffective <-function(ref_day, start_datetime,
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
  start_time_8 <- as.POSIXct(paste(ref_day, "21:50:00"), tz='Europe/Brussels')
  end_time_8 <- as.POSIXct(paste(ref_day, "23:59:00"), tz='Europe/Brussels')
  # il faut encore que pour la premiere enchere post news
  # on trade le creneau horaire impacte par l'evenement
  

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
  
  if (ref_datetime< pre_bid_time  ){
  }else if (ref_datetime < end_time_4 & ref_datetime > pre_bid_time){
    if (start_time_4 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 2
    if ( first_bid_time_4 > end_datetime || end_datetime <  end_time_4){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      last_bid_time <- first_bid_time_4
      first_bid_time <- ceiling_date(end_time_4, unit = "hour")
    }
    
  }else if (ref_datetime < end_time_5 & ref_datetime > pre_bid_time){
    if (start_time_5 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 3
    if (first_bid_time_5 > end_datetime || end_datetime <  end_time_5){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      last_bid_time <- first_bid_time_5
      first_bid_time <- ceiling_date(end_time_5, unit = "hour")
    }
    
  }else if(ref_datetime < end_time_6 & ref_datetime > pre_bid_time){
    if (start_time_6 < pub_datetime){
      in_auction_time <- 1
    }
    auction_before <- 4
    if (first_bid_time_6 > end_datetime || end_datetime <  end_time_6){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      last_bid_time <- first_bid_time_6
      first_bid_time <- ceiling_date(end_time_6, unit = "hour")
    }
  }else if(ref_datetime < end_time_7 & ref_datetime > pre_bid_time){
      auction_before <- 5
      if (first_bid_time_7 > end_datetime || end_datetime <  end_time_7){
        auction_after <- auction_before
      }else{
        auction_after <- auction_before+1
        last_bid_time <- first_bid_time_7
        first_bid_time <- ceiling_date(end_time_7, unit = "hour")
      }
  }else if(ref_datetime < end_time_8 & ref_datetime > pre_bid_time){
    auction_before <- 6
    if (first_bid_time_8 > end_datetime || end_datetime <  end_time_8){
      auction_after <- auction_before
    }else{
      auction_after <- auction_before+1
      last_bid_time <- first_bid_time_8
      first_bid_time <- ceiling_date(end_time_8, unit = "hour")
  }
  }

  first_affect_time_period <- ifelse(!is.na(first_bid_time), hour(max(first_bid_time, ref_datetime))+1, NA)
  
  last_affect_time_period <- ifelse(!is.na(last_bid_time), hour(max(last_bid_time, start_datetime)), NA)
  
  
  return(c(auction_before, auction_after,
           in_auction_time, first_affect_time_period,  last_affect_time_period))
  
}





pub_auction_window_uneffective <- function(row){
  
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
  
  ref_datetime <- min(start_datetime, pub_datetime)
  
  # look at the different cases depending on the time of pub
  # which market day could it have an impact on
  date_pub <- format(ref_datetime, "%Y-%m-%d")
  # Convert the date part to a Date object
  date_obj <- as.Date(date_pub)
  
  
  #before the end of the day ahead market
  
  #end time pour next day impact
  end_time_da <- as.POSIXct(paste(date_pub, "12:00:00"),  tz = 'Europe/Brussels')
  
  
  #after the end of the last auction for this datetime slot
  #end time pour next day impact
  end_time_next_day <- as.POSIXct(paste(date_pub, "21:50:00"),  tz = 'Europe/Brussels')
  
  
  
  if (ref_datetime < end_time_next_day ){
    ref_day <- date_pub
  }else{
    ref_day <-  as.character(date_obj + 1)
  }
  
  
  auctions <- get_auction_numbers_uneffective(ref_day, start_datetime,
                                  end_datetime, pub_datetime)
  
  # what are the times subject to auctions
  # that could be affected bounded by start and end
  hours_affected <- plages_horaires(pub_datetime,start_datetime, 
                                                end_datetime, ref_day)
  

  
  
  result <- data.frame(t(c(mrid, auctions, hours_affected, ref_day)))
  colnames(result) <- uneffective_names_pub
  if (!exists("result_df") || is.null(result_df)) {
    result_df <<- result
  } else {
    result_df <<- bind_rows(result_df, result)
  }
  
  
  
  
}  

