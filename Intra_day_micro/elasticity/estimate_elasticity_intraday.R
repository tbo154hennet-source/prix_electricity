library(data.table)

setwd("Z:/code_supply_demand/code_intraday")
#reconstruct_full_curve_shift_supply <- read_parquet( 'reconstruct_full_curve_shift_qty_intraday_supply_unmatched.parquet')
reconstruct_full_curve_shift_supply_matched <- as.data.table(read_parquet( 'reconstruct_full_curve_shift_qty_intraday_supply_matched.parquet')
)
# rm(reconstruct_full_curve_shift_supply_matched)
# gc()
#reconstruct_full_curve_shift_supply_matched <-  reconstruct_full_curve_shift_supply_matched)
#test <- reconstruct_full_curve_shift_supply_matched[1:10000,]
#test <- reconstruct_full_curve_shift_supply_matched[DATE == as.Date("2021-09-27")]


# R?sum? avec groupement et filtrage
auction_summary <- reconstruct_full_curve_shift_supply_matched[
  , .SD[QTY == max(QTY, na.rm = TRUE)][which.min(PRICE)], 
  by = .(Hour, DATE, AUCTION)
]

write_parquet(auction_summary, 'auction_summary_intraday.parquet')

setwd("Z:/code_supply_demand/code_intraday")
auction_summary <-  read_parquet( 'auction_summary_intraday.parquet')
#get day ahead exchanged quantity
auction_summary <- auction_summary %>%
  group_by(DATE, Hour) %>%
  mutate(gen_exch_da = ifelse(any(AUCTION == 0),
                              QTY[AUCTION == 0][1], # take the QTY from auction 0
                              NA_real_)) %>%
  ungroup()


#estimate elasticity

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_entsoe_output)
#write_parquet(first_stage_data_clean, "first_stage_data_clean")

first_stage_data <- read_parquet( "first_stage_data_OMIE.parquet")


colnames_with_capacity <- colnames(first_stage_data)[grepl("Capacity", colnames(first_stage_data))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables")

first_stage_data[colnames_with_capacity] <- lapply(first_stage_data[colnames_with_capacity], function(x) ifelse(is.na(x), 0, x))


#adjuster l'ordre de grandeur des indispos pour ne pas avoir des coefficients nuls
first_stage_data_clean <- first_stage_data%>%
  filter(is.finite(log_price_diff) & 
           is.finite(Published.Unavailable.Capacity))%>% 
  mutate(across(all_of(capacity_fields), 
                ~ . / 1000)) %>% 
  mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
         # Published.Dismissed.Unavailable.Capacity = Published.Dismissed.Unavailable.Capacity - Published.Dismissed.Unavailable.Nuclear.Capacity,
         Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
         # Unpublished.Dismissed.Unavailable.Capacity = Unpublished.Dismissed.Unavailable.Capacity - Unpublished.Dismissed.Unavailable.Nuclear.Capacity
  )%>% 
  rename(day_ahead_renewable_forecast= day_ahead_forecast)

# Créer une variable factorielle pour clusteriser
first_stage_data_clean <- first_stage_data_clean %>% 
  mutate(cluster_var = interaction(auction_date_plus_one, auction))

non_finite_rows <- first_stage_data %>%
  filter(!is.finite(log_price_diff) | !is.finite(Published.Unavailable.Capacity))

first_stage_data_clean$weekday <- weekdays(first_stage_data_clean$delivery_date)


colnames(first_stage_data_clean)




#nota bene pas la day-ahead auction
# first_stage_data_clean %>% 
#   group_by(auction) %>% 
#   summarise(n = n())

variables <- c("day_ahead_renewable_forecast"    ,"intraday_forecast",
               "unpredicted_renewables" ,  "intraday_update_renewables" ,
               "Fore_Solar", "Fore_Wind"  , "Wind Onshore",
               "intraday_Solar"  , "intraday_Wind", "auction_before",
               "Published.Unavailable.Capacity",  "Published.Unavailable.Nuclear.Capacity" ,   
               "Published.Predicted.Unavailable.Capacity",   "Unpublished.Unavailable.Capacity",          
               "Unpublished.Unavailable.Nuclear.Capacity",   "Unpublished.Predicted.Unavailable.Capacity",
               "Uneffective.Unavailable.Capacity" ,          "Uneffective.Unavailable.Nuclear.Capacity"  ,
               "Uneffective.Predicted.Unavailable.Capacity",
               "temperature_supply" , "precipitation_supply" , "auction_date_plus_one", "creneau"  ,      
               "price_diff", "log_price_diff",   "spot_gas", 
               "relative_humidity_supply", "year", "month", "weekday", "creneau"
)



  
  
auction_reg_data <- left_join(first_stage_data_clean %>% 
                                distinct(),
                              price_indispo_merge_esp%>%
                                dplyr::select(datetime, price, gen_tot_da, spot_gas) %>%
                                rename(price_da = price,
                                       gen_pred_da = gen_tot_da
                                       )%>% 
                                distinct(),
                              by = "datetime"
)
library(lubridate)
auction_reg_data$hour <- hour(auction_reg_data$datetime)+1


auction_reg_data <- left_join(
  auction_reg_data %>% 
    dplyr::select(datetime, delivery_date, hour, auction,
                  datetime, price_da, gen_tot, gen_pred_da,
                  variables
    ),
  auction_summary,
  by = c("delivery_date" = "DATE",
         "hour" = "Hour",
         "auction" = "AUCTION")
) %>% 
  filter(!is.na(PRICE))

# 
# auction_reg_data %>% 
#   group_by(auction) %>% 
#   summarize(n = n())




#prepare renewable

#on remarque une diff?rence avec le gen_tot_da

# View(price_indispo_merge_esp%>%
#        dplyr::select(datetime, delivery_date, price, gen_tot, gen_tot_da) %>%
#        filter(delivery_date == "2021-09-27"))
# View(auction_summary %>%
#        filter(AUCTION == 0 & DATE == as.Date("2021-09-27")))
# View(first_stage_data_clean %>%
#        filter(auction_before ==0 & delivery_date== "2021-09-27") %>%
#        mutate(price_da = as.numeric(price) - as.numeric(price_diff) )%>% 
#        dplyr::select(datetime, auction,price_da, price, price_diff, log_price_diff, auction_before, auction_after))
# View(first_stage_data_clean %>%
#        filter( Published.Unavailable.Capacity > 0 ) %>%
#        dplyr::select(datetime, auction, price, price_diff, log_price_diff, auction_before, auction_after, Published.Unavailable.Capacity))


#colnames(auction_reg_renewable_data)

auction_reg_renewable_data <- auction_reg_data  %>%
  group_by(auction_date_plus_one, creneau) %>%
  summarise(
    # Aggregate over auction_before 1 to 5
    price_diff_1to5 = sum(price_diff[auction_before < 5], na.rm = TRUE),
    log_price_diff_1to5 = sum(log_price_diff[auction < 5], na.rm = TRUE),
    qty_1to5 = sum(QTY[auction_before < 5], na.rm = TRUE),
    
    # Separate variable for auction_before == 6
    price_diff_5 = sum(price_diff[auction == 5], na.rm = TRUE),
    log_price_diff_5 = sum(log_price_diff[auction_before == 5], na.rm = TRUE),
    qty_5 = sum(QTY[auction_before ==5 ], na.rm = TRUE),
    
    gen_exch_spot = gen_exch_da + sum(QTY, na.rm = TRUE),
    
    # Count auctions
    number_auctions = n(),
    
    # For all other columns, take the first value (excluding those already handled)
    across(
      .cols = !any_of(c("price_diff", "log_price_diff")),
      .fns = ~ first(.x)
    ),
    
    .groups = "drop"
  )%>% 
  arrange(auction_date_plus_one, as.numeric(creneau))



# View(auction_reg_renewable_data %>% 
#        select(datetime, gen_tot, 
#               gen_pred_da, gen_exch_da,
#               gen_exch_spot))

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)
write_parquet(auction_reg_renewable_data, "auction_reg_renewable_data.parquet")
write_parquet(auction_reg_data, "auction_reg_data.parquet")


auction_reg_renewable_data_bis <- auction_reg_data  %>%
  group_by(auction_date_plus_one, creneau) %>%
  summarise(
    # Aggregate over auction_before 1 to 5
    price_diff_1to5 = sum(price_diff[auction_before < 6], na.rm = TRUE),
    log_price_diff_1to5 = sum(log_price_diff[auction < 6], na.rm = TRUE),
    qty_1to5 = sum(QTY[auction_before < 6], na.rm = TRUE),
    gen_exch_spot = gen_exch_da + sum(QTY, na.rm = TRUE),
    # Separate variable for auction_before == 6
    # price_diff_5 = sum(price_diff[auction == 5], na.rm = TRUE),
    # log_price_diff_5 = sum(log_price_diff[auction_before == 5], na.rm = TRUE),
    # qty_5 = sum(QTY[auction_before ==5 ], na.rm = TRUE),
    
    # Count auctions
    number_auctions = n(),
    
    # For all other columns, take the first value (excluding those already handled)
    across(
      .cols = !any_of(c("price_diff", "log_price_diff")),
      .fns = ~ first(.x)
    ),
    
    .groups = "drop"
  )%>% 
  arrange(auction_date_plus_one, as.numeric(creneau))

write_parquet(auction_reg_renewable_data_bis, "auction_reg_renewable_data_bis.parquet")




