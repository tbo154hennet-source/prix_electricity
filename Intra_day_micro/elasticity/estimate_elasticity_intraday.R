library(data.table)

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/code_intraday")
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

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/code_intraday")
auction_summary <-  read_parquet( 'auction_summary_intraday.parquet')
#get day ahead exchanged quantity
auction_summary <- auction_summary %>%
  group_by(DATE, Hour) %>%
  mutate(gen_exch_da = ifelse(any(AUCTION == 0),
                              QTY[AUCTION == 0][1], # take the QTY from auction 0
                              NA_real_)) %>%
  ungroup()


View(auction_summary %>% 
       filter( DATE == as.Date("2020-03-15") & Hour ==1 ))
#estimate elasticity

output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")


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



output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")


auction_reg_data <- left_join(first_stage_data_clean %>% 
                                distinct(),
                              price_indispo_merge_esp_npc %>%
                                dplyr::select(datetime, price, load_tot, load_forecast, spot_gas) %>%
                                rename(price_da = price,
                                       load_tot_da = load_tot,
                                       load_forecast_da = load_forecast
                                       )%>% 
                                distinct(),
                              by = "datetime"
)
library(lubridate)
auction_reg_data$hour <- hour(auction_reg_data$datetime)+1


auction_reg_data <- left_join(
  auction_reg_data,
  auction_summary,
  by = c("delivery_date" = "DATE",
         "hour" = "Hour",
         "auction" = "AUCTION")
) %>% 
  filter(!is.na(PRICE))


auction_reg_renewable_data <- auction_reg_data %>%
  mutate(
    price    = as.numeric(price),
    price_da = as.numeric(price_da),
    QTY      = as.numeric(QTY)
  ) %>%
  group_by(auction_date_plus_one, creneau) %>%
  summarise(
    #Separate for auction before 0 to 1
    price_first_1 = if (any(auction_before == 0, na.rm = TRUE)) {
      first(price[auction_before == 0])
    } else {
      NA_real_
    },
    qty_first_1 = if (any(auction_before == 0, na.rm = TRUE)) {
      first(QTY[auction_before == 0])
    } else {
      NA_real_
    },
    price_diff_0to1 = price_first_1 - price_da,
    
    # Aggregate over auction_before 1 to 5
    price_diff_1to5 = sum(price_diff[ auction_before >= 1 & auction_before < 5], na.rm = TRUE),
    log_price_diff_1to5 = sum(log_price_diff[auction_before >= 1 & auction < 5], na.rm = TRUE),
    qty_1to5 = sum(QTY[auction_before >= 1 & auction_before < 5], na.rm = TRUE),
    
    # Separate variable for auction_before == 6
    price_diff_5 = sum(price_diff[auction == 5], na.rm = TRUE),
    log_price_diff_5 = sum(log_price_diff[auction_before == 5], na.rm = TRUE),
    qty_5 = sum(QTY[auction_before ==5 ], na.rm = TRUE),
    
    #gen_exch_spot = gen_exch_da + sum(QTY, na.rm = TRUE),
    
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

View(auction_reg_data  %>% 
       filter(datetime == as.POSIXct("2020-03-15 00:00:00")))
View(auction_reg_renewable_data  %>% 
       filter(datetime == as.POSIXct("2020-03-15 00:00:00")) %>% 
       dplyr::select(datetime, auction, price_da, price_diff_0to1, price_diff_1to5, price_diff_5))


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
    load_tot_spot = load_tot_da + sum(QTY, na.rm = TRUE),
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




