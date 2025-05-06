

result_df <- NULL

sample <- unexpected_indispo_ENTSOE_data %>%
  sample_n(10)
apply(sample, 1, pub_auction_window)



sample <- unexpected_indispo_ENTSOE_data %>%
  sample_n(10)
apply(test2, 1, pub_auction_window)

test <- apply(unexpected_indispo_ENTSOE_data, 1, pub_auction_window)

print(length(unexpected_indispo_ENTSOE_data$mrid))

print(length(unique(unexpected_indispo_ENTSOE_data$mrid)))

test2 <- unexpected_indispo_ENTSOE_data[duplicated(unexpected_indispo_ENTSOE_data$mrid)
                               | duplicated(unexpected_indispo_ENTSOE_data$mrid, fromLast = TRUE), ]


#generate mrid key without redoing everything
result_df_unique <- unique(result_df)

result_df %>% 
  filter(electricity_delivery_date == "2024-06-03")

class(result_df$electricity_delivery_date)



doublons <- doublons[, -1]
nrow(result_df)
result_df <- setdiff(result_df, doublons)

