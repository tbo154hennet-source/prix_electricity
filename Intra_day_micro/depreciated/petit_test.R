test_sample <- unexpected_indispo_ENTSOE_data %>% 
  mutate(hour = hour(datetime_pub)) %>% 
  filter(hour == 13)

View(actual_ENTSOE_indispo %>%
  filter(mrid =="TCsl9aZiYjUzZC-KvWAw-g"))
row <- test_sample %>%
  filter(mrid =="TCsl9aZiYjUzZC-KvWAw-g")

result_df <- NULL
test <- apply(row, 1, pub_auction_window)
