library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_ter.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE_unavailabilities/ES/formatted'
setwd(save_path_esp)

bout_indispo_ENTSOE_data_esp <- read_parquet("indispo_ENTSOE_data_post_2018.parquet") %>% 
  arrange(datetime_debut) %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"),
         publication_duration = as.numeric(datetime_pub - datetime_debut, units = "hours"))


bout_indispo_ENTSOE_data_esp <- 
  bout_indispo_ENTSOE_data_esp[!duplicated(bout_indispo_ENTSOE_data_esp$mrid), ]


bout_indispo_ENTSOE_data_esp <- 
  bout_indispo_ENTSOE_data_esp[!duplicated(bout_indispo_ENTSOE_data_esp$mrid), ] %>% 
  filter(duration > 1 & duration < 24*365)



result_df <- NULL


row <- bout_indispo_ENTSOE_data_esp %>% 
 filter(mrid =="DRDpGztCsBzVFjPnxosE8Q")
my_test <- bout_indispo_ENTSOE_data_esp[1:100, ]
View(my_test %>% 
       ungroup() %>% 
       select(mrid, datetime_debut, datetime_pub, datetime_fin, publication_duration, duration))

test <- apply(row, 1, pub_auction_window)

test <- apply(unexpected_indispo_ENTSOE_data, 1, pub_auction_window)

getwd()
setwd("Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
write_parquet(result_df, "result_df.parquet")
# Convert the result to a data frame
#result_df_bis <- data.frame(t(result_df))
setwd("Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
result_df <- read_parquet("result_df.parquet") %>% 
  arrange(electricity_delivery_date)

# hour_pub <- hour(row["datetime_pub"][[1]])
# minute_pub <- minute(row["datetime_pub"][[1]])
# time_ref <- sprintf("%02d:%02d", 17, 50)
# time_str <- sprintf("%02d:%02d", hour_pub, minute_pub)
# print(time_str < time_ref)


auction_ENTSOE_indispo <- result_df %>% 
  left_join(unexpected_indispo_ENTSOE_data,  by = "mrid")

# auction_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
#   mutate( dummy = ifelse( auction_before ==2 & auction_after ==3 & sprintf("%02d:%02d", hour(datetime_pub), minute_pub )< time_ref, 1, 0),
#           auction_before = ifelse( dummy, 1, auction_before),
#           auction_after = ifelse( dummy, 2, auction_after))

# Get the number of columns in the data frame
#num_cols <- ncol(auction_ENTSOE_indispo)
#names(auction_ENTSOE_indispo)[(num_cols-7):num_cols] <- new_names_pub

actual_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)
colnames(actual_ENTSOE_indispo)

  
# je decide d'enlever les evene;ents de moins d'une heure
actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  filter(duration >= 1)

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)
write_parquet(actual_ENTSOE_indispo, 'actual_ENTSOE_indispo.parquet')

actual_ENTSOE_indispo <- read_parquet('actual_ENTSOE_indispo.parquet')

actual_ENTSOE_indispo %>% 
  group_by(auction_before, auction_after) %>% 
  summarise(n = n())


