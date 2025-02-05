library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE'
setwd(save_path_entsoe)
source("auxiliary_file_ter.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut)

unexpected_indispo_ENTSOE_data  <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Unplanned outage')

unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data[!(unexpected_indispo_ENTSOE_data$mrid
                                                                   %in% unexpected_indispo_ENTSOE_data$mrid[duplicated(unexpected_indispo_ENTSOE_data$mrid)]), ]


#problem is i suppose that there is immediate transmission of the news
# from the start date
#because it was not published on REMIT
# but published on some national website 


result_df <<- NULL

# sample <- unexpected_indispo_ENTSOE_data %>%
#   filter(datetime_debut >= '2020-01-01' & datetime_debut <= '2020-01-02')
# sample <- unexpected_indispo_ENTSOE_data %>%
#   ungroup() %>% 
#   sample_n(100)
# colnames(sample)
# sample <- sample %>% 
#   filter(hour(datetime_pub) >12 & hour(datetime_debut) < 18 )
test <- apply(row, 1, pub_auction_window_theoretical)

test <- apply(unexpected_indispo_ENTSOE_data, 1, pub_auction_window_theoretical)

# Convert the result to a data frame
#result_df <- data.frame(t(result_df))


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
write_parquet(result_df, "result_df_theoretical.parquet")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
result_df <- read_parquet("result_df_theoretical.parquet")

colnames(result_df)
colnames(unexpected_indispo_ENTSOE_data)

auction_ENTSOE_indispo <- result_df %>% 
  left_join(unexpected_indispo_ENTSOE_data,  by = "mrid")

auction_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
  mutate( dummy = ifelse( auction_before ==2 & auction_after ==3 & sprintf("%02d:%02d", hour(datetime_pub), minute_pub )< time_ref, 1, 0),
          auction_before = ifelse( dummy, 1, auction_before),
          auction_after = ifelse( dummy, 2, auction_after))


actual_ENTSOE_indispo <- auction_ENTSOE_indispo  %>% 
  filter(auction_before!=auction_after)

# je decide d'enlever les evene;ents de moins d'une heure
actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))

actual_ENTSOE_indispo <- actual_ENTSOE_indispo %>% 
  filter(duration >= 1)



save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)
write_parquet(actual_ENTSOE_indispo, 'actual_ENTSOE_indispo_theoretical.parquet')


colnames(actual_ENTSOE_indispo)

actual_ENTSOE_indispo %>%
  mutate(end_before_pub = datetime_pub < datetime_fin) %>%
  group_by(end_before_pub) %>% 
  summarise(n = n())

actual_ENTSOE_indispo %>% 
  group_by(auction_before, auction_after) %>% 
  summarise(n = n())

