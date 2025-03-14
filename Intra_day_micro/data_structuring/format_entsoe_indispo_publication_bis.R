library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)

save_path_entsoe <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_ter.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp'
setwd(save_path_esp)

bout_indispo_ENTSOE_data <- read_parquet("indispo_ENTSOE_data.parquet") %>% 
  arrange(datetime_debut)

unexpected_indispo_ENTSOE_data  <- bout_indispo_ENTSOE_data %>% 
  filter(businesstype=='Unplanned outage') %>% 
  ungroup()%>% 
  #exclude events which publication delay is greater than 24 hours
  mutate(lapse = difftime(datetime_debut, datetime_pub, units = "hours")) %>% 
  filter(lapse < 24 & lapse > -24 )


# test <- bout_indispo_ENTSOE_data %>% 
#   filter(datetime_debut >= as.POSIXct("2021-01-01") & datetime_debut <= as.POSIXct("2021-12-31")) %>%
#   filter(plant_type=="Nuclear" & businesstype == "Unplanned outage")



colnames(unexpected_indispo_ENTSOE_data)

#on se concentre sur les publications qui ont été faites 
#à peu près en meêm temps que le début d'indispo
#sinon cela signifie que ce n'est potentiellement pas le premier message

# sample <- unexpected_indispo_ENTSOE_data %>% 
#   sample_n(10)



#je vais diviser mon échantillon entre

#pour mon event study
#pendant l'indispo (à l'heure près)


# ont été publiées en même temps que le début de l'indispo


# ont été publiées après la fin de l'indispo
# entre les deux

#on voudrait les auctions que cela aurait pu affecter
#si jamais l'information était fluide
# dans les fonctions suivantes, il ne faut donc aucune
# référence à la date de publication de l'indispo




publication_lapse <- unexpected_indispo_ENTSOE_data %>% 
  mutate(year = year(datetime_debut))

# publication_lapse %>% 
#   filter(lapse <0 & year>2015)


publi_stat <- publication_lapse %>% 
  group_by(year) %>%
  summarise(lapse = mean(lapse, na.rm = TRUE))

library(ggplot2)
ggplot(publi_stat %>% filter(year>2015), aes(x = year, y = lapse)) +
  geom_line() +
  geom_point() +
  labs(title = "Publication delay for unplanned outages",
       x = "Year",
       y = "Average publication delay (hours)")

ggplot(publication_lapse , aes(x = lapse)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", alpha = 0.7) +
  labs(title = "Distribution of Lapse Pub (in Hours)", 
       x = "Lapse Pub (Hours)", 
       y = "Frequency") +
  theme_minimal()
#pour avoir un effet le retard de publication ne peut pas être supérieur à 24h


#rendre unique l'identifiant mrid
#seulement 2 doublons
unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data[!(unexpected_indispo_ENTSOE_data$mrid
                                                                   %in% unexpected_indispo_ENTSOE_data$mrid[duplicated(unexpected_indispo_ENTSOE_data$mrid)]), ]

result_df <- NULL

# sample <- unexpected_indispo_ENTSOE_data %>%
#   filter(hour(datetime_pub) < 2 )
# row <- unexpected_indispo_ENTSOE_data %>%
#   filter(mrid == "8Rw2gEMG3cv5wnQSvzL3Jg")
# test <- apply(row, 1, pub_auction_window)
# sample <- unexpected_indispo_ENTSOE_data %>%
#   ungroup() %>% 
#   filter(datetime_pub < datetime_fin) %>% 
#   sample_n(1000)
# colnames(sample)
# sample <- sample %>% 
#   filter(hour(datetime_pub) >12 & hour(datetime_debut) < 18 )

# test3 <- apply(test, 1, pub_auction_window)
# auction_ENTSOE_indispo_test <- result_df %>% 
#   left_join(test,  by = "mrid")%>% 
#   mutate(duration = as.numeric(datetime_fin - datetime_debut, units = "hours"))%>% 
#   filter(auction_before!=auction_after & duration >1)

# row <- unexpected_indispo_ENTSOE_data %>% 
#   filter(mrid =="DRDpGztCsBzVFjPnxosE8Q")


test <- apply(unexpected_indispo_ENTSOE_data, 1, pub_auction_window)

getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
write_parquet(result_df, "result_df.parquet")
# Convert the result to a data frame
#result_df_bis <- data.frame(t(result_df))
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
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

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs'
setwd(save_path_entsoe_output)
write_parquet(actual_ENTSOE_indispo, 'actual_ENTSOE_indispo.parquet')

actual_ENTSOE_indispo %>% 
  group_by(auction_before, auction_after) %>% 
  summarise(n = n())


