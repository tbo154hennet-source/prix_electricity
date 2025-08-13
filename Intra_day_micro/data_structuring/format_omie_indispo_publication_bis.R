library(arrow)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(lubridate)
library(furrr)
library(purrr)
library(dplyr)


save_path_entsoe <- 'Z:/prix_electricite_gitlab/Intra_day_micro/data_structuring'
setwd(save_path_entsoe)
source("auxiliary_file_sixt.R")

save_path_esp <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Unavailabilities_plants/ENTSOE_unavailabilities/ES/formatted'
setwd(save_path_esp)

setwd("Z:/elec")

bout_indispo_ENTSOE_data <- read_parquet("all_matched_indispo.parquet") %>% 
  arrange(datetime_debut) %>% 
  filter(datetime_debut > as.Date("2018-01-01"))


colnames(bout_indispo_ENTSOE_data)
#probl�me avec certians datetime debut chelou
#suppression

unexpected_indispo_ENTSOE_data  <- bout_indispo_ENTSOE_data %>% 
  #filter(businesstype=='Unplanned outage') %>% 
  ungroup()%>% 
  #exclude events which publication delay is greater than 24 hours
  mutate(lapse = difftime(datetime_debut, datetime_pub, units = "hours")) %>% 
  filter(lapse < 36 & lapse > -36 )



colnames(unexpected_indispo_ENTSOE_data)



publication_lapse <- unexpected_indispo_ENTSOE_data %>% 
  mutate(year = year(datetime_debut),
         lapse = difftime(datetime_pub, datetime_debut, unit = "hours"))

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
# unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data[!(unexpected_indispo_ENTSOE_data$mrid
#                       %in% unexpected_indispo_ENTSOE_data$mrid[duplicated(unexpected_indispo_ENTSOE_data$mrid)]), ]

unexpected_indispo_ENTSOE_data <- unexpected_indispo_ENTSOE_data %>% 
  group_by(mrid, message_order) %>% 
  slice_head(n=1) %>% 
  rename(id_indispo = mrid) %>% 
  mutate(mrid = paste(id_indispo, message_order)) %>% 
  ungroup()


# colnames(unexpected_indispo_ENTSOE_data)
# 
# unexpected_indispo_test <- unexpected_indispo_ENTSOE_data %>% 
#   distinct(mrid, message_order, .keep_all = TRUE)
# 
# unexpected_indispo_test_bis <- unexpected_indispo_ENTSOE_data %>%
#   group_by(mrid, message_order) %>%
#   filter(n() > 1) %>%
#   ungroup()


dim(unexpected_indispo_test)
# ⚡ Active le parallélisme
plan(multisession) 

#row <- unexpected_indispo_ENTSOE_data[1,]

counter <- 0
total_rows <- nrow(unexpected_indispo_ENTSOE_data)

result_list <- future_map(seq_len(total_rows), function(i) {
  counter <<- counter + 1
  if (counter %% 200 == 0) message("Lignes traitées : ", counter)
  tryCatch({
    pub_auction_window(unexpected_indispo_ENTSOE_data[i, ])
  }, error = function(e) {
    print(unexpected_indispo_ENTSOE_data$mrid[i])  # Correction ici
    return(NULL)  # Ignorer les erreurs
  })
  
})


# ✅ Suppression des valeurs NULL et conversion en dataframe
result_df <- bind_rows(unlist(result_list, recursive = FALSE))

getwd()
setwd("Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
write_parquet(result_df, "result_df_OMIE_all.parquet")
# Convert the result to a data frame
#result_df_bis <- data.frame(t(result_df))
setwd("Z:/prix_electricite_gitlab/Intra_day_instrumnet_motivation/ENTSOE/outputs")
result_df <- read_parquet("result_df_OMIE_all.parquet") %>% 
  arrange(electricity_delivery_date)

# hour_pub <- hour(row["datetime_pub"][[1]])
# minute_pub <- minute(row["datetime_pub"][[1]])
# time_ref <- sprintf("%02d:%02d", 17, 50)
# time_str <- sprintf("%02d:%02d", hour_pub, minute_pub)
# print(time_str < time_ref)


auction_ENTSOE_indispo <- result_df %>% 
  left_join(unexpected_indispo_ENTSOE_data,  by = c("mrid"))

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
write_parquet(actual_ENTSOE_indispo, 'actual_OMIE_indispo_all.parquet')

actual_ENTSOE_indispo <- read_parquet('actual_ENTSOE_indispo.parquet')

actual_ENTSOE_indispo %>% 
  group_by(auction_before, auction_after) %>% 
  summarise(n = n())


