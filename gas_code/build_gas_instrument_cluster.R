
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")

news_data_bert <- read.csv("clustered_articles_bert_labeled_full_sample.csv")

news_data_bert$datetime <- as.Date(format(as.Date(news_data_bert$date, "%b %d, %Y"), "%Y-%m-%d"))

news_data_bert$datetime <- ifelse( is.na(news_data_bert$datetime), as.Date(format(as.Date(news_data_bert$date, "%d %b %Y"), "%Y-%m-%d")), news_data_bert$datetime)

news_data_bert <- news_data_bert %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime)


cluster_news_count <- news_data_bert %>% 
  select(media, datetime, cluster, cluster_label) %>%
  distinct() %>% 
  group_by(datetime, cluster, cluster_label) %>% 
  summarise(news_per_day = n())

cluster_labels <- unique(news_data_bert[, c("cluster_label", "cluster")])

full_dates <- tibble(datetime = seq(min(news_data_bert$datetime, na.rm=TRUE), max(news_data_bert$datetime, na.rm=TRUE), by = "day"))
# Créer toutes les combinaisons de dates et d'événements
full_cluster_dates <- full_dates %>%
  crossing(cluster_labels)

# Joindre avec les données de nouvelles
cluster_complete_data <- full_cluster_dates %>%
  left_join(cluster_news_count, by = c("datetime", "cluster_label", "cluster")) %>%
  replace_na(list(news_per_day = 0))



shock_data_cluster <- cluster_complete_data %>%
  arrange(datetime) %>%  # Replace 'date_column' with your actual date column name
  mutate(moving_avg_news = rollapply(news_per_day, width = 30, FUN = mean, fill = NA, align = "right"),
         moving_std_news = rollapply(news_per_day, width = 30, FUN = sd, fill = NA, align = "right"),
         binary_shock= (news_per_day >  moving_avg_news + 1.96*moving_std_news)
  )

instrument_days_cluster <- shock_data_cluster %>% 
  filter(binary_shock == TRUE & news_per_day>=3)

nrow(instrument_days_cluster %>% 
       filter(binary_shock))

# on s'interroge sur à quel point 
#il ne faut pas aussi prendre en compte les news du lendemain

#unification des sujets
#est ce que cela traite de la même chose
#on envoie le corpus du jour à chatgpt et on lui demande de faire le job
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
options(warn = -1)
gas_data_shock <- read_parquet("gas_data_shock.parquet")


info_cluster <- left_join(gas_data_shock, shock_data_cluster,
                  by = c("Date" = "datetime"))

info_cluster <- info_cluster %>%
  group_by(Date) %>%
  slice_max(news_per_day, with_ties = FALSE) %>%
  ungroup()
  
#on verifie si il y a une intersection avec les jours 
#où les variations sont les plus fortes

save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

write_parquet(info_cluster, "info_cluster.parquet")







