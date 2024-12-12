setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')

# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")

news_data <- read_parquet("labeled_news.parquet")

news_data_LLM <- read_parquet("sorted_news_data_LLM.parquet")

news_data_LLM <- news_data_LLM %>% 
  distinct(title, datetime)

news_data %>% 
  filter(datetime < max(news_data_LLM$datetime)) %>% 
  count()

news_data$datetime <- as.Date(format(as.Date(news_data$date, "%b %d, %Y"), "%Y-%m-%d"))

news_data$datetime <- ifelse( is.na(news_data$datetime), as.Date(format(as.Date(news_data$date, "%d %b %Y"), "%Y-%m-%d")), news_data$datetime)

news_data <- news_data %>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime)