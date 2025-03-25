setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/LLM_news')

# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")

#news_data_LLM<- read_parquet("labeled_news.parquet")
parquet_files <- list.files(pattern = "\\.parquet$")


news_data_LLM<- NULL
for (file in parquet_files){
  data <- read_parquet(file)
  if (is.null(news_data_LLM)){
    news_data_LLM<- data
  } else {
    news_data_LLM<- rbind(news_data_LLM, data)
  }
}


news_data_LLM<- news_data_LLM%>% 
  distinct() 

#news_data_LLM <- read_parquet("sorted_news_data_LLM.parquet")

news_data_LLM%>% 
  filter(datetime < max(news_data_LLM$datetime)) %>% 
  count()

news_data$datetime <- as.Date(format(as.Date(news_data$date, "%b %d, %Y"), "%Y-%m-%d"))

news_data$datetime <- ifelse( is.na(news_data$datetime), as.Date(format(as.Date(news_data$date, "%d %b %Y"), "%Y-%m-%d")), news_data$datetime)

news_data_LLM<- news_data_LLM%>% 
  mutate(datetime = as.Date(datetime)) %>% 
  arrange(datetime)







