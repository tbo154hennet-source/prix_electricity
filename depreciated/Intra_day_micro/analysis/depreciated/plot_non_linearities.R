library(ggplot2)
colnames(long_format_indispo)
long_format_indispo %>% 
  group_by(auction_before) %>% 
  summarise(n= n())

model <- lm(
  log_price_diff ~ Published.Unavailable.Share + Published.Unavailable.Nuclear.Share,
  data= first_stage_data_clean_post_2021)

summary(model)
# Ou équivalent avec predict()
fitted_values <- predict(model)
# Ajouter les fitted values au dataframe d'origine
first_stage_data_clean_post_2021$fitted_values <- fitted_values



ggplot(
  data = first_stage_data_clean_post_2021[, c("Published.Unavailable.Nuclear.Share", "price_diff")] %>% 
  filter(Published.Unavailable.Nuclear.Share >0))+
  geom_point(aes(x= Published.Unavailable.Nuclear.Share, y = price_diff))
  
ggplot(
  data = first_stage_data_clean_post_2021[, c("Published.Unavailable.Share", "log_price_diff")] %>% 
    filter(Published.Unavailable.Share >0))+
  geom_point(aes(x= Published.Unavailable.Share, y = log_price_diff))


  
  
