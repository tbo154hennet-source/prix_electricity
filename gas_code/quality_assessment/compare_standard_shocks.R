setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')

gas_data_shock <- read_parquet("gas_data_shock.parquet")
gas_data_tot <- read_excel("TTF_futures.xlsx")

events_data <- read.csv("alex_gassani_shocks.csv")
events_data <- events_data %>% 
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date)
colnames(gas_data_shock)
alex_gassani_benchmark <- left_join( events_data, gas_data_shock %>% 
                                       mutate(gas_shock = TTF - lag(TTF)),
                                     by = c("Date"))

events_data_serie <- alex_gassani_benchmark %>% 
  mutate(month = month(Date), year= year(Date))%>%
  group_by(month, year) %>% 
  summarize(Value_PC1_AG = sum(PC1), Value_dms_M1_AG = sum(gas_shock), Date = first(Date)) %>% 
  arrange(Date)
events_data_serie
setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data')
gas_fundamental_data <- read_parquet( 
              "gas_fundamental_data.parquet")

colnames(gas_fundamental_data)

reg_data <- left_join( gas_fundamental_data, events_data_serie, by = c("month", "year")) %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))


ggplot()+
  geom_line(data= plot_data, aes(x = last_Date, y = gas_shock, color = day_type, group = day_type))+
  geom_line(data= plot_data, aes(x = last_Date, y = PC1, color = day_type, group = day_type), linetype = "dashed")+
  geom_line(data= reg_data, aes(x = last_Date, y = 10*Value_PC1_AG, color = "AG"))+
  geom_line(data= reg_data, aes(x = last_Date, y = last_spot_diff, color = "Observed"))+
  theme_minimal()





supply_model <- lm(last_spot_diff  ~ Value_PC1_AG, 
                   #last_spot_diff  ~ Fund,
                   data = reg_data)

summary(supply_model)


model <- lm(last_spot_diff  ~ Value_dms_M1_AG , 
                   data = reg_data)
summary(model)



