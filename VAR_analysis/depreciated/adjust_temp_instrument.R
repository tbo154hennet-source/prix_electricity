temperature_instrument

dates <- seq(from = as.Date("2015-01-01"), 
             to = as.Date("2023-12-31"), 
             by = "month")
all_dates <- data.frame(dates) %>% 
  mutate(year = year(dates), 
         month = month(dates))

temp_search <- left_join(all_dates,temperature_instrument, 
                         by = c("year", "month"))

# Fill NA values with column means
temp_search_filled <- temp_search %>%
  mutate(across(
    all_of(c("cold_excess_instrument", "heat_excess_instrument",
             "CDD_excess_instrument", "HDD_excess_instrument",
             "CDD_intensity_instrument", "HDD_intensity_instrument")),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  ))


temp_search_detrended <- detrend(as.matrix(
  temp_search_filled  %>% 
    dplyr::select(all_of(c("cold_excess_instrument", "heat_excess_instrument",
                           "CDD_excess_instrument",  "HDD_excess_instrument",
                           "CDD_intensity_instrument",  "HDD_intensity_instrument")))
)) %>% as.data.frame() 


plot(temp_search_detrended$CDD_intensity_instrument, type="l")



ts_data <- ts(detrend(head(renewable_test$renewable_generation, 108)),
              start = start_date, frequency = frequency)
class(ts_data)

stl_fit <- stl(ts_data[,1], s.window = "periodic")
test_serie <- seasadj(stl_fit)

par(mar = c(2, 2, 4, 4))
plot(ts_data, type="l")


mean(temp_search_detrended[,1], na.rm = TRUE)

colnames(temp_search) <- c("date", "temperature")



renewable_test$datetime
ggplot(renewable_test)+
  geom_line(aes(x = datetime, y = wind, color = "wind"))+
  geom_line(aes(x = datetime, y = solar, color = "solar"))


