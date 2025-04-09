
library(tidyr)


#regarder quelles sont les dates perdues dans le merge
#données 2024
save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp.parquet")


save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
price_indispo_merge_fr <- read_parquet("price_indispo_merge_fr.parquet")

colnames(price_indispo_merge_esp)

min_headers <- c("datetime", "year", "month", "day",  "delivery_date", "price","gen_tot","wind_solar_tot", "day_ahead_forecast"  ,                      
           "intraday_forecast" , "unpredicted_renewables","intraday_update_renewables" )

price_merge_CBF <- merge(price_indispo_merge_fr  %>% 
                           dplyr::select(min_headers, contains("NTC")) %>% 
                           rename_with(~ paste0(., "_fr"), -c(datetime, year, month, day, delivery_date)),
                         price_indispo_merge_esp  %>% 
                           dplyr::select(min_headers) %>% 
                           rename_with(~ paste0(., "_esp"), -c(datetime, year, month, day, delivery_date)),
                         by= c("datetime", "year", "month", "day", "delivery_date")
)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/cross_border_flows")

cross_border_flows <- read_parquet("fr_esp_cross_border_flows.parquet")
colnames(cross_border_flows)

ntc_cols <- grep("^NTC", colnames(cross_border_flows), value = TRUE)



price_merge_CBF <- merge(price_merge_CBF, cross_border_flows 
                         # %>%dplyr::select(time, ntc_cols)
                         ,
                         by.x ="datetime", by.y ="time")

colnames(price_merge_CBF)
price_merge_CBF %>% 
  filter(gen_tot_fr>0 & gen_tot_esp>0) %>% 
      mutate(CBF = CBF_ESP_FR - CBF_FR_ESP,
             renewable_hors_esp = day_ahead_forecast_fr*NTC_FR_ESP/gen_tot_fr,
             renewable_hors_fr = day_ahead_forecast_esp*NTC_ESP_FR/gen_tot_esp
             ) %>% 
      dplyr::select(CBF, renewable_hors_esp, renewable_hors_fr) %>% 
  cor(use = "complete.obs")


getwd()
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

write_parquet(price_merge_CBF, "price_merge_CBF.parquet")




neighbours_esp <- c("FR", "PT")


# 
# 
# setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")
# price_merge_CBF <- read_parquet( "price_merge_CBF.parquet")%>% 
#   
#   
#   
#   group_by(month, year) %>%
#   summarise(
#     renewable_forecast_hors_esp = weighted_mean(day_ahead_forecast_fr*NTC_FR_ESP, gen_tot_fr*gen_tot_esp),
#     renewable_forecast_hors_fr = weighted_mean(day_ahead_forecast_esp*NTC_ESP_FR, gen_tot_fr*gen_tot_esp),
#     datetime = first(datetime)
#   ) %>%
#   arrange(datetime) %>% 
#   ungroup()
# 
# write_parquet(price_merge_CBF, "price_merge_CBF.parquet")




