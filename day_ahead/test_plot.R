test <- price_merge_CBF %>% 
       mutate(
         Taux_Sat_ES_FR = SCH_EX_DA_ES_FR/NTC_ESP_FR,
         Taux_Sat_FR_ES = SCH_EX_DA_FR_ES/NTC_FR_ESP,
         
       ) %>% 
  na.omit()
library(dplyr)

test <- test %>%
  mutate(bins_ES_FR = cut(Taux_Sat_ES_FR, breaks = c(-1, 0.95, 1.1, Inf),
                    labels = c("Normal", "Saturated", "Outlier"),
                    right = TRUE),
         bins_FR_ES = cut(Taux_Sat_FR_ES, breaks = c(-1, 0.95, 1.1, Inf),
                          labels = c("Normal", "Saturated", "Outlier"),
                          right = TRUE))
test %>% 
  group_by(bins_ES_FR, bins_FR_ES) %>%
  summarise(n = n(), mean = mean(price_diff_fr_esp), sd = sd(price_diff_fr_esp))
