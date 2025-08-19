
# 2021-02-17 06:00:00

save_path_OMIE_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_output)
tot_publi_OMIE_indispo <-   read_parquet('tot_publi_OMIE_indispo.parquet')



View(tot_publi_OMIE_indispo %>% 
  filter(datetime <= as.POSIXct("2021-02-18")  &datetime >= as.POSIXct("2021-02-16")) )


save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_entsoe_output)
long_format_indispo <- read_parquet('long_format_indispo_OMIE.parquet')


View(long_format_indispo %>% 
       filter(datetime_debut <= as.POSIXct("2021-02-18")  &datetime_fin >= as.POSIXct("2021-02-16")) )


save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
rf_predictions_data<- read_parquet('xgb_predictions_data_OMIE.parquet')

View(rf_predictions_data %>% 
       filter(`__index_level_0__`<= as.POSIXct("2021-02-18")  &`__index_level_0__` >= as.POSIXct("2021-02-16")) )




View(long_format_indispo_rf %>% 
       filter(datetime_debut <= as.POSIXct("2021-02-18")  &datetime_fin >= as.POSIXct("2021-02-16")) )

View(
  final_real_indispo_rf %>% 
    filter(
      electricity_delivery_date <= "2021-02-18" &
        electricity_delivery_date   >= "2021-02-16"
    )
)
