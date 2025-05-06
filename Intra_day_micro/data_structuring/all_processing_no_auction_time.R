
save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/long_format'
setwd(save_path_OMIE_all_output)
getwd()
long_format_indispo <- read_parquet('long_format_indispo_OMIE_all.parquet') %>% 
  filter(in_auction_time == 0)


long_format_indispo_nuclear <- read_parquet('long_format_nuclear_indispo_OMIE_all.parquet') %>% 
  filter(in_auction_time == 0)


long_format_indispo_theoretical <- read_parquet('long_format_indispo_theoretical_OMIE_all.parquet') %>% 
  filter(in_auction_time == 0)

long_format_indispo_nuclear_theoretical <- read_parquet('long_format_nuclear_indispo_theoretical_OMIE_all.parquet') %>% 
  filter(in_auction_time == 0)

long_format_indispo_non_published <- long_format_indispo_theoretical %>% 
  anti_join(long_format_indispo, by = c("production_resource_psr_name", "datetime_pub", 
                                        "datetime_debut", "datetime_fin", "auction_before", "creneau"))%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)
# Charger les fichiers Parquet
long_format_indispo_nuclear_non_published <- long_format_indispo_nuclear_theoretical %>% 
  anti_join(long_format_indispo_nuclear, by = c("production_resource_psr_name", "datetime_pub", 
                                                "datetime_debut", "datetime_fin", "auction_before", "creneau"))%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)

long_format_indispo_uneffective <- read_parquet('long_format_indispo_uneffective_OMIE_all.parquet') %>% 
  filter(in_auction_time == 0)

long_format_indispo_uneffective_nuclear <- read_parquet('long_format_indispo_nuclear_uneffective_OMIE_all.parquet') %>%
  filter(in_auction_time == 0)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/rf_prediction'
setwd(save_path_entsoe_output)
rf_predictions_data <- read_parquet('xgb_predictions_data_OMIE_all.parquet')

colnames(rf_predictions_data)

process_indispo_rf <- function(indispo_df, rf_predictions_data) {
  indispo_df %>%
    mutate(datetime_slot = as.POSIXct(electricity_delivery_date) +
             as.difftime(creneau - 1, units = "hours")) %>%
    merge(rf_predictions_data,
          by.y = c('__index_level_0__', 'centrale', 'creneau'),
          by.x = c('datetime_slot', 'production_resource_psr_name', 'creneau')) %>%
    rename(datetime = datetime_slot) %>%
    filter(prediction_prod_conservative > 50) %>%
    #get predicted wanted change
    mutate(predicted_real_unavailability = ifelse(
      Unavailable.Capacity > 0,
      pmin(prediction_prod - Available.Capacity, Unavailable.Capacity),
      -pmin(prediction_prod - Available.Capacity, -Unavailable.Capacity)
    )) %>%
    #check same sign
    filter(Unavailable.Capacity * predicted_real_unavailability > 0) %>%
    #only large unavailabilities
    filter(abs(predicted_real_unavailability) > 100)
}

long_format_indispo_rf <- process_indispo_rf(long_format_indispo, rf_predictions_data)%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)

long_format_indispo_non_published_rf <- process_indispo_rf(long_format_indispo_non_published, rf_predictions_data)%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)
long_format_indispo_uneffective_rf <- process_indispo_rf(long_format_indispo_uneffective, rf_predictions_data)%>% 
  group_by(electricity_delivery_date, creneau, production_resource_psr_name, auction_before, Unavailable.Capacity) %>% 
  arrange(desc(datetime_pub)) %>%
  slice_head(n=1)


create_final_publi <- function(df, rf = FALSE) {
  if (rf) {
    df %>%
      group_by(electricity_delivery_date, auction_before, auction_after, creneau) %>%
      summarise(
        Installed.Capacity = sum(as.numeric(Installed.Capacity), na.rm = TRUE),
        Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity), na.rm = TRUE),
        Available.Capacity = sum(as.numeric(Available.Capacity), na.rm = TRUE),
        predicted_real_unavailability = sum(as.numeric(predicted_real_unavailability), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(creneau = as.character(creneau)) %>%
      arrange(electricity_delivery_date, auction_before, creneau)
  } else {
    df %>%
      group_by(electricity_delivery_date, auction_before, auction_after, creneau) %>%
      summarise(
        Installed.Capacity = sum(as.numeric(Installed.Capacity), na.rm = TRUE),
        Unavailable.Capacity = sum(as.numeric(Unavailable.Capacity), na.rm = TRUE),
        Available.Capacity = sum(as.numeric(Available.Capacity), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(creneau = as.character(creneau)) %>%
      arrange(electricity_delivery_date, auction_before, creneau)
  }
}



final_publi_OMIE_all_indispo_out_auction <- create_final_publi(long_format_indispo)
final_not_publi_OMIE_all_indispo_out_auction<- create_final_publi(long_format_indispo_non_published)
final_uneffective_OMIE_all_indispo_out_auction <- create_final_publi(long_format_indispo_uneffective)

final_publi_OMIE_all_indispo_out_auction_nuclear <- create_final_publi(long_format_indispo_nuclear)
final_not_publi_OMIE_all_indispo_out_auction_nuclear <- create_final_publi(long_format_indispo_nuclear_non_published)
final_uneffective_OMIE_all_indispo_out_auction_nuclear <- create_final_publi(long_format_indispo_uneffective_nuclear)

final_publi_OMIE_all_indispo_out_auction_rf <- create_final_publi(long_format_indispo_rf, rf = TRUE)
final_not_publi_OMIE_all_indispo_out_auction_rf <- create_final_publi(long_format_indispo_non_published_rf, rf = TRUE)
final_uneffective_OMIE_all_indispo_out_auction_rf <- create_final_publi(long_format_indispo_uneffective_rf, rf = TRUE)


tot_publi_OMIE_without_auction <- final_publi_OMIE_all_indispo_out_auction %>% 
  rename(Published.Unavailable.Capacity = Unavailable.Capacity)  %>%
  left_join(final_publi_OMIE_all_indispo_out_auction_nuclear %>% 
              rename(Published.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity', 'Available.Capacity')))  %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, as.numeric(creneau)-1)
                               , format="%Y-%m-%d %H", tz = 'Europe/Brussels')) %>%
  left_join(final_publi_OMIE_all_indispo_out_auction_rf %>%
              mutate(Published.Predicted.Unavailable.Capacity = predicted_real_unavailability)%>%
              select(-predicted_real_unavailability))


tot_publi_OMIE_without_auction <- tot_publi_OMIE_without_auction %>% 
  full_join(final_uneffective_OMIE_all_indispo_out_auction %>% 
              rename(Unpublished.Unavailable.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity', 'Available.Capacity'))) %>% 
  full_join(final_not_publi_OMIE_all_indispo_out_auction_nuclear %>% 
              rename(Unpublished.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>% 
              select(-c('Installed.Capacity', 'Available.Capacity'))) %>%
  left_join(final_not_publi_OMIE_all_indispo_out_auction_rf %>%
              mutate(Unpublished.Predicted.Unavailable.Capacity = predicted_real_unavailability) %>%
              select(-predicted_real_unavailability, -Installed.Capacity, -Available.Capacity, -Unavailable.Capacity)
  )

tot_publi_OMIE_without_auction <- tot_publi_OMIE_without_auction %>%
  full_join(final_uneffective_OMIE_all_indispo_out_auction_rf %>%
              rename(Uneffective.Unavailable.Capacity = Unavailable.Capacity) %>%
              select(-c('Installed.Capacity', 'Available.Capacity'))) %>%
  full_join(final_uneffective_OMIE_all_indispo_out_auction_nuclear %>%
              rename(Uneffective.Unavailable.Nuclear.Capacity = Unavailable.Capacity) %>%
              select(-c('Installed.Capacity', 'Available.Capacity'))) %>%
  left_join(final_uneffective_OMIE_all_indispo_out_auction_rf %>%
              mutate(Uneffective.Predicted.Unavailable.Capacity = -predicted_real_unavailability) %>%
              select(-predicted_real_unavailability, -Installed.Capacity, -Available.Capacity, -Unavailable.Capacity)
  )

getwd()
save_path_OMIE_all_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE'
setwd(save_path_OMIE_all_output)
write_parquet(tot_publi_OMIE_without_auction, 'tot_publi_OMIE_without_auction_time.parquet')



