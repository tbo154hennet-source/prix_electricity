library(stringr)
library(sandwich)
library(lmtest)
library(ggplot2)

decalage = 0
table_coefficients_predicted_full <- NULL

save_path_OMIE_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_OMIE_output)
price_diff_data <- read_parquet('price_diff_data.parquet')

save_path_data <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Wind_solar_forecast/ES"
setwd(save_path_data)

#publi_indispo <- tot_publi_OMIE_all_indispo

renewable_forecasts <- read_parquet('renewable_forecasts_esp.parquet')

dynamic_regression_estimation_log <- function(decalage, publi_indispo){
  print(decalage)
  first_stage_data_decale <- price_diff_data %>% 
    mutate(electricity_delivery_date = as.character(delivery_date)) %>% 
    left_join(publi_indispo %>%
                mutate(auction_before = as.character(as.numeric(auction_before) + decalage),
                       auction_after = as.character(as.numeric(auction_after) + decalage)) %>% 
                #some minor changes to get good join
                select(-datetime) 
              , by = c('auction_before', 'auction_after', 'creneau', "electricity_delivery_date")) %>% 
    arrange(datetime)
  
  first_stage_data_decale <- first_stage_data_decale %>% 
    arrange(datetime, creneau, auction_before)
  
  #nota bene intradailiy forecast commence à partir du 13 février 2018 13-02-2018
  #sample start
  
  first_stage_data_decale <- first_stage_data_decale %>%
    left_join(renewable_forecasts, by=c('datetime' = '__index_level_0__'))
  
  # Identify all capacity-related columns dynamically
  capacity_cols <- colnames(first_stage_data_decale)[grepl("Capacity", colnames(first_stage_data_decale))]
  
  # Drop rows with NA in log_price column
  first_stage_data_decale  <- 
    first_stage_data_decale[!is.na(first_stage_data_decale$log_price_diff), ]%>%
    mutate(across(all_of(capacity_cols), ~ replace_na(.x, 0))) 
  
  
  colnames_with_capacity <- colnames(first_stage_data_decale)[grepl("Capacity", colnames(first_stage_data_decale))]
  #get monthly avg of unavailabilities
  capacity_fields <- 
    c(colnames_with_capacity,
      "day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables")
  
  first_stage_data_decale[colnames_with_capacity] <- lapply(first_stage_data_decale[colnames_with_capacity], function(x) ifelse(is.na(x), 0, x))
  
  
  #adjuster l'ordre de grandeur des indispos pour ne pas avoir des coefficients nuls
  first_stage_data_decale_clean <- first_stage_data_decale%>%
    filter(is.finite(log_price_diff) & !is.na(log_price_diff) &
             is.finite(Published.Predicted.Unavailable.Capacity) & !is.na(Published.Predicted.Unavailable.Capacity) 
           & is.finite(Unpublished.Predicted.Unavailable.Capacity) & !is.na(Unpublished.Predicted.Unavailable.Capacity) 
           & is.finite(Uneffective.Predicted.Unavailable.Capacity) & !is.na(Uneffective.Predicted.Unavailable.Capacity) 
           
    )%>% 
    mutate(across(all_of(capacity_fields), 
                  ~ . / 1000)) %>% 
    mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
           # Published.Dismissed.Unavailable.Capacity = Published.Dismissed.Unavailable.Capacity - Published.Dismissed.Unavailable.Nuclear.Capacity,
           Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
           # Unpublished.Dismissed.Unavailable.Capacity = Unpublished.Dismissed.Unavailable.Capacity - Unpublished.Dismissed.Unavailable.Nuclear.Capacity
    )%>% 
    rename(day_ahead_renewable_forecast= day_ahead_forecast)
  
  # Créer une variable factorielle pour clusteriser
  first_stage_data_decale_clean <- first_stage_data_decale_clean %>% 
    mutate(cluster_var = interaction(auction_date_plus_one, auction)) 
  
  non_finite_rows <- first_stage_data_decale_clean  %>%
    filter(!is.finite(log_price_diff) | is.na(log_price_diff))
  
  first_stage_data_decale_clean$weekday <- weekdays(first_stage_data_decale_clean$delivery_date) 
  
  
  # perform first stage regression
  m_real_fixest <- feols(
    log_price_diff ~ Published.Predicted.Unavailable.Capacity + 
      Unpublished.Predicted.Unavailable.Capacity + 
      Uneffective.Predicted.Unavailable.Capacity + gen_tot +
      as.factor(year) + as.factor(month) + as.factor(weekday) +
      as.factor(creneau) + as.factor(auction_before) +
      intraday_update_renewables + unpredicted_renewables +
      day_ahead_renewable_forecast,
    data = first_stage_data_decale_clean,
    cluster = ~cluster_var
  )
  
  #summary(m_real_fixest)
  
  
  
  # Coefficient name (assumes you're interested in the second regressor)
  # Get all coefficient names from model
  coef_names <- names(coef(m_real_fixest))
  
  # Filter to those that contain "Unavailability.Capacity"
  target_vars <- coef_names[grepl("Capacity", coef_names)]
  
  # Degrees of freedom
  n_ddl <- df.residual(m_real_fixest)
  
  
  # Loop through each matching coefficient and build rows
  results_list <- lapply(target_vars, function(varname) {
    status <-  str_replace(varname, "\\..*", "")
    # Résultat : "Published"
    
    est <- coef(m_real_fixest)[varname]
    
    se_clustered <-  se(m_real_fixest)[varname]
    
    
    data.frame(
      variable = decalage,
      term = varname,
      estimate = est,
      #std_error_classic = se_classic,
      std_error_clustered = se_clustered,
      #t_value_classic = est / se_classic,
      t_value_clustered = est / se_clustered,
      #p_value_classic = 2 * (1 - pt(abs(est / se_classic), df = n_ddl)),
      p_value_clustered = 2 * (1 - pt(abs(est / se_clustered), df = n_ddl)),
      status = status
    )
  })
  # Combine all rows into a single dataframe
  temp_df_all <- do.call(rbind, results_list)
  
  return(temp_df_all)
}

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE")
tot_publi_OMIE_indispo <- read_parquet( 'tot_publi_OMIE_indispo.parquet')


results_list <- lapply(-2:2, function(d) {
  result <- dynamic_regression_estimation_log(decalage = d, 
                                          publi_indispo = tot_publi_OMIE_indispo)
  result$decalage <- d
  return(result)
})

table_coefficients_predicted_full  <- bind_rows(results_list)


table_coefficients_predicted_full<- table_coefficients_predicted_full %>%
  arrange(as.numeric(gsub("price_", "", variable)))


# Ajouter un léger décalage sur l'axe X en fonction de 'status'
ggplot(data = table_coefficients_predicted_full, 
       aes(x = as.numeric(gsub("price_", "", variable)), 
           y = estimate, 
           color = status)) +
  geom_point(position = position_dodge(width = 0.2)) +  # Décalage horizontal
  geom_errorbar(aes(ymin = estimate - qnorm(0.975) * std_error_clustered,
                    ymax = estimate + qnorm(0.975) * std_error_clustered), 
                width = 0.2, 
                alpha = 0.5, 
                position = position_dodge(width = 0.2)) +  # Décalage identique pour barres d'erreur
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic response to plant availability shock",
       x = "Auction",
       y = "EUR/MWh Immobilisé")



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE")
tot_publi_OMIE_all_indispo <- read_parquet( 'tot_publi_OMIE_all_indispo.parquet')

results_list <- lapply(-2:2, function(d) {
  result <- dynamic_regression_estimation_log(decalage = d, 
                                          publi_indispo = tot_publi_OMIE_all_indispo)
  result$decalage <- d
  return(result)
})


table_coefficients_predicted_all  <- bind_rows(results_list)


table_coefficients_predicted_all<- table_coefficients_predicted_all %>%
  arrange(as.numeric(gsub("price_", "", variable)))


# Ajouter un léger décalage sur l'axe X en fonction de 'status'
ggplot(data = table_coefficients_predicted_all, 
       aes(x = as.numeric(gsub("price_", "", variable)), 
           y = estimate, 
           color = status)) +
  geom_point(position = position_dodge(width = 0.2)) +  # Décalage horizontal
  geom_errorbar(aes(ymin = estimate - qnorm(0.975) * std_error_clustered,
                    ymax = estimate + qnorm(0.975) * std_error_clustered), 
                width = 0.2, 
                alpha = 0.5, 
                position = position_dodge(width = 0.2)) +  # Décalage identique pour barres d'erreur
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic response to plant availability shock",
       x = "Auction",
       y = "EUR/MWh Immobilisé")





setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs/final/OMIE")
tot_publi_OMIE_indispo_without_auction <- read_parquet( 'tot_publi_OMIE_without_auction_time.parquet')

results_list <- lapply(-2:2, function(d) {
  result <- dynamic_regression_estimation_log(decalage = d, 
                                          publi_indispo = tot_publi_OMIE_indispo_without_auction)
  result$decalage <- d
  return(result)
})


table_coefficients_predicted_without_auction  <- bind_rows(results_list)


table_coefficients_predicted_without_auction<- table_coefficients_predicted_without_auction %>%
  arrange(as.numeric(gsub("price_", "", variable)))


# Ajouter un léger décalage sur l'axe X en fonction de 'status'
ggplot(data = table_coefficients_predicted_without_auction, 
       aes(x = as.numeric(gsub("price_", "", variable)), 
           y = estimate, 
           color = status)) +
  geom_point(position = position_dodge(width = 0.2)) +  # Décalage horizontal
  geom_errorbar(aes(ymin = estimate - qnorm(0.975) * std_error_clustered,
                    ymax = estimate + qnorm(0.975) * std_error_clustered), 
                width = 0.2, 
                alpha = 0.5, 
                position = position_dodge(width = 0.2)) +  # Décalage identique pour barres d'erreur
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Dynamic response to plant availability shock",
       x = "Auction",
       y = "EUR/MWh Immobilisé")





