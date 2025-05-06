



colnames(price_merge_CBF_max)
class(price_merge_CBF_max$year)
price_merge_CBF_max$year <- as.numeric(price_merge_CBF_max$year)
price_merge_CBF_max$month <- as.factor(price_merge_CBF_max$month)
price_merge_CBF_max$creneau <- as.factor(price_merge_CBF_max$creneau)

# Loop through each NET_FLOW column
for (flow in net_flow_col) {
  print(flow)
  
  predictors <- paste(c(gen_col, renew_col), collapse = " + ")
  
  # Create the formula: flow ~ predictors
  formula <- as.formula(paste(flow, "~", predictors, "+ year + as.factor(month) + as.factor(weekday) + as.factor(creneau)"))
  
  # Example: use formula in a linear model
  model <- lm(formula, data = price_merge_CBF_max %>% 
                filter(as.numeric(year) <= 2020))
  
  cat(flow, " R² ", round(summary(model)$r.squared, 3), "\n")
  
  price_merge_CBF_max[[paste0(flow, "_pred")]] <- predict(model, price_merge_CBF_max)
  ntc_col_export <- sub("NET_FLOW", "NTC", flow)
  
  # Split the string and reorder the country codes
  split_parts <- unlist(strsplit(ntc_col_export, "_"))
  ntc_col_import <- paste0(split_parts[1], "_", split_parts[3], "_", split_parts[2])
  
  #ntc_col %in% colnames(price_merge_CBF_max)
  
  price_merge_CBF_max[[paste0(flow, "_pred_sat")]] <-
    ifelse(price_merge_CBF_max[[paste0(flow, "_pred")]] > price_merge_CBF_max[[ntc_col_import]],
           price_merge_CBF_max[[ntc_col_import]], price_merge_CBF_max[[paste0(flow, "_pred")]])
  price_merge_CBF_max[[paste0(flow, "_pred_sat")]] <-
    ifelse(price_merge_CBF_max[[paste0(flow, "_pred")]] < -price_merge_CBF_max[[ntc_col_import]],
           -price_merge_CBF_max[[ntc_col_import]], price_merge_CBF_max[[paste0(flow, "_pred")]])
  
  price_merge_CBF_max[[paste0(flow, "_pred_sat")]] <-
    ifelse(price_merge_CBF_max[[paste0(flow, "_pred")]] < -price_merge_CBF_max[[ntc_col_export]],
           -price_merge_CBF_max[[ntc_col_export]], price_merge_CBF_max[[paste0(flow, "_pred")]])
  price_merge_CBF_max[[paste0(flow, "_pred_sat")]] <-
    ifelse(price_merge_CBF_max[[paste0(flow, "_pred")]] > price_merge_CBF_max[[ntc_col_export]],
           price_merge_CBF_max[[ntc_col_export]], price_merge_CBF_max[[paste0(flow, "_pred")]])
  
  # View(price_merge_CBF_max %>%
  #        dplyr::select(datetime, flow, paste0(flow, "_pred"), paste0(flow, "_pred_sat"),
  #               ntc_col_export, ntc_col_import)  %>% 
  #   filter(datetime == "2015-07-05 12:00:00"))
  
  # 
  pred <- price_merge_CBF_max[[flow]]
  obs <- price_merge_CBF_max[[paste0(flow, "_pred_sat")]]
  
  # Compute R² manually
  r2_sat <- 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  
  
  cat(flow, " Pseudo saturated R² ", r2_sat, "\n")
  
}

price_merge_CBF_max <- price_merge_CBF_max %>% 
  mutate(CBF_ES_pred = rowSums(dplyr::select(., starts_with("NET_FLOW_ES") & ends_with("pred"))),
         CBF_FR_pred = rowSums(dplyr::select(., starts_with("NET_FLOW_FR")& ends_with("pred"))),
         CBF_ES_pred_sat = rowSums(dplyr::select(., starts_with("NET_FLOW_ES") & ends_with("pred_sat"))),
         CBF_FR_pred_sat = rowSums(dplyr::select(., starts_with("NET_FLOW_FR")& ends_with("pred_sat"))),
  )


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")
price_merge_CBF_max$year <- as.numeric(as.character(price_merge_CBF_max$year))
price_merge_CBF_max$month <- as.numeric(as.character(price_merge_CBF_max$month))
price_merge_CBF_max$creneau <- as.numeric(as.character(price_merge_CBF_max$creneau))


write_parquet(price_merge_CBF_max, 
              "price_merge_CBF_max_out_of_sample_lin.parquet")


#colnames(import_data_reg)
price_merge_CBF_max_monthly <- price_merge_CBF_max %>% 
  mutate(year = as.numeric(year), month = as.numeric(month)) %>%
  group_by(year, month) %>% 
  summarise(
    CBF_ES = mean(CBF_ES, na.rm = TRUE),
    CBF_ES_pred = mean(CBF_ES_pred, na.rm = TRUE),
    CBF_ES_pred_sat = mean(CBF_ES_pred_sat, na.rm = TRUE),
    CBF_FR = mean(CBF_FR, na.rm = TRUE),
    CBF_FR_pred = mean(CBF_FR_pred, na.rm = TRUE),
    CBF_FR_pred_sat = mean(CBF_FR_pred_sat, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarise
  ) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-01")))
library(ggplot2)
ggplot(price_merge_CBF_max_monthly, aes(x = Date)) +
  geom_line(aes(y = CBF_ES, color = "Actual", group = "Actual")) +
  geom_line(aes(y = CBF_ES_pred, color = "Predicted", group = "Predicted")) +
  geom_line(aes(y = CBF_ES_pred_sat, color = "Saturated Predicted", group  = "Saturated Predicted" )) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dashed", color = "purple")+
  labs(title = "Monthly Average CBF vs Predictions",
       x = "Date", y = "CBF_DA")+ 
  theme_minimal()



library(ggplot2)
ggplot(price_merge_CBF_max_monthly, aes(x = Date)) +
  geom_line(aes(y = CBF_ES, color = "ES", linetype = "Actual")) +
  geom_line(aes(y = CBF_ES_pred, color = "ES", linetype = "Predicted")) +
  geom_line(aes(y = CBF_FR, color ="FR" , linetype ="Actual" )) +
  geom_line(aes(y = CBF_FR_pred, color = "FR", linetype = "Predicted" )) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dashed", color = "purple")+
  labs(title = "Monthly Average CBF vs Predictions",
       x = "Date", y = "CBF")+ 
  theme_minimal()


