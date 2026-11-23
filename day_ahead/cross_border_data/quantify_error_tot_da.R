
library(stringr)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")

price_merge_CBF_max <- read_parquet("final_price_merge_CBF_tot.parquet")

price_merge_CBF_max$weekday <- weekdays(price_merge_CBF_max$delivery_date)
price_merge_CBF_max$creneau <- hour(price_merge_CBF_max$datetime)

# vérifier l'approximation day ahead commercial flows,
# par rapport à flux totaux
#on cherche à savoir quelle erreur on fait lorsque l'on prend tot à la place de day-ahead 

verif_data <- price_merge_CBF_max %>% 
  filter(datetime >= as.POSIXct("2022-01-01 00:00:00", tz = "UTC"))

colnames(verif_data)

# On suppose que les colonnes sont nommées de la forme "SCH_EX_DA_GB_FR" ou "SCH_EX_TOT_GB_FR"

# Step 1: Get the suffix (e.g., "GB_FR") from each column
colnames_split <- strcapture("SCH_EX_(DA|TOT)_(.+)", colnames(verif_data),
                             proto = data.frame(type = character(), zone = character()))

# Step 2: Bind to column names
col_info <- data.frame(
  name = colnames(verif_data),
  type = colnames_split$type,
  zone = colnames_split$zone,
  stringsAsFactors = FALSE
) %>% 
  filter(!is.na(zone))


# Step 3: Loop over unique zones and compute correlations
results <- lapply(unique(col_info$zone), function(z) {
  print(z)
  da_col <- col_info$name[col_info$type == "DA" & col_info$zone == z]
  tot_col <- col_info$name[col_info$type == "TOT" & col_info$zone == z]
  if (length(da_col) == 1 && length(tot_col) == 1) {
    cor_val <- cor(as.numeric(verif_data[[da_col]]), as.numeric(verif_data[[tot_col]]), use = "complete.obs")
    return(data.frame(zone = z, correlation = cor_val))
  } else {
    return(NULL)
  }
})

# Step 4: Combine results
cor_table <- do.call(rbind, results)





#modifie pour prendre day head out tot selon ce qui est disponible


# Exemple : noms des colonnes
# colnames(price_merge_CBF_max) <- c("SCH_EX_DA_GB_FR", "SCH_EX_TOT_GB_FR", "SCH_EX_DA_IT_FR", "SCH_EX_TOT_IT_FR")

# 1. Extraire les noms de zones
col_info <- str_match(colnames(price_merge_CBF_max), "SCH_EX_(DA|TOT)_(.+)")
col_info <- as.data.frame(col_info)
col_info$full_name <- colnames(price_merge_CBF_max)
col_info <- col_info[!is.na(col_info$V2), ]  # garder seulement les lignes correspondantes
colnames(col_info) <- c("match", "type", "zone", "full_name")

price_merge_CBF_max <- price_merge_CBF_max %>% 
  mutate(across(all_of(col_info$match), ~as.numeric(.)))

# 2. Pour chaque zone, créer une colonne fusionnée
for (z in unique(col_info$zone)) {
  da_col <- col_info$full_name[col_info$type == "DA" & col_info$zone == z]
  tot_col <- col_info$full_name[col_info$type == "TOT" & col_info$zone == z]
  
  if (length(da_col) == 1 & length(tot_col) == 1) {
    price_merge_CBF_max[[paste0("SCH_EX_COMBINED_", z)]] <- ifelse(
      !is.na(price_merge_CBF_max[[da_col]]) & price_merge_CBF_max[[da_col]] != 0,
      price_merge_CBF_max[[da_col]],
      price_merge_CBF_max[[tot_col]]
    )
  } else if (length(da_col) == 1) {
    price_merge_CBF_max[[paste0("SCH_EX_COMBINED_", z)]] <- price_merge_CBF_max[[da_col]]
  } else if (length(tot_col) == 1) {
    price_merge_CBF_max[[paste0("SCH_EX_COMBINED_", z)]] <- price_merge_CBF_max[[tot_col]]
  }
}



price_merge_CBF_max <- price_merge_CBF_max %>% 
  dplyr::select(-col_info$match)
colnames(price_merge_CBF_max)



#get renewable correlation



#get correlation across european renewables

renew_col <- grep("day_ahead_forecast", colnames(price_merge_CBF_max ), value = TRUE)

# Step 4: Combine results
renew_cor_table <- price_merge_CBF_max %>% 
  dplyr::select(all_of(renew_col)) %>%
  cor(use = "pairwise.complete.obs")

#check whether or not my model is not overfitted

#predict cross border flows with only exogenous variables
# that are generation day ahead forecasts, NTC and renewables


#I could try to predict each flow and sum them
#or I could try to producing the resulting flow at once


price_merge_CBF_max <- price_merge_CBF_max %>% 
  mutate(CBF_ES = rowSums(dplyr::select(., starts_with("SCH_EX_COMBINED") & ends_with("_ES")))
         - rowSums(dplyr::select(., starts_with("SCH_EX_COMBINED_ES"))),
         CBF_FR = rowSums(dplyr::select(., starts_with("SCH_EX_COMBINED") & ends_with("_FR"))) -
  rowSums(dplyr::select(., starts_with("SCH_EX_COMBINED_FR"))))


# Liste des pays partenaires avec la France
partners <- c("ES", "PT", "BE", "CH", "DE", "GB", "IT", "FR")

# Créer les noms des colonnes entrantes et sortantes
incoming_cols <- c(paste0("SCH_EX_COMBINED_", partners, "_FR"), paste0("SCH_EX_COMBINED_", partners, "_ES"))
outgoing_cols <- c(paste0("SCH_EX_COMBINED_FR_", partners), paste0("SCH_EX_COMBINED_ES_", partners))
net_cols <- c(paste0("NET_FLOW_FR_", partners), paste0("NET_FLOW_ES_", partners))

i <-4
# Calculer les flux nets pour chaque partenaire
for (i in seq_along(net_cols)) {
  inc <- incoming_cols[i]
  out <- outgoing_cols[i]
  net <- net_cols[i]
  
  if (inc %in% names(price_merge_CBF_max) & out %in% names(price_merge_CBF_max)) {
    price_merge_CBF_max[[net]] <- price_merge_CBF_max[[inc]] - price_merge_CBF_max[[out]]
    print(net)
  }
}

  
# Get the column names
net_flow_col <- grep("NET_FLOW", colnames(price_merge_CBF_max), value = TRUE)
renew_col <- grep("day_ahead_forecast", colnames(price_merge_CBF_max), value = TRUE)
gen_col <- grep("gen_tot", colnames(price_merge_CBF_max), value = TRUE)
ntc_col <- grep("NTC", colnames(price_merge_CBF_max), value = TRUE)



# colnames(price_merge_CBF_max)
# formula_mini <- "NET_FLOW_FR_ES ~ gen_tot_fr + gen_tot_esp + 
#   renewable_day_ahead_forecast_esp + renewable_day_ahead_forecast_fr +
# as.factor(year)+ as.factor(month)+ as.factor(weekday) + as.factor(creneau)"
# # Example: use formula in a linear model
# model_mini <- lm(formula_mini, data = price_merge_CBF_max)
# summary(model_mini)

flow <- "NET_FLOW_ES_FR"
#models_xgb <- list()
# Loop through each NET_FLOW column
for (flow in net_flow_col) {
  print(flow)
  # Exogenous predictors only: day-ahead renewable forecasts (the day-ahead
  # total-generation forecasts gen_tot_da_* reflect auction schedules).
  predictors <- paste(renew_col, collapse = " + ")

  # Create the formula: flow ~ predictors
  formula <- as.formula(paste(flow, "~", predictors, "+ as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)"))

  # Example: use formula in a linear model
  model <- lm(formula, data = price_merge_CBF_max)

  cat(flow, " R² ", round(summary(model)$r.squared, 3), "\n")
  
  price_merge_CBF_max[[paste0(flow, "_pred")]] <- predict(model, price_merge_CBF_max)
  
  
  # # Collapse predictors into a string: gen columns + renew columns
  # predictors <- c(gen_col, renew_col)  # Assuming gen_col and renew_col are vectors with column names
  # 
  # 
  # # Prepare the formula and subset the predictors
  # # Create dummy variables using model.matrix for categorical variables
  # X_data <- price_merge_CBF_max %>%
  #   dplyr::select(all_of(c(predictors, "year", "month", "weekday", "creneau"))) %>%
  #   mutate(across(where(is.factor), as.character)) %>%
  #   bind_cols(
  #     model.matrix(~ year + month + weekday + creneau - 1, data = price_merge_CBF_max)
  #   ) %>%
  #   dplyr::select(-c(year, month, weekday, creneau))  # Remove the original factor columns
  # 
  # 
  # # Prepare flow column (as numeric)
  # y_data <- price_merge_CBF_max[[flow]]
  # 
  # # Convert X_data to a matrix
  # X_matrix <- as.matrix(X_data)
  # 
  # # Fit the XGBoost model
  # xgb_model <- xgboost(data = X_matrix, label = y_data, 
  #                      objective = "reg:squarederror",  # For regression
  #                      nrounds = 100,                  # Number of boosting rounds
  #                      verbose = 0)                    # Suppress output
  # 
  # models_xgb[[flow]] <- xgb_model
  # 
  # # Print R² for the fitted model
  # predictions <- predict(xgb_model, X_matrix)
  # rss <- sum((predictions - y_data)^2)
  # tss <- sum((y_data - mean(y_data))^2)
  # r2_xgb <- 1 - rss / tss
  # cat(flow, " XGBoost R² ", round(r2_xgb, 3), "\n")
  # 
  # # Predict using XGBoost
  # price_merge_CBF_max[[paste0(flow, "_pred")]] <- predict(xgb_model, X_matrix)
  
 
  
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
  obs <- price_merge_CBF_max[[flow]]
  pred  <- price_merge_CBF_max[[paste0(flow, "_pred_sat")]]
  
  # Compute R² manually
  r2_sat <- 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  
  
  cat(flow, " Pseudo saturated R² ", r2_sat, "\n")
  
}


#REGARDER MAUVAISES DONNEES TIALIENNES 
#POTENTIELLEMENT IDEM ANGLETERRE
#ntc TROP FAIBLE OU PLUS PROBABLEMENT DOUBLE COMPTAGE DES FLUX


price_merge_CBF_max <- price_merge_CBF_max %>% 
  mutate(CBF_ES_pred = rowSums(dplyr::select(., starts_with("NET_FLOW_ES") & ends_with("pred"))),
         CBF_FR_pred = rowSums(dplyr::select(., starts_with("NET_FLOW_FR")& ends_with("pred"))),
         CBF_ES_pred_sat = rowSums(dplyr::select(., starts_with("NET_FLOW_ES") & ends_with("pred_sat"))),
         CBF_FR_pred_sat = rowSums(dplyr::select(., starts_with("NET_FLOW_FR")& ends_with("pred_sat"))),
         )

View(price_merge_CBF_max %>%
         dplyr::select(datetime, CBF_ES, CBF_ES_pred, CBF_ES_pred_sat) %>% 
       filter(CBF_ES_pred!= CBF_ES_pred_sat) )

View(price_merge_CBF_max %>%
       dplyr::select(datetime, CBF_ES, CBF_ES_pred, CBF_ES_pred_sat,
              NTC_ES_PT, NTC_PT_ES, NTC_ES_FR, NTC_FR_ES,
              NET_FLOW_ES_FR_pred, NET_FLOW_ES_PT_pred,
              NET_FLOW_ES_FR_pred_sat, NET_FLOW_ES_PT_pred_sat,
              NET_FLOW_ES_FR, NET_FLOW_ES_PT
              
              ) %>% 
       filter(CBF_ES_pred!= CBF_ES_pred_sat) )


obs <- price_merge_CBF_max$CBF_ES
pred <- price_merge_CBF_max$CBF_ES_pred
pred_sat <- price_merge_CBF_max$CBF_ES_pred_sat

# Compute R² manually
r2lin <- 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
r2lin_cap_sat <- 1 - sum((obs - pred_sat)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)

cat("R² Linear model:", round(r2lin, 4), "\n")
cat("R² Linear model (saturated):", round(r2lin_cap_sat, 4), "\n")



obs <- price_merge_CBF_max$CBF_FR
pred <- price_merge_CBF_max$CBF_FR_pred
pred_sat <- price_merge_CBF_max$CBF_FR_pred_sat

# Compute R² manually
r2lin <- 1 - sum((obs - pred)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
r2lin_cap_sat <- 1 - sum((obs - pred_sat)^2, na.rm = TRUE) / sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)

cat("R² Linear model:", round(r2lin, 4), "\n")
cat("R² Linear model (saturated):", round(r2lin_cap_sat, 4), "\n")



colnames(price_merge_CBF_max)
getwd()

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")
price_merge_CBF_max$year <- as.numeric(as.character(price_merge_CBF_max$year))
price_merge_CBF_max$month <- as.numeric(as.character(price_merge_CBF_max$month))
price_merge_CBF_max$creneau <- as.numeric(as.character(price_merge_CBF_max$creneau))
# saveRDS(models_xgb, "models_xgb.rds")
# saveRDS(models_xgb_bis, "models_xgb__out_of_sample.rds")
write_parquet(price_merge_CBF_max, 
              "price_merge_CBF_max.parquet")


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/cross_border")
price_merge_CBF_max <- read_parquet("price_merge_CBF_max.parquet")
#plot the series at the monthly level
#show how well I predict net cross border flows

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
  labs(title = "Monthly Average CBF vs Predictions",
       x = "Date", y = "CBF_DA")+ 
  theme_minimal()



library(ggplot2)
ggplot(price_merge_CBF_max_monthly, aes(x = Date)) +
  geom_line(aes(y = CBF_ES, color = "ES", linetype = "Actual")) +
  geom_line(aes(y = CBF_ES_pred, color = "ES", linetype = "Predicted")) +
  geom_line(aes(y = CBF_FR, color ="FR" , linetype ="Actual" )) +
  geom_line(aes(y = CBF_FR_pred, color = "FR", linetype = "Predicted" )) +
  labs(title = "Monthly Average CBF vs Predictions",
       x = "Date", y = "CBF")+ 
  theme_minimal()


