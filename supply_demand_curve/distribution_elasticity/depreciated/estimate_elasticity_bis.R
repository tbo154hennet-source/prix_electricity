library(data.table)
library(lubridate)
library(fixest)
library(arrow)
library(tidyr)
library(dplyr)


# setwd(save_path_data)
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp_NTC.parquet") %>% as.data.table()
setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)

price_indispo_merge_esp$global_tilted_irradiance <- price_indispo_merge_esp$global_tilted_irradiance/1000
# PrÃ©traitement sur l'autre table
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(Hour = creneau + 1)

# Remplir les NAs dans la colonne 'spot_gas' avec la derni?re valeur non manquante pr?c?dente
price_indispo_merge_esp[, spot_gas := nafill(spot_gas, "locf")]


colnames_with_capacity <- colnames(price_indispo_merge_esp)[grepl("Capacity", colnames(price_indispo_merge_esp))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables",
    "renewable_day_ahead_forecast_ex_es", "cbf_es",
    "NTC_ES_FR" ,"NTC_FR_ES",                                 
    "NTC_ES_PT", "NTC_PT_ES"
  )
price_indispo_merge_esp[, (colnames_with_capacity) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = colnames_with_capacity]

price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(across(all_of(capacity_fields), 
                ~ . / 1000))
# price_indispo_merge_esp_subset <- price_indispo_merge_esp[, ..selected_cols]
# price_indispo_merge_esp_subset[, Hour := creneau + 1]

#add mismatch
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(cold_excess_mismatch = cold_excess_demand - cold_excess_supply,
         heat_excess_mismatch = heat_excess_demand - heat_excess_supply,
         precipitation_mismatch = precipitation_demand - precipitation_supply,
         relative_humidity_mismatch = relative_humidity_demand - relative_humidity_supply,
         temperature_mismatch = temperature_demand - temperature_supply
  )


setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_qty_demand_unmatched<- read_parquet( 'reconstruct_full_curve_shift_qty_demand_unmatched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_demand_unmatched)

test <- reconstruct_full_curve_shift_qty_demand_unmatched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_demand_unmatched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_demand_unmatched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_demand_unmatched<- reconstruct_full_curve_shift_qty_demand_unmatched[complete.cases(reconstruct_full_curve_shift_qty_demand_unmatched)]


demand <- feols(log(QTY) ~ log(PRICE), data = reconstruct_full_curve_shift_qty_demand_unmatched)

# Global model with fixed effects
demand_FE <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                   data = reconstruct_full_curve_shift_qty_demand_unmatched)


#fusion
reconstruct_full_curve_shift_qty_demand_unmatched_local <- merge(
  reconstruct_full_curve_shift_qty_demand_unmatched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)

# Filtrage par groupe
reconstruct_full_curve_shift_qty_demand_unmatched_local <- reconstruct_full_curve_shift_qty_demand_unmatched_local[
  , .SD[
    PRICE >= price - 20 & PRICE <= price + 20&
      QTY   >= gen_tot_da - 2000 & QTY   <= gen_tot_da + 2000
  ],
  by = .(DATE, Hour)
]


local_demand <- feols(log(QTY) ~ log(PRICE),
                      data = reconstruct_full_curve_shift_qty_demand_unmatched_local)

# Global model with fixed effects
local_demand_FE <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                         data = reconstruct_full_curve_shift_qty_demand_unmatched_local)


#avoir une élast

etable(demand, demand_FE,
       local_demand, local_demand_FE,
       
       dict = c("log(QTY)" = "log Quantity", 
                "log(PRICE)" = "log Price"),
       title = "Elasticity Estimates of demand")

# rm(reconstruct_full_curve_shift_qty_demand_unmatched)
# gc()


reconstruct_full_curve_shift_qty_supply_unmatched<- read_parquet( 'reconstruct_full_curve_shift_qty_supply_unmatched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_supply_unmatched)

test <- reconstruct_full_curve_shift_qty_supply_unmatched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_supply_unmatched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_supply_unmatched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_supply_unmatched<- reconstruct_full_curve_shift_qty_supply_unmatched[complete.cases(reconstruct_full_curve_shift_qty_supply_unmatched)]


supply <- feols(log(QTY) ~ log(PRICE), data = reconstruct_full_curve_shift_qty_supply_unmatched)

# Global model with fixed effects
supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                   data = reconstruct_full_curve_shift_qty_supply_unmatched)


#fusion
reconstruct_full_curve_shift_qty_supply_unmatched_local <- merge(
  reconstruct_full_curve_shift_qty_supply_unmatched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)

# Filtrage par groupe
reconstruct_full_curve_shift_qty_supply_unmatched_local <- reconstruct_full_curve_shift_qty_supply_unmatched_local[
  , .SD[
    PRICE >= price - 20& PRICE <= price + 20&
      QTY   >= gen_tot_da - 2000 & QTY   <= gen_tot_da + 2000
  ],
  by = .(DATE, Hour)
]


local_supply <- feols(log(QTY) ~ log(PRICE),
                      data = reconstruct_full_curve_shift_qty_supply_unmatched_local)

# Global model with fixed effects
local_supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                         data = reconstruct_full_curve_shift_qty_supply_unmatched_local)


#avoir une élast

etable(supply, supply_FE,
       local_supply, local_supply_FE,
       
       dict = c("log(QTY)" = "log Quantity", 
                "log(PRICE)" = "log Price"),
       title = "Elasticity Estimates of Supply")






jours_selection <- as.Date(c("2023-01-10", "2023-01-11", "2023-01-12",
                             "2023-01-13", "2023-01-14"))
hours_selection <- c(10, 12, 14)

subset_data <- reconstruct_full_curve_shift_qty_supply_unmatched_local[
  DATE %in% jours_selection & Hour %in% hours_selection
]

ggplot(subset_data, aes(x = PRICE, y = QTY, color = factor(DATE), alpha = factor(Hour))) +
  geom_point() +
  labs(
    title = "Nuage de points : Prix vs Quantité (5 journées, heures 10 et 14)",
    x = "Prix",
    y = "Quantité",
    color = "Date"
  ) +
  theme_minimal()


# Global model with fixed effects
m_select <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                         data = subset_data)

coef(m_select)



subset_data <- reconstruct_full_curve_shift_qty_demand_unmatched_local[
  DATE %in% jours_selection
]

ggplot(subset_data, aes(x = PRICE, y = QTY, color = factor(DATE))) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Nuage de points : Prix vs Quantité (5 journées)",
    x = "Prix",
    y = "Quantité",
    color = "Date"
  ) +
  theme_minimal()

# Global model with fixed effects
m_select <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                  data = subset_data)

coef(m_select)


