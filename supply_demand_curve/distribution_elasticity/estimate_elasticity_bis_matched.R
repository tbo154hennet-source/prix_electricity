library(data.table)
library(lubridate)
library(fixest)
library(arrow)
library(tidyr)
library(dplyr)
library(ggplot2)

# setwd(save_path_data)
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp_NTC.parquet") %>% as.data.table()
setnames(price_indispo_merge_esp, "day_ahead_forecast", "renewable_day_ahead_forecast")

price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp $delivery_date)
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
reconstruct_full_curve_shift_qty_demand_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_demand_matched)

test <- reconstruct_full_curve_shift_qty_demand_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_demand_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_demand_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_demand_matched<- reconstruct_full_curve_shift_qty_demand_matched[complete.cases(reconstruct_full_curve_shift_qty_demand_matched)]


demand <- feols(log(QTY) ~ log(PRICE), data = reconstruct_full_curve_shift_qty_demand_matched)

# Global model with fixed effects
demand_FE <- feols(log(QTY) ~ log(PRICE) | Hour+ DATE + month + year, 
                   data = reconstruct_full_curve_shift_qty_demand_matched)


#fusion
reconstruct_full_curve_shift_qty_demand_matched_local <- merge(
  reconstruct_full_curve_shift_qty_demand_matched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


# Filtrage par groupe
reconstruct_full_curve_shift_qty_demand_matched_local <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , .SD[
    PRICE >= price - 20 & PRICE <= price  &
      QTY   >= load_tot - 2000 & QTY   <= load_tot + 2000
  ],
  by = .(DATE, Hour)
]


local_demand <- feols(log(QTY) ~ log(PRICE),
                      data = reconstruct_full_curve_shift_qty_demand_matched_local)

# Global model with fixed effects
local_demand_FE <- feols(log(QTY) ~ log(PRICE) | Hour+ month + year, 
                         data = reconstruct_full_curve_shift_qty_demand_matched_local)

local_slopes_demand <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , {
    if (.N > 1) {  # need at least 2 points to fit a line
      fit <- lm(log(QTY) ~ log(PRICE))
      .(slope = coef(fit)["log(PRICE)"], intercept = coef(fit)["(Intercept)"])
    } else {
      .(slope = NA_real_, intercept = NA_real_)
    }
  },
  by = .(DATE, Hour)
]
mean(local_slopes_demand$slope, na.rm= TRUE)
# rm(reconstruct_full_curve_shift_qty_demand_matched)
# gc()


reconstruct_full_curve_shift_qty_supply_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_supply_matched)

test <- reconstruct_full_curve_shift_qty_supply_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_supply_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_supply_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_supply_matched<- reconstruct_full_curve_shift_qty_supply_matched[complete.cases(reconstruct_full_curve_shift_qty_supply_matched)]


supply <- feols(log(QTY) ~ log(PRICE), data = reconstruct_full_curve_shift_qty_supply_matched)

# Global model with fixed effects
supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month + year, 
                   data = reconstruct_full_curve_shift_qty_supply_matched)

length(unique(reconstruct_full_curve_shift_qty_supply_matched$DATE))
#fusion
reconstruct_full_curve_shift_qty_supply_matched_local <- merge(
  reconstruct_full_curve_shift_qty_supply_matched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)

# Filtrage par groupe
reconstruct_full_curve_shift_qty_supply_matched_local <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , .SD[
    PRICE >= price - 40 & PRICE <= price +40 &
      QTY   >= load_tot - 2000 & QTY   <= load_tot + 2000
  ],
  by = .(DATE, Hour)
]

reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]


local_supply <- feols(log(QTY) ~ log(PRICE),
                      data = reconstruct_full_curve_shift_qty_supply_matched_local)

reconstruct_full_curve_shift_qty_supply_matched_local$Hour_Date <- 
  reconstruct_full_curve_shift_qty_supply_matched_local$DATE + reconstruct_full_curve_shift_qty_supply_matched_local$Hour
# Global model with fixed effects
local_supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour_Date , 
                         data = reconstruct_full_curve_shift_qty_supply_matched_local)


local_slopes <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , {
    if (.N > 1) {  # need at least 2 points to fit a line
      fit <- lm(log(QTY) ~ log(PRICE))
      .(slope = coef(fit)["log(PRICE)"], intercept = coef(fit)["(Intercept)"])
    } else {
      .(slope = NA_real_, intercept = NA_real_)
    }
  },
  by = .(DATE, Hour)
]
class(local_slopes)
mean(local_slopes$slope, na.rm= TRUE)



#avoir une élast

etable(demand, demand_FE,
       local_demand, local_demand_FE,
       
       dict = c("log(QTY)" = "log Quantity", 
                "log(PRICE)" = "log Price"),
       title = "Elasticity Estimates of demand")

#avoir une élast

etable(supply, supply_FE,
       local_supply, local_supply_FE,
       
       dict = c("log(QTY)" = "log Quantity", 
                "log(PRICE)" = "log Price"),
       title = "Elasticity Estimates of Supply")


save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)
etable(
  demand, demand_FE,
  local_demand, local_demand_FE,
  dict = c("log(QTY)" = "log Quantity", 
           "log(PRICE)" = "log Price"),
  title = "Elasticity Estimates of Demand",
  tex = TRUE,
  file = "elasticity_demand_direct.tex"   # enregistre le tableau en LaTeX
)
etable(
  supply, supply_FE,
  local_supply, local_supply_FE,
  dict = c("log(QTY)" = "log Quantity", 
           "log(PRICE)" = "log Price"),
  title = "Elasticity Estimates of Supply",
  tex = TRUE,
  file = "elasticity_supply_direct.tex"
)





jours_selection <- as.Date(c("2021-09-27"))
hours_selection <- c(19)

subset_data <- reconstruct_full_curve_shift_qty_supply_matched_local[
  DATE %in% jours_selection #& Hour %in% hours_selection
]

ggplot(subset_data, aes(x = QTY, y = PRICE,  color = factor(Hour))) +#, color = factor(DATE))) +
  geom_line() +
  labs(
    title = "Nuage de points : Prix vs Quantité (5 journées, heures 10 et 14)",
    
    x = "Quantité",
    y = "Prix",
    color = "Hour"
  ) +
  theme_minimal()


subset_data <- reconstruct_full_curve_shift_qty_supply_matched[
  DATE %in% jours_selection #& Hour %in% hours_selection
]

ggplot(subset_data, aes(x = QTY, y = PRICE,  color = factor(Hour))) +#, color = factor(DATE))) +
  geom_line() +
  labs(
    title = "Nuage de points : Prix vs Quantité (5 journées, heures 10 et 14)",
    x = "Quantité",
    y = "Prix",
    
    color = "Date"
  ) +
  theme_minimal()


# Global model with fixed effects
m_select <- feols(log(QTY) ~ log(PRICE) | Hour + DATE + month +year, 
                  data = subset_data)

coef(m_select)



subset_data <- reconstruct_full_curve_shift_qty_demand_matched_local[
  DATE %in% jours_selection & Hour %in% hours_selection
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


