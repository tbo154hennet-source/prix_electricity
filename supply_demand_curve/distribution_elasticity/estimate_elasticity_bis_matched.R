library(data.table)
library(lubridate)
library(fixest)
library(arrow)
library(tidyr)
library(dplyr)
library(ggplot2)

# Source commune avec les estimations IV.
output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_esp <- read_parquet(
  "price_indispo_merge_esp_npc_all_final_2026.parquet"
) %>%
  filter(
    datetime <= as.POSIXct("2025-09-30 23:00:00", tz = "UTC")
  ) %>%
  as.data.table()
price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp$date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)

price_indispo_merge_esp$global_tilted_irradiance <- price_indispo_merge_esp$global_tilted_irradiance/1000
# Prétraitement sur l'autre table
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(Hour = as.character(creneau + 1))

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
         precipitation_mismatch = precipitation_demand - precipitation_ss,
         relative_humidity_mismatch = relative_humidity_2m_demand - relative_humidity_2m_ss,
         temperature_mismatch = temperature_demand - temperature_ss
  )


setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_qty_demand_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_demand_matched)
reconstruct_full_curve_shift_qty_demand_matched[, Hour := as.character(Hour)]

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
  price_indispo_merge_esp[, .(date, Hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("date", "Hour")
)


# Filtrage par groupe
reconstruct_full_curve_shift_qty_demand_matched_local <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , .SD[
    PRICE >= price - 20 & PRICE <= price  &
      QTY   >= load_tot - 2000 & QTY   <= load_tot + 2000 &
      price >= 10
  ],
  by = .(DATE, Hour)
]


estimate_grouped_log_benchmark <- function(dt, curve) {
  estimation_sample <- copy(
    dt[is.finite(PRICE) & is.finite(QTY) & PRICE > 0 & QTY > 0]
  )

  slopes <- estimation_sample[
    , tryCatch({
      x <- log(PRICE)
      y <- log(QTY)
      if (.N <= 1 || uniqueN(x) <= 1) {
        stop('Insufficient within-group price variation')
      }
      fit <- lm(y ~ x)
      .(
        slope = unname(coef(fit)['x']),
        se_slope = unname(summary(fit)$coefficients['x', 'Std. Error']),
        intercept = unname(coef(fit)['(Intercept)']),
        mean_price = mean(PRICE),
        aggregation_weight = sum((x - mean(x))^2),
        n_obs = .N
      )
    }, error = function(e) {
      .(
        slope = NA_real_,
        se_slope = NA_real_,
        intercept = NA_real_,
        mean_price = mean(PRICE, na.rm = TRUE),
        aggregation_weight = NA_real_,
        n_obs = .N
      )
    }),
    by = .(DATE, Hour)
  ]

  valid_slopes <- slopes[
    is.finite(slope) & is.finite(aggregation_weight) &
      aggregation_weight > 0
  ]
  model <- feols(
    log(QTY) ~ log(PRICE) | DATE^Hour,
    data = estimation_sample
  )
  fe_coefficient <- unname(coef(model)['log(PRICE)'])
  weighted_local <- weighted.mean(
    valid_slopes$slope,
    valid_slopes$aggregation_weight
  )
  recovery_difference <- fe_coefficient - weighted_local
  tolerance <- 1e-10 * max(1, abs(fe_coefficient))

  if (!isTRUE(abs(recovery_difference) <= tolerance)) {
    stop(sprintf(
      '%s: FE and weighted local slopes differ by %.3e',
      curve, recovery_difference
    ))
  }

  benchmark <- data.table(
    curve = curve,
    specification = 'log-log',
    mean_local = mean(valid_slopes$slope),
    weighted_local = weighted_local,
    fe_coefficient = fe_coefficient,
    recovery_difference = recovery_difference,
    n_groups = nrow(valid_slopes),
    n_obs = nrow(estimation_sample)
  )

  list(
    sample = estimation_sample,
    slopes = slopes,
    model = model,
    benchmark = benchmark
  )
}

local_demand <- feols(log(QTY) ~ log(PRICE),
                      data = reconstruct_full_curve_shift_qty_demand_matched_local)

# Standardized objects used by elasticity_comparison.R.
demand_local_results <- estimate_grouped_log_benchmark(
  reconstruct_full_curve_shift_qty_demand_matched_local,
  curve = 'Demand'
)
local_slopes_demand <- demand_local_results$slopes
local_demand_FE <- demand_local_results$model
write_parquet(
  local_slopes_demand,
  'local_slopes_demand_matched_FE.parquet'
)
# rm(reconstruct_full_curve_shift_qty_demand_matched)
# gc()


reconstruct_full_curve_shift_qty_supply_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_supply_matched)
reconstruct_full_curve_shift_qty_supply_matched[, Hour := as.character(Hour)]

test <- reconstruct_full_curve_shift_qty_supply_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_supply_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_supply_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_supply_matched<- reconstruct_full_curve_shift_qty_supply_matched[complete.cases(reconstruct_full_curve_shift_qty_supply_matched)]


supply <- feols(log(QTY) ~ log(PRICE), data = reconstruct_full_curve_shift_qty_supply_matched)

# Global model with fixed effects
supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour^DATE,
                   data = reconstruct_full_curve_shift_qty_supply_matched)

length(unique(reconstruct_full_curve_shift_qty_supply_matched$DATE))
#fusion
reconstruct_full_curve_shift_qty_supply_matched_local <- merge(
  reconstruct_full_curve_shift_qty_supply_matched,
  price_indispo_merge_esp[, .(date, Hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("date", "Hour")
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

# Same definition and aggregation rule for supply.
supply_local_results <- estimate_grouped_log_benchmark(
  reconstruct_full_curve_shift_qty_supply_matched_local,
  curve = 'Supply'
)
local_slopes_supply <- supply_local_results$slopes
local_supply_FE <- supply_local_results$model
write_parquet(
  local_slopes_supply,
  'local_slopes_supply_matched_FE.parquet'
)

local_elasticity_FE_benchmarks <- rbind(
  demand_local_results$benchmark,
  supply_local_results$benchmark
)
write_parquet(
  local_elasticity_FE_benchmarks,
  'local_elasticity_FE_benchmarks.parquet'
)
print(local_elasticity_FE_benchmarks)



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
