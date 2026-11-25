library(data.table)
library(arrow)
library(fixest)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
# Local windows (+/- 15 EUR around the clearing price, quantity within 2,000 MWh of
# gen_forecast_net, clearing price >= 10 EUR) written by estimate_elasticity_bis_matched.R.
reconstruct_full_curve_shift_qty_demand_matched_local <- as.data.table(read_parquet('curve_local_window_demand.parquet'))
reconstruct_full_curve_shift_qty_supply_matched_local <- as.data.table(read_parquet('curve_local_window_supply.parquet'))

# Price brackets to test (half-width in EUR); 10 EUR is the benchmark of estimate_elasticity_bis_matched.R.
k_values <- c(5, 10, 15)

# Fonction générique pour calculer les pentes locales
compute_local_slopes <- function(dt, k, type = c("demand", "supply")) {
  type <- match.arg(type)

  # Étape 1 : garder les observations à +/- k euros du prix d'équilibre
  dt_k <- dt[abs(PRICE - price) <= k]
  dt_k <- dt_k[is.finite(PRICE) & is.finite(QTY) & PRICE > 0 & QTY > 0]
  # Étape 2 : régression locale
  

  local_slopes <- dt_k[
    , {
      if (.N > 1) {
        res <- tryCatch({
          x <- log(PRICE)
          y <- log(QTY)
          fit <- lm(y ~ x)
          coef_fit <- coef(fit)
          .(
            slope = unname(coef_fit["x"]),
            se_slope = unname(summary(fit)$coefficients["x", "Std. Error"]),
            intercept = unname(coef_fit["(Intercept)"]),
            mean_log_price = mean(PRICE, na.rm = TRUE),
            aggregation_weight = sum((x - mean(x))^2),
            n_obs = .N
          )
        }, error = function(e) {
          .(
            slope = NA_real_,
            se_slope = NA_real_,
            intercept = NA_real_,
            mean_log_price = NA_real_,
            aggregation_weight = NA_real_,
            n_obs = .N
          )
        })
        res
      } else {
        .(
          slope = NA_real_,
          se_slope = NA_real_,
          intercept = NA_real_,
          mean_log_price = NA_real_,
          aggregation_weight = NA_real_,
          n_obs = .N
        )
      }
    },
    by = .(DATE, Hour)
  ]
  
  # Étape 3 : sauvegarde
  outfile <- sprintf("local_slopes_%s_%d.parquet", type, k)
  write_parquet(local_slopes, outfile)

  # Apply the same sample rules as elasticity_comparison.R.
  slopes_comparison <- local_slopes[
    mean_log_price >= 10 & se_slope >= 1e-5 &
      is.finite(slope) & is.finite(aggregation_weight) &
      aggregation_weight > 0
  ]
  comparison_keys <- slopes_comparison[, .(DATE, Hour)]
  benchmark_sample <- merge(dt_k, comparison_keys, by = c('DATE', 'Hour'))

  benchmark_model <- feols(
    log(QTY) ~ log(PRICE) | DATE^Hour,
    data = benchmark_sample
  )
  benchmark_fe <- unname(coef(benchmark_model)['log(PRICE)'])
  benchmark_aggregation <- weighted.mean(
    slopes_comparison$slope,
    slopes_comparison$aggregation_weight
  )
  benchmark_difference <- benchmark_fe - benchmark_aggregation
  
  message(sprintf("✅ Saved %s", outfile))
  benchmark_summary <- data.table(
    type = type,
    k = k,
    average_hourly = mean(slopes_comparison$slope),
    fe_benchmark = benchmark_fe,
    iv_aggregation = benchmark_aggregation,
    fe_minus_aggregation = benchmark_difference,
    n_groups = nrow(slopes_comparison),
    n_obs = nrow(benchmark_sample)
  )
  write_parquet(
    benchmark_summary,
    sprintf('local_slope_benchmarks_%s_%d.parquet', type, k)
  )

  tolerance <- 1e-10 * max(1, abs(benchmark_fe))
  if (!isTRUE(abs(benchmark_difference) <= tolerance)) {
    stop(sprintf(
      '%s k=%d: FE and local-slope aggregation differ by %.3e',
      type, k, benchmark_difference
    ))
  }
  message(sprintf(
    'FE = %.8f, aggregation = %.8f (difference %.3e)',
    benchmark_fe, benchmark_aggregation, benchmark_difference
  ))
  return(local_slopes)
}

k =10
# Boucle principale
for (k in k_values) {
  local_slopes_demand <- compute_local_slopes(reconstruct_full_curve_shift_qty_demand_matched_local, k, type = "demand")
  local_slopes_supply <- compute_local_slopes(reconstruct_full_curve_shift_qty_supply_matched_local, k, type = "supply")
}


compute_local_slopes_reg <- function(dt, k, type = c("demand", "supply")) {
  type <- match.arg(type)
  
  # Étape 1 : garder les k observations les plus proches
  dt_k <- dt[, .SD[order(abs(PRICE - price))][1:k], by = .(DATE, Hour)]
  colnames(dt_k)
  # Étape 2 : régression locale
  
  
  local_slopes_reg <- dt_k[
    , {
      if (.N > 1) {
        res <- tryCatch({
          fit <- lm(QTY ~ PRICE)
          w <- PRICE^2
          coef_fit <- coef(fit)
          .(
            slope = coef_fit["PRICE"],
            se_slope = summary(fit)$coefficients["PRICE", "Std. Error"],
            intercept = coef_fit["(Intercept)"],
            mean_log_price = mean(PRICE, na.rm = TRUE),
            w_mean_log_price = weighted.mean(PRICE, w, na.rm = TRUE)
          )
        }, error = function(e) {
          .(
            slope = NA_real_,
            se_slope = NA_real_,
            intercept = NA_real_,
            mean_log_price = NA_real_,
            w_mean_log_price = NA_real_
          )
        })
        res
      } else {
        .(
          slope = NA_real_,
          se_slope = NA_real_,
          intercept = NA_real_,
          mean_log_price = NA_real_,
          w_mean_log_price = NA_real_
        )
      }
    },
    by = .(DATE, Hour)
  ]
  
  # Étape 3 : sauvegarde
  outfile <- sprintf("local_slopes_reg_%s_%d.parquet", type, k)
  write_parquet(local_slopes_reg, outfile)
  
  message(sprintf("✅ Saved %s", outfile))
  return(local_slopes_reg)
}

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")

local_slopes_demand_reg <- compute_local_slopes_reg(reconstruct_full_curve_shift_qty_demand_matched_local, 10, type = "demand")
local_slopes_supply_reg <- compute_local_slopes_reg(reconstruct_full_curve_shift_qty_supply_matched_local, 10, type = "supply")


#jeter à la poubelle toutes les slopes tq se_slope nulles



