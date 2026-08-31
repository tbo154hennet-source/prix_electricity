library(data.table)
library(arrow)
library(fixest)

# Répertoire de sortie
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")

# Valeurs de k à tester
k_values <- c(5, 10, 15)

# Fonction générique pour calculer les pentes locales
compute_local_slopes <- function(dt, k, type = c("demand", "supply")) {
  type <- match.arg(type)
  
  # Étape 1 : garder les k observations les plus proches
  dt_k <- dt[, head(.SD[order(abs(PRICE - price))], k), by = .(DATE, Hour)]
  dt_k <- dt_k[is.finite(PRICE) & is.finite(QTY)]
  
  # Étape 2 : régression locale
  local_slopes <- dt_k[
    , {
      if (.N > 1) {
        res <- tryCatch({
          fit <- lm(QTY ~ PRICE)
          x <- PRICE
          coef_fit <- coef(fit)
          .(
            slope = coef_fit["PRICE"],
            se_slope = summary(fit)$coefficients["PRICE", "Std. Error"],
            intercept = coef_fit["(Intercept)"],
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
  outfile <- sprintf("local_slopes_reg_%s_%d.parquet", type, k)
  write_parquet(local_slopes, outfile)

  slopes_comparison <- local_slopes[
    se_slope >= 1e-5 & is.finite(slope) &
      is.finite(aggregation_weight) & aggregation_weight > 0
  ]
  comparison_keys <- slopes_comparison[, .(DATE, Hour)]
  benchmark_sample <- merge(dt_k, comparison_keys, by = c('DATE', 'Hour'))

  benchmark_model <- feols(QTY ~ PRICE | DATE^Hour, data = benchmark_sample)
  benchmark_fe <- unname(coef(benchmark_model)['PRICE'])
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
    sprintf('local_slope_benchmarks_reg_%s_%d.parquet', type, k)
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

estimate_grouped_linear_benchmark <- function(dt, curve) {
  estimation_sample <- copy(
    dt[is.finite(PRICE) & is.finite(QTY)]
  )

  slopes <- estimation_sample[
    , tryCatch({
      x <- PRICE
      y <- QTY
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
  model <- feols(QTY ~ PRICE | DATE^Hour, data = estimation_sample)
  fe_coefficient <- unname(coef(model)['PRICE'])
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
    specification = 'linear',
    mean_local = mean(valid_slopes$slope),
    weighted_local = weighted_local,
    fe_coefficient = fe_coefficient,
    recovery_difference = recovery_difference,
    n_groups = nrow(valid_slopes),
    n_obs = nrow(estimation_sample)
  )

  list(slopes = slopes, model = model, benchmark = benchmark)
}

demand_local_results_lin <- estimate_grouped_linear_benchmark(
  reconstruct_full_curve_shift_qty_demand_matched_local,
  curve = 'Demand'
)
supply_local_results_lin <- estimate_grouped_linear_benchmark(
  reconstruct_full_curve_shift_qty_supply_matched_local,
  curve = 'Supply'
)

local_demand_FE_lin <- demand_local_results_lin$model
local_supply_FE_lin <- supply_local_results_lin$model
write_parquet(
  demand_local_results_lin$slopes,
  'local_slopes_reg_demand_matched_FE.parquet'
)
write_parquet(
  supply_local_results_lin$slopes,
  'local_slopes_reg_supply_matched_FE.parquet'
)

local_elasticity_FE_benchmarks_lin <- rbind(
  demand_local_results_lin$benchmark,
  supply_local_results_lin$benchmark
)
write_parquet(
  local_elasticity_FE_benchmarks_lin,
  'local_elasticity_FE_benchmarks_lin.parquet'
)
print(local_elasticity_FE_benchmarks_lin)

k =10
# Boucle principale
for (k in k_values) {
  local_slopes_demand <- compute_local_slopes(reconstruct_full_curve_shift_qty_demand_matched_local, k, type = "demand")
  local_slopes_supply <- compute_local_slopes(reconstruct_full_curve_shift_qty_supply_matched_local, k, type = "supply")
}


