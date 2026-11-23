library(data.table)
library(arrow)

# Répertoire de sortie
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")

# Valeurs de k à tester
k_values <- c(5, 10, 15)

# Fonction générique pour calculer les pentes locales
compute_local_slopes <- function(dt, k, type = c("demand", "supply")) {
  type <- match.arg(type)
  
  # Étape 1 : garder les k observations les plus proches
  dt_k <- dt[, .SD[order(abs(PRICE - price))][1:k], by = .(DATE, Hour)]
  
  # Étape 2 : régression locale
  local_slopes <- dt_k[
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
  write_parquet(local_slopes, outfile)
  
  message(sprintf("✅ Saved %s", outfile))
  return(local_slopes)
}

k =10
# Boucle principale
for (k in k_values) {
  local_slopes_demand <- compute_local_slopes(reconstruct_full_curve_shift_qty_demand_matched_local, k, type = "demand")
  local_slopes_supply <- compute_local_slopes(reconstruct_full_curve_shift_qty_supply_matched_local, k, type = "supply")
}


                                              