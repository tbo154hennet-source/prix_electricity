library(data.table)
library(arrow)


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

#fusion
reconstruct_full_curve_shift_qty_demand_matched_local <- merge(
  reconstruct_full_curve_shift_qty_demand_matched,
  price_indispo_merge_esp_npc[, .(delivery_date, Hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "Hour")
)


reconstruct_full_curve_shift_qty_supply_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_supply_matched)

test <- reconstruct_full_curve_shift_qty_supply_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_supply_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_supply_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_supply_matched<- reconstruct_full_curve_shift_qty_supply_matched[complete.cases(reconstruct_full_curve_shift_qty_supply_matched)]

#fusion
reconstruct_full_curve_shift_qty_supply_matched_local <- merge(
  reconstruct_full_curve_shift_qty_supply_matched,
  price_indispo_merge_esp_npc[, .(delivery_date, Hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "Hour")
)

# Répertoire de sortie
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")

# Valeurs de k à tester
k_values <- c(5, 10, 15)

# Fonction générique pour calculer les pentes locales
compute_local_slopes <- function(dt, k, type = c("demand", "supply")) {
  type <- match.arg(type)
  
  # Étape 1 : garder les k observations les plus proches
  dt_k <- dt[, .SD[order(abs(PRICE - price))][1:k], by = .(DATE, Hour)]
  colnames(dt_k)
  # Étape 2 : régression locale
  

  local_slopes <- dt_k[
    , {
      if (.N > 1) {
        res <- tryCatch({
          fit <- lm(log(QTY+1) ~ log(PRICE+1))
          w <- log(PRICE+1)^2
          coef_fit <- coef(fit)
          .(
            slope = coef_fit["log(PRICE + 1)"],
            se_slope = summary(fit)$coefficients["log(PRICE + 1)", "Std. Error"],
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
  outfile <- sprintf("local_slopes_%s_%d.parquet", type, k)
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

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")

local_slopes_demand_reg <- compute_local_slopes_reg(reconstruct_full_curve_shift_qty_demand_matched_local, 10, type = "demand")
local_slopes_supply_reg <- compute_local_slopes_reg(reconstruct_full_curve_shift_qty_supply_matched_local, 10, type = "supply")


#jeter à la poubelle toutes les slopes tq se_slope nulles



