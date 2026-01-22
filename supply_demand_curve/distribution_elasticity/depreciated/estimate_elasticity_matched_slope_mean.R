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
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


# Filtrage par groupe

reconstruct_full_curve_shift_qty_demand_matched_local_bis <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]






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
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, load_tot)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]



library(fixest)



local_slopes <- local_slopes_demand
mean(local_slopes_supply$slope)
describe_local_slopes <- function(local_slopes, slope_col = "slope", 
                                  se_col = "se_slope", intercept_col = "intercept",
                                  mean_col = "mean_log_price") {
  x <- local_slopes[[slope_col]]
  se_x <- local_slopes[[se_col]]
  intercept_x <- local_slopes[[intercept_col]]
  p_x <- local_slopes[[mean_col]]


  eta <- cov(p_x, x * p_x, use = "complete.obs")/var(p_x)
  # Centrage
  p_x_centered <- p_x - mean(p_x, na.rm = TRUE)
  
  # Poids OLS
  w <- (p_x_centered  * p_x) / sum(p_x_centered ^2, na.rm = TRUE)
  
  
  #w <- if(!is.null(w_col)) local_slopes[[w_col]] else NULL
  
  
  # enlever NA
  valid <- !is.na(x) & !is.na(se_x)
  x <- x[valid]
  se_x <- se_x[valid]
  if (!is.null(w)) w <- w[valid]
  
  n <- length(x)
  if (n == 0) return(data.frame())
  
  # stats simples
  mean_x <- mean(x)
  median_x <- median(x)
  sd_x <- sd(x)
  se_mean <- sqrt(sum(se_x^2)) / n       # propagation de l'erreur
  min_x <- min(x)
  max_x <- max(x)
  quantiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
  
  # moyenne pond?r?e simple
  if (!is.null(w)) {
    w_mean <- weighted.mean(x, w)
    se_w_mean <- sqrt(sum((w^2) * (se_x^2))) / sum(w)   # propagation de l'erreur pond?r?e
  } else {
    w_mean <- NA
    se_w_mean <- NA
  }
  
  alpha = cor(intercept_x, p_x)
  
  
  data.frame(
    mean = mean_x,
    se_mean = se_mean,
    w_mean = w_mean,
    alpha = alpha,
    se_w_mean = se_w_mean,
    median = median_x,
    sd = sd_x,
    min = min_x,
    max = max_x,
    Q1 = quantiles[1],
    Q2 = quantiles[2],
    Q3 = quantiles[3],
    n = n
    
  )
}

# ?tape 1 : calculer slope et son SE localement
local_slopes_demand <- reconstruct_full_curve_shift_qty_demand_matched_local_bis[
  , {
    if (.N > 1) {
      
      res <- tryCatch({
        fit <- lm(log(QTY) ~ log(PRICE+1))
        w <- PRICE^2
        coef_fit <- coef(fit)
        .(
          slope = coef(fit)["log(PRICE + 1)"],
          se_slope = summary(fit)$coefficients["log(PRICE + 1)", "Std. Error"], # SE
          intercept = coef(fit)["(Intercept)"],
          mean_log_price = mean(log(PRICE), na.rm = TRUE),
          w_mean_log_price = weighted.mean(log(PRICE), w, na.rm = TRUE)  # moyenne pond?r?e
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
library(arrow)
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
write_parquet(local_slopes_demand, "local_slopes_demand_5.parquet")

local_slopes_demand <-  read_parquet( "local_slopes_demand.parquet")


# Exemple d'utilisation
my_demand <- describe_local_slopes(local_slopes_demand)


# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_demand$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_demand$slope, probs = 0.975, na.rm = TRUE)

# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_demand[, slope_w := pmin(pmax(slope, q_low), q_high)]

# V?rifier les r?sultats

# On calcule la moyenne pond?r?e des pentes
w_mean_demand <- my_demand$w_mean + my_demand$alpha




#je suis en train de faire ma r?gression OLS
#alors que je voudrais la pente locale


local_slopes_supply <- reconstruct_full_curve_shift_qty_supply_matched_local_bis[
  , {
    if (.N > 1) {
      
      res <- tryCatch({
        fit <- lm(log(QTY) ~ log(PRICE+1))
        w <- PRICE^2
        coef_fit <- coef(fit)
        .(
          slope = coef(fit)["log(PRICE + 1)"],
          se_slope = summary(fit)$coefficients["log(PRICE + 1)", "Std. Error"], # SE
          intercept = coef(fit)["(Intercept)"],
          mean_log_price = mean(log(PRICE), na.rm = TRUE),
          w_mean_log_price = weighted.mean(log(PRICE), w, na.rm = TRUE)  # moyenne pond?r?e
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
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
write_parquet(local_slopes_supply, "local_slopes_supply_5.parquet")

local_slopes_supply <- read_parquet("local_slopes_supply.parquet")

colnames(local_slopes_supply)
# Exemple d'utilisation



# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_supply$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_supply$slope, probs = 0.975, na.rm = TRUE)

# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_supply[, slope_w := pmin(pmax(slope, q_low), q_high)]

my_supply <- describe_local_slopes(local_slopes_supply)

w_mean_supply <- my_supply$w_mean + my_supply$alpha



# On calcule la moyenne pond?r?e des pentes
# w_mean_supply <- weighted.mean(
#   local_slopes_supply$slope,
#   local_slopes_supply$mean_log_price^2,  # poids = p^2
#   na.rm = TRUE
# )
my_demand$mean
my_supply$mean


sigma = 0.5 # choc de prix de 5%




local_slopes_demand_pond <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , {
    if (.N > 1) {
      w <- exp(-0.5 * ((log(PRICE + 1) - log(price + 1)) / sigma)^2)
      
      res <- tryCatch({
        fit <- lm(log(QTY) ~ log(PRICE + 1), weights = w)
        
        coef_fit <- coef(fit)
        summ_fit <- summary(fit)$coefficients
        
        .(
          slope = if ("log(PRICE + 1)" %in% names(coef_fit)) coef_fit["log(PRICE + 1)"] else NA_real_,
          se_slope = if ("log(PRICE + 1)" %in% rownames(summ_fit)) summ_fit["log(PRICE + 1)", "Std. Error"] else NA_real_,
          intercept = coef_fit["(Intercept)"],
          mean_log_price = log(price + 1)
          #w_mean_log_price = weighted.mean(log(PRICE + 1), w, na.rm = TRUE)
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


local_slopes_demand_pond <- local_slopes_demand_pond %>% 
  distinct()

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
write_parquet(local_slopes_demand_pond, "local_slopes_demand_pond.parquet")

rm(reconstruct_full_curve_shift_qty_demand_matched_local)
gc()
local_slopes_supply_pond <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , {
    if (.N > 1) {
      w <- exp(-0.5 * ((log(PRICE + 1) - log(price + 1)) / sigma)^2)
      
      res <- tryCatch({
        fit <- lm(log(QTY) ~ log(PRICE + 1), weights = w)
        
        coef_fit <- coef(fit)
        summ_fit <- summary(fit)$coefficients
        
        .(
          slope = if ("log(PRICE + 1)" %in% names(coef_fit)) coef_fit["log(PRICE + 1)"] else NA_real_,
          se_slope = if ("log(PRICE + 1)" %in% rownames(summ_fit)) summ_fit["log(PRICE + 1)", "Std. Error"] else NA_real_,
          intercept = coef_fit["(Intercept)"],
          mean_log_price = log(price + 1)#,
          #w_mean_log_price = weighted.mean(log(PRICE + 1), w, na.rm = TRUE)
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


local_slopes_supply_pond <- local_slopes_supply_pond %>% 
  distinct()

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
write_parquet(local_slopes_supply_pond, "local_slopes_supply_pond.parquet")




# test_data <- reconstruct_full_curve_shift_qty_demand_matched_local_bis[DATE == "2019-12-21" & Hour ==23]

# ggsave(my_elas,
#        filename = "elasticity_check_intra_day.png" , width = 12, height = 6, dpi = 300)
