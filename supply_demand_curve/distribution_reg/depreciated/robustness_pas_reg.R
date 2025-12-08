setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_qty_demand_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_demand_matched)

#test <- reconstruct_full_curve_shift_qty_demand_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_demand_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_demand_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_demand_matched<- reconstruct_full_curve_shift_qty_demand_matched[complete.cases(reconstruct_full_curve_shift_qty_demand_matched)]


#fusion
reconstruct_full_curve_shift_qty_demand_matched_local <- merge(
  reconstruct_full_curve_shift_qty_demand_matched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
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


#fusion
reconstruct_full_curve_shift_qty_demand_matched_local <- merge(
  reconstruct_full_curve_shift_qty_demand_matched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)



# Filtrage par groupe

reconstruct_full_curve_shift_qty_demand_matched_local_bis <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]

# Filtrage par groupe

reconstruct_full_curve_shift_qty_demand_matched_local_bis <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , .SD[order(abs(PRICE - price))][1:20], 
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
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , .SD[order(abs(PRICE - price))][1:20], 
  by = .(DATE, Hour)
]


# ?tape 1 : calculer slope et son SE localement
local_slopes_demand_20 <- reconstruct_full_curve_shift_qty_demand_matched_local_bis[
  , {
    if (.N > 1) {
      fit <- lm(log(QTY) ~ log(PRICE))
      w <- PRICE^2
      .(
        slope = coef(fit)["log(PRICE)"],
        se_slope = summary(fit)$coefficients["log(PRICE)", "Std. Error"], # SE
        intercept = coef(fit)["(Intercept)"],
        mean_log_price = mean(log(PRICE), na.rm = TRUE),
        w_mean_log_price = weighted.mean(log(PRICE), w, na.rm = TRUE)  # moyenne pond?r?e
      )
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
write_parquet(local_slopes_demand_20, "local_slopes_demand_20.parquet")


# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_demand_20$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_demand_20$slope, probs = 0.975, na.rm = TRUE)

# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_demand_20[, slope_w := pmin(pmax(slope, q_low), q_high)]

# Exemple d'utilisation
my_demand_20 <- describe_local_slopes(local_slopes_demand_20)


# On calcule la moyenne pond?r?e des pentes
w_mean_demand_20 <- my_demand_20$w_mean + my_demand_20$alpha





local_slopes_supply_20 <- reconstruct_full_curve_shift_qty_supply_matched_local_bis[
  , {
    if (.N > 1) {
      fit <- lm(log(QTY) ~ log(PRICE+1))
      w <- PRICE^2
      .(
        slope = coef(fit)["log(PRICE + 1)"],
        se_slope = summary(fit)$coefficients["log(PRICE + 1)", "Std. Error"], # SE
        intercept = coef(fit)["(Intercept)"],
        mean_log_price = mean(log(PRICE+1), na.rm = TRUE),
        w_mean_log_price = weighted.mean(log(PRICE+1), w, na.rm = TRUE)  # moyenne pond?r?e
      )
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

write_parquet(local_slopes_supply_20, "local_slopes_supply_20.parquet")


# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_supply_20$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_supply_20$slope, probs = 0.975, na.rm = TRUE)

# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_supply_20[, slope_w := pmin(pmax(slope, q_low), q_high)]

my_supply_20 <- describe_local_slopes(local_slopes_supply_20)

w_mean_supply_20 <- my_supply_20$w_mean + my_supply_20$alpha



#plot the distribution


my_elas_compare <- ggplot() +
  #accolade

  geom_histogram(
    data = local_slopes_demand,
    aes(x = slope_w, y = ..count../sum(..count..)*10),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7, fill = "red"
  )+ 
  geom_segment(
    aes(x = w_mean_demand, xend = w_mean_demand, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "red",
    size = 1
  )+
  geom_histogram(
    data = local_slopes_demand_20,
    aes(x = slope_w, y = ..count../sum(..count..)*10),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7, fill = "darkred"
  )+ 
  geom_segment(
    aes(x = w_mean_demand_20, xend = w_mean_demand_20, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "darkred",
    size = 1
  )+
  geom_histogram(
    data = local_slopes_supply,
    aes(x = slope_w, y = ..count../sum(..count..)*10),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7, fill = "blue"
  )+ 
  geom_segment(
    aes(x = w_mean_supply, xend = w_mean_supply, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "blue",
    size = 1
  )+
  geom_histogram(
    data = local_slopes_supply_20,
    aes(x = slope_w, y = ..count../sum(..count..)*10),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7, fill = "darkblue"
  )+ 
  geom_segment(
    aes(x = w_mean_supply_20, xend = w_mean_supply_20, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "darkblue",
    size = 1
  )

my_elas_compare





#check whether or not diff is a good approximation
# delta alpha et delta eta *P << eta* delta P

colnames(local_slopes_demand)

local_slopes_demand_check <- local_slopes_demand %>% 
  mutate(intercept_diff = intercept - lag(intercept),
         slope_diff = slope - lag(slope),
         price_diff = mean_log_price - lag(mean_log_price)
         )
sd(local_slopes_demand_check$intercept_diff, na.rm =TRUE)
sd(local_slopes_demand_check$mean_log_price* local_slopes_demand_check$slope_diff, na.rm =TRUE)
sd(local_slopes_demand_check$price_diff* local_slopes_demand_check$slope, na.rm =TRUE)



