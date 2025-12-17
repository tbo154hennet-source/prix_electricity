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
  price_indispo_merge_esp[, .(delivery_date, hour,  weekday, price, gen_tot, gen_tot_da)],
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
  price_indispo_merge_esp[, .(delivery_date, hour,  weekday, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]

colnames(reconstruct_full_curve_shift_qty_demand_matched_local_bis)



library(fixest)


reconstruct_full_curve_shift_qty_supply_matched_local_bis[, Hour_Date := as.POSIXct(DATE) + as.difftime(Hour, units = "hours")]
reconstruct_full_curve_shift_qty_demand_matched_local_bis[, Hour_Date := as.POSIXct(DATE) + as.difftime(Hour, units = "hours")]



local_supply_FE <- feols(QTY ~ PRICE | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)


# Global model with fixed effects
local_demand_FE <- feols(QTY ~ PRICE | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_demand_matched_local_bis)

etable(local_supply_FE, local_demand_FE ,
       local_supply_fine_FE, local_demand_fine_FE)


# Residualized PRICE after FE (within transformation)
price_resid_supply <- residuals(feols(QTY ~ 1 | Hour_Date, data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)) 
price_resid_demand <- residuals(feols(QTY ~ 1 | Hour_Date, data = reconstruct_full_curve_shift_qty_demand_matched_local_bis))

# Supply
eta_2SLS_supply <- cov(reconstruct_full_curve_shift_qty_supply_matched_local_bis$PRICE, price_resid_supply, use = "complete.obs") / var(reconstruct_full_curve_shift_qty_supply_matched_local_bis$PRICE, na.rm = TRUE)

# Demand
eta_2SLS_demand <- cov(reconstruct_full_curve_shift_qty_demand_matched_local_bis$PRICE, price_resid_demand, use = "complete.obs") / var(reconstruct_full_curve_shift_qty_demand_matched_local_bis$PRICE, na.rm = TRUE)




summary_supply <- summary(local_supply_FE)
summary_demand <- summary(local_demand_FE)

coef_supply <- summary_supply$coeftable["PRICE", "Estimate"]
se_supply   <- summary_supply$coeftable["PRICE", "Std. Error"]

coef_demand <- summary_demand$coeftable["PRICE", "Estimate"]
se_demand   <- summary_demand$coeftable["PRICE", "Std. Error"]

results_all <- data.frame(
  Model = c("Supply", "Demand"),
  Coefficient = c(coef_supply, coef_demand),
  StdError = c(se_supply, se_demand)
)


print(results_all)
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
write.csv(results_all, "linear_response.csv")

