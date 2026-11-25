# Slopes of the Spanish electricity supply and demand curves by horizon (Table tab:slope_horizon of the working paper).
# Daily local projections: the average log load over days t..t+H-1 on the average log price over the same days, both
# relative to their average over the previous week (t-7..t-1). The price is instrumented by the daily surprise in
# temperature discomfort (demand shifter -> supply slope) or in the day-ahead wind forecast (supply shifter -> demand
# slope). Daily surprise = residual of the daily mean on a trend, day-of-week dummies, three annual Fourier pairs and
# seven own lags (as in VAR_analysis/R/09_elasticity_svar.R). Controls: trend, day of week, Fourier terms, seven lags
# of daily log price and log load, current and lagged log gas price. Newey-West standard errors (lag H + 7).
# Usage: Rscript VAR_analysis/supplementary/daily_slope_horizon.R [hourly parquet]
suppressPackageStartupMessages({library(arrow); library(fixest)})
root_dir <- Sys.getenv("REPLICATION_ROOT", ".")
args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args)) args[[1]] else
  file.path(root_dir, "supply_demand_curve/outputs/coef_reg_no_conditioning/price_indispo_merge_esp_npc_all_final_2026.parquet")
out_dir <- file.path(root_dir, "VAR_analysis/output/elasticity/tables")

hourly <- as.data.frame(read_parquet(path, col_select = c("datetime", "price", "load_tot", "spot_gas", "Fore_Wind",
                                                          "temperature_discomfort_demand")))
hourly <- hourly[!duplicated(hourly$datetime), ]
hourly$day <- as.Date(format(hourly$datetime, "%Y-%m-%d", tz = "Europe/Madrid"))
daily <- aggregate(cbind(price, load_tot, spot_gas, Fore_Wind, temperature_discomfort_demand) ~ day, hourly,
                   mean, na.action = na.pass, na.rm = TRUE)
daily <- daily[order(daily$day), ]
all_days <- data.frame(day = seq(min(daily$day), max(daily$day), by = "day"))
daily <- merge(all_days, daily, all.x = TRUE)
n <- nrow(daily)
daily$lp <- log(pmax(daily$price, 1)); daily$ll <- log(daily$load_tot); daily$lg <- log(daily$spot_gas)
doy <- as.numeric(format(daily$day, "%j"))
fourier <- cbind(sapply(1:3, function(k) sin(2 * pi * k * doy / 365.25)), sapply(1:3, function(k) cos(2 * pi * k * doy / 365.25)))
colnames(fourier) <- paste0("f", 1:6)
daily <- cbind(daily, fourier)
daily$trend <- seq_len(n) / n
daily$dow <- factor(format(daily$day, "%u"))
lag_value <- function(z, k) c(rep(NA_real_, k), head(z, -k))
lead_mean <- function(z, h) sapply(seq_len(n), function(t) if (t + h - 1 <= n) mean(z[t:(t + h - 1)]) else NA_real_)
prev_week <- function(z) sapply(seq_len(n), function(t) if (t > 7) mean(z[(t - 7):(t - 1)]) else NA_real_)
deterministic <- cbind(1, daily$trend, stats::model.matrix(~ dow, daily)[, -1], fourier)
surprise <- function(z) {
  regressors <- cbind(deterministic, sapply(1:7, function(k) lag_value(z, k)))
  ok <- rowSums(is.finite(cbind(z, regressors))) == ncol(regressors) + 1L
  out <- rep(NA_real_, n); out[ok] <- stats::lm.fit(regressors[ok, , drop = FALSE], z[ok])$residuals; out
}
daily$z_temp <- surprise(daily$temperature_discomfort_demand)
daily$z_wind <- surprise(daily$Fore_Wind / 1000)
for (k in 1:7) { daily[[paste0("lp_l", k)]] <- lag_value(daily$lp, k); daily[[paste0("ll_l", k)]] <- lag_value(daily$ll, k) }
daily$lg_l1 <- lag_value(daily$lg, 1)
daily$id <- 1L; daily$t <- seq_len(n)
controls <- paste(c("trend", "dow", paste0("f", 1:6), paste0("lp_l", 1:7), paste0("ll_l", 1:7), "lg", "lg_l1"), collapse = " + ")

horizons <- c("1 day" = 1, "1 week" = 7, "2 weeks" = 14, "30 days" = 30, "60 days" = 60)
rows <- list()
for (label in names(horizons)) {
  h <- horizons[[label]]
  daily$y <- lead_mean(daily$ll, h) - prev_week(daily$ll)
  daily$x <- lead_mean(daily$lp, h) - prev_week(daily$lp)
  for (side in c("Supply", "Demand")) {
    instrument <- if (side == "Supply") "z_temp" else "z_wind"
    model <- feols(stats::as.formula(paste0("y ~ ", controls, " | x ~ ", instrument)), data = daily,
                   panel.id = ~ id + t, vcov = NW(h + 7), notes = FALSE)
    ivf <- fitstat(model, "ivwald1")[[1]]$stat
    rows[[length(rows) + 1L]] <- data.frame(Horizon = label, Slope = side, Estimate = unname(coef(model)["fit_x"]),
                                            SE = unname(se(model)["fit_x"]), First_stage_F_HAC = ivf, N = nobs(model))
  }
}
result <- do.call(rbind, rows)
print(format(result, digits = 3), row.names = FALSE)
if (!length(args)) utils::write.csv(result, file.path(out_dir, "daily_slope_horizon.csv"), row.names = FALSE)
