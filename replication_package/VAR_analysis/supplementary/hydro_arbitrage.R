# Hydro reservoirs: monthly vs within-day arbitrage (working paper, Section on the VAR variables, hydro paragraph).
# 1. Monthly change in the hydro reservoir stock (REE) and monthly hydro generation, July 2015 - September 2025:
#    R2 of calendar-month fixed effects, plus hydrology (precipitation and 5 lags, temperature, cold and heat excess,
#    previous stock), plus market variables (log electricity and gas prices, log load, log wind forecast).
# 2. Cross-month arbitrage test: monthly log hydro generation (and stock change) on the log electricity price,
#    instrumented by the log TTF gas price, controlling for hydrology, load and wind (Newey-West, 6 lags).
# 3. Within-day arbitrage: hourly log hydro generation on the hourly log price with day fixed effects, against daily
#    averages with month fixed effects (Driscoll-Kraay), and variance shares of hourly hydro generation.
# Usage: Rscript VAR_analysis/supplementary/hydro_arbitrage.R   (writes VAR_analysis/output/supplementary/)
suppressPackageStartupMessages({library(arrow); library(dplyr); library(fixest)})
setFixest_notes(FALSE)
root_dir <- Sys.getenv("REPLICATION_ROOT", ".")
source(file.path(root_dir, "VAR_analysis", "config.R"))
cfg <- make_config(root_dir)
out_dir <- file.path(root_dir, "VAR_analysis", "output", "supplementary")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

m <- as.data.frame(read_parquet(cfg$data$spain))
m$date <- as.Date(m$observation_date)
hy <- utils::read.csv(cfg$data$hydro_reserves)
hy$date <- as.Date(paste0(hy$month, "-01"))
h <- read_parquet(cfg$data$spain_high_frequency_proxies, col_select = c("datetime", "hydro_tot", "price")) %>%
  distinct(datetime, .keep_all = TRUE) %>% filter(is.finite(hydro_tot), is.finite(price)) %>%
  mutate(day = as.Date(datetime), date = as.Date(format(datetime, "%Y-%m-01")))
hm <- h %>% group_by(date) %>% summarise(hydro_gen = mean(hydro_tot), .groups = "drop") %>%
  mutate(hydro_gen = hydro_gen * 24 * as.numeric(lubridate::days_in_month(date)))  # MWh per month

lag_k <- function(x, k) dplyr::lag(x, k)
d <- m %>% select(date, precipitation, temperature, cold_excess, heat_excess, spot_price_log, spot_gas_log,
                  load_tot_log, wind_forecast_log) %>%
  inner_join(hy %>% select(date, stock = total_mwh), by = "date") %>% left_join(hm, by = "date") %>% arrange(date) %>%
  mutate(d_stock = stock - lag(stock), stock_l1 = lag(stock), month = factor(format(date, "%m")),
         precipitation_l1 = lag_k(precipitation, 1), precipitation_l2 = lag_k(precipitation, 2),
         precipitation_l3 = lag_k(precipitation, 3), precipitation_l4 = lag_k(precipitation, 4),
         precipitation_l5 = lag_k(precipitation, 5), hydro_gen_log = log(hydro_gen)) %>%
  filter(is.finite(precipitation_l5), is.finite(d_stock), is.finite(hydro_gen),
         date >= as.Date("2015-07-01"), date <= as.Date("2025-09-01"))

weather <- paste("precipitation + precipitation_l1 + precipitation_l2 + precipitation_l3 + precipitation_l4 +",
                 "precipitation_l5 + temperature + cold_excess + heat_excess + stock_l1")
market <- "spot_price_log + spot_gas_log + load_tot_log + wind_forecast_log"
fit_r2 <- function(rhs, y) fixest::r2(feols(as.formula(paste(y, "~", rhs)), d), "r2")
r2_table <- do.call(rbind, lapply(c("d_stock", "hydro_gen"), function(y) data.frame(
  Dependent = y,
  Month_FE = fit_r2("month", y),
  Month_FE_weather = fit_r2(paste("month +", weather), y),
  Month_FE_market = fit_r2(paste("month +", market), y),
  Month_FE_weather_market = fit_r2(paste("month +", weather, "+", market), y))))

nw <- NW(6) ~ date
iv_gen <- feols(as.formula(paste("hydro_gen_log ~ month +", weather, "+ load_tot_log + wind_forecast_log | spot_price_log ~ spot_gas_log")),
                d, vcov = nw, panel.id = NULL)
iv_stock <- feols(as.formula(paste("I(d_stock / 1000) ~ month +", weather, "+ load_tot_log + wind_forecast_log | spot_price_log ~ spot_gas_log")),
                  d, vcov = nw, panel.id = NULL)

h <- h %>% filter(price > 0) %>% mutate(lp = log(price), lh = log(pmax(hydro_tot, 1)))
within_day <- feols(lh ~ lp | day, h, vcov = DK(24) ~ datetime)
daily <- h %>% group_by(day, date) %>% summarise(lh = log(mean(hydro_tot)), lp = log(mean(price)), .groups = "drop")
between_days <- feols(lh ~ lp | date, daily, vcov = DK(7) ~ day)
v <- h %>% group_by(day) %>% mutate(dm = mean(hydro_tot)) %>% group_by(date) %>% mutate(mm = mean(hydro_tot)) %>% ungroup()
total <- var(v$hydro_tot)

coef_row <- function(model, name) data.frame(
  Regression = name, Coef = unname(coef(model)[grep("lp$|spot_price_log", names(coef(model)))]),
  Se = unname(se(model)[grep("lp$|spot_price_log", names(coef(model)))]), N = nobs(model),
  First_stage_F = if (!is.null(model$iv_first_stage)) fitstat(model, "ivf")[[1]]$stat else NA_real_)
results <- rbind(
  coef_row(iv_gen, "Monthly log hydro generation on log price, IV gas"),
  coef_row(iv_stock, "Monthly stock change (GWh) on log price, IV gas"),
  coef_row(within_day, "Hourly log hydro generation on log price, day FE"),
  coef_row(between_days, "Daily log hydro generation on log price, month FE"))
shares <- data.frame(Component = c("within day", "between days within month", "between months"),
                     Share = c(var(v$hydro_tot - v$dm), var(v$dm - v$mm), var(v$mm)) / total)

utils::write.csv(r2_table, file.path(out_dir, "hydro_arbitrage_r2.csv"), row.names = FALSE)
utils::write.csv(results, file.path(out_dir, "hydro_arbitrage_regressions.csv"), row.names = FALSE)
utils::write.csv(shares, file.path(out_dir, "hydro_generation_variance_shares.csv"), row.names = FALSE)
cat("Sample:", format(min(d$date)), "-", format(max(d$date)), "(", nrow(d), "months )\n")
print(r2_table, digits = 3); print(results, digits = 3); print(shares, digits = 3)
