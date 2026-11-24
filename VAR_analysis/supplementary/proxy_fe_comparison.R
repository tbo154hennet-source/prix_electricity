# Spain: proxy SVAR with the monthly output of Eq. supply_demand_bis used directly versus residualised as in
# Eq. proxy_levels (calendar-month and year fixed effects), plus intermediate treatments.
# Usage (repo root): Rscript VAR_analysis/supplementary/proxy_fe_comparison.R
root_dir <- normalizePath(".", winslash = "/")
pipeline_dir <- file.path(root_dir, "VAR_analysis")
source(file.path(pipeline_dir, "config.R"))
invisible(lapply(sort(list.files(file.path(pipeline_dir, "R"), "[.]R$", full.names = TRUE)), source))
cfg <- make_config(root_dir)
check_dependencies(TRUE)
cfg$overleaf$enabled <- FALSE
out_dir <- file.path(cfg$output_dir, "proxy_fe_comparison")
cfg$figure_dir <- file.path(out_dir, "figures"); cfg$table_dir <- file.path(out_dir, "tables")
for (d in c(cfg$figure_dir, cfg$table_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

spain <- read_country_data(cfg$data$spain, "Spain")
spain <- attach_synthetic_level_proxies(spain, benchmark_hourly_path(cfg))

# Raw monthly load-weighted level of the Eq. 2 fitted shifts (same aggregation as attach_synthetic_level_proxies).
hourly <- as.data.frame(arrow::read_parquet(cfg$data$spain_high_frequency_proxies,
  col_select = c("datetime", "load_tot", "fit_supply_unmatched", "fit_demand_unmatched")))
hourly <- hourly[!duplicated(hourly$datetime), , drop = FALSE]
month <- format(hourly$datetime, "%Y-%m-01", tz = "Europe/Madrid")
weight <- ifelse(is.finite(hourly$load_tot), hourly$load_tot, 0)
monthly <- function(v) { ok <- is.finite(v); tapply(weight[ok] * v[ok], month[ok], sum) / tapply(weight[ok], month[ok], sum) }
key <- format(as.Date(spain$data$observation_date), "%Y-%m-01")
raw <- list(S = as.numeric(monthly(hourly$fit_supply_unmatched)[key]),
            D = as.numeric(monthly(hourly$fit_demand_unmatched)[key]))
rows <- which(is.finite(raw$S) & is.finite(raw$D))
dates <- as.Date(spain$data$observation_date[rows])
calendar <- factor(format(dates, "%m")); year <- factor(format(dates, "%Y")); trend <- seq_along(rows)

treatments <- list(
  raw = NULL,
  month = ~ calendar,
  year = ~ year,
  month_year = ~ calendar + year,
  month_trend = ~ calendar + trend
)
labels <- c(raw = "Eq. 2 output, raw", month = "Month FE", year = "Year FE",
            month_year = "Month + year FE (proxy_levels)", month_trend = "Month FE + trend")
variance_rows <- list()
for (k in c("S", "D")) for (t in names(treatments)) {
  x <- raw[[k]][rows]
  z <- if (is.null(treatments[[t]])) x - mean(x) else stats::resid(stats::lm(stats::update(treatments[[t]], x ~ .)))
  column <- paste0("Z_", k, "_", t)
  spain$data[[column]] <- NA_real_
  spain$data[[column]][rows] <- z
  variance_rows[[column]] <- data.frame(Shift = k, Treatment = labels[[t]], Share_of_raw_variance = stats::var(z) / stats::var(x))
}

fits <- list()
for (k in c("S", "D")) for (t in names(treatments)) {
  shock <- if (k == "S") "Supply" else "Demand"
  fits[[paste(shock, labels[[t]], sep = ": ")]] <- cache_model(cfg, paste0("proxy_fe_", k, "_", t, "_p", cfg$p), function()
    fit_proxy_svar(spain, paste0("Z_", k, "_", t), "spot_price_log", shock, cfg, use_inference = TRUE))
}

# Where does each component of the raw proxy load on the VAR residuals? Correlation of the price residual with
# the raw proxy and with its fitted month / year components.
decomposition <- dplyr::bind_rows(lapply(c("S", "D"), function(k) {
  fit <- fits[[paste(if (k == "S") "Supply" else "Demand", labels[["raw"]], sep = ": ")]]
  u <- fit$residuals[, "spot_price_log"]
  d <- fit$dates
  x <- raw[[k]][match(format(d, "%Y-%m-01"), key)]
  keep <- is.finite(x); x <- x[keep]; u <- u[keep]; d <- d[keep]
  dm <- format(d, "%m"); dy <- format(d, "%Y")
  month_part <- stats::ave(x, dm) - mean(x)
  year_part <- stats::ave(x, dy) - mean(x)
  within <- x - stats::ave(x, dm) - stats::ave(x, dy) + mean(x)
  data.frame(Shift = k,
             Component = c("Raw", "Calendar-month means", "Year means", "Within-year deviation"),
             Variance_share = c(1, stats::var(month_part), stats::var(year_part), stats::var(within)) / stats::var(x),
             Corr_with_price_residual = c(stats::cor(x, u), stats::cor(month_part, u), stats::cor(year_part, u), stats::cor(within, u)))
}))

save_csv(dplyr::bind_rows(variance_rows), cfg, "proxy_variance_by_treatment.csv")
save_csv(decomposition, cfg, "proxy_components_vs_price_residual.csv")
relevance <- run_relevance(fits, cfg, csv_name = "proxy_relevance_fe_comparison.csv", tex_name = "proxy_relevance_fe_comparison.tex")
for (shock in c("Supply", "Demand")) {
  chosen <- fits[paste(shock, labels[c("raw", "month", "month_year", "month_trend")], sep = ": ")]
  names(chosen) <- labels[c("raw", "month", "month_year", "month_trend")]
  save_plot(plot_irf_comparison(chosen, shock, ""), cfg, sprintf("%s_irf_proxy_fe_comparison.png", tolower(shock)),
            width = 14, height = 8, theme_overrides = ggplot2::theme(legend.position = "bottom"))
}
print(relevance[, c("Specification", "First_stage_coefficient", "F_statistic", "HAC_F_statistic", "Proxy_residual_correlation")], digits = 3)
print(decomposition, digits = 3)
