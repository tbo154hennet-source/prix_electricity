# France, VAR extended to 2010-01 (Data_structuring/france_var_backfill).
#
# The macro block is observed from 2010; the wind supply proxies start at
# different dates (ERA5 2010, RTE load factor 2012, ENTSO-E forecast 2015) and
# are NA before, so that proxy_instrument() drops those months from the
# moments. Outputs are written locally to output/france_2010 only.

france_extended_labels <- c(
  wind_era5_surprise = "ERA5 wind surprise (2010-)",
  wind_rte_surprise = "RTE wind surprise (2012-)",
  Fore_Wind_log = "Wind forecast, log (2015-)",
  wind_era5_cf_log = "ERA5 load factor, log (2010-)",
  wind_rte_cf_log = "RTE load factor, log (2012-)",
  wind_era5_surprise_tadj = "ERA5 wind surprise, net of degree days (2010-)",
  wind_rte_surprise_tadj = "RTE wind surprise, net of degree days (2012-)",
  wind_era5_cf_log_tadj = "ERA5 load factor, log, net of degree days (2010-)",
  wind_rte_cf_log_tadj = "RTE load factor, log, net of degree days (2012-)"
)

load_france_extended <- function(cfg) {
  france <- read_country_data(cfg$data$france_2010, "France")
  proxies <- cfg$france_2010$supply_proxies
  assert_columns(france$data, proxies, "French 2010 wind proxies")
  france$wind_proxy <- proxies[[1]]
  france$supply_proxy <- proxies[[1]]
  france$supply_proxies <- proxies
  france
}

france_extended_cfg <- function(cfg) {
  local_cfg <- cfg
  local_cfg$figure_dir <- file.path(cfg$output_dir, "france_2010", "figures")
  local_cfg$table_dir <- file.path(cfg$output_dir, "france_2010", "tables")
  local_cfg$overleaf$enabled <- FALSE
  dir.create(local_cfg$figure_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(local_cfg$table_dir, showWarnings = FALSE, recursive = TRUE)
  local_cfg
}

proxy_coverage <- function(result) {
  instrument <- proxy_instrument(result$sample, result$proxy, result$fit$p)
  observed_dates <- result$dates[instrument$observed]
  data.frame(
    Proxy = result$proxy,
    VAR_start = format(min(result$sample$observation_date), "%Y-%m"),
    VAR_end = format(max(result$sample$observation_date), "%Y-%m"),
    Residual_months = length(result$dates),
    Proxy_start = format(min(observed_dates), "%Y-%m"),
    Proxy_months = length(observed_dates)
  )
}

run_france_extended_stage <- function(benchmark_wind, benchmark_demand, cfg,
                                      demand_elasticity, get_model, cache_name) {
  local_cfg <- france_extended_cfg(cfg)
  france <- load_france_extended(cfg)
  main_proxy <- france$supply_proxy

  supply <- lapply(france$supply_proxies, function(proxy) {
    get_model(cache_name(paste0("france2010_iv_supply_", proxy), TRUE), function() {
      fit_proxy_svar(france, proxy, "spot_price_log", "Supply", cfg)
    })
  })
  names(supply) <- france$supply_proxies
  demand <- get_model(cache_name("france2010_iv_demand_temperature", TRUE), function() {
    fit_proxy_svar(france, france$demand_proxy, "load_tot_log", "Demand", cfg)
  })
  mr <- get_model(cache_name(paste0("france2010_mertens_ravn_", main_proxy)), function() {
    fit_mertens_ravn(france, france$demand_proxy, main_proxy, demand_elasticity, cfg)
  })
  main <- supply[[main_proxy]]
  legend_bottom <- ggplot2::theme(legend.position = "bottom")

  save_plot(
    plot_iv_irf(main, "France 2010-2025: electricity supply shock",
                paste0(france_extended_labels[[main_proxy]], "; MSW confidence intervals")),
    local_cfg, "wind_supply_shock_fr_2010.png", width = 10, height = 6
  )
  sample_comparison <- list(
    "2015-25: forecast" = benchmark_wind,
    "2010-25: ERA5 net of DD" = supply[["wind_era5_cf_log_tadj"]],
    "2010-25: ERA5" = supply[["wind_era5_cf_log"]],
    "2010-25: forecast (2015-)" = supply[["Fore_Wind_log"]]
  )
  save_plot(
    plot_irf_comparison(
      sample_comparison[!vapply(sample_comparison, is.null, logical(1))],
      "Supply", "France: supply shock, benchmark and extended samples"
    ),
    local_cfg, "supply_shock_france_sample_compare.png", width = 14, height = 8,
    theme_overrides = legend_bottom
  )
  save_plot(
    plot_irf_comparison(
      list("2015-25" = benchmark_demand, "2010-25" = demand),
      "Demand", "France: demand shock, benchmark and extended samples"
    ),
    local_cfg, "demand_shock_france_sample_compare.png", width = 14, height = 8,
    theme_overrides = legend_bottom
  )
  save_plot(
    plot_irf_comparison(
      list("Wind IV" = main, "Mertens-Ravn" = mr), "Supply",
      "France 2010-2025: supply-shock identification robustness",
      paste0("Mertens-Ravn demand elasticity = ", round(demand_elasticity, 3))
    ),
    local_cfg, "supply_shock_france_2010_irf_compare.png", width = 14, height = 8,
    theme_overrides = legend_bottom
  )
  save_plot(
    plot_irf_comparison(
      list("Temperature IV" = demand, "Mertens-Ravn" = mr), "Demand",
      "France 2010-2025: demand-shock identification robustness",
      paste0("Mertens-Ravn demand elasticity = ", round(demand_elasticity, 3))
    ),
    local_cfg, "demand_shock_france_2010_irf_compare.png", width = 14, height = 8,
    theme_overrides = legend_bottom
  )

  irfs <- dplyr::bind_rows(
    lapply(names(supply), function(proxy) {
      transform(supply[[proxy]]$irf, Identification = france_extended_labels[[proxy]])
    }),
    list(
      transform(demand$irf, Identification = "Temperature IV"),
      transform(mr$irf, Identification = "Mertens-Ravn")
    )
  )
  save_csv(irfs, local_cfg, "irfs_france_2010.csv")
  save_csv(
    dplyr::bind_rows(lapply(c(list(benchmark_wind, benchmark_demand), supply, list(demand)), proxy_coverage)),
    local_cfg, "france_2010_samples.csv"
  )

  relevance_models <- c(
    list("2015-2025 supply: wind forecast" = benchmark_wind),
    stats::setNames(supply, paste0("2010-2025 supply: ", france_extended_labels[names(supply)])),
    list(
      "2015-2025 demand: temperature" = benchmark_demand,
      "2010-2025 demand: temperature" = demand
    )
  )
  run_relevance(
    relevance_models, local_cfg,
    csv_name = "proxy_relevance_france_2010.csv",
    tex_name = "relevance_france_2010.tex"
  )

  iv_table <- contribution_table_from_hd(
    combine_single_proxy_hd(main, demand), cfg$windows, "France",
    paste0("External instruments 2010-2025: ", france_extended_labels[[main_proxy]], " and temperature"),
    france$data
  )
  export_contributions(
    iv_table, local_cfg,
    "contributions_france_2010_iv.csv", "cumulative_contributions_iv_france_2010.tex"
  )
  mr_table <- contribution_table_from_hd(
    list(hd = mr$hd, dates = mr$dates, variables = mr$variables), cfg$windows, "France",
    paste0("Mertens-Ravn 2010-2025; demand elasticity ", round(demand_elasticity, 6)),
    france$data
  )
  export_contributions(
    mr_table, local_cfg,
    "contributions_france_2010_mertens_ravn.csv", "cumulative_contributions_mr_france_2010.tex"
  )
  invisible(list(supply = supply, demand = demand, mr = mr))
}
