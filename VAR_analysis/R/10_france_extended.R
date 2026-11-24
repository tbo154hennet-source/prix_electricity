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

# ----------------------------------------------------------------------------------------------------
# Bayesian SVAR with the demand slope fixed at the French hourly estimate (fast-track identification of the
# working paper: Step 1, wind as a pure supply shifter in the hourly data; Step 2, the elasticity SVAR of
# 09_elasticity_svar.R with eps_d fixed, GLP reduced form, innovations netted of the gas price). No proxy is
# needed, so the 2010-2014 months identify the shocks as well.
# ----------------------------------------------------------------------------------------------------
read_france_demand_elasticity <- function(cfg) {
  path <- cfg$data$micro_elasticity_fr
  if (!file.exists(path)) {
    stop("French hourly elasticity not found: ", path,
         "\nRun supply_demand_curve/estimate_elasticity/fr/estimate_elas_standard_IV_fr.R")
  }
  estimates <- utils::read.csv(path)
  assert_columns(estimates, c("Coef", "Se", "FE"), "French hourly elasticity")
  # first row: same fixed effects as the Spanish standard IV (year, month, hour)
  c(coef = estimates$Coef[[1]], se = estimates$Se[[1]])
}

france_elasticity_specs <- function(cfg, eps_fr, eps_es) {
  # central reduced form: OLS (diffuse prior, posterior centred on OLS); GLP shrinkage as robustness
  base <- list(p = cfg$elasticity$p_central, var_prior = "diffuse", proxies = list(), gas_netted = TRUE)
  spec <- function(name, ...) utils::modifyList(c(base, list(name = name)), list(...))
  list(
    central = spec(sprintf("France 2010-25, hourly French demand elasticity (%.3f)", eps_fr), fixed_demand = eps_fr),
    glp = spec("France 2010-25, GLP prior", fixed_demand = eps_fr, var_prior = "glp"),
    spanish = spec(sprintf("France 2010-25, hourly Spanish demand elasticity (%.3f)", eps_es), fixed_demand = eps_es),
    no_gas = spec("France 2010-25, innovations not netted of gas", fixed_demand = eps_fr, gas_netted = FALSE),
    sample_2015 = spec("France 2015-25, hourly French demand elasticity", fixed_demand = eps_fr)
  )
}

run_france_elasticity <- function(cfg, get_model, demand_elasticity_es) {
  ocfg <- france_extended_cfg(cfg)                     # local tables and robustness figures
  pcfg <- cfg; pcfg$figure_dir <- ocfg$figure_dir; pcfg$table_dir <- ocfg$table_dir   # main figures: Overleaf too
  micro <- read_france_demand_elasticity(cfg)
  message("French hourly demand elasticity: ", round(micro[["coef"]], 4), " (s.e. ", round(micro[["se"]], 4), ")")
  specs <- france_elasticity_specs(cfg, micro[["coef"]], demand_elasticity_es)
  long <- read_country_data(cfg$data$france_2010, "France")
  short <- read_country_data(cfg$data$france, "France")
  tag <- sprintf("_d%d_h%d", cfg$elasticity$draws, cfg$elasticity$horizon)
  fits <- lapply(names(specs), function(key) {
    data <- if (key == "sample_2015") short else long
    get_model(paste0("france2010_elasticity_bvar_", specs[[key]]$var_prior, "_", key, tag, "_eps", round(1000 * specs[[key]]$fixed_demand)), function() {
      message("France elasticity SVAR: ", specs[[key]]$name)
      fit_elasticity_bvar(data, specs[[key]], cfg)
    })
  })
  names(fits) <- names(specs)

  horizon <- cfg$elasticity$horizon
  quantiles <- dplyr::bind_rows(lapply(fits, elasticity_quantiles, horizon = horizon))
  save_csv(quantiles, ocfg, "irfs_france_elasticity_svar.csv")
  slopes <- dplyr::bind_rows(lapply(fits, function(fit) {
    q <- stats::quantile(fit$eps_s, c(0.5, 0.05, 0.95))
    data.frame(Specification = fit$spec$name, Demand_slope = fit$spec$fixed_demand,
               Supply_slope_median = q[[1]], Supply_slope_p05 = q[[2]], Supply_slope_p95 = q[[3]],
               Draws_kept = length(fit$eps_s), Draws = cfg$elasticity$draws,
               Sample_start = format(min(fit$sample$observation_date), "%Y-%m"),
               Sample_end = format(max(fit$sample$observation_date), "%Y-%m"))
  }))
  save_csv(slopes, ocfg, "france_elasticity_svar_slopes.csv")

  central_q <- quantiles[quantiles$Specification == fits$central$spec$name, , drop = FALSE]
  save_plot(plot_elasticity_irf(central_q, "Supply"), pcfg, cfg$plot_names$france_supply_elasticity,
            overleaf_dir = "macro", width = 14, height = 8)
  save_plot(plot_elasticity_irf(central_q, "Demand"), pcfg, cfg$plot_names$france_demand_elasticity,
            overleaf_dir = "macro", width = 14, height = 8)
  legend_bottom <- ggplot2::theme(legend.position = "bottom")
  for (shock in c("Supply", "Demand")) {
    save_plot(plot_elasticity_comparison(quantiles, shock), ocfg,
              paste0(tolower(shock), "_shock_france_elasticity_robustness.png"),
              width = 14, height = 9, theme_overrides = legend_bottom)
  }
  invisible(list(fits = fits, slopes = slopes, quantiles = quantiles))
}
