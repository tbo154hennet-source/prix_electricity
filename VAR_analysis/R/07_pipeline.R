run_pipeline <- function(cfg, selected_stages = "all") {
  set.seed(cfg$seed)
  inputs <- stage_log(cfg, "data", load_inputs(cfg))
  inputs$Spain <- attach_wedge_proxy(inputs$Spain)
  demand_elasticity <- read_micro_demand_elasticity(cfg)
  message("Micro demand-price elasticity: ", round(demand_elasticity, 6))

  state <- new.env(parent = emptyenv())
  get_model <- function(name, builder) {
    if (!exists(name, envir = state, inherits = FALSE)) {
      assign(name, cache_model(cfg, name, builder), envir = state)
    }
    get(name, envir = state, inherits = FALSE)
  }
  requested <- unique(unlist(strsplit(selected_stages, ",", fixed = TRUE)))
  should_run <- function(stage) "all" %in% requested || stage %in% requested
  cache_name <- function(base, inference_sensitive = FALSE) {
    inference_tag <- if (inference_sensitive) {
      if (cfg$use_inference) "_msw" else "_point"
    } else {
      ""
    }
    paste0(base, "_p", cfg$p, "_h", cfg$horizon, inference_tag)
  }

  spain_supply <- function() get_model(cache_name("spain_iv_supply_benchmark", TRUE), function() {
    fit_proxy_svar(
      inputs$Spain, inputs$Spain$supply_proxy,
      "spot_price_log", "Supply", cfg
    )
  })
  spain_demand <- function() get_model(cache_name("spain_iv_demand_benchmark", TRUE), function() {
    fit_proxy_svar(
      inputs$Spain, inputs$Spain$demand_proxy,
      "load_tot_log", "Demand", cfg
    )
  })
  spain_supply_kilian <- function() get_model(
    cache_name("spain_iv_supply_kilian_aggregation_v1", TRUE),
    function() {
      fit_proxy_svar(
        inputs$Spain, inputs$Spain$supply_proxy_kilian,
        "spot_price_log", "Supply", cfg
      )
    }
  )
  spain_demand_kilian <- function() get_model(
    cache_name("spain_iv_demand_kilian_aggregation_v1", TRUE),
    function() {
      fit_proxy_svar(
        inputs$Spain, inputs$Spain$demand_proxy_kilian,
        "load_tot_log", "Demand", cfg
      )
    }
  )
  spain_supply_mixed_frequency <- function() get_model(
    cache_name(paste0("spain_iv_supply_mixed_frequency_daily_v3_boot", cfg$mixed_frequency_bootstrap), TRUE),
    function() {
      fit_mixed_frequency_proxy_svar(
        inputs$Spain,
        inputs$Spain$supply_proxies_mixed_frequency,
        "spot_price_log", "Supply", cfg
      )
    }
  )
  spain_demand_mixed_frequency <- function() get_model(
    cache_name(paste0("spain_iv_demand_mixed_frequency_daily_v3_boot", cfg$mixed_frequency_bootstrap), TRUE),
    function() {
      fit_mixed_frequency_proxy_svar(
        inputs$Spain,
        inputs$Spain$demand_proxies_mixed_frequency,
        "load_tot_log", "Demand", cfg
      )
    }
  )
  spain_gas_supply <- function() get_model(
    cache_name("spain_iv_gas_supply_gazzani_2025", TRUE),
    function() {
      fit_proxy_svar(
        inputs$Spain, inputs$Spain$gas_supply_proxy,
        "spot_gas_log", "Gas supply", cfg
      )
    }
  )
  spain_wind <- function() get_model(cache_name("spain_iv_supply_wind", TRUE), function() {
    fit_proxy_svar(
      inputs$Spain, inputs$Spain$wind_proxy,
      "spot_price_log", "Supply", cfg
    )
  })
  spain_wedge <- function() get_model(cache_name("spain_iv_supply_wedge_only", TRUE), function() {
    fit_proxy_svar(
      inputs$Spain, inputs$Spain$wedge_proxy,
      "spot_price_log", "Supply", cfg
    )
  })
  spain_counterfactual_proxy <- function() get_model(
    cache_name("spain_iv_supply_incorporated_counterfactual"),
    function() {
      fit_proxy_svar(
        inputs$Spain, inputs$Spain$counterfactual_proxy,
        "spot_price_log", "Supply", cfg,
        use_inference = FALSE
      )
    }
  )
  spain_cholesky <- function() get_model(cache_name("spain_cholesky_price_quantity"), function() {
    fit_cholesky(inputs$Spain, cfg)
  })
  spain_mr <- function() get_model(cache_name("spain_mertens_ravn_demand_curve_v2"), function() {
    fit_mertens_ravn(
      inputs$Spain,
      inputs$Spain$demand_proxy,
      inputs$Spain$supply_proxy,
      demand_elasticity,
      cfg
    )
  })
  france_wind <- function() get_model(cache_name("france_iv_supply_wind", TRUE), function() {
    fit_proxy_svar(
      inputs$France, inputs$France$wind_proxy,
      "spot_price_log", "Supply", cfg
    )
  })
  france_demand <- function() get_model(cache_name("france_iv_demand_temperature", TRUE), function() {
    fit_proxy_svar(
      inputs$France, inputs$France$demand_proxy,
      "load_tot_log", "Demand", cfg
    )
  })
  france_mr <- function() get_model(cache_name("france_mertens_ravn_demand_curve_v2"), function() {
    fit_mertens_ravn(
      inputs$France,
      inputs$France$demand_proxy,
      inputs$France$supply_proxy,
      demand_elasticity,
      cfg
    )
  })
  bvar_name <- function(base) {
    paste0(
      base, "_p", cfg$p, "_h", cfg$bvar$horizon, "_d", cfg$bvar$draws,
      if (cfg$bvar$stable_only) "_stable" else ""
    )
  }
  spain_supply_bvar <- function() get_model(bvar_name("spain_bvar_supply_benchmark"), function() {
    fit_proxy_bvar(
      inputs$Spain, inputs$Spain$supply_proxy,
      "spot_price_log", "Supply", cfg
    )
  })
  spain_demand_bvar <- function() get_model(bvar_name("spain_bvar_demand_benchmark"), function() {
    fit_proxy_bvar(
      inputs$Spain, inputs$Spain$demand_proxy,
      "load_tot_log", "Demand", cfg
    )
  })

  if (should_run("irfs")) {
    stage_log(cfg, "irfs", {
      supply <- spain_supply()
      demand <- spain_demand()
      save_plot(
        plot_iv_irf(
          supply,
          "Spain: electricity supply shock",
          "Benchmark PCA proxy; MSW confidence intervals"
        ),
        cfg, cfg$plot_names$spain_supply,
        overleaf_dir = "macro", width = 10, height = 6
      )
      save_plot(
        plot_iv_irf(
          demand,
          "Spain: electricity demand shock",
          "Benchmark PCA proxy; MSW confidence intervals"
        ),
        cfg, cfg$plot_names$spain_demand,
        overleaf_dir = "macro", width = 10, height = 6
      )
      gas_supply <- spain_gas_supply()
      save_plot(
        plot_iv_irf(
          gas_supply,
          "Spain: adverse gas-supply shock",
          "Alessandri-Gazzani external instrument; gas-price normalization"
        ),
        cfg, "gas_supply_shock_spain_irf.png",
        overleaf_dir = "macro", width = 10, height = 6
      )
      benchmark_irfs <- dplyr::bind_rows(
        transform(supply$irf, Specification = "Supply benchmark"),
        transform(demand$irf, Specification = "Demand benchmark"),
        transform(gas_supply$irf, Specification = "Gas supply: Gazzani 2025")
      )
      save_csv(benchmark_irfs, cfg, "irfs_spain_benchmark.csv")
    })
  }

  if (should_run("relevance")) {
    stage_log(cfg, "relevance", {
      run_relevance(
        list(
          "Supply proxy: benchmark PCA" = spain_supply(),
          "Demand proxy: benchmark PCA" = spain_demand(),
          "Supply proxy: Kilian (2024)" = spain_supply_kilian(),
          "Demand proxy: Kilian (2024)" = spain_demand_kilian(),
          "Supply proxy: mixed frequency" = spain_supply_mixed_frequency(),
          "Demand proxy: mixed frequency" = spain_demand_mixed_frequency(),
          "Supply proxy: wind" = spain_wind(),
          "Supply proxy: Iberian wedge only" = spain_wedge(),
          "Gas supply proxy: Alessandri-Gazzani 2025" = spain_gas_supply()
        ),
        cfg,
        csv_name = "proxy_relevance_spain.csv",
        tex_name = "iv_relevance_spain_tot.tex"
      )
    })
  }

  # With the elasticity SVAR as central specification, the proxy-based contribution and
  # counterfactual outputs are kept locally and do not overwrite the Overleaf files.
  legacy_cfg <- cfg
  if (identical(cfg$central_identification, "elasticity")) legacy_cfg$overleaf$enabled <- FALSE

  if (should_run("contributions")) {
    stage_log(cfg, "contributions", {
      run_spain_contributions(
        spain_supply(), spain_demand(), spain_gas_supply(),
        legacy_cfg, inputs$Spain$data
      )
    })
  }

  if (should_run("counterfactuals")) {
    stage_log(cfg, "counterfactuals", {
      run_counterfactuals(spain_supply(), legacy_cfg)
    })
  }

  if (should_run("robustness")) {
    stage_log(cfg, "robustness-proxy", {
      wind <- spain_wind()
      wedge <- spain_wedge()
      benchmark <- spain_supply()
      benchmark_demand <- spain_demand()
      kilian_supply <- spain_supply_kilian()
      kilian_demand <- spain_demand_kilian()
      mixed_frequency_supply <- spain_supply_mixed_frequency()
      mixed_frequency_demand <- spain_demand_mixed_frequency()

      aggregation_supply_results <- list(
        "Monthly average (benchmark)" = benchmark,
        "Kilian (2024) timing weights" = kilian_supply,
        "Mixed-frequency daily proxy" = mixed_frequency_supply
      )
      aggregation_demand_results <- list(
        "Monthly average (benchmark)" = benchmark_demand,
        "Kilian (2024) timing weights" = kilian_demand,
        "Mixed-frequency daily proxy" = mixed_frequency_demand
      )
      aggregation_theme <- ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "horizontal"
      )
      aggregation_guides <- ggplot2::guides(
        colour = ggplot2::guide_legend(ncol = 3, byrow = TRUE),
        linetype = ggplot2::guide_legend(ncol = 3, byrow = TRUE),
        fill = ggplot2::guide_legend(ncol = 3, byrow = TRUE)
      )
      save_plot(
        plot_irf_comparison(
          aggregation_supply_results,
          "Supply",
          "Spain: supply-shock proxy aggregation robustness",
          "Monthly average, Kilian (2024) timing weights, and daily mixed-frequency instruments"
        ) + aggregation_guides,
        cfg, cfg$plot_names$spain_supply_proxy_aggregation,
        overleaf_dir = "macro", width = 11, height = 7,
        theme_overrides = aggregation_theme
      )
      save_plot(
        plot_irf_comparison(
          aggregation_demand_results,
          "Demand",
          "Spain: demand-shock proxy aggregation robustness",
          "Monthly average, Kilian (2024) timing weights, and daily mixed-frequency instruments"
        ) + aggregation_guides,
        cfg, cfg$plot_names$spain_demand_proxy_aggregation,
        overleaf_dir = "macro", width = 11, height = 7,
        theme_overrides = aggregation_theme
      )

      aggregation_irfs <- dplyr::bind_rows(
        transform(
          prepare_iv_plot_data(benchmark),
          Structural_shock = "Supply",
          Aggregation = "Monthly average (benchmark)"
        ),
        transform(
          prepare_iv_plot_data(kilian_supply),
          Structural_shock = "Supply",
          Aggregation = "Kilian (2024) timing weights"
        ),
        transform(
          prepare_iv_plot_data(mixed_frequency_supply),
          Structural_shock = "Supply",
          Aggregation = "Mixed-frequency daily proxy"
        ),
        transform(
          prepare_iv_plot_data(benchmark_demand),
          Structural_shock = "Demand",
          Aggregation = "Monthly average (benchmark)"
        ),
        transform(
          prepare_iv_plot_data(kilian_demand),
          Structural_shock = "Demand",
          Aggregation = "Kilian (2024) timing weights"
        ),
        transform(
          prepare_iv_plot_data(mixed_frequency_demand),
          Structural_shock = "Demand",
          Aggregation = "Mixed-frequency daily proxy"
        )
      )
      save_csv(
        aggregation_irfs,
        cfg,
        "irfs_spain_proxy_aggregation_robustness.csv"
      )
      proxy_columns <- c(
        "observation_date", "delta_s_tot", "delta_d_tot",
        "delta_s_tot_kilian", "delta_d_tot_kilian",
        "proxy_supply_pc", "proxy_demand_pc",
        "proxy_supply_kilian_pc", "proxy_demand_kilian_pc"
      )
      save_csv(
        inputs$Spain$data[, proxy_columns, drop = FALSE],
        cfg,
        "spain_monthly_proxies_kilian_aggregation.csv"
      )
      mixed_frequency_weights <- dplyr::bind_rows(
        transform(
          mixed_frequency_supply$first_stage_weights,
          Specification = "Supply"
        ),
        transform(
          mixed_frequency_demand$first_stage_weights,
          Specification = "Demand"
        )
      )
      save_csv(
        mixed_frequency_weights,
        cfg,
        "mixed_frequency_daily_first_stage_weights.csv"
      )
      mixed_frequency_proxy_columns <- c(
        "observation_date",
        inputs$Spain$supply_proxies_mixed_frequency,
        inputs$Spain$demand_proxies_mixed_frequency
      )
      save_csv(
        inputs$Spain$data[, mixed_frequency_proxy_columns, drop = FALSE],
        cfg,
        "spain_mixed_frequency_daily_proxy_matrix.csv"
      )
      save_csv(
        inputs$Spain$mixed_frequency_daily_proxies,
        cfg,
        "spain_mixed_frequency_daily_proxies.csv"
      )
      save_plot(
        plot_iv_irf(
          wind,
          "Spain: electricity supply shock",
          "Wind proxy; MSW confidence intervals"
        ),
        cfg, cfg$plot_names$spain_wind,
        overleaf_dir = "macro", width = 10, height = 6
      )
      save_plot(
        plot_iv_irf(
          wedge,
          "Spain: electricity supply shock",
          "Standalone Iberian wedge proxy"
        ),
        cfg, cfg$plot_names$spain_wedge,
        overleaf_dir = "macro", width = 10, height = 6
      )
      save_plot(
        plot_proxy_comparison(benchmark, wind, wedge) +
          ggplot2::guides(
            colour = ggplot2::guide_legend(ncol = 3, byrow = TRUE),
            linetype = ggplot2::guide_legend(ncol = 3, byrow = TRUE)
          ),
        cfg, cfg$plot_names$spain_proxy_compare,
        overleaf_dir = "macro", width = 11, height = 7,
        theme_overrides = ggplot2::theme(
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      )
      proxy_irfs <- dplyr::bind_rows(
        transform(benchmark$irf, Specification = "Benchmark PCA"),
        transform(wind$irf, Specification = "Wind"),
        transform(wedge$irf, Specification = "Iberian wedge only")
      )
      save_csv(proxy_irfs, cfg, "irfs_spain_proxy_robustness.csv")
    })

    stage_log(cfg, "robustness-identification", {
      cholesky <- spain_cholesky()
      mr <- spain_mr()
      supply_results <- list(
        "Benchmark IV" = spain_supply(),
        "Cholesky: price then quantity" = cholesky,
        "Mertens-Ravn" = mr
      )
      demand_results <- list(
        "Benchmark IV" = spain_demand(),
        "Cholesky: price then quantity" = cholesky,
        "Mertens-Ravn" = mr
      )
      save_plot(
        plot_irf_comparison(
          supply_results, "Supply",
          "Spain: supply-shock identification robustness",
          paste0("Mertens-Ravn demand elasticity = ", round(demand_elasticity, 3))
        ) +
          ggplot2::guides(
            colour = ggplot2::guide_legend(ncol = 3, byrow = TRUE),
            linetype = ggplot2::guide_legend(ncol = 3, byrow = TRUE)
          ),
        cfg, cfg$plot_names$spain_supply_identification,
        overleaf_dir = "macro", width = 10, height = 7,
        theme_overrides = ggplot2::theme(
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      )
      save_plot(
        plot_irf_comparison(
          demand_results, "Demand",
          "Spain: demand-shock identification robustness",
          paste0("Mertens-Ravn demand elasticity = ", round(demand_elasticity, 3))
        ) +
          ggplot2::guides(
            colour = ggplot2::guide_legend(ncol = 3, byrow = TRUE),
            linetype = ggplot2::guide_legend(ncol = 3, byrow = TRUE)
          ),
        cfg, cfg$plot_names$spain_demand_identification,
        overleaf_dir = "macro", width = 10, height = 7,
        theme_overrides = ggplot2::theme(
          legend.position = "bottom",
          legend.direction = "horizontal"
        )
      )
      identification_irfs <- dplyr::bind_rows(
        transform(spain_supply()$irf, Identification = "Benchmark IV"),
        transform(spain_demand()$irf, Identification = "Benchmark IV"),
        transform(cholesky$irf, Identification = "Cholesky"),
        transform(mr$irf, Identification = "Mertens-Ravn")
      )
      save_csv(
        identification_irfs,
        cfg,
        "irfs_spain_identification_robustness.csv"
      )
      mr_decomposition <- list(
        hd = mr$hd,
        dates = mr$dates,
        variables = mr$variables
      )
      mr_table <- contribution_table_from_hd(
        mr_decomposition,
        cfg$windows,
        "Spain",
        paste0("Mertens-Ravn; demand elasticity ", round(demand_elasticity, 6)),
        inputs$Spain$data
      )
      export_contributions(
        mr_table, cfg,
        "contributions_spain_mertens_ravn.csv",
        "cumulative_contributions_mr_spain.tex"
      )
    })
  }

  # Opt-in: not part of "all", the manuscript figures stay OLS/MSW.
  if ("bvar" %in% requested) {
    stage_log(cfg, "bvar", {
      horizon <- cfg$bvar$horizon
      subtitle <- paste0(
        "Benchmark PCA proxy; diffuse NIW prior; 68% and 90% posterior bands"
      )
      supply_bvar <- spain_supply_bvar()
      demand_bvar <- spain_demand_bvar()
      save_plot(
        plot_iv_irf(supply_bvar, "Spain: electricity supply shock (BVAR)", subtitle),
        cfg, cfg$plot_names$spain_supply_bvar,
        overleaf_dir = "macro", width = 10, height = 6
      )
      save_plot(
        plot_iv_irf(demand_bvar, "Spain: electricity demand shock (BVAR)", subtitle),
        cfg, cfg$plot_names$spain_demand_bvar,
        overleaf_dir = "macro", width = 10, height = 6
      )
      save_plot(
        plot_irf_comparison(
          list(
            "OLS (MSW)" = truncate_plot_irfs(spain_supply(), horizon),
            "BVAR" = supply_bvar
          ),
          "Supply",
          "Spain: supply shock, OLS vs BVAR"
        ),
        cfg, cfg$plot_names$spain_supply_bvar_ols,
        overleaf_dir = "macro", width = 12, height = 7
      )
      save_plot(
        plot_irf_comparison(
          list(
            "OLS (MSW)" = truncate_plot_irfs(spain_demand(), horizon),
            "BVAR" = demand_bvar
          ),
          "Demand",
          "Spain: demand shock, OLS vs BVAR"
        ),
        cfg, cfg$plot_names$spain_demand_bvar_ols,
        overleaf_dir = "macro", width = 12, height = 7
      )
      save_csv(
        rbind(supply_bvar$plot_irfs, demand_bvar$plot_irfs),
        cfg, "irf_bvar_spain.csv"
      )
    })
  }

  if (should_run("france")) {
    stage_log(cfg, "france", {
      wind <- france_wind()
      demand <- france_demand()
      mr <- france_mr()

      save_plot(
        plot_iv_irf(
          wind,
          "France: electricity supply shock",
          "Wind proxy; MSW confidence intervals"
        ),
        cfg, cfg$plot_names$france_wind,
        overleaf_dir = "macro", width = 10, height = 6
      )
      save_plot(
        plot_irf_comparison(
          list("Wind IV" = wind, "Mertens-Ravn" = mr),
          "Supply",
          "France: supply-shock identification robustness",
          paste0("Mertens-Ravn demand elasticity = ", round(demand_elasticity, 3))
        ),
        cfg, cfg$plot_names$france_supply_identification,
        overleaf_dir = "macro", width = 10, height = 7
      )
      save_plot(
        plot_irf_comparison(
          list("Temperature IV" = demand, "Mertens-Ravn" = mr),
          "Demand",
          "France: demand-shock identification robustness",
          paste0("Mertens-Ravn demand elasticity = ", round(demand_elasticity, 3))
        ),
        cfg, cfg$plot_names$france_demand_identification,
        overleaf_dir = "macro", width = 10, height = 7
      )

      france_irfs <- dplyr::bind_rows(
        transform(wind$irf, Identification = "Wind IV"),
        transform(demand$irf, Identification = "Temperature IV"),
        transform(mr$irf, Identification = "Mertens-Ravn")
      )
      save_csv(france_irfs, cfg, "irfs_france.csv")

      iv_decomposition <- combine_single_proxy_hd(wind, demand)
      iv_table <- contribution_table_from_hd(
        iv_decomposition, cfg$windows, "France",
        "External instruments: wind and temperature",
        inputs$France$data
      )
      export_contributions(
        iv_table, cfg,
        "contributions_france_iv.csv",
        "cumulative_contributions_iv_france.tex"
      )
      mr_decomposition <- list(
        hd = mr$hd,
        dates = mr$dates,
        variables = mr$variables
      )
      mr_table <- contribution_table_from_hd(
        mr_decomposition, cfg$windows, "France",
        paste0("Mertens-Ravn; demand elasticity ", round(demand_elasticity, 6)),
        inputs$France$data
      )
      export_contributions(
        mr_table, cfg,
        "contributions_france_mertens_ravn.csv",
        "cumulative_contributions_mr_france.tex"
      )
      run_relevance(
        list(
          "Spain supply: PCA" = spain_supply(),
          "Spain demand: PCA" = spain_demand(),
          "France supply: wind" = wind,
          "France demand: temperature" = demand
        ),
        cfg,
        csv_name = "proxy_relevance_spain_france.csv",
        tex_name = "relevance_international_macro.tex"
      )
    })
  }

  if (should_run("france2010")) {
    stage_log(cfg, "france2010", {
      run_france_extended_stage(
        france_wind(), france_demand(), cfg, demand_elasticity,
        get_model, cache_name
      )
    })
  }

  if (should_run("elasticity")) {
    stage_log(cfg, "elasticity", {
      run_elasticity_stage(inputs$Spain, cfg)
    })
  }

  manifest <- data.frame(
    Order = seq_len(10),
    Stage = c(
      "data", "irfs", "relevance", "contributions",
      "counterfactuals", "robustness-proxy",
      "robustness-identification", "france", "france2010", "elasticity"
    ),
    Description = c(
      "Load and validate Spain/France monthly inputs",
      "Spain benchmark external-instrument IRFs",
      "First-stage proxy relevance",
      "Supply/demand contributions to inflation episodes",
      "No-gas-subsidy Iberian path and 2030-wind-capacity counterfactuals",
      "Wind and wedge proxy robustness",
      "Price-quantity Cholesky and Mertens-Ravn robustness",
      "France wind-IV and Mertens-Ravn replication",
      "France VAR extended to 2010 with wind proxies (local outputs)",
      "Central Bayesian elasticity SVAR (GLP prior) and alternative specifications"
    )
  )
  save_csv(manifest, cfg, "pipeline_manifest.csv")
  invisible(as.list(state))
}
