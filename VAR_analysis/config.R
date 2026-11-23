make_config <- function(root_dir) {
  bool_env <- function(name, default = TRUE) {
    value <- tolower(Sys.getenv(name, if (default) "true" else "false"))
    value %in% c("1", "true", "yes", "oui")
  }

  pipeline_dir <- file.path(root_dir, "VAR_analysis")
  output_dir <- file.path(pipeline_dir, "output")
  overleaf_root <- Sys.getenv(
    "VAR_OVERLEAF_ROOT",
    "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images"
  )

  list(
    root = root_dir,
    pipeline_dir = pipeline_dir,
    legacy_data_dir = file.path(root_dir, "VAR_analysis", "data"),
    output_dir = output_dir,
    model_dir = file.path(output_dir, "models"),
    table_dir = file.path(output_dir, "tables"),
    figure_dir = file.path(output_dir, "figures"),
    log_dir = file.path(output_dir, "logs"),
    overleaf = list(
      enabled = bool_env("VAR_EXPORT_OVERLEAF", TRUE),
      macro = file.path(overleaf_root, "macro"),
      policy = file.path(overleaf_root, "policy"),
      tables = file.path(overleaf_root, "latex_tables")
    ),
    data = list(
      spain = file.path(
        root_dir, "VAR_analysis", "data",
        "IV_data_esp_log_elas_sd_2010.parquet"
      ),
      spain_high_frequency_proxies = file.path(
        root_dir, "supply_demand_curve", "outputs",
        "coef_reg_no_conditioning",
        "price_indispo_merge_esp_npc_all_final_2026.parquet"
      ),
      france = file.path(
        root_dir, "VAR_analysis", "data",
        "IV_data_fr_log_elas_sd_2026.parquet"
      ),
      micro_elasticity = file.path(
        root_dir, "supply_demand_curve", "estimate_elasticity", "outputs", "elasticity_standard.csv"
      ),
      spain_counterfactual_proxy = file.path(
        root_dir, "VAR_analysis", "data",
        "IV_data_esp_log_elas_sd_2026_pc.parquet"
      ),
      gas_supply_shocks = Sys.getenv(
        "VAR_GAS_SUPPLY_SHOCKS",
        "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/GasSupplyShocks.xlsx"
      ),
      iberian_no_subsidy = file.path(
        root_dir, "day_ahead", "outputs",
        "prediction_gas_price_counterfactual_monthly.parquet"
      ),
      wind_2030 = file.path(
        root_dir, "day_ahead", "outputs",
        "price_variance_counterfactual_esp_ter.parquet"
      ),
      # France extended to 2010-01: Data_structuring/france_var_backfill/build_france_var_2010.R
      france_2010 = file.path(
        root_dir, "VAR_analysis", "data",
        "IV_data_fr_log_elas_sd_2010.parquet"
      ),
      france_price_level = file.path(
        root_dir, "Data_structuring", "output", "IV_data_fr_elas.parquet"
      )
    ),
    p = as.integer(Sys.getenv("VAR_LAGS", "12")),
    horizon = as.integer(Sys.getenv("VAR_HORIZON", "40")),
    confidence = c(0.68, 0.90),
    nw_lags = as.integer(Sys.getenv("VAR_NW_LAGS", "0")),
    mixed_frequency_bootstrap = as.integer(Sys.getenv("VAR_MF_BOOT", "500")),
    use_inference = bool_env("VAR_USE_MSW_INFERENCE", TRUE),
    # "elasticity": the Bayesian elasticity SVAR is the central specification and exports the
    # contribution and counterfactual outputs; the proxy-based stages then write locally only.
    central_identification = Sys.getenv("VAR_CENTRAL", "elasticity"),
    force = bool_env("VAR_FORCE", FALSE),
    seed = as.integer(Sys.getenv("VAR_SEED", "20260914")),
    counterfactual_bootstrap = as.integer(Sys.getenv("VAR_CF_BOOT", "200")),
    counterfactual_block_size = as.integer(Sys.getenv("VAR_CF_BLOCK", "0")),
    bvar = list(
      draws = as.integer(Sys.getenv("VAR_BVAR_DRAWS", "5000")),
      horizon = as.integer(Sys.getenv("VAR_BVAR_HORIZON", "24")),
      stable_only = bool_env("VAR_BVAR_STABLE_ONLY", FALSE)
    ),
    # Central identification: Bayesian SVAR with priors on the electricity curve slopes (09_elasticity_svar.R).
    elasticity = list(
      draws = as.integer(Sys.getenv("VAR_ELAS_DRAWS", "2000")),
      proposals = as.integer(Sys.getenv("VAR_ELAS_PROPOSALS", "400")),
      proposal_range = c(0.01, 0.70),
      horizon = as.integer(Sys.getenv("VAR_ELAS_HORIZON", "24")),
      p_central = 12L,
      # Supply slope: micro hourly IV 0.237 (s.e. 0.033); daily local projections at 30 days 0.161 (0.034).
      prior_supply = c(0.20, 0.04),
      prior_supply_micro = c(0.237, 0.033),
      # Demand slope: daily local projections, -0.02 to -0.05 between 1 and 30 days.
      prior_demand = c(-0.04, 0.03),
      # Synthetic proxies: full hourly supply and demand curve shifts, Kilian-aggregated (two-proxy exclusions).
      supply_proxy = "k_S_syn_surp",
      demand_proxy = "k_D_syn_surp",
      # robustness: temperature component of the demand shift only
      temperature_proxy = "k_D_block_surp",
      # benchmark without slope priors: wider support for the supply slope
      proposal_range_flat = c(0.01, 1.5),
      # posterior draws kept for the historical decompositions and contribution bands
      stored_draws = as.integer(Sys.getenv("VAR_ELAS_STORED", "300")),
      export_overleaf = bool_env("VAR_ELASTICITY_OVERLEAF", TRUE)
    ),
    # Stage france2010: wind supply proxies, the first one is the central one
    # (ERA5 load factor, observed from 2010, net of the degree-day anomalies:
    # windy winter months are mild, so raw wind also loads on heating demand).
    france_2010 = list(
      supply_proxies = c(
        "wind_era5_cf_log_tadj", "wind_era5_cf_log", "Fore_Wind_log",
        "wind_rte_cf_log_tadj", "wind_rte_cf_log",
        "wind_era5_surprise_tadj", "wind_era5_surprise",
        "wind_rte_surprise_tadj", "wind_rte_surprise"
      )
    ),
    wind_capacity_2030_mw = 62000,
    windows = list(
      inflation = as.Date(c("2021-04-01", "2022-09-01")),
      disinflation = as.Date(c("2022-07-01", "2023-06-01"))
    ),
    electricity_price_windows = list(
      inflation = as.Date(c("2021-04-01", "2022-03-01")),
      disinflation = as.Date(c("2022-04-01", "2023-03-01"))
    ),
    plot_names = list(
      spain_supply = "supply_msw_main_plot.png",
      spain_supply_elasticity = "supply_elasticity_main_plot.png",
      spain_demand_elasticity = "demand_elasticity_main_plot.png",
      spain_elasticity_slopes = "elasticity_slopes_prior_posterior.png",
      spain_demand = "demand_msw_main_plot.png",
      spain_supply_bvar = "supply_bvar_main_plot.png",
      spain_demand_bvar = "demand_bvar_main_plot.png",
      spain_supply_bvar_ols = "supply_bvar_vs_ols_spain.png",
      spain_demand_bvar_ols = "demand_bvar_vs_ols_spain.png",
      spain_wind = "supply_msw_wind_plot.png",
      spain_wedge = "supply_msw_wedge_plot.png",
      spain_proxy_compare = "irf_wind_delta_supply_demand_spain.png",
      spain_supply_proxy_aggregation = "supply_shock_spain_irf_aggregation_robustness.png",
      spain_demand_proxy_aggregation = "demand_shock_spain_irf_aggregation_robustness.png",
      spain_supply_identification = "supply_shock_spain_irf_compare.png",
      spain_demand_identification = "demand_shock_spain_irf_compare.png",
      france_supply_identification = "supply_shock_france_irf_compare.png",
      france_demand_identification = "demand_shock_france_irf_compare.png",
      france_wind = "wind_supply_shock_fr.png",
      iberian_shock = "iberian_shock.png",
      iberian_price = "iberian_exception_plot_price_counterfactual.png",
      iberian_inflation = "iberian_exception_plot_inflation_counterfactual.png",
      wind_counterfactual = "wind_counterfactuals.png",
      macro_fluctuations = "macro_fluct.png"
    )
  )
}
