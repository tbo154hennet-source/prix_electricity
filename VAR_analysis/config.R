make_config <- function(root_dir) {
  bool_env <- function(name, default = TRUE) {
    value <- tolower(Sys.getenv(name, if (default) "true" else "false"))
    value %in% c("1", "true", "yes", "oui")
  }

  pipeline_dir <- file.path(root_dir, "VAR_analysis")
  output_dir <- file.path(pipeline_dir, "output")
  overleaf_root <- Sys.getenv(
    "VAR_OVERLEAF_ROOT",
    "Z:/Overleaf_elec/images"
  )

  list(
    root = root_dir,
    pipeline_dir = pipeline_dir,
    legacy_data_dir = file.path(root_dir, "VAR_analysis_fr", "VAR_output"),
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
        root_dir, "VAR_analysis_fr", "VAR_output",
        "IV_data_esp_log_elas_sd_2026.parquet"
      ),
      spain_high_frequency_proxies = file.path(
        root_dir, "supply_demand_curve", "outputs",
        "coef_reg_no_conditioning",
        "price_indispo_merge_esp_npc_all_final_2026.parquet"
      ),
      france = file.path(
        root_dir, "VAR_analysis_fr", "VAR_output",
        "IV_data_fr_log_elas_sd_2026.parquet"
      ),
      micro_elasticity = file.path(
        root_dir, "day_ahead", "outputs", "esp", "df_clean_da.csv"
      ),
      spain_counterfactual_proxy = file.path(
        root_dir, "VAR_analysis_fr", "VAR_output",
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
      france_price_level = file.path(
        root_dir, "Data_Structuring", "output", "IV_data_fr_elas.parquet"
      )
    ),
    p = as.integer(Sys.getenv("VAR_LAGS", "12")),
    horizon = as.integer(Sys.getenv("VAR_HORIZON", "40")),
    confidence = c(0.68, 0.90),
    nw_lags = as.integer(Sys.getenv("VAR_NW_LAGS", "0")),
    mixed_frequency_bootstrap = as.integer(Sys.getenv("VAR_MF_BOOT", "500")),
    use_inference = bool_env("VAR_USE_MSW_INFERENCE", TRUE),
    force = bool_env("VAR_FORCE", FALSE),
    seed = as.integer(Sys.getenv("VAR_SEED", "20260914")),
    counterfactual_bootstrap = as.integer(Sys.getenv("VAR_CF_BOOT", "200")),
    counterfactual_block_size = as.integer(Sys.getenv("VAR_CF_BLOCK", "0")),
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
      spain_demand = "demand_msw_main_plot.png",
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
