# Backward-compatible entry point for the integrated supply/demand HD chain.
#
# The former version depended on global objects and could use coefficients from
# a different VAR when computing demand and supply decompositions.

pipeline_file <- file.path(
  'Z:/prix_electricite_gitlab', 'VAR_analysis_fr', 'supply_demand',
  'HD_FEVD', 'esp', 'compare_HD_with_without_gas.R'
)

if (!file.exists(pipeline_file)) {
  stop('Integrated HD pipeline not found: ', pipeline_file)
}

source(pipeline_file, local = FALSE)

# Legacy names retained for notebooks which source this script.
historical_decomp_iv_tibo <- models[['Including Gas']][['hd']]
historical_decomp_iv_without_gas <- models[['Electricity Specific']][['hd']]
dates_formatted <- models[['Including Gas']][['dates']]

saveRDS(
  historical_decomp_iv_tibo,
  file.path(dirs[['hd']], 'hd_iv_log.rds')
)
saveRDS(
  historical_decomp_iv_without_gas,
  file.path(dirs[['hd']], 'hd_iv_log_without_gas.rds')
)

message('HD estimation complete: Including Gas and Electricity Specific.')
