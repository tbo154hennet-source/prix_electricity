save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/log_diff/benchmark'
setwd(save_rds_path)

gen_benchmark_esp <- readRDS( "gen_benchmark_esp.rds")
cumulative_response <- gen_benchmark_esp$lambda_0
#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
print(passthrough_esp)

gen_benchmark_fr <- readRDS( "gen_benchmark_fr.rds")
cumulative_response <- gen_benchmark_fr$lambda_0
#one year cumulative pass-through
passthrough_fr <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
print(passthrough_fr)


gen_benchmark_esp <- readRDS( "gen_benchmark_esp_gas_mp.rds")
cumulative_response <- gen_benchmark_esp$lambda_0
#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
print(passthrough_esp)

gen_benchmark_fr <- readRDS( "gen_benchmark_fr_gas_mp.rds")
cumulative_response <- gen_benchmark_fr$lambda_0
#one year cumulative pass-through
passthrough_fr <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
print(passthrough_fr)




