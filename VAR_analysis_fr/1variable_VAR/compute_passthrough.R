
#comparison with alessandir gazzani

save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

gen_benchmark_esp <- readRDS( "gen_benchmark_esp_supply.rds")
cumulative_response <- gen_benchmark_esp$lambda_0
#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 1] # 0.0055844890
print(passthrough_esp)


gen_benchmark_esp <- readRDS( "gen_benchmark_esp_mp_supply.rds")
cumulative_response <- gen_benchmark_esp$lambda_0
#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 1] # 0.0055844890
print(passthrough_esp)

save_rds_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/1var'
setwd(save_rds_path)

gen_benchmark_esp <- readRDS( "gen_benchmark_esp_double.rds")
cumulative_response <- gen_benchmark_esp$lambda_0
#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 2]# 0.0055844890
print(passthrough_esp)


gen_benchmark_esp <- readRDS( "gen_benchmark_esp_double_mp.rds")
cumulative_response <- gen_benchmark_esp$lambda_0
#one year cumulative pass-through
passthrough_esp <- cumulative_response[12, 4, 2] # 0.0055844890
print(passthrough_esp)



# gen_benchmark_fr <- readRDS( "gen_benchmark_fr_supply.rds")
# cumulative_response <- gen_benchmark_fr$lambda_0
# #one year cumulative pass-through
# passthrough_fr <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
# print(passthrough_fr)
# 
# gen_benchmark_fr <- readRDS( "gen_benchmark_fr_gas_mp_supply.rds")
# cumulative_response <- gen_benchmark_fr$lambda_0
# #one year cumulative pass-through
# passthrough_fr <- cumulative_response[12, 4, 2]/ cumulative_response[12, 2, 2] # 0.0055844890
# print(passthrough_fr)


gen_benchmark_esp$bandwith_68
gen_benchmark_esp$bandwith_90

