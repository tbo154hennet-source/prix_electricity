

#on se rend compte qu'il m'inverse les chocs de supply et demand 
#comment faire ?




#on se rend compte qu'il m'inverse les chocs de supply et demand 
#comment faire ?

structural_shocks_IV_double_bis <- structural_shocks_IV_double[c(2,1), ]
structural_shocks_IV_double_bis[2,] <- -structural_shocks_IV_double_bis[2,]
#Plot the shocks
dim(structural_shocks_IV_double_bis)
data_IV <- as.data.frame(t(rbind(structural_shocks_IV_double_bis,
                                 structural_shocks_IV_gen, structural_shocks_IV_spot) )
)
colnames(data_IV) <- c( "demand_double_bis", "supply_double_bis", "demand", "supply")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
my_stru_shock <- ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$demand_double_bis/10, color = "Demand", linetype = "Double"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply_double_bis, color = "Supply", linetype = "Double")) +
  geom_line(aes(x = dates_formatted, y = data_IV$demand, color = "Demand", linetype = "Baseline"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply/10, color = "Supply", linetype = "Baseline")) +
  theme_minimal()+
  scale_y_continuous(
    name = "Demand Shock",
    sec.axis = sec_axis(~ . * 10, name = "Supply Shock")
  ) +
  labs(title = "Structural Shocks from IV Analysis",
       x = "Date",
       y = "Shock Value")

my_stru_shock 
save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/macro'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("structural_shocks.png", plot = my_stru_shock , width = 12, height = 6, dpi = 300)





summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE)
)

data_IV %>% 
  cor()



lambda_0_iv_double_bis <- lambda_0_iv_double[,,c(2,1)]
lambda_0_iv_double_bis[,,2] <- -lambda_0_iv_double_bis[,,2]
apply(lambda_0_iv_double_bis , c(2, 3), function(x) cumsum(x))


#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)
historical_decomp_iv_tibo_double_bis <-array(NA, dim = c(n_obs,  n_vars, k))

# lambda_0_iv <- copy(lambda_0)
# 
# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]

k<-2
#j is the variable of interest
for (j in c(1:n_vars)){
  for (shock in c(1:k)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0_iv_double_bis[hori,j, shock]*structural_shocks_IV_double_bis[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo_double_bis[t,j, shock] <- contribution
    }
  }
  
}


historical_decomp_iv_tibo_double_bis
apply(historical_decomp_iv_tibo_double_bis, c(2,3), function(x) cumsum(x))

historical_decomp_iv_tibo_double_bis <- historical_decomp_iv_tibo_double_bis[, , c(2, 1)]

# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls

save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo_double_bis, "hd_iv_4v_double_bis.rds")
hd_cholesky_double_bis <- readRDS( "hd_chol_4v.rds")


# hd_pelec <- plot_hd(historical_decomp_iv_tibo_double_bis, series = 1)
# print(hd_pelec)





#historical_decomp_iv_tibo_double_bis <- historical_decomp_iv_tibo_double_bis[, , c(2, 1)]
# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest

# Example: get contribution of electricity price shocks to inflation

# Compute contributions for 2021 Q2 to 2022 Q4
contribution_data_double_bis <- compute_contributions(
  start_date = "2021-04-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky_double_bis,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo_double_bis,
  series = 4
)

contribution_table_wide_double_bis <- contribution_data_double_bis %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/contribution")
write_csv(contribution_table_wide_double_bis, "contribution_table_wide_double_bis.csv")




gt_table <- contribution_table_wide_double_bis %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  gt::cols_label(
    Identification = "Identification",
    `$P_{elec}$` = gt::md("$P_{elec}$"),
    `$Q_{elec}$` = gt::md("$Q_{elec}$")
  ) %>%
  gt::fmt_number(
    columns = c(`$P_{elec}$`, `$Q_{elec}$`),
    decimals = 2
  )

gt_table








#faire la même chose avec gas control



structural_shocks_IV_double_gas_bis <- structural_shocks_IV_double_gas[c(2,1), ]
structural_shocks_IV_double_gas_bis[2,] <- -structural_shocks_IV_double_gas_bis[2,]
#Plot the shocks
dim(structural_shocks_IV_double_gas_bis)
data_IV <- as.data.frame(t(rbind(structural_shocks_IV_double_gas_bis,
                                 structural_shocks_IV_gen, structural_shocks_IV_spot) )
)
colnames(data_IV) <- c( "demand_double_gas_bis", "supply_double_gas_bis", "demand", "supply")
dates_formatted <- as.yearmon(time(x))
dates_formatted <- dates_formatted[(p+1):length(dates_formatted)]
ggplot()+
  geom_line(aes(x = dates_formatted, y = data_IV$demand_double_gas_bis/10, color = "Demand", linetype = "Double"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply_double_gas_bis, color = "Supply", linetype = "Double")) +
  geom_line(aes(x = dates_formatted, y = data_IV$demand, color = "Demand", linetype = "Baseline"))+
  geom_line(aes(x = dates_formatted, y = data_IV$supply/10, color = "Supply", linetype = "Baseline")) +
  theme_minimal()+
  scale_y_continuous(
    name = "Demand Shock",
    sec.axis = sec_axis(~ . * 10, name = "Supply Shock")
  ) +
  labs(title = "Structural Shocks from IV Analysis",
       x = "Date",
       y = "Shock Value")

summary_stats <- data.frame(
  mean = colMeans(data_IV, na.rm = TRUE),
  sd = apply(data_IV, 2, sd, na.rm = TRUE)
)

data_IV %>% 
  cor()



lambda_0_iv_double_gas_bis <- lambda_0_iv_double_gas[,,c(2,1)]
lambda_0_iv_double_gas_bis[,,2] <- -lambda_0_iv_double_gas_bis[,,2]
apply(lambda_0_iv_double_gas_bis , c(2, 3), function(x) cumsum(x))


#question sur la maniere de normaliser les chocs
#a priori ce sont juste des erreurs heteroskedastique independantes
library(data.table)
historical_decomp_iv_tibo_double_gas_bis <-array(NA, dim = c(n_obs,  n_vars, k))

# lambda_0_iv <- copy(lambda_0)
# 
# lambda_0_iv[,,1] <- lambda_0_iv[,,1] * irf_model$irf[[1]][1,1]
# lambda_0_iv[,,2] <- lambda_0_iv[,,2] * irf_model$irf[[2]][1,2]

k<-2
#j is the variable of interest
for (j in c(1:n_vars)){
  for (shock in c(1:k)){
    for (t in c(1:T)){
      contribution <-0
      for (hori in c(1:irhor)){
        # ne doit pas dépasser l'horizon pour lequel on calcule
        # restriction pour les premiers chocs où l'on a pas les 20 précédents
        #pn prend juste ceux qui sont disponibles
        shock_horizon = max(0,t-hori+1)
        contrib_hori <- lambda_0_iv_double_gas_bis[hori,j, shock]*structural_shocks_IV_double_gas_bis[shock,shock_horizon]
        if (!length(as.numeric(contrib_hori))==0){
          contribution <- contribution + contrib_hori
          
        }
      }
      historical_decomp_iv_tibo_double_gas_bis[t,j, shock] <- contribution
    }
  }
  
}


historical_decomp_iv_tibo_double_gas_bis
apply(historical_decomp_iv_tibo_double_gas_bis, c(2,3), function(x) cumsum(x))

historical_decomp_iv_tibo_double_gas_bis <- historical_decomp_iv_tibo_double_gas_bis[, , c(2, 1)]

# historical_decomp_extended <- array(NA, dim = c(n_obs,  n_vars, (n_vars +1)))
# historical_decomp_extended[,,1:n_vars] <- hd_cholesky
# historical_decomp_extended[,,(n_vars +1)] <- hd_controls

save_rds_path_hd <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/plot_irfs_rds/diff/hd"
setwd(save_rds_path_hd)
saveRDS(historical_decomp_iv_tibo_double_gas_bis, "hd_iv_4v_double_gas_bis.rds")
hd_cholesky_double_gas_bis <- readRDS( "hd_chol_4v.rds")


# hd_pelec <- plot_hd(historical_decomp_iv_tibo_double_gas_bis, series = 1)
# print(hd_pelec)





#historical_decomp_iv_tibo_double_gas_bis <- historical_decomp_iv_tibo_double_gas_bis[, , c(2, 1)]
# Suppose:
# hd_array: array of dim (T, n_var, n_shocks) -> historical decomposition
# var_idx: index of the variable (e.g., 2 for "spot_price")
# shock_idx: index of the shock of interest

# Example: get contribution of electricity price shocks to inflation

# Compute contributions for 2021 Q2 to 2022 Q4
contribution_data_double_gas_bis <- compute_contributions(
  start_date = "2021-04-01",
  end_date = "2022-12-01",
  dates_formatted = dates_formatted,
  IV_data_log_diff_esp = IV_data_log_diff_esp,
  hd_cholesky = hd_cholesky_double_gas_bis,
  historical_decomp_iv_tibo = historical_decomp_iv_tibo_double_gas_bis,
  series = 4
)

contribution_table_wide_double_gas_bis <- contribution_data_double_gas_bis %>%
  tidyr::pivot_wider(
    names_from = Contribution_Type,
    values_from = Contribution
  )

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output/contribution")
write_csv(contribution_table_wide_double_gas_bis, "contribution_table_wide_double_gas_bis.csv")




gt_table <- contribution_table_wide_double_gas_bis %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md("**Cumulative Contribution to Inflation**"),
    subtitle = gt::md("from 2021 Q3 to 2022 Q4 (in %)")
  ) %>%
  gt::cols_label(
    Identification = "Identification",
    `$P_{elec}$` = gt::md("$P_{elec}$"),
    `$Q_{elec}$` = gt::md("$Q_{elec}$")
  ) %>%
  gt::fmt_number(
    columns = c(`$P_{elec}$`, `$Q_{elec}$`),
    decimals = 2
  )

gt_table







