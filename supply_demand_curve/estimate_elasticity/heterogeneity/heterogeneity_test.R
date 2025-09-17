
instruments <- c("renewable_day_ahead_forecast", "heat_excess", "cold_excess", 
                "Published.Predicted.Unavailable.Capacity", "cbf_es", "wind_speed_100m", "Fore_Wind")

price_indispo_merge_delta_clean <- merge(price_indispo_merge_delta_clean %>% 
                                           rename(heat_excess = heat_excess_demand,
                                                  cold_excess = cold_excess_demand
                                                  ),
                                         price_indispo_merge_bronte_clean %>% select(
                                                                                     wind_speed_100m,
                                                                                     delta_s_wind_speed_100m,
                                                                                     delta_d_wind_speed_100m,
                                                                                     Fore_Wind,
                                                                                     delta_s_Fore_Wind,
                                                                                     delta_d_Fore_Wind,
                                                                                     year, month, weekday, creneau
                                                                                     
                                                                                     ) ,
                                         by = c("year"   , "month" ,   "weekday"  , "creneau"  ), all.x = TRUE)

cols_supply<- paste0("delta_s_", instruments)
cols_demand<- paste0("delta_d_", instruments)


# cols_supply<- paste0(instruments)
# cols_demand<- paste0( instruments)

results_list_supply_bis <- setNames(
  lapply(cols_supply, function(instr) {
    estimate_iv_sd(instr = instr, 
                   controls = c(controls_vars, sub("_d_", "_s_", instr)),
                   data = price_indispo_merge_delta_clean, mode = "supply")
  }),
  nm = cols_supply
)


results_list_demand_bis <- setNames(
  lapply(cols_demand, function(instr) {
    estimate_iv_sd(instr = instr, 
                   controls = c(controls_vars, sub("_s_", "_d_", instr) ),
                   data = price_indispo_merge_delta_clean, mode = "demand")
  }),
  nm = cols_demand
)

# Combine into a single list
results_list_all_bis <- c(results_list_demand_bis, results_list_supply_bis)


# Summarize results
iv_results_df_bis <- data.frame(
  Instrument = rep(instruments, each = 2),
  Mode = rep(c("Demand", "Supply"), times = length(instruments)),
  Estimate = NA,
  StdError = NA
)


for (i in seq_along(results_list_all_bis)[1:7]) {
  res_d <- extract_coef_se(model =results_list_all_bis[[i]], variable="log(gen_tot)")
  res_s <- extract_coef_se(results_list_all_bis[[i+7]], "log(price)")
  iv_results_df_bis$Estimate[(2*i - 1)] <- res_d["Estimate"]
  iv_results_df_bis$StdError[(2*i - 1)] <- res_d["StdError"]
  iv_results_df_bis$Estimate[(2*i)] <- res_s["Estimate"]
  iv_results_df_bis$StdError[(2*i)] <- res_s["StdError"]
}



# Step 2: Format string with SE in parentheses
iv_results_df_bis <- iv_results_df_bis %>%
  mutate(
    Elasticity_CI = paste0(
      round(Estimate, 3), " (", round(StdError, 3), ")"
    )
  )

# Step 3: Add Instrument and Type columns
iv_results_df_bis <- iv_results_df_bis %>%
  rename(Instrument = Instrument, Type = Mode) %>%
  mutate(
    Type = case_when(
      Type == "Demand" ~ "Demand",
      Type == "Supply" ~ "Supply"
    ),
    Instrument = case_when(
      Instrument == "renewable_day_ahead_forecast" ~ "Renewables",
      Instrument == "heat_excess" ~ "Heat",
      Instrument ==  "cold_excess" ~ "Cold",
      Instrument == "Published.Predicted.Unavailable.Capacity" ~ "Unavailability" ,
      Instrument == "cbf_es" ~ "CBF" ,
      Instrument == "wind_speed_100m" ~ "Wind Speed",
      Instrument == "Fore_Wind" ~ "Wind Forecast"
    )
  )

# Step 4: Format wide table: rows = Type, columns = Instrument
iv_results_df_bis_plot_table_wide <- iv_results_df_bis %>%
  dplyr::select(Type, Instrument, Elasticity_CI) %>%
  complete(Type, Instrument, fill = list(Elasticity_CI = "-")) %>%
  pivot_wider(names_from = Instrument, values_from = Elasticity_CI)


iv_results_df_bis_plot_table_wide

gt_table <- iv_results_df_bis_plot_table_wide %>%
  gt() %>%
  tab_header(
    title = "Instrument Heterogeneity of Supply and Demand Elasticities"
  )
gt_table















