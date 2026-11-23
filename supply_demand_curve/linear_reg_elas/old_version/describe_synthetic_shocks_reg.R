
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_esp_delta <- read_parquet( "price_indispo_merge_esp_delta_supply_demand_exo.parquet")

cols_supply <- colnames(price_indispo_merge_esp_delta)[grepl("delta_s", colnames(price_indispo_merge_esp_delta))]
cols_demand <- colnames(price_indispo_merge_esp_delta)[grepl("delta_d", colnames(price_indispo_merge_esp_delta))]

# Group by month and sum shocks
monthly_shocks_supply <- price_indispo_merge_esp_delta %>%
  group_by(year, month) %>%
  summarise(across(all_of(cols_supply), mean, na.rm = TRUE),
            datetime = first(datetime)
  ) %>%
  ungroup()
# Reshape to long format
monthly_shocks_long_supply  <- monthly_shocks_supply %>%
  pivot_longer(
    cols = cols_supply,      # chaque colonne delta_s_*
    names_to = "Instrument",
    values_to = "Value"
  )

ggplot(monthly_shocks_long_supply , aes(x = datetime, y = Value, color = Instrument)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Monthly Supply Shock",
    title = "Monthly Aggregated Supply Shocks by Instrument"
  ) +
  theme_minimal()


# Group by month and sum shocks
monthly_shocks_demand <- price_indispo_merge_esp_delta %>%
  group_by(year, month) %>%
  summarise(across(all_of(cols_demand), mean, na.rm = TRUE),
            datetime = first(datetime)
  ) %>%
  ungroup()
# Reshape to long format
monthly_shocks_long_demand  <- monthly_shocks_demand %>%
  pivot_longer(
    cols = cols_demand,      # chaque colonne delta_s_*
    names_to = "Instrument",
    values_to = "Value"
  )

ggplot(monthly_shocks_long_demand , aes(x = datetime, y = Value, color = Instrument)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Monthly Demand Shock",
    title = "Monthly Aggregated Supply Shocks by Instrument"
  ) +
  theme_minimal()









# Construire les noms de colonnes

#sapply(as.data.frame(monthly_shocks_supply), function(x) class(x))




instruments_tot <- c("Fore_Wind",
                     "Fore_Solar",
                     "Published.Predicted.Unavailable.Capacity",
                     "cbf_es",
                     "cold_excess_demand",
                     "heat_excess_demand")
instru <- c("Wind"  ,"Solar" ,                             
                    "Unavailabilities", "CBF"   ,   "Temperature",                            
                     "Temperature" )

factors_vars <- c("year", "month", "weekday", "creneau")

# Pour l'offre

# mapping raw variable names -> nice labels
var_labels <- setNames(instru, instruments_tot)

synthetic_var_data <- my_results_df %>% 
  filter(Matching == "unmatched") %>% 
  dplyr::select(contrib, Type, variable) %>%
  mutate(variable_group = case_when(
    variable %in% factors_vars ~ "Time FE",       # group all FE
    variable %in% instruments_tot ~ var_labels[variable],                      # nice labels
    TRUE ~ variable                                                            # keep any other variable as is
  ))
synthetic_var_data_grouped <- synthetic_var_data %>%
  group_by(variable_group, Type) %>%
  summarise(contrib = sum(contrib, na.rm = TRUE)) %>%
  ungroup()

synthetic_plot <- ggplot(synthetic_var_data_grouped , aes(x = variable_group, y = contrib, fill = Type)) +
  geom_col(position = position_dodge(width = 0.4), color = "black", width = 0.35) +
  scale_fill_manual(values = c("supply" = "steelblue", "demand" = "tomato")) +
  labs(y = "Variance Contribution")+
  theme(
    #legend.key.size = unit(1.5, "cm") , 
    legend.text = element_text(size = 15),
    axis.text.x = element_text(size = 14, angle = 30, hjust =1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),   # taille du titre axe y
    axis.text.y  = element_text(size = 12) )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
synthetic_plot

save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/data_description'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("describe_synthetic_shocks_unmatched_FE.png", plot = synthetic_plot, width = 12, height = 6, dpi = 300)





