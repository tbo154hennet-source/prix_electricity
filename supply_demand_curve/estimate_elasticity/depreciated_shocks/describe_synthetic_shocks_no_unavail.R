
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_esp_delta <- read_parquet( "price_indispo_merge_esp_delta_supply_demand_exo_no_unavail.parquet")


cols_supply <- colnames(price_indispo_merge_esp_delta)[grepl("delta_s", colnames(price_indispo_merge_esp_delta))]
cols_demand <- colnames(price_indispo_merge_esp_delta)[grepl("delta_d", colnames(price_indispo_merge_esp_delta))]

# Group by month and sum shocks
monthly_shocks_supply <- price_indispo_merge_esp_delta %>%
  group_by(year, month) %>%
  summarise(across(all_of(cols_supply), sum, na.rm = TRUE),
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
  summarise(across(all_of(cols_demand), sum, na.rm = TRUE),
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


contrib_variance <- function(df, cols, total_col) {
  V_total <- var(df[[total_col]], na.rm = TRUE)
  contribs <- sapply(cols, function(c) {
    cov(df[[c]], df[[total_col]], use = "complete.obs") / V_total
  })
  return(contribs)
}


# Construire les noms de colonnes

#sapply(as.data.frame(monthly_shocks_supply), function(x) class(x))


# Contributions à la variance de l'offre
contrib_supply <- contrib_variance(as.data.frame(monthly_shocks_supply), cols_supply, "delta_s_total")

# Contributions à la variance de la demande
contrib_demand <- contrib_variance(as.data.frame(monthly_shocks_demand), cols_demand, "delta_d_total")


cols_supply <- colnames(price_indispo_merge_esp_delta)[grepl("delta_s", colnames(price_indispo_merge_esp_delta))]
cols_demand <- colnames(price_indispo_merge_esp_delta)[grepl("delta_d", colnames(price_indispo_merge_esp_delta))]
cols <- c(cols_supply, cols_demand)

monthly_shocks<- price_indispo_merge_esp_delta %>%
  group_by(year, month) %>%
  summarise(across(cols, sum, na.rm = TRUE),
            datetime = first(datetime)
  ) %>%
  ungroup()
colnames(monthly_shocks)

monthly_shocks <- monthly_shocks %>% 
  mutate(delta_d_temp = delta_d_heat_excess + delta_d_cold_excess,
         delta_s_temp = delta_s_heat_excess + delta_s_cold_excess
  ) %>% 
  select(-delta_d_heat_excess, -delta_d_cold_excess,
         -delta_s_heat_excess, -delta_s_cold_excess)


cols_supply <- colnames(monthly_shocks)[grepl("delta_s", colnames(monthly_shocks))]
cols_demand <- colnames(monthly_shocks)[grepl("delta_d", colnames(monthly_shocks))]


contrib_variance_cond <- function(df, cols, total_col, control_col) {
  # Total et contrôle
  Y <- df[[total_col]]
  Z <- df[[control_col]]
  
  # Variance totale conditionnelle
  V_total_cond <- var(lm(Y ~ Z)$residuals, na.rm = TRUE)
  
  # Contribution conditionnelle
  contribs <- sapply(cols, function(c) {
    X <- df[[c]]
    # résidus de X après avoir enlevé l'effet de Z
    X_resid <- lm(X ~ Z)$residuals
    cov(X_resid, lm(Y ~ Z)$residuals, use = "complete.obs") / V_total_cond
  })
  # Supprimer la colonne total_col du résultat
  contribs <- contribs[names(contribs) != total_col]
  
  return(contribs)
}

# Contribution de l'offre conditionnelle sur la demande
contrib_supply_cond <- contrib_variance_cond(
  df = as.data.frame(monthly_shocks),
  cols_supply,
  total_col = "delta_s_total",
  control_col = "delta_d_total"
)

# Contribution de la demande conditionnelle sur l'offre
contrib_demand_cond <- contrib_variance_cond(
  as.data.frame(monthly_shocks),
  cols_demand,
  total_col = "delta_d_total",
  control_col = "delta_s_total"
)


instruments_tot <- c("Wind"  ,"Solar" , "CBF"   ,                               
                     "Temperature" )
# Pour l'offre



# Construire le dataframe combiné
contrib_combined_df <- bind_rows(
  data.frame(Instrument = instruments_tot, Contribution = contrib_supply_cond, Market = "Supply"),
  data.frame(Instrument = instruments_tot, Contribution = contrib_demand_cond, Market = "Demand")
) %>%
  mutate(
    Type = ifelse(Contribution >= 0, "Positive", "Negative"),
    Instrument = factor(Instrument, levels = unique(Instrument)) # fixe l'ordre
  )

synthetic_plot <- ggplot(contrib_combined_df, aes(x = Instrument, y = Contribution, fill = Market)) +
  geom_col(position = position_dodge(width = 0.4), color = "black", width = 0.35) +
  scale_fill_manual(values = c("Supply" = "steelblue", "Demand" = "tomato")) +
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
ggsave("describe_synthetic_shocks_no_unavail.png", plot = synthetic_plot, width = 12, height = 6, dpi = 300)



variation_matrix <- monthly_shocks %>% 
  dplyr::select(-year, -month, -datetime) %>% 
  cov(use = "complete.obs")


# 1?????? Transformer la matrice en dataframe tidy
var_df <- as.data.frame(variation_matrix) %>%
  rownames_to_column(var = "Instrument") %>%
  select(Instrument) %>%
  mutate(
    Contribution = diag(variation_matrix),  # diagonale = variance de chaque variable
    Market = ifelse(grepl("^delta_s_", Instrument), "Supply", "Demand"),
  )%>%
  mutate(Contribution_bis = ifelse(Market == "Demand", Contribution/variation_matrix["delta_d_total", "delta_d_total"] ,Contribution /variation_matrix["delta_s_total", "delta_s_total"]),
         Contribution = ifelse(Market == "Demand", Contribution/10**9 ,Contribution /10**9),
         Contribution_scaled = ifelse(Market == "Demand", Contribution * 10, Contribution)) %>% 
  arrange(Instrument)


# 1?????? Renommer les instruments
var_df$Instrument <- rep(c("Solar", "Wind",  "CBF", "Temperature", "Synthetic"), 2)

# 2?????? Fixer l'ordre comme facteur pour ggplot
instrument_order <- c("Solar", "Wind", "Unavailabilities", "CBF", "Temperature", "Synthetic")
var_df$Instrument <- factor(var_df$Instrument, levels = instrument_order)



# 3?????? Histogramme vertical, barres côte à côte
ggplot(var_df, aes(x = Instrument, y = Contribution, fill = Market)) +
  geom_col(position = position_dodge(width = 0.5), color = "black", width = 0.4) +
  scale_fill_manual(values = c("Supply" = "steelblue", "Demand" = "tomato")) +
  # scale_y_continuous(
  #   name = "Variance Supply",
  #   sec.axis = sec_axis(~ . /10, name = "Variance Demand")  # remettre l'échelle originale pour Demand
  # ) +
  labs(
    y = "Variance (GWh)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 
# save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/data_description'
# setwd(save_plot_path)
# # Assume your ggplot object is called 'p'
# ggsave("describe_synthetic_shocks_no_normalize_no_unavail.png", plot = synthetic_plot, width = 12, height = 6, dpi = 300)
# 
