
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_esp_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo_ortho.parquet")


cols_supply <- colnames(price_indispo_merge_esp_delta)[grepl("delta_s_ortho", colnames(price_indispo_merge_esp_delta))]
cols_demand <- colnames(price_indispo_merge_esp_delta)[grepl("delta_d_ortho", colnames(price_indispo_merge_esp_delta))]

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
    y = "Monthly Supply Shock",
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
contrib_supply <- contrib_variance(as.data.frame(monthly_shocks_supply), cols_supply, "delta_s_ortho_total")

# Contributions à la variance de la demande
contrib_demand <- contrib_variance(as.data.frame(monthly_shocks_demand), cols_demand, "delta_d_ortho_total")


# Construire un data.frame tidy
contrib_df <- data.frame(
  Instrument = names(contrib_supply),
  Contribution = contrib_supply
) %>%
  mutate(
    Type = ifelse(Contribution >= 0, "Positive", "Negative")
  )

# Barplot des contributions
ggplot(contrib_df, aes(x = reorder(Instrument, Contribution), y = Contribution, fill = Type)) +
  geom_col() +
  coord_flip() +  # horizontale pour mieux lire les noms
  scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "tomato")) +
  labs(
    x = "Instrument",
    y = "Contribution à la variance totale",
    title = "Contributions relatives à la variance de l'offre",
    fill = "Type"
  ) +
  theme_minimal()


cols_supply <- colnames(price_indispo_merge_esp_delta)[grepl("delta_s", colnames(price_indispo_merge_esp_delta))]
cols_demand <- colnames(price_indispo_merge_esp_delta)[grepl("delta_d", colnames(price_indispo_merge_esp_delta))]
cols <- c(cols_supply, cols_demand)
cols_wind <- cols[grepl("Fore_Wind", cols)]

monthly_shocks_wind <- price_indispo_merge_esp_delta %>%
  group_by(year, month) %>%
  summarise(across(cols_wind, sum, na.rm = TRUE),
            datetime = first(datetime)
  ) %>%
  ungroup()

monthly_shocks_long_wind<- monthly_shocks_wind %>%
  pivot_longer(
    cols = cols_wind,
    names_to = "Instrument",
    values_to = "Value"
  ) %>%
  mutate(
    Type = ifelse(
      grepl("ortho", Instrument), "Ortho", "Basic"
    ),
    Instrument = case_when(
      grepl("^delta_s", Instrument) ~ "Supply",
      grepl("^delta_d", Instrument) ~ "Demand",
      TRUE ~ "other"
    )
    
    
  )

ggplot(monthly_shocks_long_wind , aes(x = datetime, y = Value, color = Instrument, linetype = Type)) +
  geom_line() +
  labs(
    x = "Month",
    y = "Monthly Supply Shock",
    title = "Monthly Aggregated Supply Shocks by Instrument"
  ) +
  theme_minimal()








