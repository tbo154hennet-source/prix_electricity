# Variance des composantes des chocs synthetiques : version normalisee et logarithmique
#
# Chaque variance est rapportee a la variance du choc synthetique du meme marche.
# L'axe logarithmique permet de conserver la lisibilite des petites composantes sans
# appliquer un facteur arbitraire different a l'offre et a la demande.

library(arrow)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

project_root <- normalizePath(Sys.getenv("REPLICATION_ROOT", "."), winslash = "/", mustWork = TRUE)

data_path <- file.path(
  project_root,
  "day_ahead", "outputs", "esp",
  "price_indispo_merge_esp_delta_supply_demand_exo.parquet"
)
output_dir <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "data_description")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

shocks <- read_parquet(data_path)
shock_cols <- names(shocks)[grepl("^delta_[sd]_", names(shocks))]

monthly_shocks <- shocks %>%
  group_by(year, month) %>%
  summarise(
    across(all_of(shock_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    delta_d_temp = delta_d_heat_excess + delta_d_cold_excess,
    delta_s_temp = delta_s_heat_excess + delta_s_cold_excess
  ) %>%
  select(
    -delta_d_heat_excess, -delta_d_cold_excess,
    -delta_s_heat_excess, -delta_s_cold_excess
  )

variation_matrix <- monthly_shocks %>%
  select(-year, -month) %>%
  cov(use = "complete.obs")

variance_df <- tibble(
  variable = colnames(variation_matrix),
  variance = diag(variation_matrix)
) %>%
  filter(grepl("^delta_[sd]_", variable)) %>%
  mutate(
    Market = if_else(grepl("^delta_s_", variable), "Supply", "Demand"),
    key = tolower(sub("^delta_[sd]_", "", variable)),
    Instrument = case_when(
      grepl("solar", key) ~ "Solar",
      grepl("wind", key) ~ "Wind",
      grepl("published|unavail", key) ~ "Unavailabilities",
      grepl("cbf", key) ~ "CBF",
      grepl("temp", key) ~ "Temperature",
      key == "total" ~ "Synthetic",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Instrument)) %>%
  group_by(Market) %>%
  mutate(
    synthetic_variance = variance[Instrument == "Synthetic"],
    relative_contribution = variance / synthetic_variance
  ) %>%
  ungroup() %>%
  mutate(
    Instrument = factor(
      Instrument,
      levels = rev(c("Solar", "Wind", "Unavailabilities", "CBF", "Temperature", "Synthetic"))
    ),
    # Transformation monotone : une contribution plus grande donne toujours
    # une barre plus longue, tout en comprimant les ecarts importants.
    contribution_log = log10(1 + 100 * relative_contribution)
  )

axis_values <- c(0, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)

log_normalized_plot <- ggplot(
  variance_df,
  aes(x = contribution_log, y = Instrument, fill = Market)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.5),
    width = 0.42,
    color = "black",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = label_percent(accuracy = 0.1)(relative_contribution)),
    position = position_dodge(width = 0.5),
    hjust = -0.12,
    size = 3.5,
    show.legend = FALSE
  ) +
  geom_vline(
    xintercept = log10(1 + 100),
    color = "grey45",
    linewidth = 0.45,
    linetype = "dashed"
  ) +
  scale_fill_manual(values = c("Supply" = "steelblue", "Demand" = "tomato")) +
  scale_x_continuous(
    name = "Variance relative to the synthetic shock (log-compressed scale)",
    breaks = log10(1 + 100 * axis_values),
    labels = label_percent()(axis_values),
    expand = expansion(mult = c(0, 0.14))
  ) +
  coord_cartesian(clip = "off") +
  labs(y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  file.path(output_dir, "describe_synthetic_shocks_log_normalized.png"),
  plot = log_normalized_plot,
  width = 12,
  height = 6,
  dpi = 300
)

