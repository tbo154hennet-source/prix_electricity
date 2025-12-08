library(fixest)
library(dplyr)

# Ajouter des quantiles de prix
reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local_bis %>%
  mutate(price_group = ntile(PRICE, 100))  # 5 groupes de prix (tu peux changer)

# Modèle avec interaction
local_supply_hetero <- feols(QTY ~ i(price_group, PRICE, ref = 1) | Hour_Date, 
                             data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)





reconstruct_full_curve_shift_qty_demand_matched_local_bis <- reconstruct_full_curve_shift_qty_demand_matched_local_bis %>%
  mutate(price_group = ntile(PRICE, 100))

local_demand_hetero <- feols(QTY ~ i(price_group, PRICE, ref = 1) | Hour_Date, 
                             data = reconstruct_full_curve_shift_qty_demand_matched_local_bis)
etable(local_supply_hetero, local_demand_hetero)


library(broom)
library(dplyr)
library(ggplot2)

coef_plot_supply_data <- broom::tidy(local_supply_hetero) %>%
  filter(grepl("price_group::[0-9]+:PRICE", term)) %>%
  mutate(
    quantile = as.numeric(gsub("price_group::([0-9]+):PRICE", "\\1", term))
  )

coef_plot_demand_data <- broom::tidy(local_demand_hetero) %>%
  filter(grepl("price_group::[0-9]+:PRICE", term)) %>%
  mutate(
    quantile = as.numeric(gsub("price_group::([0-9]+):PRICE", "\\1", term))
  )

plot_hetero <- ggplot() +
  geom_point(data = coef_plot_supply_data, aes(x = quantile, y = estimate, color = "Supply"), size = 2) +
  geom_ribbon(data = coef_plot_supply_data,
    aes(x = quantile,ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, fill = "Supply"),
    width = 0.2
  ) +
  geom_point(data = coef_plot_demand_data, aes(x = quantile, y = estimate, color = "Demand"), size = 2) +
  geom_ribbon(data = coef_plot_demand_data,
                aes(x = quantile,ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, fill = "Demand"),
                width = 0.2
  ) +
  labs(
    x = "Price quantile",
    y = "Linear price response (€/MWh)",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )+ guides(color = "none")


plot_hetero



save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("elasticity_price_heterogeneity_linear.png", plot = plot_hetero, width = 12, height = 6, dpi = 300)



