
library(fixest)
library(dplyr)




############# By time period

colnames(local_slopes_demand)

########## By season

slopes_demand_by_month <- local_slopes_demand%>%
  mutate(month = month(DATE)) %>% 
  group_by(month) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )
slopes_supply_by_month <- local_slopes_supply%>%
  mutate(month = month(DATE)) %>%
  group_by(month) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )

plot_hetero_by_month <- ggplot() +
  geom_point(data = slopes_demand_by_month, aes(x = month, y = mean_slope, color = "Demand"), size = 2) +
  geom_errorbar(data = slopes_demand_by_month,
                aes(x = month,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = "Demand"),
                width = 0.2, alpha = 0.4
  ) +
  geom_point(data = slopes_supply_by_month, aes(x = month, y = mean_slope, color = "Supply"), size = 2) +
  geom_errorbar(data = slopes_supply_by_month,
                aes(x = month,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = "Supply"),
                width = 0.2, alpha = 0.4
  ) +
  labs(
    x = "Month of the year",
    y = "Elasticity",
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


plot_hetero_by_month

#on ne voit pas grand chose par mois

############### By hour of the day

slopes_demand_by_hour <- local_slopes_demand%>%
  group_by(Hour) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )
slopes_supply_by_hour <- local_slopes_supply%>%
  group_by(Hour) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )

plot_hetero_by_hour <- ggplot() +
  geom_point(data = slopes_demand_by_hour, aes(x = Hour, y = mean_slope, color = "Demand"), size = 2) +
  geom_errorbar(data = slopes_demand_by_hour,
              aes(x = Hour,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = "Demand"),
              width = 0.2, alpha = 0.4
  ) +
  geom_point(data = slopes_supply_by_hour, aes(x = Hour, y = mean_slope, color = "Supply"), size = 2) +
  geom_errorbar(data = slopes_supply_by_hour,
              aes(x = Hour,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = "Supply"),
              width = 0.2, alpha = 0.4
  ) +
  labs(
    x = "Hour of the day",
    y = "Elasticity",
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


plot_hetero_by_hour


############# HEterogeneity by price


slopes_demand_by_quantile <- local_slopes_demand%>%
  mutate(
    price_quantile = ntile(mean_log_price, 100)   # 20 groupes égaux
  ) %>%
  group_by(price_quantile) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )
slopes_supply_by_quantile <- local_slopes_supply%>%
  mutate(
    price_quantile = ntile(mean_log_price, 100)   # 20 groupes égaux
  ) %>%
  group_by(price_quantile) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )

plot_hetero_by_quantile <- ggplot() +
  geom_point(data = slopes_demand_by_quantile, aes(x = mean_price, y = mean_slope, color = "Demand"), size = 2) +
  geom_ribbon(data = slopes_demand_by_quantile,
              aes(x = mean_price,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = "Demand"),
              width = 0.2, alpha = 0.4
  ) +
  geom_point(data = slopes_supply_by_quantile, aes(x = mean_price, y = mean_slope, color = "Supply"), size = 2) +
  geom_ribbon(data = slopes_supply_by_quantile,
              aes(x = mean_price,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = "Supply"),
              width = 0.2, alpha = 0.4
  ) +
  labs(
    x = "Price quantile",
    y = "Elasticity",
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


plot_hetero_by_quantile



slopes_demand_by_quantile <- local_slopes_demand%>%
  mutate(price_quantile = ntile(mean_log_price, 100) ,
         month = month(DATE),
         season = ifelse(month %in% c(12,1,2), "winter", 
                         ifelse(month %in% c(6,7,8), "summer", NA))
  ) %>% 
  filter(!is.na(season)) %>%
  group_by(price_quantile, season) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )

slopes_supply_by_quantile <- local_slopes_supply%>%
  mutate(price_quantile = ntile(mean_log_price, 100) ,
         month = month(DATE),
         season = ifelse(month %in% c(12,1,2), "winter", 
                         ifelse(month %in% c(6,7,8), "summer", NA))
  ) %>% 
  filter(!is.na(season)) %>%
  group_by(price_quantile, season) %>%
  summarise(
    mean_slope = mean(slope, na.rm = TRUE),
    mean_se = mean(se_slope, na.rm = TRUE),
    mean_price = exp(mean(mean_log_price, na.rm = TRUE)),
    n = n()
  )


custom_colors <- c(
  "Demand winter" = "darkred"	,                     # Brown
  "Demand summer" = "red",   # Gr
  "Supply winter" = "darkblue"	,                     # Brown
  "Supply summer" = "blue"
)

plot_hetero_by_quantile_season <- ggplot() +
  geom_point(data = slopes_demand_by_quantile, aes(x = mean_price, y = mean_slope, color = paste("Demand", season)), size = 2) +
  geom_ribbon(data = slopes_demand_by_quantile,
              aes(x = mean_price,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = paste("Demand", season)),
              width = 0.2, alpha = 0.4
  ) +
  geom_point(data = slopes_supply_by_quantile, aes(x = mean_price, y = mean_slope, color = paste("Supply", season)), size = 2) +
  geom_ribbon(data = slopes_supply_by_quantile,
              aes(x = mean_price,ymin = mean_slope - 1.96 * mean_se, ymax = mean_slope + 1.96 * mean_se, fill = paste("Supply", season)),
              width = 0.2, alpha = 0.4
  ) +
  labs(
    x = "Price quantile",
    y = "Elasticity",  
    fill = "Legend",
    title = ""
  ) +
  scale_color_manual(
    values = custom_colors
  )+
  scale_fill_manual(
    values = custom_colors
  )+
  theme_minimal(base_size = 14) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )+ guides(color = "none")


plot_hetero_by_quantile_season







# 
# # Ajouter des quantiles de prix
# reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local_bis %>%
#   mutate(price_group = ntile(PRICE, 100))  # 5 groupes de prix (tu peux changer)
# 
# # Modèle avec interaction
# local_supply_hetero <- feols(log(QTY) ~ i(price_group, log(PRICE +1 ), ref = 1) | Hour_Date, 
#                              data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)
# 
# 
# 
# 
# 
# reconstruct_full_curve_shift_qty_demand_matched_local_bis <- reconstruct_full_curve_shift_qty_demand_matched_local_bis %>%
#   mutate(price_group = ntile(PRICE, 100))
# 
# local_demand_hetero <- feols(log(QTY) ~ i(price_group, log(PRICE +1), ref = 1) | Hour_Date, 
#                              data = reconstruct_full_curve_shift_qty_demand_matched_local_bis)
# etable(local_supply_hetero, local_demand_hetero)
# 
# 
# library(broom)
# library(dplyr)
# library(ggplot2)
# 
# 
# coef_plot_supply_data <- broom::tidy(local_supply_hetero) %>%
#   filter(grepl("^price_group::[0-9]+:log\\(PRICE \\+ 1\\)$", term)) %>%
#   mutate(
#     quantile = as.numeric(sub("price_group::([0-9]+):.*", "\\1", term))
#   )
# 
# coef_plot_demand_data <- broom::tidy(local_demand_hetero) %>%
#   filter(grepl("^price_group::[0-9]+:log\\(PRICE \\+ 1\\)$", term)) %>%
#   mutate(
#     quantile = as.numeric(sub("price_group::([0-9]+):.*", "\\1", term))
#   )
# 
# plot_hetero <- ggplot() +
#   geom_point(data = coef_plot_supply_data, aes(x = quantile, y = estimate, color = "Supply"), size = 2) +
#   geom_ribbon(data = coef_plot_supply_data,
#     aes(x = quantile,ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, fill = "Supply"),
#     width = 0.2, alpha = 0.4
#   ) +
#   geom_point(data = coef_plot_demand_data, aes(x = quantile, y = estimate, color = "Demand"), size = 2) +
#   geom_ribbon(data = coef_plot_demand_data,
#                 aes(x = quantile,ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error, fill = "Demand"),
#                 width = 0.2, alpha = 0.4
#   ) +
#   labs(
#     x = "Price quantile",
#     y = "Elasticity",
#     title = ""
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(legend.text = element_text(size = 15),      # taille du texte
#         legend.title = element_text(size = 16),     # taille du titre
#         legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
#         axis.title.x = element_text(size = 14),   # taille du titre axe x
#         axis.title.y = element_text(size = 14),   # taille du titre axe y
#         axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
#         axis.text.y  = element_text(size = 12)    # taille des ticks y
#   )+ guides(color = "none")
# 
# 
# plot_hetero
# 
# 
# 
# save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
# setwd(save_plot_path)
# # Assume your ggplot object is called 'p'
# ggsave("elasticity_price_heterogeneity_linear.png", plot = plot_hetero, width = 12, height = 6, dpi = 300)
# 


