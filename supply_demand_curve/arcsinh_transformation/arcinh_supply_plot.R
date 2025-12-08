library(dplyr)
library(fixest)

# 1) Estimer en asinh (avec FE si besoin)
fit_asinh <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, 
                   data = reconstruct_full_curve_shift_qty_supply_matched_local_bis %>% 
                     filter(!is.na(PRICE), !is.na(QTY)))

coef_beta <- coef(fit_asinh)["asinh(PRICE)"]
se_beta   <- sqrt(vcov(fit_asinh)["asinh(PRICE)", "asinh(PRICE)"])

sample_n_data_supply_sinh <- reconstruct_full_curve_shift_qty_supply_matched_local_bis %>%
  tail(1000) %>% 
  mutate(asinh_price = asinh(PRICE),
         asinh_price_add = asinh(PRICE + mean_price),
         Hour_Date_bis = paste0(as.character(Hour_Date), "_new"))

sample_n_data_supply <- reconstruct_full_curve_shift_qty_supply_matched_local_bis %>%
  tail(1000) %>% 
  mutate(log_price = log(PRICE+1),
         log_price_add = log(PRICE + mean_price))

library(ggplot2)

my_plot <- ggplot() +
  # --- Partie sinh : rouge ---
  geom_point(
    data = sample_n_data_supply_sinh,
    aes(x = asinh_price, y = asinh(QTY), color = factor(Hour_Date_bis)),
    alpha = 0.6, size = 1, show.legend = FALSE
  ) +
  geom_smooth(
    data = sample_n_data_supply_sinh,
    aes(x = asinh_price, y = asinh(QTY), group = Hour_Date_bis),
    method = "lm", se = FALSE, linewidth = 0.3, color = "red"
  ) +
  
  # --- Partie log : bleu ---
  geom_point(
    data = sample_n_data_supply,
    aes(x = log_price, y = log(QTY), color = factor(Hour_Date)),
    alpha = 0.6, size = 1, show.legend = FALSE
  ) +
  geom_smooth(
    data = sample_n_data_supply,
    aes(x = log_price, y = log(QTY), group = Hour_Date),
    method = "lm", se = FALSE, linewidth = 0.3, color = "blue"
  ) +
  
  # --- Thème et titres ---
  scale_color_manual(values = c(colorRampPalette(c("firebrick", "darkred"))(length(unique(sample_n_data_supply$Hour_Date))),
                     colorRampPalette(c("lightblue", "royalblue"))(length(unique(sample_n_data_supply_sinh$Hour_Date_bis))))
                     ) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "PRICE",
    y = "QTY",
    title = "Supply Curve Points (Sample of 10,000) with Hourly Linear Fits",
    subtitle = "Blue = asinh transformation, Red = log transformation"
  )

my_plot


# save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
# setwd(save_plot_path)
# # Assume your ggplot object is called 'p'
# ggsave("elasticity_price_heterogeneity.png", plot = my_plot, width = 12, height = 6, dpi = 300)
# 


#on veut designer pour les prix proches de zéro des notions d'élasticités à gauche et à droite


# Points à gauche (PRICE <= price)
left_points <- reconstruct_full_curve_shift_qty_supply_matched_local_bis[PRICE <= price,
                  by = .(DATE_Hour)]

# Points à droite (PRICE >= price)
right_points <- reconstruct_full_curve_shift_qty_supply_matched_local_bis[PRICE >= price,
                   by = .(DATE_Hour)]


# Optionnel : estimer séparément pour left et right
local_supply_FE_left <- feols(log(QTY + 1) ~ log(PRICE + 1) | Hour_Date, data = left_points)
local_supply_FE_right <- feols(log(QTY + 1) ~ log(PRICE + 1) | Hour_Date, data = right_points)

local_supply_asinh_FE_left <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, data =left_points)
local_supply_asinh_FE_right <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, data = right_points)

etable(local_supply_FE_left, local_supply_FE_right,
       local_supply_asinh_FE_left, local_supply_asinh_FE_right
       )

dim(left_points)

# Supposons que left_points est un data.table
left_points[, .(n = .N), by = .(price > 10)]
  
right_points[, .(n = .N), by = .(price > 10)]


local_supply_FE_left_10 <- feols(log(QTY + 1) ~ log(PRICE + 1) | Hour_Date, data = left_points %>% filter(price >10))
local_supply_FE_right_10 <- feols(log(QTY + 1) ~ log(PRICE + 1) | Hour_Date, data = right_points%>% filter(price >10))

local_supply_asinh_FE_left_10 <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, data =left_points%>% filter(price >10))
local_supply_asinh_FE_right_10 <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, data = right_points%>% filter(price >10))

etable(local_supply_FE_left_10, local_supply_FE_right_10,
       local_supply_asinh_FE_left_10, local_supply_asinh_FE_right_10
)
