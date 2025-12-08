
sample_n_data_supply_sinh_left <- left_points %>%
  tail(1000) %>% 
  mutate(asinh_price = asinh(PRICE),
         asinh_price_add = asinh(PRICE + mean_price),
         Hour_Date_bis = paste0(as.character(Hour_Date), "_new"))

sample_n_data_supply_sinh_right <- right_points %>%
  tail(1000) %>% 
  mutate(asinh_price = asinh(PRICE),
         asinh_price_add = asinh(PRICE + mean_price),
         Hour_Date_bis = paste0(as.character(Hour_Date), "_new"))


sample_n_data_supply_left <- left_points %>%
  tail(1000) %>% 
  mutate(log_price = log(PRICE+1),
         log_price_add = log(PRICE + mean_price))

sample_n_data_supply_right <- right_points %>%
  tail(1000) %>% 
  mutate(log_price = log(PRICE+1),
         log_price_add = log(PRICE + mean_price))


my_plot <- ggplot() +
  # --- Sinh : rouge ---
  geom_point(
    data = sample_n_data_supply_sinh_left,
    aes(x = asinh_price, y = asinh(QTY), color = "Sinh Left", shape = "Sinh Left"),
    alpha = 0.6, size = 1
  ) +
  geom_smooth(
    data = sample_n_data_supply_sinh_left,
    aes(x = asinh_price, y = asinh(QTY), group = Hour_Date_bis),
    method = "lm", se = FALSE, linewidth = 0.3, color = "red"
  ) +
  geom_point(
    data = sample_n_data_supply_sinh_right,
    aes(x = asinh_price, y = asinh(QTY), color = "Sinh Right", shape = "Sinh Right"),
    alpha = 0.6, size = 1
  ) +
  geom_smooth(
    data = sample_n_data_supply_sinh_right,
    aes(x = asinh_price, y = asinh(QTY), group = Hour_Date_bis),
    method = "lm", se = FALSE, linewidth = 0.3, color = "darkred"
  ) +
  
  # --- Log : bleu ---
  geom_point(
    data = sample_n_data_supply_left,
    aes(x = log_price, y = log(QTY), color = "Log Left", shape = "Log Left"),
    alpha = 0.6, size = 1
  ) +
  geom_smooth(
    data = sample_n_data_supply_left,
    aes(x = log_price, y = log(QTY), group = Hour_Date),
    method = "lm", se = FALSE, linewidth = 0.3, color = "blue"
  ) +
  geom_point(
    data = sample_n_data_supply_right,
    aes(x = log_price, y = log(QTY), color = "Log Right", shape = "Log Right"),
    alpha = 0.6, size = 1
  ) +
  geom_smooth(
    data = sample_n_data_supply_right,
    aes(x = log_price, y = log(QTY), group = Hour_Date),
    method = "lm", se = FALSE, linewidth = 0.3, color = "darkblue"
  ) +
  
  # --- Scales fixes ---
  scale_color_manual(
    name = "Group",
    values = c(
      "Sinh Left"  = "red",
      "Sinh Right" = "darkred",
      "Log Left"   = "blue",
      "Log Right"  = "darkblue"
    )
  ) +
  scale_shape_manual(
    name = "Group",
    values = c(
      "Sinh Left"  = 1,   # cercle vide
      "Sinh Right" = 16,  # cercle plein
      "Log Left"   = 1,
      "Log Right"  = 16
    )
  ) +
  
  # --- Thème et titres ---
  theme_minimal() +
  labs(
    x = "PRICE",
    y = "QTY",
    title = "Supply Curve Points with Hourly Linear Fits",
    subtitle = "Circle empty = Left, Circle filled = Right"
  )

my_plot


