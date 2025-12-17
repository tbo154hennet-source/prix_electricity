library(ggplot2)
library(dplyr)

mean_price <- mean(price_indispo_merge_esp$price, na.rm = TRUE)
min_price <- min(reconstruct_full_curve_shift_qty_supply_matched_local_bis$PRICE)


sample_n_data <- reconstruct_full_curve_shift_qty_supply_matched_local_bis %>%
  tail(1000) %>% 
  mutate(log_price = log(PRICE+1),
         log_price_add = log(PRICE + min_price),
         asinh_price = asinh(PRICE))


my_plot <- ggplot(sample_n_data, aes(x = log_price, y = log(QTY), color = factor(Hour_Date))) +
  geom_point(alpha = 0.6, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.3)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "log(PRICE)",
    y = "log(QTY)",
    title = "Supply Curve Points (Sample of 10,000) with Hourly Linear Fits"
  )
my_plot