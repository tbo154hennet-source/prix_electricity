library(dplyr)

price_merge_CBF_max <- arrow::read_parquet(
  file.path(
    Sys.getenv("REPLICATION_ROOT", "."),
    "day_ahead/outputs/cross_border/price_merge_CBF_max_out_of_sample_lin.parquet"
  )
)
#colnames(import_data_reg)
price_merge_CBF_max_monthly <- price_merge_CBF_max %>% 
  mutate(year = as.numeric(year), month = as.numeric(month)) %>%
  group_by(year, month) %>% 
  summarise(
    CBF_ES = mean(CBF_ES, na.rm = TRUE),
    CBF_ES_pred = mean(CBF_ES_pred, na.rm = TRUE),
    CBF_ES_pred_sat = mean(CBF_ES_pred_sat, na.rm = TRUE),
    CBF_FR = mean(CBF_FR, na.rm = TRUE),
    CBF_FR_pred = mean(CBF_FR_pred, na.rm = TRUE),
    CBF_FR_pred_sat = mean(CBF_FR_pred_sat, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarise
  ) %>%
  mutate(Date = as.Date(paste0(year, "-", month, "-01")))


library(ggplot2)

out_of_sample_pred <- ggplot(price_merge_CBF_max_monthly, aes(x = Date)) +
  geom_line(aes(y = CBF_ES, color = "Actual", group = "Actual")) +
  geom_line(aes(y = CBF_ES_pred, color = "Predicted", group = "Predicted")) +
  geom_line(aes(y = CBF_ES_pred_sat, color = "Saturated Predicted", group  = "Saturated Predicted" )) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dashed", color = "purple")+
  labs(#title = "Monthly Average CBF vs Predictions",
       x = "Date", y = "CBF")+ 
  theme_minimal()
out_of_sample_pred

out_of_sample_pred_countries <- ggplot(price_merge_CBF_max_monthly, aes(x = Date)) +
  geom_line(aes(y = CBF_ES, color = "ES", linetype = "Actual")) +
  geom_line(aes(y = CBF_ES_pred, color = "ES", linetype = "Predicted")) +
  geom_line(aes(y = CBF_FR, color ="FR" , linetype ="Actual" )) +
  geom_line(aes(y = CBF_FR_pred, color = "FR", linetype = "Predicted" )) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = "dashed", color = "purple")+
  labs(#title = "Monthly Average CBF vs Predictions",
       x = "Date", y = "CBF")+ 
  theme_minimal()
out_of_sample_pred_countries

save_plot_path <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "data_description")
setwd(save_plot_path)

# Assume your ggplot object is called 'p'
#ggsave("qty_shock_spain_mp.png", plot = irf_gen_shock_esp, width = 8, height = 6, dpi = 300)
ggsave("out_of_sample_CBF_prediction.png", plot = out_of_sample_pred, width =12, height = 6, dpi = 300)
ggsave("predicting_CBF_tot.png", plot = out_of_sample_pred_countries, width = 12, height = 6, dpi = 300)




