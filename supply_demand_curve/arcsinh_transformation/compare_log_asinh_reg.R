library(ggplot2)
library(dplyr)

mean_price <- mean(price_indispo_merge_esp$price, na.rm = TRUE)
min_price <- min(reconstruct_full_curve_shift_qty_supply_matched_local_bis$PRICE)


sample_n_data <- reconstruct_full_curve_shift_qty_supply_matched_local_bis %>%
  tail(1000) %>% 
  mutate(log_price = log(PRICE+1),
         log_price_add = log(PRICE + min_price),
         asinh_price = asinh(PRICE))
dim(sample_n_data)
View(sample_n_data %>% 
       filter(PRICE <= 5) %>% 
       select(PRICE, log_price, log_price_add, asinh_price))

my_plot <- ggplot(sample_n_data, aes(x = log_price_add, y = log(QTY), color = factor(Hour_Date))) +
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

my_plot <- ggplot(sample_n_data, aes(x = asinh_price, y = log(QTY), color = factor(Hour_Date))) +
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

View(sample_n_data %>% 
  filter(log_price_add <= 4.5))

save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/post_seminaire'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("supply_elas_pb.png", plot = my_plot, width = 12, height = 6, dpi = 300)



#le problème d'utiliser log(PRICE +1)
# c'est que j'obtiens des passages d'ordre de grandeur énormes pour des prix bas


local_supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)

# reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour_Date <- 
#   reconstruct_full_curve_shift_qty_demand_matched_local_bis$DATE + reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour


local_supply_FE_bis <- feols(log(QTY) ~ log(PRICE + 1) | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)

#toujours le problème qui correspond au passage de zéro qui fait appraître
#des élasticités très positives pour éviter de passer ce cap


local_supply_FE_ter <- feols(log(QTY) ~ log(PRICE - min_price +1) | Hour_Date, 
                             data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)


local_supply_FE_quat <- feols(log(QTY) ~ log(PRICE) | Hour_Date, 
                             data = reconstruct_full_curve_shift_qty_supply_matched_local_bis %>% 
                               filter(PRICE >10))



etable(local_supply_FE, 
       local_supply_FE_bis, 
       local_supply_FE_ter,
       local_supply_FE_quat
       )

local_supply_asinh_FE <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)

# reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour_Date <- 
#   reconstruct_full_curve_shift_qty_demand_matched_local_bis$DATE + reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour


local_supply_asinh_FE_bis <- feols(asinh(QTY) ~ asinh(PRICE + 1) | Hour_Date, 
                             data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)

#toujours le problème qui correspond au passage de zéro qui fait appraître
#des élasticités très positives pour éviter de passer ce cap


local_supply_asinh_FE_ter <- feols(asinh(QTY) ~ asinh(PRICE - min_price +1) | Hour_Date, 
                             data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)

local_supply_asinh_FE_quat <- feols(asinh(QTY) ~ asinh(PRICE) | Hour_Date, 
                              data = reconstruct_full_curve_shift_qty_supply_matched_local_bis %>% 
                                filter(PRICE >10))

etable(local_supply_asinh_FE, 
       local_supply_asinh_FE_bis, 
       local_supply_asinh_FE_ter,
       local_supply_asinh_FE_quat)




local_supply <- feols(QTY ~ PRICE | Hour_Date, 
                               data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)

# reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour_Date <- 
#   reconstruct_full_curve_shift_qty_demand_matched_local_bis$DATE + reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour


local_supply_bis <- feols(QTY ~ PRICE | Hour_Date, 
                          data = reconstruct_full_curve_shift_qty_supply_matched_local_bis %>% 
                            filter(PRICE >0))

#toujours le problème qui correspond au passage de zéro qui fait appraître
#des élasticités très positives pour éviter de passer ce cap


local_supply_ter  <- feols(QTY ~ PRICE | Hour_Date, 
                                              data = reconstruct_full_curve_shift_qty_supply_matched_local_bis %>% 
                                                filter(PRICE > -1))

local_supply_quat  <- feols(QTY ~ PRICE | Hour_Date, 
                           data = reconstruct_full_curve_shift_qty_supply_matched_local_bis %>% 
                             filter(PRICE > 10))


etable(local_supply, 
       local_supply_bis, 
       local_supply_ter,
       local_supply_quat)
# Create LaTeX table output
# etable_output <- etable(
#   local_supply_FE, local_demand_FE,
#   local_supply_FE_bis, local_demand_FE_bis,
#   tex = TRUE,           # generate LaTeX
#   fitstat = ~ n + r2,   # optional: number of obs and R²
#   title = "Fixed Effects Estimations for Supply and Demand Curves"
# )
# save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/post_seminaire'
# setwd(save_plot_path)
# # Save as .tex file
# writeLines(etable_output, "etable_results.tex")


# Fonction d’extraction directe pour un modèle feols
extract_feols <- function(model, name) {
  data.frame(
    Formula = name,
    estimate = coef(model)[1],
    std.error = se(model)[1]
  )
}

# Extraire coefficients log
coef_log <- bind_rows(
  extract_feols(local_supply_FE, "log(PRICE)"),
  extract_feols(local_supply_FE_bis, "log(PRICE + 1)"),
  extract_feols(local_supply_FE_ter, "log(PRICE - min_price + 1)"),
  extract_feols(local_supply_FE_quat, "log(PRICE)")
)

# Extraire coefficients asinh
coef_asinh <- bind_rows(
  extract_feols(local_supply_asinh_FE, "asinh(PRICE)"),
  extract_feols(local_supply_asinh_FE_bis, "asinh(PRICE + 1)"),
  extract_feols(local_supply_asinh_FE_ter, "asinh(PRICE - min_price + 1)"),
  extract_feols(local_supply_asinh_FE_quat, "asinh(PRICE)")
)

coef_reg <- bind_rows(
  extract_feols(local_supply, "PRICE"),
  extract_feols(local_supply_bis, "PRICE"),
  extract_feols(local_supply_ter, "PRICE"),
  extract_feols(local_supply_quat, "PRICE")
)

# Fusion des deux
coef_compare <- rbind(
  coef_log %>% rename(Estimate = estimate, SE = std.error) %>% 
    mutate(Type = "Log"),
  coef_asinh %>% rename(Estimate = estimate, SE = std.error)%>% 
    mutate(Type = "Asinh"),
  coef_reg %>% rename(Estimate = estimate, SE = std.error)%>% 
    mutate(Type = "Linear")
)
coef_compare$Model <- rep(c("PRICE", "PRICE + 1", "PRICE - min + 1", "PRICE >10"),3 )
coef_compare <- coef_compare %>%
  mutate(
    Estimate = as.numeric(Estimate),
    SE = as.numeric(SE),
    Elasticity = sprintf("%.3f (%.3f)", Estimate, SE)
  ) %>%
  dplyr::select(Model, Elasticity, Type)

# Pivot en wide format
coef_wide <- coef_compare %>%
  pivot_wider(
    names_from = Type,
    values_from = Elasticity
  ) %>%
  rename(
    eta_log = Log,
    eta_asinh = Asinh,
    eta_linear = Linear
  )

# Table GT avec headers LaTeX
gt_table <- coef_wide %>%
  gt() %>%
  tab_header(
    title = "Estimated Elasticities"
  ) %>%
  cols_label(
    Model = "Price Specification",
    eta_log = "$\\eta_{log}$",
    eta_asinh = "$\\eta_{asinh}$",
    eta_linear = "$\\eta_{lin}$"
  ) %>%
  fmt_markdown(columns = everything()) %>%
  opt_table_outline()%>%
  # Ajouter footnote pour min = -5,6 et euro
  tab_footnote(
    footnote = md(paste0("`min` corresponds to the minimum observed price: ",as.character(round(min_price,3))," €"))
  )

gt_table

# Chemin où sauvegarder
save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/post_seminaire'
save_file <- file.path(save_plot_path, "log_asinh_compare.tex")

# Sauvegarder en LaTeX
gtsave(gt_table, save_file)






