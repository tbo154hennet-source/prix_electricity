#plot the distribution
library(dplyr)
library(purrr)
library(data.table)
library(ggplot2)

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")

local_slopes_demand_5 <- read_parquet( "local_slopes_demand_5.parquet")
local_slopes_demand_10 <- read_parquet( "local_slopes_demand_10.parquet")
local_slopes_demand_15 <- read_parquet( "local_slopes_demand_15.parquet")

local_slopes_supply_5 <- read_parquet( "local_slopes_supply_5.parquet")
local_slopes_supply_10 <- read_parquet( "local_slopes_supply_10.parquet")
local_slopes_supply_15 <- read_parquet( "local_slopes_supply_15.parquet")

# process_local_slopes <- function(df, probs = c(0.025, 0.975)) {
#   
#   df <- as.data.table(df)
#   
#   # Calcul bornes de winsorisation
#   q <- quantile(df$slope, probs = probs, na.rm = TRUE)
#   
#   # Winsorisation
#   df[, slope_w := pmin(pmax(slope, q[1]), q[2])]
#   
#   # Résumé via ta fonction existante
#   desc <- describe_local_slopes(df)
#   
#   tibble(
#     w_mean = desc$w_mean,
#     alpha  = desc$alpha,
#     w_mean_plus_alpha = desc$w_mean + desc$alpha
#   )
# }
# # Résultats
# results_demand <- lapply(all_demand, process_local_slopes)
# results_supply <- lapply(all_supply, process_local_slopes)


all_demand <- list(
  d5  = local_slopes_demand_5,
  d10 = local_slopes_demand_10,
  d15 = local_slopes_demand_15
)

all_supply <- list(
  s5  = local_slopes_supply_5,
  s10 = local_slopes_supply_10,
  s15 = local_slopes_supply_15
)


# Moyennes pondérées – demand
demand_means <- sapply(all_demand, function(x) {
  weighted.mean(x$slope, x$mean_log_price^2, na.rm = TRUE)
})

# Moyennes pondérées – supply
supply_means <- sapply(all_supply, function(x) {
  weighted.mean(x$slope, x$mean_log_price^2, na.rm = TRUE)
})

demand_se_means <- sapply(all_demand, function(x) {
  weighted.mean(x$se, x$mean_log_price^2, na.rm = TRUE)
})

supply_se_means <- sapply(all_supply, function(x) {
  weighted.mean(x$se, x$mean_log_price^2, na.rm = TRUE)
})


names(demand_means) <- sub("^d", "", names(demand_means))
names(demand_se_means) <- sub("^d", "", names(demand_se_means))
names(supply_means)    <- sub("^s", "", names(supply_means))
names(supply_se_means) <- sub("^s", "", names(supply_se_means))

library(tidyverse)

plot_df <- tibble(
  horizon = as.numeric(names(demand_means)),
  demand  = demand_means,
  demand_se = demand_se_means,
  supply  = supply_means,
  supply_se = supply_se_means
) %>%
  pivot_longer(
    cols = c(demand, supply),
    names_to = "type",
    values_to = "slope"
  ) %>%
  mutate(
    se = ifelse(type == "demand", demand_se, supply_se),
    ymin = slope - 2 * se,
    ymax = slope + 2 * se
  )



pas_robustness_check <- ggplot(plot_df, aes(x = horizon, y = slope, color = type)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    position = position_dodge(width = 0.6),
    width = 0.5,
    linewidth = 0.8
  ) +
  scale_x_continuous(breaks = c(5, 10, 15)) +
  labs(
    x = "Horizon",
    y = "Elasticity",
    color = ""
  ) +
  theme_minimal(base_size = 14)


save_latex_path <- "Z:/Overleaf_elec/images/elasticity"
setwd(save_latex_path)
ggsave(pas_robustness_check,
       filename = "pas_robustness_check.png" , width = 12, height = 6, dpi = 300)




