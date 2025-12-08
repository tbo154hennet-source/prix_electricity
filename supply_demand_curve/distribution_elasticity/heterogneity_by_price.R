library(data.table)
library(arrow)
library(dplyr)
library(ggplot2)


#echelle de variance d?composition en niveau temporelle

library(lme4)

fit <- lmer(price ~ 1 + (1|year) + (1|month) + (1|weekday) + (1|hour),
            data = price_indispo_merge_esp)

summary(fit)
VarCorr(fit)
sd(price_indispo_merge_esp$price)

#one sd movement in price corresponds to 55.9 euros
#this variance is partly explained by non-comparable moments of the day
# 43% explained by year, 6% by months, 5% by weekday and 7% by hour
#thus taking the residual from FE model
#one sd movement in price 56*36% residuals = 20.1euros

#non repartie entre mouvements d'offre et de demande




setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
# --- Lire les fichiers ---
local_slopes_demand_5 <- read_parquet("local_slopes_demand_5.parquet")
local_slopes_demand_10 <- read_parquet("local_slopes_demand.parquet")
local_slopes_demand_15 <- read_parquet("local_slopes_demand_15.parquet")
local_slopes_demand_pond <- read_parquet("local_slopes_demand_pond.parquet")


local_slopes_demand_5 %>% 
  summarise(n_na_slope = sum(is.na(slope)))
local_slopes_demand_5 <- local_slopes_demand_5%>% 
  filter(!is.na(slope))
local_slopes_demand_pond %>% 
  summarise(n_na_slope = sum(!is.finite(slope)))

# indices des lignes o? slope n'est pas fini
bad_idx <- which(!is.finite(local_slopes_demand_pond$slope))
View(local_slopes_demand_10[bad_idx,])


  
# --- Appliquer aux diff?rents fichiers ---
my_demand_5 <- describe_local_slopes(local_slopes_demand_5)
my_demand_10 <- describe_local_slopes(local_slopes_demand_10)
my_demand_15 <- describe_local_slopes(local_slopes_demand_15)
my_demand_pond <- describe_local_slopes(local_slopes_demand_pond %>% 
                                          filter(is.finite(slope) & is.finite(se_slope)))
# my_demand_pond <- describe_local_slopes(local_slopes_demand_pond %>% 
#                                          mutate(slope = ifelse(is.na(slope), 0, slope),
#                                         se_slope = ifelse(!is.finite(se_slope), 0, se_slope)))
#                                         

local_slopes_supply_5 <- read_parquet("local_slopes_supply_5.parquet")
local_slopes_supply_10 <- read_parquet("local_slopes_supply.parquet")
local_slopes_supply_15 <- read_parquet("local_slopes_supply_15.parquet")
local_slopes_supply_pond <- read_parquet("local_slopes_supply_pond.parquet")

#weigthed mean
#pour pond?rer je droppe les NA slope qui correspondent toutes ? des petits prix donc changent beaucoup
#car pond?rer par log
local_slopes_supply_5 %>% 
  summarise(n_na_slope = sum(is.na(slope)))


local_slopes_supply_5 <- local_slopes_supply_5%>% 
  filter(!is.na(slope))

# --- Appliquer aux diff?rents fichiers ---
my_supply_5 <- describe_local_slopes(local_slopes_supply_5)
my_supply_10 <- describe_local_slopes(local_slopes_supply_10)
my_supply_15 <- describe_local_slopes(local_slopes_supply_15)
my_supply_pond <- describe_local_slopes(local_slopes_supply_pond %>% 
                                          filter(is.finite(slope) & is.finite(se_slope)))


# --- Table pour la demande ---
demand_table <- tibble(
  Horizon = c(5, 10, 15, "Pond"),
  Type = "Demand",
  Mean = c(my_demand_5$mean, my_demand_10$mean, my_demand_15$mean, my_demand_pond$mean),
  Weighted_Mean = c(my_demand_5$w_mean, my_demand_10$w_mean, my_demand_15$w_mean, my_demand_pond$w_mean),
  Alpha = c(my_demand_5$alpha, my_demand_10$alpha, my_demand_15$alpha, my_demand_pond$alpha)
)

# --- Table pour l'offre ---
supply_table <- tibble(
  Horizon = c(5, 10, 15, "Pond"),
  Type = "Supply",
  Mean = c(my_supply_5$mean, my_supply_10$mean, my_supply_15$mean, my_supply_pond$mean),
  Weighted_Mean = c(my_supply_5$w_mean, my_supply_10$w_mean, my_supply_15$w_mean, my_supply_pond$w_mean),
  Alpha = c(my_supply_5$alpha, my_supply_10$alpha, my_supply_15$alpha, my_supply_pond$alpha)
)

# Combiner les deux
combined_table <- bind_rows(demand_table, supply_table) %>% 
  mutate(elas_app = Weighted_Mean + Alpha)





# Créer la table gt
table_gt_instruments <- combined_table %>%
  gt() %>%
  # Labels propres
  cols_label(
    
  ) %>%
  tab_header(
    title = "Robustness elasticity supply demand curve"
  ) %>%
  fmt_number(
    columns = everything(),  # formater toutes les colonnes numériques
    decimals = 3
  ) %>%
  opt_row_striping() 



# Chemin où sauvegarder
save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
save_file <- file.path(save_plot_path, "instrument_var_share.tex")

# Sauvegarder en LaTeX
gtsave(table_gt_instruments, save_file)


# Param?tres pour les bins
n_points_per_bin <- 100

# Fonction pour cr?er les bins
bin_slopes <- function(df, n_points_per_bin) {
  n_bins <- ceiling(nrow(df) / n_points_per_bin)
  df %>%
    mutate(price = exp(mean_log_price)) %>%
    arrange(price) %>%
    mutate(bin = rep(1:n_bins, each = n_points_per_bin, length.out = n())) %>%
    group_by(bin) %>%
    summarise(
      price_bin = mean(price, na.rm = TRUE),
      slope_bin = mean(slope, na.rm = TRUE),
      slope_sd = sd(slope, na.rm = TRUE),
      n = n(),
      slope_se = slope_sd / sqrt(n)
    )
}

# Cr?er les bins pour la demande et l'offre
demand_binned <- bin_slopes(local_slopes_demand_10 %>% 
                              mutate(price = exp(mean_log_price)) %>% 
                              filter(price <= 400), n_points_per_bin) %>%
  mutate(type = "Demand")
supply_binned <- bin_slopes(local_slopes_supply_10 %>% 
                              mutate(price = exp(mean_log_price)) %>% 
                              filter(price <= 400), n_points_per_bin) %>%
  mutate(type = "Supply")


# Combiner les deux datasets
combined_binned <- bind_rows(demand_binned, supply_binned)

# Plot
hetero_elas <- ggplot(combined_binned, aes(x = price_bin, y = slope_bin, color = type)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = slope_bin - slope_se, ymax = slope_bin + slope_se), width = 0.0) +
  geom_smooth(method = "lm", se = FALSE) +  # fit lin?aire pour chaque type
  labs(
    x = "Price",
    y = "Local Elasticity",
    title = "",
    color = "Type"
  ) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )

hetero_elas 

save_plot_path <- 'Z:/Overleaf_elec/images/post_seminaire'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("elasticity_price_heterogeneity.png", plot = hetero_elas, width = 12, height = 6, dpi = 300)



