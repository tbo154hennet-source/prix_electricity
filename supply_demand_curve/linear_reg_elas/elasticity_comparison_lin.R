library(ggplot2)
library(ggpubr)
library(ggbrace)

library(dplyr)
library(lubridate)

setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
list.files()
local_slopes_demand_lin  <- read_parquet("local_slopes_reg_demand_10.parquet")
local_slopes_supply_lin  <- read_parquet("local_slopes_reg_supply_10.parquet")

local_slopes_demand_lin <- local_slopes_demand_lin %>%
  mutate(
    year  = year(DATE),
    month = month(DATE)
  )%>%
  mutate(
    price_residualized = resid(
      lm(mean_log_price ~ factor(Hour) + factor(year) + factor(month),
         data = .)
    )
  )

local_slopes_supply_lin <- local_slopes_supply_lin %>%
  mutate(
    year  = year(DATE),
    month = month(DATE)
  )%>%
  mutate(
    price_residualized = resid(
      lm(mean_log_price ~ factor(Hour) + factor(year) + factor(month),
         data = .)
    )
  )
# Exemple d'utilisation
my_demand <- describe_local_slopes(local_slopes_demand_lin)
# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_demand_lin$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_demand_lin$slope, probs = 0.975, na.rm = TRUE)
# Winsorisation
local_slopes_demand_lin[, slope_w := pmin(pmax(slope, q_low), q_high)]

# On calcule la moyenne pond?r?e des pentes
w_mean_demand <- weighted.mean(local_slopes_demand_lin$slope, local_slopes_demand_lin$mean_log_price^2, na.rm= TRUE)
w_mean_demand <- weighted.mean(local_slopes_demand_lin$slope, local_slopes_demand_lin$price_residualized^2, na.rm= TRUE)
w_mean_demand <- mean(local_slopes_demand_lin$slope, na.rm= TRUE)
colnames(local_slopes_demand_lin)

mean_demand <- mean(local_slopes_demand_lin$slope, na.rm= TRUE)


my_supply <- describe_local_slopes(local_slopes_supply_lin)
# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_supply_lin$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_supply_lin$slope, probs = 0.975, na.rm = TRUE)
# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_supply_lin[, slope_w := pmin(pmax(slope, q_low), q_high)]
w_mean_supply <- weighted.mean(local_slopes_supply_lin$slope, local_slopes_supply_lin$mean_log_price^2, na.rm= TRUE)
w_mean_supply <- weighted.mean(local_slopes_supply_lin$slope, local_slopes_supply_lin$price_residualized^2, na.rm= TRUE)
w_mean_supply <- mean(local_slopes_supply_lin$slope, na.rm= TRUE)

# w_mean_supply <- weighted.mean(price_indispo_merge_esp_elas$slope_supply, price_indispo_merge_esp_elas$resid_price^2, na.rm= TRUE)

save_path <- "Z:/prix_electricite_gitlab/supply_demand_curve/estimate_elasticity/outputs"
setwd(save_path)
summary_synthetic <- read.csv( "elasticity_synthetic_lin.csv")
summary_shift_share <- read.csv( "elasticity_shift_share_lin.csv")
summary_standard <- read.csv( "elasticity_standard_lin.csv")



summary_table<- summary_standard[2:nrow(summary_standard),] %>%
  mutate(
   Methodology = case_when(
     Model== "OLS"~"OLS",
      Lag == 0 & Model!= "OLS"~ "Bonte et al (2015) \n Babii(2022)",
      Lag == 1 ~ "Tiedemann et al (2023)"
    ),
  Elasticity = case_when(
    Model== "OLS"~"OLS",
    Model == "Supply" ~ "Demand",
    Model == "Demand" ~ "Supply"
  )
)

summary_table<-rbind(summary_table ,
                     summary_synthetic %>% 
                       filter(Matching == "Unmatched" & Lag ==1) %>% 
                       mutate(
                         Methodology = "Synthetic",
                         Elasticity = case_when(
                           Model== "OLS"~"OLS",
                           Model == "Supply" ~ "Demand",
                           Model == "Demand" ~ "Supply"
                         )) %>% 
                       dplyr::select(-Matching),
                         summary_shift_share %>% 
                           filter(Lag ==1) %>% 
                           mutate(
                             Methodology = "Shift share",
                             Elasticity = case_when(
                               Model== "OLS"~"OLS",
                               Model == "Supply" ~ "Demand",
                               Model == "Demand" ~ "Supply"
                             )
                     )
)


df_clean_supply <- summary_table %>% 
  filter(Model == "OLS" | grepl("Supply", Model))
df_clean_demand <- summary_table %>% 
  filter(Model == "OLS" | grepl("Demand", Model))


# cr?er un y num?rique pour les Methodologys

# D?finir l'ordre personnalis?
model_order <- c("OLS", "Bonte et al (2015) \n Babii(2022)", "Tiedemann et al (2023)", "Shift share", "Synthetic")

df_clean_demand <- df_clean_demand %>%
  mutate(Methodology = factor(Methodology, levels = model_order)) %>%  # transformer en facteur
  arrange(Methodology) %>%                                       # trier selon le facteur
  mutate(y_numeric = 1:n()) 
#colnames(df_clean_demand)

colnames(summary_table)

summary_table <- full_join(summary_table,
                      df_clean_demand %>% 
                        dplyr::select(Methodology, y_numeric),
                      by = c("Methodology"))%>%
  mutate(Methodology = ifelse(Methodology == "OLS", "OLS", Methodology),
         # Methodology =ifelse(Methodology == "IV", "IV SD", Methodology),
         # Methodology =ifelse(Methodology == "IV Ortho", "IV SD Orth", Methodology),
         # 
  )

# position du label ? droite (juste au-dessus du max de l'histogramme)
x_max <- max(local_slopes_supply_lin$slope_w, na.rm = TRUE) - 0.5


summary_table$x <- rep(0.95, nrow(summary_table))
summary_table$y <- seq(from = 1.5, to = 2.5, length.out = nrow(summary_table))

# Plot avec trait vertical et texte
my_elas <- ggplot(summary_table, aes(x=x, y=y)) +
  #accolade
  geom_point(alpha =0) +
  theme_classic() +
  theme(legend.position="none")+ 
  stat_brace(rotate = 270, width = 0.1)+
  geom_histogram(
    data = local_slopes_demand_lin,
    aes(x = slope_w, y = ..count../sum(..count..)*10, fill = "Demand"),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7
  )+ 
  geom_segment(
    aes(x = w_mean_demand, xend = w_mean_demand, y = 0, yend = 5),  # yend = hauteur max souhait?e
    lineMethodology = "dashed",
    color = "red",
    size = 1
  )+
  # geom_vline(
  #   xintercept = w_mean_demand,
  #   lineMethodology = "dashed",
  #   color = "red",
  #   size = 1
  # ) +
  annotate(
    "text",
    x = w_mean_demand,
    y = -0.5,
    #y = max(table(cut(local_slopes_demand_lin$slope_w, breaks = 50)))/sum(table(cut(local_slopes_demand_lin$slope_w, breaks = 50))) - 0.02,  # proportion
    label = round(w_mean_demand), #paste0("w_mean = ", round(w_mean_demand, 3)),
    color = "red",
    vjust = -0.5,
    hjust = 0,
    size = 5   # augmente ou r?duit la taille du texte
  ) +
  geom_histogram(
    data = local_slopes_supply_lin,
    aes(x = slope_w, y = ..count../sum(..count..)*10, fill = "Supply"),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7
  )+ 
  geom_segment(
    aes(x = w_mean_supply, xend = w_mean_supply, y = 0, yend = 5),  # yend = hauteur max souhait?e
    lineMethodology = "dashed",
    color = "blue",
    size = 1
  )+
  annotate(
    "text",
    x = w_mean_supply - 0.2,
    y = -0.5,
    #y = max(table(cut(local_slopes_supply_lin$slope_w, breaks = 50)))/sum(table(cut(local_slopes_supply_lin$slope_w, breaks = 50))) - 0.02,  # proportion
    label = round(w_mean_supply),#paste0("w_mean = ", round(w_mean_supply, 3)),
    color = "blue",
    vjust = -0.5,
    hjust = 0,
    size = 5   # augmente ou r?duit la taille du texte
  ) +
  geom_point(data = summary_table, aes(x = Coef, y = y_numeric, color = Elasticity), size = 3) +
  geom_errorbarh(data = summary_table, 
                 aes(xmin = Coef - Se, xmax = Coef + Se, y = y_numeric, color = Elasticity),
                 height = 0.2) +
  # labels Methodologys ? droite
  geom_text(data = summary_table,
            aes(x = 250 , y = y_numeric, label = Methodology),
            hjust = 0, size = 5) +
  
  labs(x = "Elasticity", y = "Proportion", color = "Methodology") +
  theme(
    #legend.key.size = unit(1.5, "cm") , 
    legend.position = "bottom",   
    legend.text = element_text(size = 15),
    #axis.text.x = element_text(size = 14, angle = 30, hjust =1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),   # taille du titre axe y
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(
    values = c(
      "Demand" = "red",
      "Supply" = "blue",
      "OLS" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Demand" = "red",
      "Supply" = "blue",
      "OLS" = "black"
    ),
    guide = "none"  # supprime la l?gende pour le fill
  )


my_elas



save_latex_path <- "Z:/Overleaf_elec/images/elasticity"
setwd(save_latex_path)
ggsave(my_elas,
       filename = "elasticity_comparison_lin.png" , width = 12, height = 6, dpi = 300)
