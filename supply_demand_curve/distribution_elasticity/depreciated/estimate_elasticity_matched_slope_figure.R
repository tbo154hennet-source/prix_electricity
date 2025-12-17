library(ggplot2)
library(ggpubr)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
df_clean_no_lag <-  read.csv( "df_clean_da.csv")
df_clean_lag <-  read.csv( "df_clean_da_seminar.csv")
#df_clean_lag_no_unavail <-  read.csv( "df_clean_da_seminar_no_unavail.csv")



df_clean_no_lag <- df_clean_no_lag %>% 
  filter(Instrument == "Bonte et al (2015)" | Instrument == "OLS") %>% 
  mutate(Instrument = ifelse(Instrument == "Bonte et al (2015)", "Bonte et al (2015) \n Babii(2022)", Instrument))

df_clean_lag <- df_clean_lag %>% 
  mutate(Instrument = ifelse(Instrument == "Bonte et al (2015)", "Tiedemann et al (2023)", Instrument),
         Instrument = ifelse(Instrument == "IV Ortho", "Synthetic Instruments", Instrument)) %>% 
  filter(Instrument == "Tiedemann et al (2023)" | Instrument == "Synthetic Instruments")

df_clean <- rbind(df_clean_no_lag, df_clean_lag)


df_clean_supply <- df_clean %>% 
  filter(Model == "OLS" | grepl("Supply", Model))
df_clean_demand <- df_clean %>% 
  filter(Model == "OLS" | grepl("Demand", Model))


# cr?er un y num?rique pour les instruments

# D?finir l'ordre personnalis?
model_order <- c("OLS", "Bonte et al (2015) \n Babii(2022)", "Tiedemann et al (2023)", "Synthetic Instruments")

df_clean_demand <- df_clean_demand %>%
  mutate(Instrument = factor(Instrument, levels = model_order)) %>%  # transformer en facteur
  arrange(Instrument) %>%                                       # trier selon le facteur
  mutate(y_numeric = 1:n()) 
#colnames(df_clean_demand)

df_clean <- full_join(df_clean,
                      df_clean_demand %>% 
                        dplyr::select(Instrument, y_numeric),
                      by = c("Instrument"))%>%
  mutate(Type = ifelse(Instrument == "OLS", "OLS", Type),
         # Instrument =ifelse(Instrument == "IV", "IV SD", Type),
         # Instrument =ifelse(Instrument == "IV Ortho", "IV SD Orth", Type),
         # 
  )

# position du label ? droite (juste au-dessus du max de l'histogramme)
x_max <- max(local_slopes_supply$slope_w, na.rm = TRUE) - 0.5


df_clean$x <- rep(0.95, nrow(df_clean))
df_clean$y <- seq(from = 1.5, to = 2.5, length.out = nrow(df_clean))

# Plot avec trait vertical et texte
my_elas <- ggplot(df_clean, aes(x=x, y=y)) +
  #accolade
  geom_point(alpha =0) +
  theme_classic() +
  theme(legend.position="none")+ 
  stat_brace(rotate = 270, width = 0.1)+
  geom_histogram(
    data = local_slopes_demand,
    aes(x = slope_w, y = ..count../sum(..count..)*10, fill = "Demand"),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7
  )+ 
  geom_segment(
    aes(x = w_mean_demand, xend = w_mean_demand, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "red",
    size = 1
  )+
  # geom_vline(
  #   xintercept = w_mean_demand,
  #   linetype = "dashed",
  #   color = "red",
  #   size = 1
  # ) +
  annotate(
    "text",
    x = w_mean_demand - 0.2,
    y = -0.5,
    #y = max(table(cut(local_slopes_demand$slope_w, breaks = 50)))/sum(table(cut(local_slopes_demand$slope_w, breaks = 50))) - 0.02,  # proportion
    label = paste0("w_mean = ", round(w_mean_demand, 3)),
    color = "red",
    vjust = -0.5,
    hjust = 0,
    size = 5   # augmente ou r?duit la taille du texte
  ) +
  geom_histogram(
    data = local_slopes_supply,
    aes(x = slope_w, y = ..count../sum(..count..)*10, fill = "Supply"),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7
  )+ 
  geom_segment(
    aes(x = w_mean_supply, xend = w_mean_supply, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "blue",
    size = 1
  )+
  annotate(
    "text",
    x = w_mean_supply - 0.2,
    y = -0.5,
    #y = max(table(cut(local_slopes_supply$slope_w, breaks = 50)))/sum(table(cut(local_slopes_supply$slope_w, breaks = 50))) - 0.02,  # proportion
    label = paste0("w_mean = ", round(w_mean_supply, 3)),
    color = "blue",
    vjust = -0.5,
    hjust = 0,
    size = 5   # augmente ou r?duit la taille du texte
  ) +
  geom_point(data = df_clean, aes(x = Estimate, y = y_numeric, color = Type), size = 3) +
  geom_errorbarh(data = df_clean, 
                 aes(xmin = Estimate - StdError, xmax = Estimate + StdError, y = y_numeric, color = Type),
                 height = 0.2) +
  # labels instruments ? droite
  geom_text(data = df_clean,
            aes(x = x_max , y = y_numeric, label = Instrument),
            hjust = 0, size = 5) +

labs(x = "Elasticity", y = "Proportion", color = "Type") +
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



save_latex_path <- "Z:/Overleaf_elec/images/intra_day"
setwd(save_latex_path)
ggsave(my_elas,
       filename = "elasticity_check_lag_no_unavail.png" , width = 12, height = 6, dpi = 300)
