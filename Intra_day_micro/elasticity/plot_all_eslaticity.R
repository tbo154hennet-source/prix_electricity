
library(stringr)
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")

df_wind_id <- read.csv( "df_wind_intrday")
df_err_id <- read.csv( "df_for_err_intrday")

df_id <- bind_rows(df_wind_id %>% 
                     mutate(Elasticity = "Demand"),
                   df_err_id%>% 
                     mutate(Elasticity = "Supply"))%>%
  # filter(Type =="Demand") %>% 
  mutate(Type = "Intraday")%>%
  mutate(
    Coef = as.numeric(str_extract(Elasticity_CI, "^-?\\d+\\.?\\d*")),
    Se = as.numeric(str_extract(Elasticity_CI, "(?<=\\()\\-?\\d+\\.?\\d*(?=\\))"))
  )


save_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/estimate_elasticity/outputs"
setwd(save_path)

summary_standard <- read.csv( "elasticity_standard.csv")
summary_standard<- summary_standard[2:nrow(summary_standard),] %>%
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
  )%>%
  filter(Model =="OLS") %>% 
  # filter(Type =="Demand") %>% 
  mutate(Instrument = "OLS")

summary_synthetic <-read.csv("elasticity_synthetic.csv") %>% 
  filter(Matching == "Unmatched" & Lag ==0) %>% 
  mutate(
    Methodology = "Synthetic",
    Elasticity = case_when(
      Model== "OLS"~"OLS",
      Model == "Supply" ~ "Demand",
      Model == "Demand" ~ "Supply"
    ))  %>%
  # filter(Type =="Demand") %>% 
  mutate(Instrument = "IV")

df_clean_da <- bind_rows(summary_standard, summary_synthetic)%>% 
  mutate(Type = "Day-ahead")


# Step 1: Combine the three dataframes
df_all <- bind_rows(df_id %>% 
                      # Instrument  == "OLS" & Elasticity = "Demand"
                      #est la même OLS sur le subsample COVID
                      filter(Instrument  == "IV" | Elasticity == "Supply") %>% 
                      dplyr::select(Instrument, Type, Elasticity, Coef, Se), 
                    df_clean_da%>% 
                      dplyr::select(Instrument, Type, Elasticity, Coef, Se)) %>%
  mutate(Instrument = ifelse( Instrument =="OLS", Instrument, paste(Instrument, Elasticity))) %>% 
  mutate(Type = factor(Type, levels = rev(c("Intraday", "Day-ahead"))))



# Step 5: Plot
plot_supply_elas <- ggplot(df_all, aes(x = Type, y = Coef, color = Instrument)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Coef - Se, ymax = Coef + Se),
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(
    x = NULL,
    y = "Elasticity",
    #title = "Time Evolution of Supply Elasticities"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "OLS" = "black",
      "IV Supply" = "blue",
      "IV Demand" = "red"
    ),
    labels = c(
      "OLS" = "OLS",
      "IV Supply" = "Supply Elasticity",
      "IV Demand" = "Demand Elasticity"
    )
  )+

  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 16, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 16)    # taille des ticks y
  )

plot_supply_elas

save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/intra_day"
setwd(save_latex_path)

# Save the plot as a PDF (or PNG)
ggsave(
  filename = "plot_supply_elas.png",   # or "plot_supply_elas.png"
  plot = plot_supply_elas,
  width = 10,    # width in inches
  height = 6,   # height in inches
  units = "in",
  device = "png" # or "png"
)

