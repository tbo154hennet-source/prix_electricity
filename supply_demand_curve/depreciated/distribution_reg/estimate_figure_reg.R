library(ggplot2)
library(ggpubr)
# Charger le package
library(ggbrace)

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs")
results_all <- read.csv( "linear_response.csv")

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
coef_table_feols_all <- read.csv( "coef_table_feols_all_linear.csv")

# m_ols <- feols(ols_formula, data = price_indispo_merge_feols_all, vcov_NW("datetime", lag = 24))
# summary_ols  <- summary(m_ols)
# coef_ols <- summary_ols$coeftable["price", "Estimate"]
# se_ols   <- summary_ols$coeftable["price", "Std. Error"]

coef_ols <-  -15.27
se_ols <-  2.79



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
df_clean_no_lag <-  read.csv( "df_clean_da.csv")
df_clean_lag <-  read.csv( "df_clean_da_seminar.csv")
#df_clean_lag_no_unavail <-  read.csv( "df_clean_da_seminar_no_unavail.csv")



df_clean_no_lag <- coef_table_feols_all %>% 
  filter(Model == "Bonte" & Lag == 0) %>% 
  mutate(Instrument = ifelse(Model == "Bonte", "Bonte et al (2015) \n Babii(2022)", Model))

df_clean_lag $Model
df_clean_lag <- coef_table_feols_all %>% 
  filter(Model == "Unmatched" | Model == "Curve_shocks" | Model == "Bonte") %>% 
  filter(Lag == 1) %>% 
  mutate(
    Instrument = ifelse(Model == "Unmatched", "Synthetic Instruments", "Synthetic Instruments"),
    Instrument = ifelse(Model == "Bonte", "Tiedemann et al (2023)", Instrument),
    Instrument = ifelse(Model == "Curve_shocks", "Curve Residuals", Instrument))


# Extraction des estimate et SE pour OLS
ols_row <- data.frame(
  X = max(df$X) + 1,  # nouvelle ligne, X suivant
  Model = "OLS",
  Coef_log_price = coef_ols,
  Std_Error = se_ols,
  Type = "Supply",      # ou "Demand" selon ton contexte
  FE = "YMWH",
  Lag = 0,
  Instrument = "OLS"
)

df_clean <- rbind(df_clean_no_lag, df_clean_lag, ols_row)


df_clean_supply <- df_clean %>% 
  filter(Type=="Supply")
df_clean_demand <- df_clean %>% 
  filter(Type=="Demand")


# cr?er un y num?rique pour les instruments

# D?finir l'ordre personnalis?
model_order <- c("OLS", "Bonte et al (2015) \n Babii(2022)", "Tiedemann et al (2023)", "Synthetic Instruments", "Curve Residuals")

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
         y_numeric = ifelse(is.na(y_numeric), 0, y_numeric)
         # Instrument =ifelse(Instrument == "IV", "IV SD", Type),
         # Instrument =ifelse(Instrument == "IV Ortho", "IV SD Orth", Type),
         # 
  )

# position du label ? droite (juste au-dessus du max de l'histogramme)
x_max <- max(df_clean$Coef_log_price, na.rm = TRUE) + 30

results_all <- results_all %>%
  mutate(
    xmin =Coefficient - 1.96 * StdError,
    xmax =Coefficient + 1.96 * StdError,
    Model = ifelse(Model == "Demand", "Supply", "Demand")
  )

df_clean$x <- rep(0.95, nrow(df_clean))
df_clean$y <- seq(from = 1.5, to = 2.5, length.out = nrow(df_clean))

# Plot avec trait vertical et texte
my_elas <- ggplot(df_clean, aes(x=x, y = y_numeric)) +
  #accolade
  geom_point(alpha =0) +
  theme_classic() +
  theme(legend.position="none")+ 
  #stat_brace(rotate = 270, width = 0.1)+
  geom_point(data = df_clean, aes(x = Coef_log_price, y = y_numeric, color = Type), size = 3) +
  geom_errorbarh(data = df_clean, 
                 aes(xmin = Coef_log_price - Std_Error, xmax = Coef_log_price +Std_Error, y = y_numeric, color = Type),
                 height = 0.2) +
  # labels instruments ? droite
  geom_text(data = df_clean,
            aes(x = x_max , y = y_numeric, label = Instrument),
            hjust = 0, size = 5) +
  
  labs(x = "Elasticity", y = "Linear Price Response (€/MWh)", color = "Type") +
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
  )+
  geom_segment( data = results_all,
                aes(x = Coefficient, xend = Coefficient,
                    y = min(df_clean$y_numeric) -1, yend = max(df_clean$y_numeric) +1,
                    color = Model),  # yend = hauteur max souhait?e
                linetype = "dashed",
                size = 1
  )


my_elas



save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/post_seminaire"
setwd(save_latex_path)
ggsave(my_elas,
       filename = "elasticity_check_lag_linear.png" , width = 12, height = 6, dpi = 300)
