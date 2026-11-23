library(ggplot2)

# Créer les données
market_data <- data.frame(
  Market = c("Day-ahead spot", 
             "Over the counter", 
             "Intraday auction", 
             "Intraday continuous"),
  Volume_TWh = c(158, 79, 35, 8)
)

# Calculer les pourcentages
market_data$Percentage <- round(market_data$Volume_TWh / sum(market_data$Volume_TWh) * 100, 1)

# Étiquettes avec noms et pourcentages
market_data$Label <- paste0(market_data$Market, "\n", market_data$Percentage, "%")

# Couleurs personnalisées : tons bleus pour intraday
custom_colors <- c(
  "Day-ahead spot\n56.4%"     = "#F8766D",  # rouge-orangé
  "Over the counter\n28.2%"   = "#7CAE00",  # vert
  "Intraday auction\n12.5%"   = "#619CFF",  # bleu clair
  "Intraday continuous\n2.9%" = "#1F78B4"   # bleu foncé
)

# Tracer le camembert
market_shares <- ggplot(market_data, aes(x = "", y = Volume_TWh, fill = Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )+
  theme(legend.text = element_text(size = 30)    # taille du texte
        
  )
market_shares
save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/motivation'
setwd(save_plot_path)
ggsave("market_shares.png", plot = market_shares, width = 10 , height = 10, dpi = 300)



