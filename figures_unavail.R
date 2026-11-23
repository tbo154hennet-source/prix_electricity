library(gt)
library(dplyr)
library(tibble)
library(arrow)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(ggplot2)
library(slider)
library(fixest)
# Données
df <- tribble(
  ~Market, ~Announcement, ~Nuclear, ~Thermal, ~Predicted,
  "Day-ahead", "Announced", "0.142*** (0.023)", "-0.012*** (0.002)", "0.109*** (0.015)",
  "Day-ahead", "Unannounced", "-0.026 (0.050)", "0.020* (0.023)", "0.062** (0.027)",
  "Intraday", "Announced", "0.027*** (0.009)", "0.009 (0.008)", "0.146*** (0.059)",
  "Intraday", "Unannounced", "-0.002 (0.089)", "0.015 (0.011)", "0.042 (0.047)"
)

# Création du tableau
table_unavail <- df %>%
  gt(rowname_col = "Announcement", groupname_col = "Market") %>%
  tab_header(
    title = "Plant Unavailability Price elasticity"
  ) %>%
  cols_label(
    Nuclear = "Nuclear",
    Thermal = "Thermal",
    Predicted = "Predicted"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(
    row_group.as_column = FALSE,             # fusion verticale automatique
    row_group.font.weight = "bold",
    table.font.size = 14,
    table.align = "center",
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#F9F9F9",
    data_row.padding = px(5),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.align = "center"
  ) %>%
  opt_table_lines()  # active les lignes horizontales



# Données du second tableau
df2 <- tribble(
  ~Market, ~Plant_Unavailability, ~Renewables, ~Crossborder_Flows,
  "Day-ahead", "0.109*** (0.015)", "-0.048*** (0.001)", "-0.029*** (0.004)",
  "Intraday", "0.146** (0.059)", "-0.040*** (0.009)", "-"
)

# Création du tableau gt
table_all <- df2 %>%
  gt(rowname_col = "Market") %>%
  tab_header(
    title = "Price semi-elasticity by shock"
  ) %>%
  cols_label(
    Plant_Unavailability = "Plant Unavailability",
    Renewables = "Renewables",
    Crossborder_Flows = "Crossborder Flows"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(
    table.font.size = 14,
    row_group.as_column = FALSE,
    row_group.font.weight = "bold",
    table.align = "center",
    data_row.padding = px(5),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.align = "center"
  ) %>%
  opt_table_lines()

# Données du second tableau
df3 <- tribble(
  ~Market, ~Cold_Excess, ~Heat_Excess,
  "Day-ahead", "0.007*** (0.0003)", "0.012*** (0.0003)"
)

# Création du tableau gt
table_demand <- df3 %>%
  gt(rowname_col = "Market") %>%
  tab_header(
    title = "Demand Elasticity by Event Type"
  ) %>%
  cols_label(
    Cold_Excess = "Cold Excess",
    Heat_Excess = "Heat Excess"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(
    table.font.size = 14,
    row_group.as_column = FALSE,
    row_group.font.weight = "bold",
    table.align = "center",
    data_row.padding = px(5),
    table.border.top.width = px(1),
    table.border.bottom.width = px(1),
    heading.align = "center"
  ) %>%
  opt_table_lines()



#US Elec data
# year	The year of the observation
# month	The month of the observation
# stateDescription	The name of the state
# sectorName	The sector of the electricity market (residential, commercial, industrial, other, or all sectors)
# customers	The number of customers (missing for some observations)
# price	The average price of electricity per kilowatt-hour (kWh) in cents
# revenue	The total revenue generated from electricity sales in millions of dollars
# sales	The total electricity sales in millions of kilowatt-hours (kWh)
US_path <- "C:/Users/DBT3KD/Desktop/uncertainty_shocks/DATA/macro_data"
setwd(US_path)
US_data_cpi <-read_excel( 'CPIAUCSL_kanzig_cpi.xlsx', sheet = "Monthly")
US_data_cpi <- US_data_cpi %>% 
  mutate(observation_date = as.Date(observation_date)) %>% 
  mutate(CPI_log_diff = log(CPIAUCSL) - log(lag(CPIAUCSL, 1)),
         inflation_12m = 100*slide_dbl(CPI_log_diff, sum, .before = 11, .complete = TRUE)) %>% 
  filter(observation_date >= as.Date("2015-01-01"))
  
FR_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/macro_data/fr"
setwd(FR_path)
FR_data_cpi <-read_excel('all_cpi_CP0000FRM086NEST.xlsx', sheet = "Monthly")
FR_data_cpi <- FR_data_cpi %>% 
  mutate(observation_date = as.Date(observation_date)) %>% 
  mutate(CPI_log_diff = log(CP0000FRM086NEST) - log(lag(CP0000FRM086NEST, 1)),
         inflation_12m = 100*slide_dbl(CPI_log_diff, sum, .before = 11, .complete = TRUE)) %>% 
  filter(observation_date >= as.Date("2015-01-01"))



#c("observation_date", "CPIAUSCL")

#index on 2015

US_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA'
setwd(US_path)
US_data <-read.csv( 'US_Elec_price_index.csv') %>% 
  mutate(month = month(observation_date),
         year = year(observation_date)) %>% 
  filter(year >= 2015 & year <= 2023)
#kwh per cent
#vs Euro per MWh

setwd("C:/Users/DBT3KD/Desktop/uncertainty_shocks/DATA")
us_eu_exchang_rate <- read_excel("USD_EU_exchange_rate.xlsx", skip = 10) %>% 
  mutate(month = month(observation_date),
         year = year(observation_date)) %>% 
  summarise(
    DEXUSEU = mean(DEXUSEU, na.rm = TRUE),
    #observation_date = first(observation_date),
    .by = c("year", "month")
  )

#conversion
US_trend <- merge(US_data, us_eu_exchang_rate, by = c("year", "month")) %>% 
  mutate(price =  APU000072610*1000 )#* DEXUSEU ) 


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data <-read_parquet( 'IV_data_fr.parquet')%>% 
  mutate(HICP_log_diff = log(HICP) - log(lag(HICP, 1)),
         inflation_12m = slide_dbl(HICP_log_diff, sum, .before = 11, .complete = TRUE))


motivation_plot <- ggplot(data = IV_data) +

  geom_line(aes(x = observation_date, y = spot_price,
                color = "FR",
                linetype = "P[elec]")) +
  
  geom_line(data = US_trend , aes(x = as.Date(observation_date), y = price, color = "US", linetype = "P[elec]")) +
  # geom_line(data = FR_data_cpi, aes(x = observation_date, y = 100 +3* inflation_12m, color = "FR", linetype = "pi"))+
  # geom_line(data = US_data_cpi, aes(x = observation_date, y = 100 + 3*inflation_12m, color = "US", linetype = "pi"))+
  geom_line(data = FR_data_cpi, aes(x = observation_date, y = 20* inflation_12m, color = "FR", linetype = "pi"))+
  geom_line(data = US_data_cpi, aes(x = observation_date, y = 20*inflation_12m, color = "US", linetype = "pi"))+
  
  scale_color_manual(
    values = c("FR" = "red",
               "US" = "blue"
               ),
    name = "Country"
  ) +
  scale_linetype_manual(
    values = c("pi" ="dashed" , "P[elec]" = "solid"),
    labels = c("pi" = expression(pi[CPI]), 
               "P[elec]" = expression(P[elec])),
    name = "Indicator"
  ) +
  labs(
    x = "Date",
    y = "Value"
  ) +
  scale_y_continuous(
    name = "Electricity Price",
    sec.axis = sec_axis(~ ./20, name = "Inflation (%)")  # On décale l’axe secondaire de 100 (car tu as fait 100 + inflation)
  ) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )
motivation_plot

save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/motivation'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("trends_elec_inflation.png", plot = motivation_plot, width = 12, height = 6, dpi = 300)



save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data_esp <-read_parquet( 'IV_data_esp.parquet')%>% 
  mutate(HICP_log_diff = log(HICP) - log(lag(HICP, 1)),
         inflation_12m = slide_dbl(HICP_log_diff, sum, .before = 11, .complete = TRUE))


motivation_plot <- ggplot(data = IV_data) +

  geom_line(aes(x = observation_date, y = spot_price,
                color = "FR",
                linetype = "P[elec]")) +
  geom_line(data = IV_data_esp, aes(x = observation_date, y = spot_price,
                color = "ESP",
                linetype = "P[elec]")) +
  # geom_line(data = FR_data_cpi, aes(x = observation_date, y = 100 +3* inflation_12m, color = "FR", linetype = "pi"))+
  # geom_line(data = US_data_cpi, aes(x = observation_date, y = 100 + 3*inflation_12m, color = "US", linetype = "pi"))+
  geom_line(data = IV_data, aes(x = observation_date, y = 100 +1000*inflation_12m, color = "FR", linetype = "pi"))+
  geom_line(data = IV_data_esp, aes(x = observation_date, y = 100 +1000*inflation_12m, color = "ESP", linetype = "pi"))+
  
  scale_color_manual(
    values = c("FR" = "red",
               "ESP" = "blue"
               ),
    name = "Country"
  ) +
  scale_linetype_manual(
    values = c("pi" ="dashed" , "P[elec]" = "solid"),
    labels = c("pi" = expression(pi[CPI]), 
               "P[elec]" = expression(P[elec])),
    name = "Indicator"
  ) +
  labs(
    x = "Date",
    y = "Value"
  ) +
  scale_y_continuous(
    name = "Electricity Spot Price",
    sec.axis = sec_axis(~ (.-100)/10, name = "Inflation (%)")  # On décale l’axe secondaire de 100 (car tu as fait 100 + inflation)
  ) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )
motivation_plot

save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/motivation'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("trends_elec_inflation_esp.png", plot = motivation_plot, width = 10, height = 6, dpi = 300)





save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Data_structuring/Output'
setwd(save_var_path)
IV_data_esp <-  read_parquet('IV_data_esp.parquet')
IV_data_fr <-  read_parquet('IV_data_fr.parquet')

my_gas_plot <- ggplot()+
  geom_line(data = IV_data_esp, aes(x = observation_date, y = spot_gas_capped, color ="Gas Capped"), linetype ="dashed")+
  geom_line(data = IV_data_esp, aes(x = observation_date, y = spot_gas, color ="Gas"))+
  geom_line(data = IV_data_esp, aes(x = observation_date, y = spot_price, color = "Electricity", linetype ="ESP"))+
  geom_line(data = IV_data_fr, aes(x = observation_date, y = spot_price, color = "Electricity", linetype ="FR"))+
  scale_color_manual(
    values = c(
      "Gas" = "darkblue",
      "Gas Capped" = "blue",
      "Electricity" = "orange"
    )
  ) +
  labs(title = "Iberian exception effect",
       x = "Date",
       y = "Price (EUR/MWh)",
       color = "Legend") 

save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/motivation'
setwd(save_plot_path)
ggsave("gas_elec_prices.png", plot = my_gas_plot, width = 18, height = 6, dpi = 300)





save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/depreciated/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff.parquet')

IV_data_log_diff_esp$observation_date


my_plot_adj_margin <- ggplot(data = IV_data_log_diff_esp) +
  # Courbe de gen_tot (divisée par 1000)
  geom_line(aes(x = observation_date, y = gen_tot / 1000), color = "blue") +
  
  # Courbe de spot_price (axe secondaire)
  geom_line(aes(x = observation_date, y = spot_price), color = "red") +
  
  labs(
    title = "Price and Quantity on the Electricity Market in Spain",
    x = "Date",
    y = "Generation (GW)"
  ) +
  
  scale_y_continuous(
    name = "Generation (GW)",
    sec.axis = sec_axis(~., name = "Spot Price (EUR/MWh)")
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )


my_plot_adj_margin 


save_plot_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/motivation'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("plot_adj_margin.png", plot = my_plot_adj_margin , width = 12, height = 6, dpi = 300)












