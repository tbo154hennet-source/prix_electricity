input_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs"
output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"

setwd(output_dir)
price_indispo_merge_esp_npc_no_cbf <- read_parquet( "price_indispo_merge_esp_npc.parquet")

# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_supply <- grep("fit.*supply", names(price_indispo_merge_esp_npc), value = TRUE)

# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_demand <- grep("fit.*demand", names(price_indispo_merge_esp_npc), value = TRUE)

# Group by month and sum shocks
monthly_shocks <- price_indispo_merge_esp_npc_no_cbf %>%
  group_by(year, month) %>%
  summarise(across(all_of(c(cols_fitted_supply,cols_fitted_demand)), mean, na.rm = TRUE),
            datetime = first(datetime)
  ) %>%
  ungroup()
# Reshape to long format
monthly_shocks_long <- monthly_shocks %>%
  pivot_longer(
    cols = c(cols_fitted_supply,cols_fitted_demand),      # chaque colonne delta_s_*
    names_to = "Instrument",
    values_to = "Value"
  )%>%
  mutate(
    FE = grepl("nfe", Instrument, ignore.case = TRUE),         # TRUE si NFE (No Fixed Effects)
    Side = case_when(
      grepl("supply", Instrument, ignore.case = TRUE) ~ "Supply",
      grepl("demand", Instrument, ignore.case = TRUE) ~ "Demand",
      TRUE ~ NA_character_
    )
  )

ggplot(monthly_shocks_long,
       aes(x = datetime, y = Value, color = Side, linetype = FE)) +
  geom_line() +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
  labs(
    x = "Month",
    y = "Shocks (in MWh)",
    color = "Side",
    linetype = "Fixed Effects",
    title = ""
  ) +
  theme_minimal()








setwd(output_dir)
contrib_table_tot <- read.csv( "contrib_table_tot.csv")
coef_table_tot<- read.csv("coef_table_tot.csv")

################ NO FE


ggplot(
  contrib_table_tot %>% 
    filter(Type == "No FE", Name == "supply_unmatched") %>% 
    mutate(Contribution = Contribution / sum(Contribution)),
  aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)
) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution (normalized)", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


ggplot(contrib_table_tot %>% filter(Type == "No FE" & Name == "demand_unmatched"), aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution", title = paste("")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############### FE

ggplot(contrib_table_tot %>% filter(Type == "FE" & Name == "supply_unmatched"), 
       aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution", title = paste("")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(contrib_table_tot %>% filter(Type == "FE" & Name == "demand_unmatched"), aes(x = reorder(Variable, -Contribution), y = Contribution, fill = Variable)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(x = "Variable", y = "Variance Contribution", title = paste("")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Decomposition of my suply and demand shocks
unique(contrib_table_tot$Variable)


contrib_table_tot <- contrib_table_tot %>%
  mutate(
    Variable2 = case_when(
      Variable %in% c("cold_excess_demand", "heat_excess_demand") ~ "Temperature",
      TRUE ~ Variable
    )
  )
nice_names <- c(
  "Fore_Solar" = "Solar",
  "Fore_Wind" = "Wind",
  "Published.Predicted.Unavailable.Capacity" = "Unavailabilities",
  "cbf_es" = "CBF",
  "Temperature" = "Temperature"
)
contrib_table_tot <- contrib_table_tot %>%
  mutate(
    Instrument = recode(Variable2, !!!nice_names)
  )

plot_table <- contrib_table_tot %>% 
  filter(Type =="FE") %>% 
  group_by(Instrument, Type, Name) %>% 
  summarise(Contribution = sum(Contribution)) %>% 
  mutate(Type = ifelse(Name == "supply_unmatched", "Supply", "Demand"))

plot_table$Name
# Pour l'offre


synthetic_plot <- ggplot(plot_table, aes(x = Instrument, y = Contribution, fill = Type)) +
  geom_col(position = position_dodge(width = 0.4), color = "black", width = 0.35) +
  scale_fill_manual(values = c("Supply" = "steelblue", "Demand" = "tomato")) +
  theme(
    #legend.key.size = unit(1.5, "cm") , 
    legend.text = element_text(size = 15),
    axis.text.x = element_text(size = 14, angle = 30, hjust =1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),   # taille du titre axe y
    axis.text.y  = element_text(size = 12) )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
synthetic_plot

save_plot_path <- 'Z:/Overleaf_elec/images/data_description'
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("describe_synthetic_shocks_npc.png", plot = synthetic_plot, width = 12, height = 6, dpi = 300)


