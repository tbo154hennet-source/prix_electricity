library(zoo)

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')

gas_tot_futures <- read_parquet("gas_data_shock.parquet")
gas_tot_futures <- gas_tot_futures %>% 
  arrange(time) %>% 
  mutate(gas_shock = TTF - lag(TTF),
         datetime = as.Date(time, format = "%Y-%m-%d")
         )


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/outputs')
instrument_days_3std <- read.csv("instrument_news_events_3std_bis.csv")
instrument_days_3std$Trading.day <- as.Date(instrument_days_3std$Trading.day, format = "%Y-%m-%d")

#on a bien aussi les demand shocks
# instrument_days_3std %>% 
#   group_by(demand_binary_shock, supply_binary_shock, uncertainty_binary_shock) %>%
#   summarise(n = n())


gas_news_days_3std <- merge(instrument_days_3std, gas_tot_futures, 
                               by.x = "Trading.day", by.y = "datetime")

instrument_days_2std <- read.csv("instrument_news_events_2std_bis.csv")
instrument_days_2std$Trading.day <- as.Date(instrument_days_2std$Trading.day, format = "%Y-%m-%d")
gas_news_days_2std <- merge(instrument_days_2std, gas_tot_futures, 
                            by.x = "Trading.day", by.y = "datetime")


#on veut exclure les jours où il y a eu des annonces de demande
demand_news_days_3std <- 
  #shock_data_3std %>% 
  gas_news_days_3std %>% 
  filter(demand_binary_shock==1 & demand_news > 2) 

demand_news_days_2std <- 
  #shock_data_2std %>% 
  gas_news_days_2std %>% 
  filter(demand_binary_shock==1 & demand_news > 2) 


# Filtrer les lignes où datetime n'est pas dans gas_OPEC_days$datetime
gas_news_days_3std <- gas_news_days_3std %>%
  filter(day_type =="supply") %>% 
  #exclude demand news that I have identified à la relecture
   filter(!(datetime %in% demand_news_days_3std$datetime))


gas_news_days_2std <- gas_news_days_2std %>%
  filter(day_type =="supply") %>% 
  #exclude demand news that I have identified à la relecture
  filter(!(datetime %in% demand_news_days_2std$datetime))

#benchmark excluding the special days whether OPEC or supply shocks

normal_gas_futures <- gas_tot_futures %>%
  filter(
      !(datetime %in% gas_news_days_2std$datetime) &
      !(datetime %in% demand_news_days_2std$datetime)
  )


summary(gas_tot_futures %>% 
          select(gas_shock, PC1))

fluct_gas_normales <- sd(normal_gas_futures$gas_shock, na.rm=TRUE)
fluct_PC_normales <- sd(normal_gas_futures$PC1, na.rm=TRUE)

fluct_gas_supply_3std  <- sd(gas_news_days_3std$gas_shock, na.rm=TRUE)
fluct_PC_supply_3std  <- sd(gas_news_days_3std$PC1, na.rm=TRUE)

fluct_gas_supply_2std  <- sd(gas_news_days_2std$gas_shock, na.rm=TRUE)
fluct_PC_supply_2std  <- sd(gas_news_days_2std$PC1, na.rm=TRUE)

fluct_gas_demand_3std  <- sd(demand_news_days_3std$gas_shock, na.rm=TRUE)
fluct_PC_demand_3std  <- sd(demand_news_days_3std$PC1, na.rm=TRUE)

fluct_gas_demand_2std  <- sd(demand_news_days_2std$gas_shock, na.rm=TRUE)
fluct_PC_demand_2std  <- sd(demand_news_days_2std$PC1, na.rm=TRUE)




# Sample data
library(gt)

data <- data.frame(
  Category = c("Normal","Supply 3std","Supply 2std","Demand 3std",
                          "Demand 2std")
               ,
  Value1 = round(c(fluct_gas_normales,fluct_gas_supply_3std,fluct_gas_supply_2std,
                   fluct_gas_demand_3std,fluct_gas_demand_2std), 3),
  Value2 = round(c(fluct_PC_normales, fluct_PC_supply_3std, fluct_PC_supply_2std,
                   fluct_PC_demand_3std,fluct_PC_demand_2std), 3)
)


# Create a gt table with styling
data %>%
  gt() %>%
  tab_header(
    title = "Comparing IV days Std",
  ) %>%
  tab_style(
    style = list(cell_fill(color = "gray95")),
    locations = cells_body(
      rows = seq(1, nrow(data), by = 2)  # Applies shading to alternate rows
    )
  ) %>%
  cols_label(
    Category = "",
    Value1 = "GasF 1M",
    Value2 = "GasF PC1"
  ) %>%
  tab_options(
    table.width = pct(60),
    table.font.size = 14
  )
#%>%gtsave("compare_OPEC_and_supply_shocks.html")





library(car)
#colnames(normal_gas_futures)

sample_days <- rbind(
  normal_gas_futures %>% select(PC1, gas_shock, datetime) %>% rename(Trading.day = datetime) %>% mutate(type = "Normal"),
  gas_news_days_3std %>% select(PC1, gas_shock, Trading.day) %>% mutate(type = "Supply 3std"),
  gas_news_days_2std %>% select(PC1, gas_shock, Trading.day) %>% mutate(type = "Supply 2std"),
  demand_news_days_3std %>% select(PC1, gas_shock, Trading.day) %>% mutate(type = "Demand 3std"),
  demand_news_days_2std %>% select(PC1, gas_shock, Trading.day) %>% mutate(type = "Demand 2std"))


news_count <- event_news_count %>% 
  pivot_wider(names_from = event_type, values_from = news_per_day) %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>% 
  mutate(day_type = ifelse(supply < 2*demand, "ambiguous", "supply")) %>%
  mutate(day_type = ifelse(supply <= demand , "demand", day_type)) %>% 
  select(supply, uncertainty, day_type) %>% 
  ungroup()


gas_large_variations <- gas_tot_futures %>% 
  left_join(news_count, by = "datetime") %>%
  arrange(desc(abs(PC1))) %>%  # Sort in ascending order
  slice_head(n = 30) %>%      # Take the first 30 rows
  mutate(news_identified = (datetime %in% c(instrument_days_2std$datetime)))

colnames(gas_large_variations)
colnames(gas_news_days_3std)

# Fusion des grandes variations avec sample_days
merged_data <- gas_large_variations %>%
  inner_join(sample_days, by = c("datetime" = "Trading.day"))

# Comptage du nombre de jours par catégorie
count_by_category <- merged_data %>%
  count(type)

# Affichage des résultats
print(count_by_category)

#gros problèe, je loupe enormement de jours d'annonce très importants


# Test de Levene pour les chocs pétroliers
levene_groups <- c("Supply 3std","Supply 2std","Demand 3std",
                   "Demand 2std")

# Test de Levene pour l'égalité des variances
levene_result <- leveneTest(PC1 ~ type, 
                            data = sample_days %>% filter(type == "Normal" | type == "Supply 3std") )



# Fonction pour obtenir les p-values des tests de Levene pour plusieurs variables
get_p_value_levene <- function(data, groups) {
  # Initialisation d'un dataframe vide pour stocker les résultats
  results <- data.frame(Group = character(), 
                        PC1_p_value = numeric(), 
                        GasShock_p_value = numeric(),
                        stringsAsFactors = FALSE)
  
  # Parcours des groupes pour effectuer les tests de Levene
  for (group in groups) {
    # Test de Levene pour chaque variable
    levene_result_PC1 <- leveneTest(PC1 ~ type, data = data %>% filter(type == "Normal" | type == group))
    levene_result_gas_shock <- leveneTest(gas_shock ~ type, data = data %>% filter(type == "Normal" | type == group))

    # Extraction des p-values
    p_value_PC1 <- levene_result_PC1$`Pr(>F)`[1]
    p_value_gas_shock <- levene_result_gas_shock$`Pr(>F)`[1]

    
    # Ajouter les résultats au dataframe
    results <- rbind(results, data.frame(Group = group, 
                                         PC1_p_value = p_value_PC1, 
                                         GasShock_p_value = p_value_gas_shock))
  }
  
  return(results)
}


# Test de Levene pour chaque groupe
p_values <- get_p_value_levene(data = sample_days, groups = levene_groups)


colnames(data) <- c("Category", "GasF 1M",  "GasF PC1")
colnames(p_values) <- c("Category", "GasF 1M",  "GasF PC1")

# Fonction pour convertir les p-values en étGases
stars <- apply(p_values[, -1], 2, function(x) {  # On exclut la première colonne 'Category'
  sapply(x, function(p) {
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return("(.)")
    } else {
      return("")
    }
  })
})

# Ajouter les étGases dans chaque colonne de p-values
data_with_p_values_and_stars <- rbind(data[1,],
  data[2:nrow(data),]  %>%
  mutate(
    `GasF 1M` = paste(round(`GasF 1M`, 3), stars[, 1], sep = " "),
    `GasF PC1` = paste(round(`GasF PC1`, 3), stars[, 2], sep = " ")
  ))

sample_count <- sample_days %>% 
  group_by(type) %>%
  summarise(size = n())

data_with_p_values_and_stars <- data_with_p_values_and_stars %>% 
  merge(sample_count, by.x = "Category", by.y = "type") 
# Afficher le tableau mis à jour
# Imposer l'ordre des catégories
data_with_p_values_and_stars$Category <- factor(data_with_p_values_and_stars$Category, 
                                                levels = c("Normal", "Supply 3std", "Supply 2std", 
                                                           "Demand 3std", "Demand 2std"))

# Créer le tableau GT avec les étGases
data_with_p_values_and_stars %>%
  arrange(Category) %>% 
  gt() %>%
  tab_header(
    title = "Comparing IV days Std"
  ) %>%
  tab_style(
    style = list(cell_fill(color = "gray95")),
    locations = cells_body(
      rows = seq(1, nrow(data), by = 2)  # Applique le style aux lignes impaires
    )
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 14
  ) %>%
  gtsave("compare_supply_shocks_with_significance.html")


getwd()

#je veux rajouter le nombre de top 20 variation days included in the corpus











