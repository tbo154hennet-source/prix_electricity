library(arrow)
library(dplyr)
library(tidyr)

save_compare_info <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/info_files"
setwd(save_compare_info)

#compare the number of instrument days

info_supply <- read_parquet("info_supply.parquet")
info_full <- read_parquet("info_full.parquet")
info_small_cluster <- read_parquet("small_info_cluster.parquet")
info_high_cluster <- read_parquet("info_high_cluster.parquet")
info_selected <- read_parquet("info_selected.parquet")
info_all<- read_parquet("info_supply_all.parquet")


save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

demand_news_days <- read.csv("demand_news_days.csv")
demand_news_days <- demand_news_days[,2]

demand_news_days_all <- read.csv("demand_news_days_all.csv")
demand_news_days_all <- demand_news_days_all[,2]

demand_news_days <- rbind(demand_news_days, demand_news_days_all)


# Define a function for filtering instrument days
# Définir une fonction pour ajouter une colonne dummy pour les instrumented days
add_instrument_dummy <- function(data, threshold_column, threshold = 3) {
  data %>%
    mutate(instrumented_day = ifelse(binary_shock == TRUE & {{ threshold_column }} > threshold, "supply_shock", 0),
           instrumented_day = ifelse(Date %in% demand_news_days, "demand_shock", instrumented_day))
  }


# Appliquer la fonction à chaque dataset
info_full <- add_instrument_dummy(info_full, news_per_day)
info_supply <- add_instrument_dummy(info_supply, supply_news)
info_small_cluster <- add_instrument_dummy(info_small_cluster, news_per_day)
info_high_cluster <- add_instrument_dummy(info_high_cluster, news_per_day)
info_selected <- add_instrument_dummy(info_selected, news_per_day)
info_all <- add_instrument_dummy(info_all, supply_news, threshold=2)

supply_shock_counts <- c(
  "full" = nrow(info_full %>% filter(instrumented_day == "supply_shock")),
  "supply" = nrow(info_supply %>% filter(instrumented_day == "supply_shock")),
  "small_cluster" = nrow(info_small_cluster %>% filter(instrumented_day == "supply_shock")),
  "high_cluster" = nrow(info_high_cluster %>% filter(instrumented_day == "supply_shock")),
  "selected" = nrow(info_selected %>% filter(instrumented_day == "supply_shock")),
  "all" = nrow(info_all %>% filter(instrumented_day == "supply_shock"))
)

# Print the named vector
print(supply_shock_counts)
#compare the 30 days with highest variation


# Define a function for processing shock info
process_supply_shock_info <- function(data, rename_columns, extra_select = NULL) {
  data %>% 
    mutate(
      daily_variation = abs(PC1),
      ptstat = (supply_news - moving_avg_news) / moving_std_news
    ) %>% 
    arrange(desc(daily_variation)) %>% 
    slice(1:30) %>% 
    dplyr::select(
      Date, daily_variation, supply_news, ptstat, binary_shock
    ) %>% 
    rename(!!!rename_columns)
}


shock_info_supply <- process_supply_shock_info(
  info_supply, 
  rename_columns = c(
    ptstat_supply = "ptstat",
    supply_shock = "binary_shock"
  )
)
shock_info_all <- process_supply_shock_info(
  info_all, 
  rename_columns = c(
    ptstat_all = "ptstat",
    all_shock = "binary_shock",
    all_news = "supply_news"
  )
)


# Define a function for processing shock info
process_shock_info <- function(data, rename_columns, extra_select = NULL) {
  data %>%
    mutate(
      daily_variation = abs(PC1),
      ptstat = (news_per_day - moving_avg_news) / moving_std_news
    ) %>%
    arrange(desc(daily_variation)) %>%
    slice(1:30) %>%
    dplyr::select(Date, daily_variation, news_per_day, ptstat, binary_shock, {{ extra_select }}) %>%
    rename(!!!rename_columns)
}


shock_info_full <- process_shock_info(
  info_full, 
  rename_columns = c(
    ptstat_full = "ptstat", 
    news_shock = "binary_shock"
  )
)



shock_info_cluster_small <- process_shock_info(
  info_small_cluster,
  rename_columns = c(
    ptstat_smcluster = "ptstat",
    smcluster_shock = "binary_shock",
    small_clustered_news = "news_per_day"
  )
)

shock_info_high_cluster <- process_shock_info(
  info_high_cluster, 
  rename_columns = c(
    ptstat_hicluster = "ptstat", 
    hicluster_shock = "binary_shock", 
    high_clustered_news = "news_per_day"
  )
)

shock_info_selected <- process_shock_info(
  info_selected, 
  rename_columns = c(
    ptstat_selected = "ptstat", 
    selected_shock = "binary_shock", 
    selected_news = "news_per_day"
  )
)


info_compare <- merge(shock_info_supply, shock_info_full)
info_compare <- merge(info_compare, shock_info_cluster_small)
info_compare <- merge(info_compare, shock_info_high_cluster)
info_compare <- merge(info_compare, shock_info_selected)
info_compare <- merge(info_compare, shock_info_all)

count_news_shocks <- info_compare %>% 
  summarize(count_full = sum(news_shock),
            count_supply = sum(supply_shock),
            count_smcluster = sum(smcluster_shock),
            count_hicluster = sum(hicluster_shock),
            count_selected = sum(selected_shock),
            count_all = sum(all_shock)
            )
count_news_shocks 



# compare the respective variations for instrumnet and none instrumnet days

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
options(warn = -1)
gas_data_shock <- read_parquet("gas_data_shock.parquet")


fluct_abs_gas_avg <- gas_data_shock %>% 
  filter(Date %in% demand_news_days) %>%
  pull(PC1) %>% 
  sd(na.rm = TRUE)



# Define a function for summarizing fluctuations
summarize_fluctuations <- function(data) {
  data %>%
    filter(is.na(instrumented_day) == FALSE) %>% 
    group_by(instrumented_day) %>%
    summarise(
      sd = sd(PC1, na.rm = TRUE),
      avg = mean(PC1, na.rm = TRUE)
    )
}


#nota bene
#on voit en regardant les demand shocks que les variations de PC1
#sont assez différentes des variations de mon precedent shock_serie


# Apply the function to each dataset
fluct_full <- summarize_fluctuations(info_full)
fluct_supply <- summarize_fluctuations(info_supply)
fluct_small_cluster <- summarize_fluctuations(info_small_cluster)
fluct_high_cluster <- summarize_fluctuations(info_high_cluster)
fluct_selected <- summarize_fluctuations(info_selected)
fluct_all <- summarize_fluctuations(info_all)

#récupérer le nombre de jours parmi les 30 avec les plus fortes variations
#qui sont inclus dans insrtumented_days



high_vol_dates <- gas_data_shock %>% 
  arrange(desc(abs(PC1))) %>% 
  slice(1:30) %>%
  pull(Date)



setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
events_data <- read.csv("alex_gassani_shocks.csv")
events_data <- events_data %>% 
  mutate(Date = as.Date(Date)) %>% 
  arrange(Date)
alex_gassani_benchmark <- left_join( events_data,gas_data_shock,
                                by = c("Date"))
fluct_alex_gassani <- alex_gassani_benchmark %>%
  summarise(
    sd = sd(PC1, na.rm = TRUE),
    avg = mean(PC1, na.rm = TRUE)
  )





# Sample DataFrame
data <- data.frame(
  Category = c("Naive News", "Supply News", "Small Clustering", "High Clustering", "Subset Cluster", "All News"),
  Normal = rep(0, 6),
  `Demand Shock` = rep(0, 6),
  `Supply shock` = rep(0, 6),
  `nSupply Days` = round(supply_shock_counts,1) ,
  `ntop variation days` = round(as.numeric(count_news_shocks),1)
)


data[, 2:4] <-round(t(data.frame(
  fluct_full$sd, fluct_supply$sd, fluct_small_cluster$sd, fluct_high_cluster$sd, 
  fluct_selected$sd, fluct_all$sd)), 2)

rownames(data) <- data$Category
data <- data %>% 
  dplyr::select(-Category)

# Transposer les données tout en conservant les noms de lignes comme une colonne
transposed_data <- as.data.frame(t(data))
transposed_data <- cbind(Label = rownames(transposed_data), transposed_data)
rownames(transposed_data) <- NULL

getwd()
library(gt)
gt_table <- transposed_data %>%
  gt() %>%
  tab_row_group(
    group = "Fluctuations",
    rows = 1:3 # Rows 1 to 3 for the first section
  ) %>%
  tab_row_group(
    group = "Description",
    rows = 4:5 # Rows 4 to 6 for the second section
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "gray", weight = px(2)),
    locations = cells_row_groups(groups = everything())
  ) %>%
  tab_options(
    table.width = pct(100)
  )%>%
  gtsave("compare_clustering_days.html")

# à comparer avec alexandre et gassani qui font mieux que nous sur la détection
#mais à la main sans framework automatiser
# manque de reproductibiolité

