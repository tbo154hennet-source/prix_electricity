save_compare <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
setwd(save_compare)

#compare the number of instrument days

info_supply <- read_parquet("info_supply.parquet")
info_full <- read_parquet("info_full.parquet")
info_cluster <- read_parquet("info_cluster.parquet")
info_small_cluster <- read_parquet("info_small_cluster.parquet")
info_high_cluster <- read_parquet("info_high_cluster.parquet")
info_selected <- read_parquet("info_selected.parquet")


# Define a function for filtering instrument days
filter_instrument_days <- function(data, threshold_column, threshold = 3) {
  data %>%
    filter(binary_shock == TRUE) %>%
    filter({{ threshold_column }} > threshold)
}

# Apply the function to each dataset
instrument_days_full <- filter_instrument_days(info_full, news_per_day)
instrument_days_supply <- filter_instrument_days(info_supply, supply_news)
instrument_days_cluster <- filter_instrument_days(info_cluster, news_per_day)
instrument_days_small_cluster <- filter_instrument_days(small_info_cluster, news_per_day)
instrument_days_high_cluster <- filter_instrument_days(info_high_cluster, news_per_day)
instrument_days_selected <- filter_instrument_days(info_selected, news_per_day)

nrow(instrument_days_full)
nrow(instrument_days_supply)
nrow(instrument_days_cluster)
nrow(instrument_days_small_cluster)
nrow(instrument_days_high_cluster)
nrow(instrument_days_selected)

#compare the 30 days with highest variation


shock_info_supply <- info_supply %>% 
  mutate(daily_variation = abs(shock_serie),
         ptstat = (supply_news -moving_avg_news)/moving_std_news ) %>% 
  arrange(desc(daily_variation)) %>% 
  slice(1:30) %>% 
  select(Date, daily_variation, supply_news, 
         ptstat, binary_shock) %>% 
  rename(ptstat_supply = ptstat, 
         supply_shock = binary_shock)

# Define a function for processing shock info
process_shock_info <- function(data, rename_columns, extra_select = NULL) {
  data %>%
    mutate(
      daily_variation = abs(shock_serie),
      ptstat = (news_per_day - moving_avg_news) / moving_std_news
    ) %>%
    arrange(desc(daily_variation)) %>%
    slice(1:30) %>%
    select(Date, daily_variation, news_per_day, ptstat, binary_shock, {{ extra_select }}) %>%
    rename(!!!rename_columns)
}


shock_info_full <- process_shock_info(
  info_full, 
  rename_columns = c(
    ptstat_full = "ptstat", 
    news_shock = "binary_shock"
  )
)

shock_info_cluster <- process_shock_info(
  info_cluster, 
  rename_columns = c(
    ptstat_cluster = "ptstat", 
    cluster_shock = "binary_shock", 
    clustered_news = "news_per_day"
  )
)

shock_info_cluster_small <- process_shock_info(
  info_cluster, 
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
info_compare <- merge(info_compare, shock_info_cluster)
info_compare <- merge(info_compare, shock_info_cluster_small)
info_compare <- merge(info_compare, shock_info_high_cluster)
info_compare <- merge(info_compare, shock_info_selected)

info_compare %>% 
  summarize(count_supply = sum(supply_shock),
            count_full = sum(news_shock),
            count_cluster = sum(cluster_shock),
            count_smcluster = sum(smcluster_shock),
            count_hicluster = sum(hicluster_shock),
            count_selected = sum(selected_shock))



high_vol_dates <- shock_info_supply %>% pull(Date)
colnames(info_supply)

View(info_supply %>% 
       filter(Date %in% high_vol_dates) %>% 
       select(Date, supply_news,
              shock_serie, supply_news, day_type,            
              moving_avg_news, moving_std_news, binary_shock))


# compare the respective variations for instrumnet and none instrumnet days

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')
# Import the functions from 'my_functions.R'
Sys.setenv(lang = "en_US")
options(warn = -1)
gas_data_shock <- read_parquet("gas_data_shock.parquet")




fluct_abs_gas_avg <- gas_data_shock %>% 
  pull(log_shock_serie) %>% 
  sd(na.rm = TRUE)


# Define a function for summarizing fluctuations
summarize_fluctuations <- function(data) {
  data %>%
    group_by(binary_shock) %>%
    summarise(
      sd = sd(log_shock_serie, na.rm = TRUE),
      avg = mean(log_shock_serie, na.rm = TRUE)
    )
}

# Apply the function to each dataset
fluct_full <- summarize_fluctuations(info_full)
fluct_supply <- summarize_fluctuations(info_supply)
fluct_cluster <- summarize_fluctuations(info_cluster)
fluct_small_cluster <- summarize_fluctuations(small_info_cluster)
fluct_high_cluster <- summarize_fluctuations(info_high_cluster)
fluct_selected <- summarize_fluctuations(info_selected)







