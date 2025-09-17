

save_var_path <- 'Z:/prix_electricite_gitlab/Data_structuring/output'
setwd(save_var_path)
IV_data_esp <- read_parquet( 'IV_data_esp_elas.parquet')

IV_data_fr <- read_parquet( 'IV_data_fr_elas.parquet')

library(dplyr)
library(tidyr)
library(gt)

dates <- c(  
           "2021-01-01", "2021-06-01","2022-01-01","2022-06-01", "2023-01-01", "2023-06-01")

# Espagne
esp <- IV_data_esp %>%
  filter(observation_date %in% dates) %>%
  select(observation_date, spot_price, spot_gas) %>%
  rename(spot_price_esp = spot_price,
         spot_gas_esp = spot_gas)

# France
fr <- IV_data_fr %>%
  filter(observation_date %in% dates) %>%
  select(observation_date, spot_price) %>%
  rename(spot_price_fr = spot_price)

# Fusion
table_data <- fr %>%
  left_join(esp, by = "observation_date") %>%
  arrange(observation_date)

# Tableau joli
table_data %>%
  gt(rowname_col = "observation_date") %>%
  tab_header(
    title = "Spot Prices France & Spain"
  ) %>%
  fmt_number(
    columns = c(spot_price_fr, spot_price_esp, spot_gas_esp),
    decimals = 2
  ) %>%
  cols_label(
    observation_date = "Date",
    spot_price_fr = "Spot Price FR",
    spot_price_esp = "Spot Price ESP",
    spot_gas_esp = "Spot Gas (ESP)"
  )



