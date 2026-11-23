#install.packages("fredr")
library(fredr)
library(data.table)
library(dplyr)
library(tidyr)
library(arrow)
library(purrr)
library(lubridate)

fredr_set_key("1722e3f225675ba35e77060cb2378d62")


fred_codes <- c(
  ffr        = "ECBMRRFR",
  NE_Index   = "00XE00FRM086NEST",
  E_Index    = "ENRGY0FRM086NEST",
  UnE        = "LRHUTTTTFRM156S",
  Bond_rate  = "IR3TIB01FRM156N",
  Spot_gas   = "PNGASEUUSDM",
  HICP       = "CP0000FRM086NEST",
  CPElec    = "CP0451FRM086NEST"
)

download_fred <- function(code, name) {
  fredr(
    series_id = code,
    observation_start = as.Date("2015-01-01")
  ) %>%
    select(date, value) %>%
    rename(!!name := value)
}

# Télécharger chaque série séparément
fred_list <- imap(fred_codes, download_fred)

# Fusionner par date
VAR_series <- reduce(fred_list, full_join, by = "date") %>%
  rename(observation_date = date) %>%
  arrange(observation_date)

VAR_series <- VAR_series %>%
  mutate(
    year  = year(observation_date),
    month = month(observation_date)
  ) %>%
  group_by(year, month) %>%
  slice_head(n = 1) %>%
  arrange(observation_date)
head(VAR_series)
tail(VAR_series,20)


# library(httr)
# library(jsonlite)
install.packages("rdbnomics")
library(rdbnomics)


# "M.BDM_EUR.IPI.SO.A21-C.INDICE.FM.SO.CVS-CJO.2021",
# series_name: "SA-WDA industrial production index (base 100 in 2021) 

x <- rdb("INSEE", "IPI-2021",
         "M.BDM_EUR.IPI.SO.A21-C.INDICE.FM.SO.CVS-CJO.2021")

# quantity = x$value (selon la fonction, c’est souvent 'value' / 'obs_value')
tail(x)

colnames(VAR_series)
colnames(x)
head(x)$period
head(VAR_series)$observation_date
class(x$period)
class(VAR_series$observation_date)
VAR_series <- merge(
  VAR_series,
  x %>%
	select(period, value) %>%
	rename(observation_date = period, IPI = value),
  by = "observation_date",
  all.x = TRUE
)


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
write_parquet(VAR_series, 'VAR_series_fr_2026.parquet')



