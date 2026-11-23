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
  IPI_FRED        = "ESPPROINDMISMEI",
  NE_Index   = "00XE00ESM086NEST",
  E_Index    = "ENRGY0ESM086NEST",
  UnE        = "LRHUTTTTESM156S",
  Bond_rate  = "IR3TIB01ESM156N",
  Spot_gas   = "PNGASEUUSDM",
  HICP       = "CP0000ESM086NEST",
  CPElec    = "CP0451ESM086NEST"
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

# # Example query: industrial production index for Spain
# url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,DSD_STES@DF_INDSERV,4.0/ESP.M.PRVM..BTE.Y...?startPeriod=2015-01&format=csvfilewithlabels"
# df<-read.csv(url)
# dim(df)
# df  %>% select(TIME_PERIOD, OBS_VALUE)%>% arrange(desc(TIME_PERIOD)) %>% head(10)
install.packages("ineapir")
library(ineapir)

# # We use the function get_metadata_operations with argument operation
# operation <- get_metadata_operations(operation = "IPI")
# as.data.frame(operation)

# # We use the function get_metadata_variables with argument operation,
# # e.g., operation code = 'IPC'
# variables <- get_metadata_variables(operation = "IPI")
# head(variables)

# # We use the function get_metadata_tables with argument operation
# tables <- get_metadata_tables_operation(operation = "IPI")
# head(tables[,c("Id","Nombre")])

# Using unnest = TRUE
series <- get_data_series(codSeries = "	IPI15382", tip = "A", nlast = 200, unnest = TRUE)
head(series[,c("COD", "Nombre", "Fecha", "Valor")])
class(series$Fecha)
ggplot()+
geom_line(data=series, aes(x=as.Date(Fecha), y=Valor, color="INE"))+
geom_line(data=VAR_series, aes(x=observation_date, y=IPI_FRED, color="fredr"))+
  theme_minimal()

VAR_series <- merge(
  VAR_series,
  series %>%
  mutate(Fecha = as.Date(Fecha)) %>%
	select(Fecha, Valor) %>%
	rename(observation_date = Fecha, IPI = Valor),
  by = "observation_date",
  all.x = TRUE
)

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
write_parquet(VAR_series, 'VAR_series_esp_2026.parquet')



