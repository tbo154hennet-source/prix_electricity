
path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/intra_day_prices/OMIE-day-ahead-complement"
files <- list.files(
  path = path,
  pattern = "\\.1$",
  full.names = TRUE
)
library(data.table)
f <- files[1]
library(data.table)

read_omie_file <- function(f) {

  dt <- fread(
    f,
    sep = ";",
    skip = 1,
    fill = TRUE,
    showProgress = FALSE
  )

  # Supprimer la ligne "*"
  dt <- dt[!V1 %in% "*"]

  # Nommer les colonnes (certain fichiers ont une colonne vide en plus)
  setnames(
    dt,
    c("year", "month", "day", "period", "price_es", "price_pt", "none")
  )

  # Conversion en numérique
  dt[, `:=`(
    year  = as.integer(year),
    month = as.integer(month),
    day   = as.integer(day),
    period = as.integer(period),
    price_es = as.numeric(price_es),
    price_pt = as.numeric(price_pt)
  )]

  # Date
  dt[, date := as.Date(paste(year, month, day, sep = "-"))]

  # 🔁 Uniformisation quart-horaire → horaire
  if (nrow(dt) == 96) {
    dt[, hour := ceiling(period / 4)]
  } else {
    dt[, hour := period]
  }

  # Agrégation horaire (robuste si mélange)
  dt_hourly <- dt[
    , .(
      price_es = mean(price_es, na.rm = TRUE),
      price_pt = mean(price_pt, na.rm = TRUE)
    ),
    by = .(date, hour)
  ]

  # Datetime (OMIE : heure 1 = 00:00–01:00)
  # Heure de début H-1
	dt_hourly[, datetime := as.POSIXct(
	paste(date, sprintf("%02d:00:00", hour - 1)),
	format = "%Y-%m-%d %H:%M:%S",
	tz = "Europe/Madrid"
	)]


  setorder(dt_hourly, datetime)

  dt_hourly[]
}


omie_prices <- rbindlist(
  lapply(files, read_omie_file),
  use.names = TRUE,
  fill = TRUE
)
head(omie_prices)
tail(omie_prices)
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(
  omie_prices, "omie_day_ahead_prices.parquet")


#comparer avec la source ENTSOE

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/day_ahead_prices/esp")
# Lister tous les fichiers dans le dossier avec l'extension .parquet
parquet_files <- list.files(pattern = "\\.parquet$")
# Lire tous les fichiers parquet et les fusionner dans un seul dataframe
# Liste des noms de colonnes souhaités
colnames_target <- c( "price", "datetime")

# Lecture + renommage dans lapply
day_ahead_prices <- parquet_files %>%
  lapply(function(file) {
    df <- read_parquet(file)
    names(df) <- colnames_target
    return(df)
  }) %>%
  bind_rows() %>%
  arrange(datetime)


day_ahead_prices <- day_ahead_prices %>% 
  mutate(year = as.character(year(datetime)),
         month = as.integer(month(datetime)),
         day = as.integer(day(datetime)),
         delivery_date = as.Date(datetime),
         price = as.numeric(price)
         ) %>% 
  #filter(delivery_date >= as.Date('2018-01-01')) %>% 
  arrange(datetime)

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(
  day_ahead_prices, "esp_day_ahead_prices.parquet")

library(ggplot2)

omie_prices[, datetime := as.POSIXct(
	paste(date, sprintf("%02d:00:00", hour - 1)),
	format = "%Y-%m-%d %H:%M:%S",
	tz = "Europe/Madrid"
	)]

head(day_ahead_prices %>% filter(delivery_date >= as.Date('2023-02-01')))
head(omie_prices %>% filter(date >= as.Date('2023-02-01')))


ggplot()+
	  geom_line(data= omie_prices %>% filter(date >= as.Date('2020-01-01')),
			aes(x=datetime, y=price_es, color="OMIE"))+
  geom_line(data= day_ahead_prices %>% filter(delivery_date >= as.Date('2020-01-01')),
			aes(x=datetime, y=price, color="ENTSOE"))+
  labs(title="Comparaison des prix day-ahead Espagne : OMIE vs ENTSOE",
	   x="Date",
	   y="Prix (€/MWh)")+
  scale_color_manual(name="Source", values=c("OMIE"="blue", "ENTSOE"="red"))+
  theme_minimal()

colnames(day_ahead_prices)
colnames(omie_prices)
day_ahead_prices_all <- rbind(day_ahead_prices %>% 
						   select(datetime, price),
						 omie_prices %>% 
						   select(datetime, price = price_es)  %>% 
						   filter(datetime >= max(day_ahead_prices$datetime))
						 )
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(
  day_ahead_prices_all, "esp_day_ahead_prices_all.parquet")



setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Generation_tot/esp")
# Get all files in the directory
# List only .parquet files in the directory
my_files <- list.files(pattern = "\\.parquet$")
# Read all files into a list of data frames
gen_list <- lapply(my_files, function(file) {
  message(file)  # equivalent to print
  read_parquet(file)
})

# Combine all data frames and remove duplicates
gen_data <- unique(do.call(rbind, gen_list))
# Rename columns
colnames(gen_data) <- c('gen_tot_da', 'gen_tot_da_conso', 'datetime')
gen_data$gen_tot_da_net <- gen_data$gen_tot_da -  gen_data$gen_tot_da_conso 

gen_data <- gen_data %>% 
  filter(datetime <= threshold_date)
threshold_date_gen <- gen_data %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(n_obs = n_distinct(datetime), .groups = "drop") %>%
  filter(n_obs > 30) %>%          # quart-horaire (ou plus fin)
  summarise(threshold_date = min(date)) %>%
  pull(threshold_date)


#attention gen_data is wrong for the last 3 months
#not provided

library(data.table)
dt <- as.data.table(gen_data)
dt_hourly_after_threshold <- dt[
  datetime >= threshold_date_gen,
  .(
    gen_tot_da       = mean(gen_tot_da, na.rm = TRUE),
    gen_tot_da_conso = mean(gen_tot_da_conso, na.rm = TRUE),
    gen_tot_da_net   = mean(gen_tot_da_net, na.rm = TRUE)
  ),
  by = .(
    datetime = as.POSIXct(
      paste(as.Date(datetime),
            sprintf("%02d:00:00", hour(datetime))),
      tz = "Europe/Madrid"
    )
  )
]
dt_hourly <- dt[ datetime <= threshold_date_gen,]

gen_data <- rbind( dt_hourly,
        dt_hourly_after_threshold
        ) 

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(gen_data, "esp_generation_day_ahead.parquet")



# #extend the generation data from supply and demand curve 2026
setwd("Z:/prix_electricite_gitlab/supply_demand_curve/outputs")
reconstruct_full_curve_shift_supply <- read_parquet('reconstruct_full_curve_shift_qty_supply_matched.parquet')

head(reconstruct_full_curve_shift_supply)
tail(reconstruct_full_curve_shift_supply)
colnames(reconstruct_full_curve_shift_supply)
library(data.table)

# Détecter quart-horaire
reconstruct_full_curve_shift_supply[, is_quarter := grepl("Q[1-4]$", Hour)]
reconstruct_full_curve_shift_supply[, hour := sub("Q[1-4]$", "", Hour)]
reconstruct_full_curve_shift_supply[, hour_num := as.integer(sub("^H", "", hour))]

threshold_date <- reconstruct_full_curve_shift_supply[is_quarter == FALSE, max(DATE)]

dt_quarter <- reconstruct_full_curve_shift_supply[
  is_quarter == TRUE
]

dt_hourly <- reconstruct_full_curve_shift_supply[
  is_quarter == FALSE
]

dt_quarter_hourly <- dt_quarter[
  ,
  .(
    QTY = mean(QTY, na.rm = TRUE)
  ),
  by = .(DATE, hour, hour_num, OFFER_TYPE, MATCHED, PRICE)
]
head(dt_quarter_hourly)
tail(dt_quarter_hourly)
dt_final <- rbindlist(
  list(dt_hourly, dt_quarter_hourly),
  use.names = TRUE,
  fill = TRUE
)
tail(dt_final)


auction_summary <- dt_final[
  , .SD[QTY == max(QTY, na.rm = TRUE)][which.min(PRICE)], 
  by = .(hour_num, DATE)
]

head(auction_summary)
# Supposons que ton dt s'appelle `dt`
auction_summary[, datetime := as.POSIXct(
  paste(DATE, sprintf("%02d:00:00", hour_num - 1)),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Europe/Madrid"
)]
tail(auction_summary)
ggplot()+
  geom_line(data= omie_prices %>% filter(date >= as.Date('2020-01-01')),
			aes(x=datetime, y=price_es, color="OMIE"))+
  geom_line(data= auction_summary %>% filter(as.Date(datetime) >= as.Date('2020-01-01')),
            aes(x=datetime, y=PRICE, color="Curve"))+
              geom_vline(
    xintercept = as.POSIXct(threshold_date),
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  labs(title="",
       x="Date",
       y="MWh")+
  scale_color_manual(name="Lignes", values=c("OMIE"="green", "Curve"="orange"))+
  theme_minimal()


ggplot()+
  geom_line(data= gen_data %>% filter(datetime >= as.Date('2020-01-01')),
			aes(x=datetime, y=gen_tot_da_net, color="OMIE"))+
  geom_line(data= auction_summary %>% filter(as.Date(datetime) >= as.Date('2020-01-01')),
            aes(x=datetime, y=QTY, color="Curve"))+
              geom_vline(
    xintercept = as.POSIXct(threshold_date),
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  labs(title="",
       x="Date",
       y="MWh")+
  scale_color_manual(name="Lignes", values=c("OMIE"="green", "Curve"="orange"))+
  theme_minimal()

en_merge <- merge(gen_data %>% 
       filter(datetime >= as.Date('2020-01-01')),
     auction_summary %>%
       filter(as.Date(datetime) >= as.Date('2020-01-01')),
     by = "datetime"
     )
en_merge%>%
select( gen_tot_da, gen_tot_da_net, QTY) %>%
  cor(use = "complete.obs")

dt_before <- gen_data[
datetime <= threshold_date
]

dt_after <- auction_summary[
  datetime > threshold_date
]

window_days <- 7

ref_before <- dt_before %>%
  filter(datetime >= threshold_date - days(window_days))

ref_after <- dt_after %>%
  filter(datetime <= threshold_date + days(window_days))

scale_factor <- mean(ref_before$gen_tot_da_net, na.rm = TRUE) /
                mean(ref_after$QTY,  na.rm = TRUE)

dt_after_aligned <- dt_after %>%
  mutate(
    QTY     = QTY     * scale_factor
  )
colnames(dt_before)
colnames(dt_after_aligned)
gen_data_aligned <- bind_rows(
  dt_before ,
  dt_after_aligned %>%
  transmute(
    datetime = datetime,
    gen_tot_da_net   = QTY,
    gen_tot_da       = NA_real_,
    gen_tot_da_conso = NA_real_
  )
) %>%
  arrange(datetime)
tail(gen_data_aligned)

en_merge <- en_merge %>%
  mutate(
    QTY     = QTY     * scale_factor
  )
ggplot()+
  geom_line(data= gen_data %>% filter(datetime >= as.Date('2020-01-01')),
			aes(x=datetime, y=gen_tot_da_net, color="OMIE"))+
  geom_line(data= en_merge %>% filter(as.Date(datetime) >= as.Date('2020-01-01')),
            aes(x=datetime, y=QTY, color="Curve"))+
              geom_vline(
    xintercept = as.POSIXct(threshold_date),
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +
  labs(title="",
       x="Date",
       y="MWh")+
  scale_color_manual(name="Lignes", values=c("OMIE"="green", "Curve"="orange"))+
  theme_minimal()

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(
  gen_data_aligned, "esp_generation_day_ahead_all.parquet")


