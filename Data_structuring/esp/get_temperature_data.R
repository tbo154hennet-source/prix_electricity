api_key = "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ0aGliYXVkLmhlbm5ldEBpbnNlZS5mciIsImp0aSI6IjZiYTUwZTNlLWU1ZDAtNGVjYi1hZTMzLTllNGYyMjFjYjMyYiIsImlzcyI6IkFFTUVUIiwiaWF0IjoxNzMzNzQyODg1LCJ1c2VySWQiOiI2YmE1MGUzZS1lNWQwLTRlY2ItYWUzMy05ZTRmMjIxY2IzMmIiLCJyb2xlIjoiIn0.VITminNdwxomfW-fysIUQguO6gITLKLJB54GBHPRa3o"

library(climaemet)
## Get api key from AEMET

## Use this function to register your API Key temporarly or permanently
aemet_api_key(api_key)
aemet_last_obs("9434")
aemet_forecast_hourly()


# Select a city
data("aemet_munic")
library(dplyr)
munis <- aemet_munic %>%
  filter(municipio_nombre %in% c("Santiago de Compostela", "Lugo")) %>%
  pull(municipio)
daily <- aemet_daily_period_all(munis)
daily <- aemet_forecast_hourly(munis)
obs <- aemet_daily_clim(c("9434", "3195"))
# Metadata
meta <- aemet_forecast_daily(munis, extract_metadata = TRUE)
glimpse(meta$campos)
obs <- aemet_monthly_clim(station = c("9434", "3195"), year = 2000)


# Vars available
aemet_forecast_vars_available(daily)


# This is nested
daily %>%
  select(municipio, fecha, nombre, temperatura)

# Select and unnest
daily_temp <- aemet_forecast_tidy(daily, "temperatura")

# This is not
daily_temp

# Wrangle and plot
daily_temp_end <- daily_temp %>%
  select(
    elaborado, fecha, municipio, nombre, temperatura_minima,
    temperatura_maxima
  ) %>%
  tidyr::pivot_longer(cols = contains("temperatura"))

# Plot
library(ggplot2)
ggplot(daily_temp_end) +
  geom_line(aes(fecha, value, color = name)) +
  facet_wrap(~nombre, ncol = 1) +
  scale_color_manual(
    values = c("red", "blue"),
    labels = c("max", "min")
  ) +
  scale_x_date(
    labels = scales::label_date_short(),
    breaks = "day"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(suffix = "º")
  ) +
  theme_minimal() +
  labs(
    x = "", y = "",
    color = "",
    title = "Forecast: 7-day temperature",
    subtitle = paste(
      "Forecast produced on",
      format(daily_temp_end$elaborado[1], usetz = TRUE)
    )
  )


# Spatial with mapSpain
library(mapSpain)
library(sf)

lugo_sf <- esp_get_munic(munic = "Lugo") %>%
  select(LAU_CODE)

daily_temp_end_lugo_sf <- daily_temp_end %>%
  filter(nombre == "Lugo" & name == "temperatura_maxima") %>%
  # Join by LAU_CODE
  left_join(lugo_sf, by = c("municipio" = "LAU_CODE")) %>%
  st_as_sf()

ggplot(daily_temp_end_lugo_sf) +
  geom_sf(aes(fill = value)) +
  facet_wrap(~fecha) +
  scale_fill_gradientn(
    colors = c("blue", "red"),
    guide = guide_legend()
  ) +
  labs(
    main = "Forecast: 7-day max temperature",
    subtitle = "Lugo, ES"
  )

