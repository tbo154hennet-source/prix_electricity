setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "day_ahead/outputs"))
tempall_data_data <- read_parquet("tempall_data_data.parquet")

wind_solar_data <- read_parquet("wind_solar_data.parquet")
#difference is 
# effective wind and solar energy including curtailment
#whereas I might have sold energy from other plants
#wind speed and sun exposition better for potential

colnames(wind_solar_data)
save_path_data <- file.path(Sys.getenv("REPLICATION_ROOT", "."), "day_ahead/outputs/esp")
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet( "price_indispo_merge_esp.parquet")
colnames(price_indispo_merge_esp)

price_indispo_merge_esp$year <- year(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$month <- month(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)
colnames(price_indispo_merge_esp)

wind_solar_data_tot <- merge(
  wind_solar_data,
  price_indispo_merge_esp %>% 
    dplyr::select(day_ahead_forecast, intraday_update_renewables, unpredicted_renewables,
                  wind_solar_tot, gen_tot,  Fore_Wind, Fore_Solar,year, month,
                  weekday, creneau,datetime),
  by.x = "date", by.y = "datetime"
)
wind_solar_data_tot <- merge(
  wind_solar_data_tot,
  tempall_data_data,
  by = "date"
)



#on essaye d'expliquer la prod de renouvelables avec mes infos et on regarde le R2
#est ce que bien encapsulé

wind_solar_data_tot %>% 
  dplyr::select(wind_speed_100m_supply, wind_speed_100m_demand,
                global_tilted_irradiance_supply, global_tilted_irradiance_demand,
                temperature_ss, temperature_demand) %>% 
  cor()



m_wind <- lm(data =wind_solar_data_tot,
             Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
               global_tilted_irradiance_supply + global_tilted_irradiance_demand)

m_windc <- lm(data =wind_solar_data_tot,
              Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                global_tilted_irradiance_supply + global_tilted_irradiance_demand +
                temperature_demand + precipitation_demand +
                relative_humidity_2m_demand)

# m_windfe <- lm(data =wind_solar_data_tot,
#                Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
#                  global_tilted_irradiance_supply + global_tilted_irradiance_demand +
#                  #temperature_demand + temperature_ss + 
#                  precipitation_demand + relative_humidity_2m_demand + 
#                  as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
# )
m_windfet <- lm(data =wind_solar_data_tot,
               Fore_Wind ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                 global_tilted_irradiance_supply + global_tilted_irradiance_demand +
                 temperature_demand + temperature_ss + 
                 precipitation_demand + relative_humidity_2m_demand + 
                 as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

m_solar <- lm(data =wind_solar_data_tot,
              Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                global_tilted_irradiance_supply + global_tilted_irradiance_demand )

m_solarc <- lm(data =wind_solar_data_tot,
               Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                 global_tilted_irradiance_supply + global_tilted_irradiance_demand +
                 temperature_demand + precipitation_demand +
                 relative_humidity_2m_demand)

# m_solarfe <- lm(data =wind_solar_data_tot,
#                 Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
#                   global_tilted_irradiance_supply + global_tilted_irradiance_demand + +
#                   # temperature_demand + temperature_ss +
#                   precipitation_demand +relative_humidity_2m_demand + 
#                   as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
# )
m_solarfet <- lm(data =wind_solar_data_tot,
                Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                  global_tilted_irradiance_supply + global_tilted_irradiance_demand + +
                  temperature_demand + temperature_ss + 
                  precipitation_demand +relative_humidity_2m_demand + 
                  as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)
stargazer(
  #m_wind, 
  m_windc, m_windfet,
  #m_solar, 
  m_solarc, m_solarfet,
  type = "text",
  keep = c(
    "wind_speed_100m_supply",
    "wind_speed_100m_demand",
    "global_tilted_irradiance_supply",
    "global_tilted_irradiance_demand"
  ),
  column.labels = c("Wind",  "Wind FE",
                    "Solar",  "Solar FE"),
  dep.var.labels = c("Production"),
  title = "Regression Results for Wind and Solar Models",
  digits = 3
)

#Good result because we manage to kill the demand variable
#that supply wind speed is the only relevant
#in demand doesn't play
#same for irradiance

#alors même qu'ils sont corrélés respectivement à 90 et 98 %

wind_solar_data_tot<- wind_solar_data_tot %>%
  mutate(
    temp_supply_above25 = pmax(temperature_ss - 25, 0),
    temp_supply_below15 = pmax(15 - temperature_ss, 0),
    
    temp_demand_above25 = pmax(temperature_demand - 25, 0),
    temp_demand_below15 = pmax(15 - temperature_demand, 0),
    rendement = ifelse(global_tilted_irradiance_supply !=0, Fore_Solar  / global_tilted_irradiance_supply, NA)
  )


#mettre en évidence la décroissance du rendement des panneaux solaires


m_sr <- lm(data =wind_solar_data_tot,
           log(rendement +1 ) ~  temp_supply_above25+ temp_supply_below15 +
             global_tilted_irradiance_supply +global_tilted_irradiance_demand + 
             as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

m_sr2 <- lm(data =wind_solar_data_tot%>% mutate(hour = hour(date)) %>% filter(hour >=10 & hour <=18),
            log(rendement +1 ) ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
              global_tilted_irradiance_supply +global_tilted_irradiance_demand + 
              temp_supply_above25+ temp_supply_below15  +temp_demand_above25 + temp_demand_below15+
              precipitation_demand +relative_humidity_2m_demand + 
              as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)
m_sr3 <- lm(data =wind_solar_data_tot,
            log(rendement +1 ) ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
              global_tilted_irradiance_supply +global_tilted_irradiance_demand + 
              temp_supply_above25+ temp_supply_below15  +temp_demand_above25 + temp_demand_below15+
              precipitation_demand +relative_humidity_2m_demand + 
              as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

#Attention le fait de contôler par  global_tilted_irradiance est essentiel

stargazer(
  #m_wind, 
  m_sr, m_sr2,m_sr3,
  type = "text",
  keep = c(
    "wind_speed_100m_supply",
    "wind_speed_100m_demand",
    "global_tilted_irradiance_supply",
    "global_tilted_irradiance_demand",
    "temperature_ss",
    "temp_supply_above25",
    "temp_supply_below15", 
    "temp_demand_above25",
    "temp_demand_below15"
  ),
  column.labels = c("Rendement","Rendement",  "Rendement"),
  dep.var.labels = c("Production"),
  title = "Regression Results for Wind and Solar Models",
  digits = 3
)


m_solarfp1 <- lm(data =wind_solar_data_tot,
                 Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                   global_tilted_irradiance_supply + global_tilted_irradiance_demand + 
                   temp_supply_above25+ temp_supply_below15 +
                   precipitation_demand +relative_humidity_2m_demand + 
                   as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

m_solarfp <- lm(data =wind_solar_data_tot,
                Fore_Solar ~ wind_speed_100m_supply  + wind_speed_100m_demand+ 
                  global_tilted_irradiance_supply + global_tilted_irradiance_demand + 
                  temp_supply_above25+ temp_supply_below15 + temp_demand_above25 + temp_demand_below15+
                  precipitation_demand +relative_humidity_2m_demand + 
                  as.factor(year) + as.factor(month) + as.factor(weekday) + as.factor(creneau)
)

stargazer(
  #m_wind, 
  m_solarfp1, m_solarfp,
  type = "text",
  keep = c(
    "wind_speed_100m_supply",
    "wind_speed_100m_demand",
    "global_tilted_irradiance_supply",
    "global_tilted_irradiance_demand",
    
    "temp_supply_above25",
    "temp_supply_below15", 
    "temp_demand_above25",
    "temp_demand_below15"
  ),
  column.labels = c(
                    "Solar FE",  "Solar FE"),
  dep.var.labels = c("Production"),
  title = "Regression Results for Wind and Solar Models",
  digits = 3
)


save_latex_path <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "latex_tables")
setwd(save_latex_path)

output_file <- file.path(save_latex_path, "shift_share_relevance.tex")

stargazer(
  #m_wind, 
  m_windc, m_windfet,
  #m_solar, 
  m_solarc, m_solarfet,  m_solarfp,
  type = "latex",
  keep = c(
    "wind_speed_100m_supply",
    "wind_speed_100m_demand",
    "global_tilted_irradiance_supply",
    "global_tilted_irradiance_demand",
    
    "temp_supply_above25",
    "temp_supply_below15", 
    "temp_demand_above25",
    "temp_demand_below15"
  ),
  column.labels = c("Wind",  "Wind FE",
                    "Solar",  "Solar FE",  "Solar FE"),
  dep.var.labels = c("Production"),
  title = "Regression Results for Wind and Solar Models",
  digits = 3,
  out = output_file
)




