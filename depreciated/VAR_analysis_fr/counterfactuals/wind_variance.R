library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)

#decrease price unifrmly wothout additional uncertianty at the monthly level
path = 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab'
setwd(path)

installed_generation_capacity <- read.csv('installed_generation_capacity_ES_2015_2025.csv')
colnames(installed_generation_capacity)[1] <- 'date'

installed_generation_capacity$Wind <- with(installed_generation_capacity, ifelse(
  is.na(`Wind.Offshore`) & is.na(`Wind.Onshore`),
  NA_real_,
  rowSums(installed_generation_capacity[, c("Wind.Offshore", "Wind.Onshore")], na.rm = TRUE)
))

wind_annual <- installed_generation_capacity %>% 
  dplyr::select(date, `Wind.Offshore`, `Wind.Onshore`, Wind) %>% 
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    Wind_Offshore = mean(`Wind.Offshore`, na.rm = TRUE),
    Wind_Onshore  = mean(`Wind.Onshore`,  na.rm = TRUE),
    Wind          = mean(Wind,            na.rm = TRUE),
    .groups = "drop"
  )

monthly_grid <- tibble(
  date = seq.Date(
    from = as.Date(paste0(min(wind_annual$year), "-01-01")),
    to   = as.Date(paste0(max(wind_annual$year), "-12-01")),
    by   = "month"
  )
) %>%
  mutate(year = year(date))

wind_monthly <- monthly_grid %>%
  left_join(wind_annual, by = "year") %>%
  arrange(date) %>%
  mutate(
    Wind_Offshore = zoo::na.approx(Wind_Offshore, na.rm = FALSE),
    Wind_Onshore  = zoo::na.approx(Wind_Onshore,  na.rm = FALSE),
    Wind          = zoo::na.approx(Wind,          na.rm = FALSE)
  )


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge <- read_parquet("price_indispo_merge_esp.parquet")


daily_var <- price_indispo_merge %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    var_spot_price    = var(price, na.rm = TRUE),
    var_wind_forecast = var(Fore_Wind, na.rm = TRUE),
    .groups = "drop"
  )

monthly_var <- price_indispo_merge %>%
  mutate(year = year(datetime),
         month = month(datetime)) %>%
  group_by(year, month) %>%
  summarise(
    var_spot_price    = var(price, na.rm = TRUE),
    var_wind_forecast = var(Fore_Wind, na.rm = TRUE),
    .groups = "drop"
  )

annual_var <- price_indispo_merge %>%
  mutate(year = year(datetime)) %>%
  group_by(year) %>%
  summarise(
    var_spot_price    = var(price, na.rm = TRUE),
    var_wind_forecast = var(Fore_Wind, na.rm = TRUE),
    .groups = "drop"
  )

monthly_from_daily_var <- daily_var %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  summarise(
    mean_daily_var_spot_price    = mean(var_spot_price, na.rm = TRUE),
    mean_daily_var_wind_forecast = mean(var_wind_forecast, na.rm = TRUE),
    .groups = "drop"
  )

all_variances <- monthly_var %>%                 # variance intra-mois (calculée sur toutes obs du mois)
  left_join(monthly_from_daily_var, by = c("year", "month")) %>%   # moyenne des variances journalières dans le mois
  left_join(annual_var, by = "year", suffix = c("_monthly", "_annual")) %>%
  arrange(year, month)


ggplot(data = all_variances, aes(x = as.Date(paste(year, month, "15", sep = "-")))) +
  geom_line(aes(y = sqrt(var_spot_price_monthly), color = "Monthly Std")) +
  geom_line(aes(y = sqrt(mean_daily_var_spot_price), color = "Mean Daily Std")) +
  geom_line(aes(y = sqrt(var_spot_price_annual), color = "Annual Std")) +
  labs(title = "Spot Price Stds Over Time",
	   x = "Date",
	   y = "Std",
	   color = "Std Type") +
  theme_minimal()

colnames(all_variances)
ggplot(data = all_variances, aes(x = as.Date(paste(year, month, "15", sep = "-")))) +
geom_line(aes(y = sqrt(var_spot_price_monthly), color = "Monthly Price Std")) +
geom_line(aes(y = sqrt(var_spot_price_annual), color = "Annual Price Std")) +
geom_line(aes(y = sqrt(var_wind_forecast_monthly)/100, color = "Monthly Wind Std")) +
  geom_line(aes(y = sqrt(var_wind_forecast_annual)/100, color = "Annual Std")) +
labs(title = "Spot Price Stds Over Time",
	x = "Date",
	y = "Std",
	color = "Std Type") +
theme_minimal()


weighted_mean <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    ok <- is.finite(x) & is.finite(w)
    x <- x[ok]; w <- w[ok]
  }
  if (length(x) == 0 || sum(w) == 0) return(NA_real_)
  sum(x * w) / sum(w)
}

monthly_VAR_data <- price_indispo_merge %>%
  mutate(month = month(datetime),
         year  = year(datetime)) %>%
  group_by(year, month) %>%
  summarise(
    across(
      c(price, load_forecast, load_tot, gen_forecast,
        Fore_Solar, Fore_Wind, wind_speed_100m_supply,
        temperature_demand, heat_excess_demand, cold_excess_demand, temperature_discomfort_demand,
        spot_gas),
      list(
        mean = ~weighted_mean(.x, load_tot, na.rm = TRUE),
        var  = ~var(.x,  na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    last_Date = max(datetime),
    .groups = "drop"
  ) %>%
  rename(spot_price_mean = price_mean,
         spot_price_var  = price_var) %>%
  arrange(year, month)

log(monthly_VAR_data$spot_price_mean)

all_variance_data <- left_join(all_variances, monthly_VAR_data , by= c('month', 'year')) %>%
  arrange(last_Date)

model_var <- lm(data = all_variance_data,
   sqrt(var_spot_price_monthly) ~ sqrt(var_wind_forecast_monthly) +
    sqrt(load_forecast_var) + sqrt(temperature_demand_var)+
	factor(year) + factor(month) )
summary(model_var)

model_var_bis <- lm(data = all_variance_data,
   sqrt(var_spot_price_monthly) ~ Fore_Wind_mean + 
   sqrt(load_forecast_var) + sqrt(temperature_demand_var)+
	factor(year) + factor(month) )

summary(model_var_bis)

#price avec une plus grande variance de l'éolien
#keep the same average price level but more variance from wind



#not accounting for adaptation form more uncertainty on profitability

compare_data <- left_join(monthly_VAR_data, 
wind_monthly %>% mutate(month = month(date)) ,
 by = c('year', 'month')) %>%
  arrange(year, month)%>%
  mutate(wind_fit = wind_speed_100m_supply_mean * Wind /100)

compare_data %>%
  select(Fore_Wind_mean, wind_fit)%>%
  cor(use = "complete.obs")

#illustre probablement le problème de curtailment 
#se gère surement en conditionnant sur les prix

model_wind <- lm(data = compare_data,
	Fore_Wind_mean ~ wind_fit + factor(year) + factor(month)
    )
summary(model_wind)

colnames(compare_data)

ggplot(data = compare_data, aes(x = as.Date(paste(year, month, "15", sep = "-")))) +
geom_line(aes(y = Fore_Wind_mean, color = "Wind forecast")) +
geom_line(aes(y = wind_speed_100m_supply_mean * Wind /100, color = "Fit Wind")) +
labs(title = "Spot Price Stds Over Time",
	x = "Date",
	y = "Std",
	color = "Std Type") +
theme_minimal()


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/VAR_analysis_fr/counterfactuals/output")
write_parquet(all_variance_data, "all_variance_data.parquet")

