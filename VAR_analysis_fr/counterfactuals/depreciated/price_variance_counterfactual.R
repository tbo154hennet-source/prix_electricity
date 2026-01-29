library(dplyr)
library(fixest)
library(ggplot2)
library(tidyr)
library(purrr)
library(data.table)
library(arrow)
library(readxl)

output_dir <- "Z:/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)
price_indispo_merge_fit <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")%>% 
  mutate(
    year  = as.numeric(as.character(year)),
    month = as.numeric(as.character(month))
  )

setwd("Z:/prix_electricite_gitlab/VAR_analysis_fr/counterfactuals")
carbon_price_data <- read_excel("D2E_carbone.xlsx") %>% 
  rename(carbon_price = value) %>% 
  mutate(time = as.Date(time))
  
  # read.csv("icap-euets-price-data-2015-01-01-2025-09-25.csv", skip =1) %>% 
  # mutate(Date = as.Date(Date))

#pour l'instant carbon price ne va que jusqu'en 2018
#Une seule année de données puisque données micro Espagne commencent en 2018

all_dates <- seq.Date(min(carbon_price_data$time),
                      max(carbon_price_data$time),
                      by = "day")

carbon_price_data_complete <- carbon_price_data %>%
  # Créer toutes les dates
  complete(time = all_dates) %>%
  # Remplir les NA par la dernière valeur connue (backfill)
  fill(carbon_price , .direction = "up")

price_indispo_merge_fit <- merge(price_indispo_merge_fit,
                                 carbon_price_data_complete %>%  
                                   dplyr::select(time, carbon_price),
                                 by.x = "delivery_date", by.y ="time")


#Add wind capacity part

path = 'Z:/prix_electricite_gitlab'
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


price_indispo_merge_fit <- price_indispo_merge_fit %>%
  mutate(year = year(delivery_date),
		 month = month(delivery_date)) %>%
  left_join(wind_monthly %>%
			  mutate(year = year(date),
					 month = month(date)) %>%
			  dplyr::select(year, month, Wind_Offshore, Wind_Onshore, Wind) %>%
			  rename(wind_capacity = Wind,
					 wind_offshore_capacity = Wind_Offshore,
					 wind_onshore_capacity  = Wind_Onshore),
			by = c("year", "month")
  )%>%
  mutate(ifelse(is.na(wind_capacity), 0, wind_capacity),
		 ifelse(is.na(wind_offshore_capacity), 0, wind_offshore_capacity),
		 ifelse(is.na(wind_onshore_capacity), 0, wind_onshore_capacity)
  )


colnames(price_indispo_merge_fit)

library(fixest)
formula_str <- "asinh(price) ~  Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity+
    #Uneffective.Unavailable.Capacity + 
    Published.Unavailable.Nuclear.Capacity+
    Unpublished.Unavailable.Nuclear.Capacity +
    #Uneffective.Unavailable.Nuclear.Capacity+
    #wind_speed_100m_supply * wind_offshore_capacity + 
	log(wind_onshore_capacity)*log(wind_speed_100m_supply) +
	 Fore_Solar +nuclear+thermal_tot + hydro_tot+ spot_gas +
     asinh(gen_forecast_net) + asinh(load_tot)+
    temperature_demand + precipitation_demand  + carbon_price +
	as.factor(year) + as.factor(weekday) + as.factor(creneau)"

equation <- as.formula(formula_str)
mpw <- feols(equation, data = price_indispo_merge_fit,
DK(24) ~datetime)

etable(
  mpw,
  keep = "wind",        # ne garde que les variables contenant "wind"
  dict = c(
    "Fore_Wind" = "Wind forecast",
    "Wind"      = "Installed wind capacity"
  )
)

# Spain aims to reach approximately 62 GW of total wind power capacity by 2030, 
# including around 3 GW of offshore wind.
# Spain targets a total wind power capacity of 62 GW by 2030 
# (including 3 GW of offshore wind, up from 50 GW and 1 GW, respectively)
#76 GW of solar PV (up from 39 GW) (including 19 GW of auto-consumption)

#suppose we double, we are in 2030 and we have 62 GW of wind capacity
#how does it translate into price uncertainty
#safety check, compare variance

price_indispo_merge_fit_new <- copy(price_indispo_merge_fit)
price_indispo_merge_fit_new$wind_onshore_capacity <- 62000
unique(price_indispo_merge_fit_new$wind_capacity)

vars_in_model <- all.vars(as.formula(formula_str))
na_counts <- price_indispo_merge_fit %>%
  dplyr::select(all_of(vars_in_model)) %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = "variable",
                      values_to = "n_na") %>%
  arrange(desc(n_na))

na_counts

price_indispo_merge_fit$price_fit <- predict(mpw, newdata = price_indispo_merge_fit)
price_indispo_merge_fit$price_counterfactual <- predict(mpw, newdata = price_indispo_merge_fit_new)

colSums(is.na(price_indispo_merge_fit))


monthly_price_stats <- unique(price_indispo_merge_fit %>%
  mutate(year  = year(datetime),
         month = month(datetime)) %>%
		#  filter(year < 2025) %>%
  group_by(year, month) %>%
  summarise(
    price_mean     = log(weighted_mean(price, load_tot)),
    price_var      = var(log(weighted_mean(price, load_tot)),      na.rm = TRUE),
    price_fit_mean = weighted_mean(price_fit, load_tot, na.rm = TRUE),
    price_fit_var  = var(price_fit,  na.rm = TRUE),
	price_counterfactual_mean     = weighted_mean(price_counterfactual, load_tot, na.rm = TRUE),
    price_counterfactual_var      = var(price_counterfactual,      na.rm = TRUE),
    date = as.Date(paste(year, month, "01", sep = "-")),
    .groups = "drop"
  ) %>%
  mutate(
    price_sd     = sqrt(price_var),
    price_fit_sd = sqrt(price_fit_var),
	price_counterfactual_sd = sqrt(price_counterfactual_var)
  )%>%
  mutate(
    # décalage pour égaliser les moyennes
    cf_mean_shift = mean(price_fit_mean, na.rm = TRUE) -
                    mean(price_counterfactual_mean, na.rm = TRUE),

    price_counterfactual_mean_adj = price_counterfactual_mean + cf_mean_shift
  ))%>%
  group_by(year) %>%
  mutate(
    # décalage annuel
    cf_mean_shift_year =
      mean(price_fit_mean, na.rm = TRUE) -
      mean(price_counterfactual_mean, na.rm = TRUE),

    # contre-factuel recentré par année
    price_counterfactual_mean_adj_bis =
      price_counterfactual_mean + cf_mean_shift_year
  ) %>%
  ungroup()
  



ggplot(monthly_price_stats, aes(x = date)) +
  geom_line(aes(y = price_mean,     color = "Observed price"), alpha = 0.7) +
  #geom_line(aes(y = price_fit_mean, color = "Fitted price"), alpha = 0.7) +
#   geom_line(aes(y = price_counterfactual_mean_adj,
  geom_line(aes(y = price_counterfactual_mean,
   color = "Counterfactual price"), alpha = 0.7) +
#    geom_line(aes(y = price_counterfactual_mean_adj,
#    color = "Counterfactual price Adj"), alpha = 0.7) +
#    geom_line(aes(y = price_counterfactual_mean_adj_bis,
#    color = "Counterfactual price bis"), alpha = 0.7) +
#   geom_line(aes(y = price_sd,     color = "Observed price SD"), alpha = 0.7) +
  geom_line(aes(y = price_fit_sd, color = "Fitted price SD"), alpha = 0.7) +
  #geom_line(aes(y = price_counterfactual_sd, color = "Counterfactual price SD"), alpha = 0.7) +
  labs(
    title = "Monthly Mean Electricity Price: Observed vs Counterfactual",
    x = "Date",
    y = "Price (EUR/MWh)",
    color = ""
  ) +
  theme_minimal()


write_parquet(monthly_price_stats,
			  "price_variance_counterfactual_esp_bis.parquet")

