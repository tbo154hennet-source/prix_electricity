
library(dplyr)
library(fixest)
library(ggplot2)
library(tidyr)
library(purrr)
library(data.table)
library(arrow)
library(readxl)

output_dir <- file.path(Sys.getenv("REPLICATION_ROOT", "."), "supply_demand_curve/outputs/coef_reg_no_conditioning")
setwd(output_dir)
price_indispo_merge_fit <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")%>% 
  mutate(
    year  = as.numeric(as.character(year)),
    month = as.numeric(as.character(month))
  )


setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "VAR_analysis/data"))
all_variance_data <- read_parquet( "all_variance_data.parquet")

setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "VAR_analysis/data"))
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

path = Sys.getenv("REPLICATION_ROOT", ".")
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






# 1) get all variables used in stage 1
vars1 <- all.vars(
  as.formula("asinh(price) ~ log(wind_speed_100m_supply) + wind_onshore_capacity +
              Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity +
              Published.Unavailable.Nuclear.Capacity + Unpublished.Unavailable.Nuclear.Capacity +
              Fore_Solar + nuclear + thermal_tot + hydro_tot + spot_gas +
              asinh(gen_forecast_net) + asinh(load_tot) +
              temperature_demand + precipitation_demand + carbon_price +
              as.factor(year) + as.factor(weekday) + as.factor(creneau)")
)

# 2) keep only rows with no NA in stage-1 variables
df_cc <- price_indispo_merge_fit %>%
  filter(complete.cases(across(all_of(vars1))))

# 3) stage 1
mp <- feols(
  asinh(price) ~ log(wind_speed_100m_supply) * log(wind_onshore_capacity) +
    Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity +
    Published.Unavailable.Nuclear.Capacity + Unpublished.Unavailable.Nuclear.Capacity +
    Fore_Solar + nuclear + thermal_tot + hydro_tot + spot_gas +
    asinh(gen_forecast_net) + asinh(load_tot) +
    temperature_demand + precipitation_demand + carbon_price +
    factor(year) + factor(weekday) + factor(creneau),
  data = df_cc
)

# 4) stage 2 target: log(resid^2) (add small epsilon to avoid log(0))
df_cc$resid_sq <- resid(mp)^2
eps <- 1e-12
df_cc$resid_sq[1:5]
var_reg <- feols(
  log(resid_sq + 1e-12) ~ log(wind_onshore_capacity) * log(wind_speed_100m_supply),
  data = df_cc
)
var_reg2 <- feols(
  log(resid_sq + 1e-12) ~ log(wind_onshore_capacity) * log(wind_speed_100m_supply)+
    Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity +
    Published.Unavailable.Nuclear.Capacity + Unpublished.Unavailable.Nuclear.Capacity +
    Fore_Solar + nuclear + thermal_tot + hydro_tot + spot_gas +
    asinh(gen_forecast_net) + asinh(load_tot) +
    temperature_demand + precipitation_demand + carbon_price +
    factor(year) + factor(weekday) + factor(creneau),
  data = df_cc
)
etable(var_reg, var_reg2, keep = "wind")

# Afficher un tableau
latex_tab <- etable(var_reg, var_reg2,
                    headers = c("Baseline","Controls"),
                    keep = "wind",
                    tex = TRUE
)
save_latex_path <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "latex_tables")
setwd(save_latex_path)
# Sauvegarde dans un fichier .tex
writeLines(latex_tab, "log_res_variance.tex")

#Conditional on wind speed and other controls, 
#higher installed wind capacity is associated with higher price volatility.
#A one-unit (MWh) increase in wind capacity 
#increases the conditional variance of electricity prices by approximately 0.03%

df_cc$wind_onshore_capacity[1:5]
## keep baseline untouched

df_cc <- df_cc %>%
  mutate(
    wind_cap2 = 62000
  )
price_baseline <- predict(mp, newdata = df_cc)  # log Var(eps_t) baseline
df2 <- df_cc %>% mutate(wind_onshore_capacity = wind_cap2)
price_variance   <- predict(mp, newdata = df2) # log Var(eps_t) under doubled wind


#aggregate at the monthly level


df_cc$fit <- price_baseline
df_cc$counterfactual <- price_variance

monthly_mean <- df_cc %>% 
  group_by(year, month) %>% 
  summarise(
    mean_price = mean(asinh(price), na.rm = TRUE),
    mean_fit   = mean(fit,   na.rm = TRUE),
    mean_cf    = mean(counterfactual,    na.rm = TRUE),
    n = n(),
    date = first(as.Date(paste0(year, "-", month, "-01"))),
    .groups = "drop"
  )

#est ce que 

monthly_mean



# --- 1. Variances horaires ---
var_obs_hour <- var(asinh(df_cc$price), na.rm = TRUE)
var_fit_hour <- var(price_baseline,     na.rm = TRUE)
var_cf_hour  <- var(price_variance,     na.rm = TRUE)

# --- 3. Variances mensuelles ---
var_obs_month <- var(monthly_mean$mean_price, na.rm = TRUE)
var_fit_month <- var(monthly_mean$mean_fit,   na.rm = TRUE)
var_cf_month  <- var(monthly_mean$mean_cf,    na.rm = TRUE)

# --- 4. Table ---
variance_table <- tibble(
  Series  = c("Observed", "Fit", "Counterfactual"),
  Hourly  = c(var_obs_hour, var_fit_hour, var_cf_hour),
  Monthly = c(var_obs_month, var_fit_month, var_cf_month)
)

library(gt)
# --- 5. GT table ---
gt_table <- variance_table %>%
  gt() %>%
  fmt_number(columns = c(Hourly, Monthly), decimals = 2) %>%
  tab_header(
    title = "Log Price Variance") %>% 
  cols_label(
    Series = "Series",
    Hourly = "Hourly Variance",
    Monthly = "Monthly Variance"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gt_table

save_latex_path <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "latex_tables")
setwd(save_latex_path)
gtsave(
  gt_table,
  filename = "price_variance_wind_counterfactual.tex"
)

monthly_mean$mean_cf_adj <- monthly_mean$mean_cf -mean(monthly_mean$mean_cf) + mean(monthly_mean$mean_price)
wind_counterfactuals <- ggplot(monthly_mean, aes(x = date)) +
  geom_line(aes(y = mean_price,     color = "Observed Log-price"), alpha = 0.7) +
  #geom_line(data = IV_data_log_esp, aes(x= observation_date, y = wind_speed_100m, color = "Wind Forecast Log"))+
  #geom_line(aes(y = price_fit_mean, color = "Fitted price"), alpha = 0.7) +
  #   geom_line(aes(y = price_counterfactual_mean_adj,
  geom_line(aes(y =  mean_cf ,
                color = "Counterfactual Log-price"), alpha = 0.7) +
  geom_line(aes(y =  mean_cf -mean(monthly_mean$mean_cf) + mean(monthly_mean$mean_price) ,
                color = " Mean-Adjusted Counterfactual Log-price"), alpha = 0.7) +
  # geom_line(aes(y = price_log_counterfactual_var_mean ,
  # color = "Level Counterfactual"), alpha = 0.7) +
  #   geom_line(aes(y = price_sd,     color = "Observed price SD"), alpha = 0.7) +
  #   geom_line(aes(y = price_counterfactual_ov_sd , color = "Counterfactual OV SD"), alpha = 0.7) +
  #   geom_line(aes(y = price_counterfactual_mv_sd , color = "Counterfactual MV SD"), alpha = 0.7) +
  labs(
    title = "",
    x = "Date",
    y = "Price (EUR/MWh)",
    color = ""
  ) +
  theme_minimal()+
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )

wind_counterfactuals

save_plot_path <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "macro")
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("wind_counterfactuals.png", plot = wind_counterfactuals  , width = 12, height = 6, dpi = 300)





setwd(file.path(Sys.getenv("REPLICATION_ROOT", "."), "day_ahead/outputs"))
write_parquet(monthly_mean ,
			  "price_variance_counterfactual_esp_ter.parquet")






