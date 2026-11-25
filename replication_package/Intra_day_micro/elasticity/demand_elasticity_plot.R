library(ggplot2)
output_dir <- file.path(Sys.getenv("REPLICATION_ROOT", "."), "supply_demand_curve/outputs/coef_reg_no_conditioning")
setwd(output_dir)
price_indispo_merge_esp_npc <- read_parquet("price_indispo_merge_esp_npc_all_final_2026.parquet")

wind_reg_data <- read_parquet(file.path(Sys.getenv("REPLICATION_ROOT", "."), "Intra_day_micro/outputs/wind_reg_data.parquet")) %>%
  mutate(
    price_diff_0to5        = price_diff_0to1 + price_diff_1to5,
    right_term_demand_0to5 = price_diff_0to5 * load_tot_da / price_da,
    qty_0to5               = qty_1to5 + qty_first_1
  )

ggplot(price_indispo_merge_esp_npc %>% 
         filter(year==2020), aes(x=datetime))+
  geom_line(aes(y=(load_tot - load_forecast), color= "Load Forecast Error"))

df_2020 <- price_indispo_merge_esp_npc %>% 
  filter(year == 2020) %>% 
  arrange(datetime) %>%
  mutate( load_error = load_tot - load_forecast, 
          gen_error = gen_tot - gen_forecast_net, 
          # variance glissante sur 30 jours,
          load_error_var_10d = rollapply( load_error, width = 10, FUN = stats::var, align = "right", fill = NA, na.rm = TRUE ),
          gen_error_var_10d = rollapply( gen_error, width = 10, FUN = stats::var, align = "right", fill = NA, na.rm = TRUE )
)

df_2020 <- merge(df_2020,
                 wind_reg_data %>% 
                   dplyr::select(datetime, price_diff_0to5, right_term_demand_0to5, qty_0to5),
                 by = "datetime")


colnames(df_2020)

df_2020_daily_var <- price_indispo_merge_esp_npc %>%
  filter(year == 2020) %>%
  mutate(
    date = as.Date(datetime),
    hour = hour(datetime),
    load_error = load_tot - load_forecast,
    gen_error  = gen_tot - gen_forecast_net
    # slot = if_else(hour < 5 | hour >= 23, "night", "day")  # ex: nuit 20h–7h59
  ) %>%
  group_by(date) %>%
  summarise(
    load_error_var = var(load_error, na.rm = TRUE),
    gen_error_var  = var(gen_error,  na.rm = TRUE),
    n_obs = sum(!is.na(load_error) & !is.na(gen_error)),
    .groups = "drop"
  ) %>%
  # tidyr::pivot_wider(
  #   names_from = slot,
  #   values_from = c(load_error_var, gen_error_var, n_obs),
  #   names_glue = "{.value}"
  # ) %>%
  arrange(date)


# Changes in testing criteria for COVID-19.
# 25-02-20 Recommended to individuals who have symptoms, and that
# in the past 14 days have been in one of the risk zones
# (China, South Korea, Japan, Singapore, Iran, and 4 regions in
#   Italy - Lombardy, Veneto, Piamonte and Emilia Romagna).
# Additionally, testing can be performed to those patients with
# severe respiratory symptoms, and that every other cause has
# been ruled out.
# 18-03-20 Person with a clinical case of acute respiratory infection that
# is hospitalized or that complies with clinical criteria to be
# admitted to other hospital; person with a clinical case of
# acute respiratory infection of various of any severity and that
# belongs to the following groups a) sanitary worker, b) other
# social services. The diagnostic test can be performed to a
# person in vulnerable situation with a clinical case of acute
# respiratory infection independent of the severity after
# individual clinical evaluation.
# Rapid test, nevertheless, can be performed to a broader
# population, vulnerable groups and persons with home
# isolation with symptoms.
# 07-04-20 Rapid antibody tests: They can be performed to symptomatic
# patients, moderate or severe in hospitals, or mild symptoms
# in the extra hospital, fundamentally in the residences of
# elderly and sociosanitary centers.




# Période	Niveau	Type de mesure
# 14–15 mars – 21 juin 2020	National	Confinement strict national, état d’alerte ðŸ‡ªðŸ‡¸ðŸ“
# Phases du plan de déconfinement
# 29 mars, tuightening of lockdown, all non essential activity has beeen stopped

#the closure of non-essential activities (1st-14th April).

# 🟡 Phase 0 – « préparation »
# Début : ~2 mai 2020
# 🟢 Phase 1 – « levée progressive »
# Début majoritaire : 11 mai 2020
# 🔵 Phase 2 – « déconfinement intermédiaire »
# Début : ~18–25 mai 2020 (selon régions)
# Octobre 2020	National/Regional	Nouvel état d’alerte national limité pour soutenir les confinements locaux ðŸ‡ªðŸ‡¸ðŸ“
# 4–14 décembre 2020	Madrid (régional)	Confinement géographique Madrid

lockdowns <- data.frame(
  start = as.POSIXct(c(
    "2020-03-15",  # confinement strict
    "2020-03-29",  # confinement strict
    "2020-04-09",  # confinement strict
    "2020-04-15",  # confinement strict
    "2020-05-02",  # phase 0
    "2020-05-11"
  ), tz = "Europe/Madrid"),
  
  end = as.POSIXct(c(
    "2020-03-28",
    "2020-04-08",  # confinement strict
    "2020-04-14",  # confinement strict
    "2020-05-01",
    "2020-05-10",
    "2020-05-24"
  ), tz = "Europe/Madrid"),
  
  phase = c(
    "National lockdown",
    "Lockdown Tightening",
    "Semana Santa",
    "Reopening Non-essential activities",
    "Easing of restrictions",
    "Additional Easing of restrictions"
  )
)
lockdowns$phase <- factor(
  lockdowns$phase,
  levels = c(
    "National lockdown",
    "Lockdown Tightening",
    "Semana Santa",
    "Reopening Non-essential activities",
    "Easing of restrictions",
    "Additional Easing of restrictions"
  )
)

colnames(df_2020_daily_var)
my_plot <- ggplot() +
  
  # bandes de confinement
  geom_rect(
    data = lockdowns,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = phase
    ),
    inherit.aes = FALSE,
    alpha = 0.25
  )+
  
  
  # séries
  geom_line(data = df_2020 %>% 
              filter(as.numeric(month) >= 2 & as.numeric(month) <= 5) %>% 
              group_by(date) %>% 
              summarise(datetime = first(datetime),
                        load_error = mean(load_error, na.rm = TRUE)
                        ), 
            aes(x = datetime, y = load_error, color = "Load Forecast Error")) +
  
  # geom_line(data = df_2020 %>%
  #             filter(as.numeric(month) >= 2 & as.numeric(month) <= 5) %>%
  #             group_by(date) %>%
  #             summarise(datetime = first(datetime),
  #                       qty_0to5 = mean(qty_0to5, na.rm = TRUE)
  #             ),
  #           aes(x = datetime, y = qty_0to5 - mean(qty_0to5), color = "Delta Price DA/ID")) +
  
  geom_line(data = df_2020_daily_var %>% 
              mutate(month = month(date),
                     datetime = as.POSIXct(date)) %>% 
              filter(as.numeric(month) >= 2 & as.numeric(month) <= 5), 
            aes(x = datetime, y = 2*sqrt(load_error_var), color = "Std Load Forecast Error
                ")) +
  # geom_line(data = df_2020_daily_var %>% 
  #             mutate(month = month(date),
  #                    datetime = as.POSIXct(date)
  #                    ) %>% 
  #             filter(as.numeric(month) >= 2 & as.numeric(month) <= 5),
  #           aes(x = datetime, y = 2*sqrt(load_error_var), color = "Std Load Forecast Error Night")) +
  # geom_line(aes(y = -abs(gen_error )/10, color = "Generation Forecast Error")) +
  # 
  # geom_line(
  #   aes(y = -sqrt(gen_error_var_10d), color = "Generation 10-day Rolling Std"),
  #   linewidth = 1
  # ) +
  
  
  labs(
    x = "",
    y = "MW",
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )

my_plot

save_plot_path <- file.path(Sys.getenv("REPLICATION_OUTPUT_DIR", "output"), "intra_day")
setwd(save_plot_path)
# Assume your ggplot object is called 'p'
ggsave("covid_elec_forecast_error.png", plot = my_plot, width = 12, height = 6, dpi = 300)




#justifier le seuil de jours définis



# 1) Construire les erreurs


sd_load_precovid <- price_indispo_merge_esp_npc %>%
  filter(year == 2019) %>%
  arrange(datetime) %>%
  mutate(
    load_error = load_tot - load_forecast,
    gen_error = gen_tot - gen_forecast_net
  ) %>%
  summarise(sd = sd(load_error, na.rm = TRUE)) %>%
  pull(sd)


# 3) Bornes fixes ±1.96σ_annuel
df_2020 <- df_2020 %>%
  mutate(
    load_error_upper =  1.96 * sd_load_precovid,
    load_error_lower = -1.96 * sd_load_precovid,
    # indicateurs d’anormalité
    load_error_abnormal = abs(load_error) > 1.96 * sd_load_precovid,
    gen_error_abnormal  = abs(gen_error)  > 1.96 * sd_load_precovid
  )






my_plot_seuil <- ggplot(
  df_2020 %>%
    filter(as.numeric(month) >= 2, as.Date(datetime) <= as.Date("2020-05-15")),
  aes(x = datetime)
) +
  # bandes de confinement
  geom_rect(
    data = lockdowns,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = phase
    ),
    inherit.aes = FALSE,
    alpha = 0.25
  ) +
  # série
  geom_line(
    data = wind_reg_data %>%
      filter(year == 2020, as.numeric(month) >= 2, as.Date(datetime) <= as.Date("2020-05-15")),
    aes(x = datetime, y = price_diff_0to5 * 100+ 2000, color = "Intraday Price Update")
  ) +
  geom_line(
    data = wind_reg_data %>%
      filter(year == 2020, as.numeric(month) >= 2, as.Date(datetime) <= as.Date("2020-05-15")),
    aes(x = datetime, y = qty_0to5 - mean(qty_0to5)- 2000 , color = "Intraday Qty")
  ) +
  geom_line(aes(y = load_error, color = "Load Forecast Error")) +
  geom_line(aes(y = load_error_upper), color = "black", linetype = "dashed") +
  geom_line(aes(y = load_error_lower), color = "black", linetype = "dashed") +
  geom_line(aes(y = 2000), color = "black", linetype = "solid") +
  geom_line(aes(y = -2000), color = "black", linetype = "solid") +
  labs(
    x = "",
    y = "MW",
    color = ""
  )+
  theme_minimal(base_size = 14) +
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
  )
my_plot_seuil





