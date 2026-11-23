
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(arrow)
library(data.table)
library(future.apply)
library(progressr)

library(patchwork)
library(cowplot)
library(gridExtra)
library(grid)
library(latex2exp)

source_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/depreciated/VAR_analysis_fr/reliability'
setwd(source_path)
source("reliability_auxiliary.R")

setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data')

gas_tot_futures <- read_parquet("gas_data_shock.parquet")
gas_tot_futures <- gas_tot_futures %>% 
  arrange(time) %>% 
  mutate(gas_shock = TTF - lag(TTF),
         datetime = as.Date(time, format = "%Y-%m-%d")
  )


setwd('C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news')
my_instrumented_days <- read.csv("final_instrumented_days.csv")


my_instrumented_days <- my_instrumented_days %>% 
  filter(day_type=="supply")%>% 
  mutate(gas_shock = TTF - lag(TTF),
         datetime = as.Date(time, format = "%Y-%m-%d")
  ) 
colnames(my_instrumented_days)


save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/depreciated/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)
IV_data_log_esp <- IV_data_log_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(heat_excess_instrument = heat_excess_demand,
         cold_excess_instrument = cold_excess_demand)

IV_data_log_fr <-  read_parquet('IV_data_fr_log.parquet')


# Passer en data.table rapidement
gas_tot_futures <- as.data.table(gas_tot_futures)
my_instrumented_days_dt <- as.data.table(my_instrumented_days)
VAR_data_log_fr_dt <- as.data.table(IV_data_log_fr)
VAR_data_log_esp_dt <- as.data.table(IV_data_log_esp)

#
#estime l'effet de l'incertitude sur tout la durée
# dependant_variable = "spot_price_log"
# VAR_data_log_dt = VAR_data_log_esp_dt
# headders_proxies = headers_proxies_tot
simulation_placebo_monthly <- function(headers_proxies, dependant_variable, country){
  
  #au lieu de choisir instrumented days
  # je tire au sort le même nombre de jours
  
  VAR_data_log_dt = var_data_list[[country]]
  
  # 1. Tirage aléatoire des jours placebo
  placebo_pc_days <- gas_tot_futures[sample(.N, nrow(my_instrumented_days_dt))]
  setorder(placebo_pc_days, datetime)
  
  # 2. Construire la série fondamentale
  placebo_pc_days[, `:=`(
    month = month(datetime),
    year = year(datetime)
  )]
  
  
  fundamental_shock_serie <- placebo_pc_days[, .(
    PC1 = sum(PC1),
    gas_shock = sum(gas_shock), 
    datetime = min(datetime)
  ), by = .(year, month)][order(datetime)]
  
  # 3. Ajustement pour obtenir la version "wider"
  fundamental_shock_serie[is.na(PC1), PC1 := 0]
  fundamental_shock_serie[is.na(gas_shock), gas_shock := 0]
  
  wider_fundamental_shock_serie_placebo <- fundamental_shock_serie[, .(
    PC1_placebo = sum(PC1, na.rm = TRUE),
    gas_shock_placebo = sum(gas_shock, na.rm = TRUE),
    datetime = max(datetime, na.rm = TRUE)
  ), by = .(year, month)]
  
  #colnames(VAR_data_log_placebo)
  # 4. Fusion avec la base principale
  VAR_data_log_placebo <- merge(
    VAR_data_log_dt[, -"datetime", with = FALSE],
    wider_fundamental_shock_serie_placebo,
    by = c("year", "month"),
    all.x = TRUE
  )
  
  # 5. Remplacer les NA par 0
  for (col in c("PC1_placebo", "gas_shock_placebo")) {
    VAR_data_log_placebo[is.na(get(col)), (col) := 0]
  }
  
  # 6. Trier
  setorder(VAR_data_log_placebo, observation_date)
  
  # 7. Analyse finale
  headers_proxies <- gsub("supply", "placebo", headers_proxies)
  
  result_placebo <- run_iv_analysis(
    as.data.frame(VAR_data_log_placebo),
    headers_var,
    headers_proxies,
    p = 12,
    dependent = dependant_variable,
    k = 1
  )
  
  # Access outputs

  return(coef(result_placebo$m_summary))

}



# Convertir la liste de triplets en un dataframe
#write_parquet(simulation_data, "placebo_simulation_monthly_rpc2.parquet")
headers_var = c("spot_price_log",
                "gen_tot_log",
                "IPI_log",
                # "ECB_rate_log",
                # "E_Index_log",
                # "Exp_pi_log",
                # "UnE_log" ,
                "NE_Index_log"
)
#total instrument version

headers_proxies_tot = c(
  "log_diff_renewable",
  "PC1_supply",
  "cold_excess_instrument",
  "heat_excess_instrument",
  "diff_cbf"
)

#minimalist version with only PC1
headers_proxies_min <- c(
  "PC1_supply"
)






#attention à PC1, tout a été divisé par 100 par rapport au reste
#simulation_placebo_monthly()



handlers(global = TRUE)  # Active les handlers de progression

handlers("txtprogressbar")  # Ou "progress" / "cli" / "rstudio" selon ton terminal

plan(multisession) # ou multicore selon ton OS


results_placebo_fr_min <- with_progress({
  p <- progressor(steps = 10000)  # Ou 10000 selon ton nombre de répétitions
  
  future_replicate(
    10000,
    {
      res <- simulation_placebo_monthly(headers_proxies= headers_proxies_min,
                                        dependant_variable= "spot_price_log",
                                        VAR_data_log_dt = VAR_data_log_fr_dt)
      p()  # Mise à jour de la barre
      res
    },
    simplify = FALSE
  )
})
simulation_data_fr_min <- as.data.frame(t(do.call(cbind, lapply(results_placebo_fr_min, as.data.frame))))




#all at once calculations

# Define the combinations of datasets and proxy headers
var_data_list <- list(
  fr = VAR_data_log_fr_dt,
  esp = VAR_data_log_esp_dt
)

proxy_headers_list <- list(
  min = headers_proxies_min,
  tot = headers_proxies_tot
)

# Define a container for the results
all_simulation_data <- list()

handlers(global = TRUE)

with_progress({
  # Set up progress bar
  total_combinations <- length(var_data_list) * length(proxy_headers_list)
  total_simulations <- 10000 * total_combinations
  p <- progressor(steps = total_simulations)
  
  for (country in names(var_data_list)) {
    for (proxy_type in names(proxy_headers_list)) {
      # Unique name for each combination
      result_name <- paste0("simulation_data_", country, "_", proxy_type)
      
      # Perform the simulations
      results <- future_replicate(
        10000,
        {
          res <- simulation_placebo_monthly(
            headers_proxies = proxy_headers_list[[proxy_type]],
            dependant_variable = "spot_price_log",
            country = country
          )
          p()  # update progress
          res
        },
        simplify = FALSE
      )
      
      # Store the result as a data frame
      all_simulation_data[[result_name]] <- as.data.frame(t(do.call(cbind, lapply(results, as.data.frame))))
    }
  }
})

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/gas_code/outputs")

saveRDS(all_simulation_data, "placebo_simulation_monthly_gas_final.rds")
#all_simulation_data <- readRDS( "placebo_simulation_monthly_gas.rds")
names(all_simulation_data)
# country = "esp"
# proxy = "tot"
library(ggplot2)

plot_placebo_distribution <- function(country = "fr", proxy = "min", component = "PC1_supply", reverse = FALSE, 
                                      level = 0.90, all_sim_data) {
  # Build the key to access simulation data
  sim_key <- paste0("simulation_data_", country, "_", proxy)
  
  # Retrieve simulation data
  simulation_data <- all_sim_data[[sim_key]]
  
  # Check if the desired component exists
  placebo_col <- gsub("_supply", "_placebo", component)
  if (!placebo_col %in% colnames(simulation_data)) {
    stop(paste("Column", placebo_col, "not found in simulation data"))
  }
  
  result <- run_iv_analysis(
    get(paste0("IV_data_log_", tolower(country))),
    headers_var = headers_var,
    headers_proxies = get(paste0("headers_proxies_", proxy)),
    p = 4,
    dependent = "spot_price_log",
    k = 1
  )
  
  # Access outputs
  selected_values <- coef(result$m_summary)
  true_effect <- selected_values["PC1_supply"][[1]] 
  
  # Compute quantile
  q_threshold <- quantile(simulation_data[[placebo_col]], probs = level)
  
  
  # Plot
  my_plot <- ggplot(data.frame(effet = simulation_data[[placebo_col]]), aes(x = effet)) +
    geom_histogram(aes(fill = country), bins = 50, color = "black", alpha = 0.7)+
    geom_vline(xintercept = q_threshold, color = "blue", linetype = "dashed", size = 1) +
    geom_vline(xintercept = true_effect, color = "red", linetype = "solid", size = 1) +
    labs(
      title = paste("Instrument prediction efficiency ", country, ", ", proxy, sep = ""),
      x = "Placebo Distribution",
      y = "Fréquence"
    ) +
    theme_minimal() +
    annotate("text", x = q_threshold, y = Inf, label = paste0(round(level*100), "% quantile"), 
             vjust = 3, color = "blue") +
    annotate("text", x = true_effect, y = Inf, label = paste0(component, "*"), vjust = 1,
             color = "red")
  return(my_plot)
}

names(all_simulation_data) <- c("simulation_data_FR_min", 
                                "simulation_data_FR_tot", 
                                "simulation_data_ESP_min",
                                "simulation_data_ESP_tot")

my_plot_fr <- plot_placebo_distribution(
  country = "FR", 
  proxy = "min", 
  component = "PC1_supply", 
  level = 0.90, 
  all_sim_data = all_simulation_data
)+
  labs(title = NULL, fill = NULL) +
  theme(axis.title.x = element_blank(), # Désactiver le titre de l'axe des x
        axis.text.x = element_blank(),
        legend.position = "none")  # Désactiver les ticks de l'axe des x

my_plot_esp <- plot_placebo_distribution(
  country = "ESP", 
  proxy = "min", 
  component = "PC1_supply", 
  reverse = TRUE,
  level = 0.90, 
  all_sim_data = all_simulation_data
)+
  labs(title = NULL, fill = NULL) +
  # theme(axis.text.x = element_blank(), # Désactiver le titre de l'axe des x
  #       axis.title.x = "Placebo Distribution")+ 
  scale_y_reverse()

# Ajouter un titre personnalisé comme annotation
#+annotate("text", x = Inf, y = -Inf, label = "Number of Power Outages", 
#hjust = 1.4, vjust = -1.5, size = 5)
# Assembler les deux graphiques verticalement avec patchwork

my_plot <- (my_plot_fr / my_plot_esp) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") & 
  scale_fill_manual(values = c("ESP" = "darkblue", "FR" = "darkred"))

# Ajouter un titre global
my_plot <- my_plot + plot_annotation(
  title = "Gas Supply Shock Prediction Efficiency",
  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
)

my_plot




plot_placebo_distribution(
  country = "FR", 
  proxy = "tot", 
  component = "PC1_supply", 
  level = 0.90, 
  all_sim_data = all_simulation_data
)


plot_placebo_distribution(
  country = "ESP", 
  proxy = "tot", 
  component = "PC1_supply", 
  level = 0.90, 
  all_sim_data = all_simulation_data
)







