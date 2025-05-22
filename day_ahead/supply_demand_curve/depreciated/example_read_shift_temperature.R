library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)

curve_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/Supply_demand_curves'
setwd(curve_path)

# my_files <- list.files()
# # Obtenez la liste des fichiers .parquet
# parquet_files <- my_files[grep("\\.parquet$", my_files)]
# 
# # Supprimez les fichiers .parquet de la liste des fichiers
# my_files <- setdiff(my_files, parquet_files)
# 
# file <- my_files[1]

curve = "curva_pbc_20230204.1"
# Extraire la date au format AAAA-MM-JJ
date <- str_extract(curve, "\\d{8}")           # extrait "20210927"
date <- as.Date(date, format = "%Y%m%d")       # convertit en "2021-09-27"
csv_file <- read.csv2(curve, header = FALSE,
                      skip = 1)
colnames(csv_file) <- as.character(csv_file[1, ]) 
csv_file <- csv_file %>% 
  filter(Pais == "ES" | Pais == "MI")
csv_file <- csv_file[-1, c(1:2, 5:8)]  # Supprimer la première ligne devenue superflue
colnames(csv_file) <- c('Hour', 'DATE', 'OFFER_TYPE','ENERGY', 'PRICE', 'MATCHED' )

hour = 1
plot_data <- csv_file %>% 
  filter(Hour == hour ) %>%
  mutate(ENERGY = gsub("\\.", "", ENERGY)) %>% 
  mutate(ENERGY = as.numeric(gsub(",", ".", ENERGY)),
         PRICE = as.numeric(gsub(",", ".", PRICE)))


# Remplace les virgules par des points et convertit en numérique

cum_grid <- seq(0, 60000,length.out = 1000)

# Filtrer les courbes d'offre et de demande
supply_curve <- plot_data %>%
  filter(OFFER_TYPE == 'V' & MATCHED == 'O') %>%
  arrange(PRICE) %>%
  mutate(cum_energy = cumsum(as.numeric(ENERGY)))

# 2. Créer la fonction en escalier
# On utilise la fonction "stepfun"
supply_curve_fit <- stepfun(supply_curve$cum_energy, c(supply_curve$PRICE[1], supply_curve$PRICE))
# 4. Évaluer la fonction en escalier
supply_curve_fit_description <- supply_curve_fit(cum_grid)


demand_curve <- plot_data %>%
  filter(OFFER_TYPE == 'C' & MATCHED == 'O') %>%
  arrange(desc(PRICE)) %>%
  mutate(cum_energy = cumsum(as.numeric(ENERGY)))# Filtrer les offres et demandes qui ont été "matched"

demand_curve_fit <- stepfun(demand_curve$cum_energy, c(demand_curve$PRICE[1], demand_curve$PRICE))
demand_curve_fit_description <- demand_curve_fit(cum_grid)

matched_supply_curve <- plot_data %>%
  filter(OFFER_TYPE == 'V' & MATCHED == 'C') %>%
  arrange(PRICE) %>%
  mutate(cum_energy = cumsum(as.numeric(ENERGY)))

matched_supply_curve_fit <- stepfun(matched_supply_curve$cum_energy, c(matched_supply_curve$PRICE[1], matched_supply_curve$PRICE))
matched_supply_curve_fit_description <- matched_supply_curve_fit(cum_grid)

matched_demand_curve <- plot_data %>%
  filter(OFFER_TYPE == 'C' & MATCHED == 'C') %>%
  arrange(desc(PRICE)) %>%
  mutate(cum_energy = cumsum(as.numeric(ENERGY)))

matched_demand_curve_fit <- stepfun(matched_demand_curve$cum_energy, c(matched_demand_curve$PRICE[1], matched_demand_curve$PRICE))
matched_demand_curve_fit_description <- matched_demand_curve_fit(cum_grid)


length(matched_demand_curve_fit)

# Tracer les courbes
ggplot() +
  geom_line(data = supply_curve, aes(x = cum_energy, y = PRICE, color = "Supply curve", linetype = "Theoretical")) +
  geom_line(data = demand_curve, aes(x = cum_energy, y = PRICE, color = "Demand curve", linetype = "Theoretical")) +
  geom_line(data = matched_supply_curve, aes(x = cum_energy, y = PRICE, color = "Supply curve", linetype = "Matched")) +
  geom_line(data = matched_demand_curve, aes(x = cum_energy, y = PRICE, color = "Demand curve", linetype = "Matched")) +
  theme_minimal()





library(purrr)
colnames(csv_file)
# 2. Calcul
result <- csv_file%>%
  mutate(ENERGY = gsub("\\.", "", ENERGY)) %>% 
  mutate(ENERGY = as.numeric(gsub(",", ".", ENERGY)),
         PRICE = as.numeric(gsub(",", ".", PRICE))) %>%
  group_by(DATE, Hour, OFFER_TYPE, MATCHED) %>%
  arrange(PRICE) %>%
  mutate(CUM_ENERGY = cumsum(as.numeric(ENERGY))) %>% 
  arrange(CUM_ENERGY) %>%
  group_modify(~ {
    df <- .x
    tibble(
      grid_point = cum_grid,
      PRICE = map_dbl(cum_grid, function(g) {
        row <- which(df$CUM_ENERGY >= g)[1]
        if (is.na(row)) NA_real_ else df$PRICE[row]
      })
    )
  })

library(data.table)

csv_file2 <- csv_file
# 1. Convertir en data.table si nécessaire
setDT(csv_file2)

# 2. Nettoyage des colonnes et conversions
csv_file2[, ENERGY := gsub("\\.", "", ENERGY)]
csv_file2[, ENERGY := as.numeric(gsub(",", ".", ENERGY))]
csv_file2[, PRICE := as.numeric(gsub(",", ".", PRICE))]

# 3. Calcul par groupe
csv_file2 <- csv_file2[
  order(DATE, Hour, OFFER_TYPE, MATCHED, PRICE),  # tri nécessaire avant cumsum
  CUM_ENERGY := cumsum(ENERGY),
  by = .(DATE, Hour, OFFER_TYPE, MATCHED)
]
# 3. Calcul pour chaque groupe
result2 <- csv_file2[
  order(CUM_ENERGY), 
  {
    res <- lapply(cum_grid, function(g) {
      i <- which(CUM_ENERGY >= g)[1]
      if (!is.na(i)) PRICE[i] else NA_real_
    })
    data.table(grid_point = cum_grid, PRICE = unlist(res))
  },
  by = .(DATE, Hour, OFFER_TYPE, MATCHED)
]


View( test %>% 
        filter(OFFER_TYPE == 'V' & MATCHED == 'C' & Hour == 23))
plot_test <- test %>% 
  filter(OFFER_TYPE == 'V' & MATCHED == 'O' & Hour == 19)
test <- csv_file%>%
  mutate(ENERGY = gsub("\\.", "", ENERGY)) %>% 
  mutate(ENERGY = as.numeric(gsub(",", ".", ENERGY)),
         PRICE = as.numeric(gsub(",", ".", PRICE))) %>%
  arrange(DATE, Hour, OFFER_TYPE, MATCHED, PRICE) %>%
  group_by(DATE, Hour, OFFER_TYPE, MATCHED) %>%
  mutate(CUM_ENERGY = cumsum(as.numeric(ENERGY)))

# 5. Visualiser le résultat
ggplot() +
  geom_step(aes(x = supply_curve$cum_energy, y = supply_curve$PRICE), color = "blue", direction = "hv") +
  geom_line(aes(x = cum_grid, y = supply_curve_fit_description), color = "red", linetype = "dashed") +
  geom_line(data = plot_test, aes(x = CUM_ENERGY, y = PRICE), color = "green") +
  labs(title = "Courbe d'offre en escalier",
       x = "Énergie cumulée (cum_energy)",
       y = "Prix")




