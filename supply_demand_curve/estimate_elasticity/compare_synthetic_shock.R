library(dplyr)
library(AER)
library(sandwich)
library(lmtest)
library(stargazer)
# Ton dossier d'outputs
output_dir <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/supply_demand_curve/outputs/coef_reg_no_conditioning"
setwd(output_dir)


price_indispo_merge_esp_sd <-read_parquet("price_indispo_merge_esp_npc.parquet")
setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_esp_delta <- read_parquet("price_indispo_merge_esp_delta_supply_demand_exo.parquet") 
# Sélectionner toutes les colonnes contenant "fitted" et "supply"
cols_fitted_supply <- grep("fit.*supply", names(price_indispo_merge_esp_sd), value = TRUE)
cols_fitted_demand <- grep("fit.*demand", names(price_indispo_merge_esp_sd), value = TRUE)

#matched shocks are endogenous to prices
#so we will rather consider unmatched
#indispo sont vraiment pareilles


# keep only "unmatched"
cols_fitted_supply <- grep("unmatched", cols_fitted_supply, value = TRUE)
cols_fitted_demand <- grep("unmatched", cols_fitted_demand, value = TRUE)

# drop "indispo"
cols_fitted_supply <- grep("indispo", cols_fitted_supply, invert = TRUE, value = TRUE)
cols_fitted_demand <- grep("indispo", cols_fitted_demand, invert = TRUE, value = TRUE)


price_indispo_merge_compare <- merge(price_indispo_merge_esp_sd %>% 
                                       dplyr::select(datetime,
                              cols_fitted_supply , cols_fitted_demand   ),
                                     price_indispo_merge_esp_delta %>% 
                                dplyr::select(delta_s_total, delta_d_total, gen_tot, load_tot, price, datetime)
) %>% distinct(datetime, .keep_all = TRUE)



View(
  price_indispo_merge_compare %>% 
    dplyr::select(c("datetime", "gen_tot", "price", "load_tot",  
             "delta_s_total", "delta_d_total",
             cols_fitted_supply,
             cols_fitted_demand)))



price_indispo_merge_compare %>% 
  dplyr::select(c("delta_s_total",cols_fitted_supply)) %>% 
  cor(use = "complete.obs")

price_indispo_merge_compare %>% 
  dplyr::select(c("delta_d_total",cols_fitted_demand)) %>% 
  cor(use = "complete.obs")


price_indispo_merge_compare <- merge(price_indispo_merge_esp_sd %>% 
                                       select(datetime,
                                              cols_fitted_supply , cols_fitted_demand   ),
                                     price_indispo_merge_esp_delta
)



price_indispo_merge_delta_clean <- price_indispo_merge_compare %>%
  dplyr::select(-log_price_diff, -price_diff) %>%       # sélectionne les colonnes dans col_names
  filter(complete.cases(.)) %>%           # garde seulement les lignes sans NA dans ces colonnes
  distinct()

dep_var <- "log(price+1)"
endog_vars <- c("log(load_tot)")
#endog_vars <- c("log(load_tot)")
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
extended_controls <- c(controls_vars, "unpredicted_renewables",  "intraday_update_renewables" )


# Orthogonaliser delta_s_total par rapport ? delta_d_total
price_indispo_merge_delta_clean$delta_s_orth <- resid(
  lm(delta_s_total ~ delta_d_total, data = price_indispo_merge_delta_clean)
)

# Orthogonaliser delta_d_total par rapport ? delta_s_total
price_indispo_merge_delta_clean$delta_d_orth <- resid(
  lm(delta_d_total ~ delta_s_total, data = price_indispo_merge_delta_clean)
)


# Sélectionner toutes les colonnes fit
cols_fitted <- grep("^fit_", names(price_indispo_merge_delta_clean), value = TRUE)

for(col in cols_fitted){
  if(grepl("supply", col)){
    # Pour les colonnes supply, régresser sur la colonne demand correspondante
    target_col <- sub("supply", "demand", col)
  } else if(grepl("demand", col)){
    # Pour les colonnes demand, régresser sur la colonne supply correspondante
    target_col <- sub("demand", "supply", col)
  } else {
    next  # ignorer si ce n’est ni supply ni demand
  }
  
  # Nouveau nom pour la colonne orthogonalisée
  new_col <- paste0(col, "_orth")
  
  # Vérifier que la colonne cible existe pour éviter les erreurs
  if(target_col %in% names(price_indispo_merge_delta_clean)){
    price_indispo_merge_delta_clean[, (new_col) := resid(
      lm(as.formula(paste(col, "~", target_col)), data = price_indispo_merge_delta_clean)
    )]
  }
}

cols_fitted_bis <- setdiff(grep("^fit_", names(price_indispo_merge_delta_clean), value = TRUE), cols_fitted)




library(AER)
ols_formula <- as.formula(paste(
  paste("log(load_tot)", "~",  paste(c("log(price+1)", controls_vars), collapse = " + "))))  

m_ols <- ivreg(ols_formula , data = price_indispo_merge_delta_clean)

nw_se_ols  <- NeweyWest(m_ols , lag = 24, prewhite = FALSE, adjust = TRUE)
# Extraire les r?sultats des coefficients avec SE NW
nw_ols  <- coeftest(m_ols , vcov. = nw_se_ols )




# Colonnes orthogonalisées de supply et demand
cols_fitted_supply_orth <- grep("fit.*supply.*_orth$", names(price_indispo_merge_delta_clean), value = TRUE)
cols_fitted_demand_orth <- grep("fit.*demand.*_orth$", names(price_indispo_merge_delta_clean), value = TRUE)



price_indispo_merge_delta_clean %>% 
  select(c("delta_s_orth",cols_fitted_supply_orth)) %>% 
  cor(use = "complete.obs")

price_indispo_merge_delta_clean %>% 
  select(c("delta_d_orth",cols_fitted_demand_orth)) %>% 
  cor(use = "complete.obs")


price_indispo_merge_delta_clean <- price_indispo_merge_delta_clean %>% 
  rename(delta_supply_orth = delta_s_orth,
         delta_demand_orth= delta_d_orth)

# Créer une liste pour stocker les résultats
iv_results <- list()
s_col  <-"delta_s_orth"# "fit_supply_matched_orth"
d_col <- "fit_demand_matched_orth"
# Boucle sur chaque couple correspondant (match par suffixe)
for(s_col in c("delta_supply_orth", cols_fitted_supply_orth)){
  
  # Identifier le suffixe
  # Extraire le suffixe et remplacer "supply" par "demand"
  suffix <- sub("fit_", "", sub("_orth$", "", s_col))
  # Nom de la colonne demand correspondante
  d_col <- sub("supply", "demand", s_col)
  print(suffix)
  # Vérifier que la colonne existe
  if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  # Formule IV pour supply
  iv_formula_s <- as.formula(paste(
    paste("log(load_tot) ~ log(price+1) +", paste(controls_vars, collapse = " + ")),
    paste(s_col, "+", paste(controls_vars, collapse = " + ")),
    sep = " | "
  ))
  
  
  # Formule IV pour demand
  iv_formula_d <- as.formula(paste(
    paste("log(load_tot) ~ log(price+1) +", paste(controls_vars, collapse = " + ")),
    paste(d_col, "+", paste(controls_vars, collapse = " + ")),
    sep = " | "
  ))
  
  # Estimation IV
  m_iv_s <- ivreg(iv_formula_s, data = price_indispo_merge_delta_clean)
  m_iv_d <- ivreg(iv_formula_d, data = price_indispo_merge_delta_clean)
  
  # Newey-West SE
  nw_se_s <- NeweyWest(m_iv_s, lag = 24, prewhite = FALSE, adjust = TRUE)
  nw_se_d <- NeweyWest(m_iv_d, lag = 24, prewhite = FALSE, adjust = TRUE)
  
  nw_s <- coeftest(m_iv_s, vcov. = nw_se_s)
  nw_d <- coeftest(m_iv_d, vcov. = nw_se_d)
  
  # Diagnostics
  diag_s <- summary(m_iv_s, diagnostics = TRUE)$diagnostics
  diag_d <- summary(m_iv_d, diagnostics = TRUE)$diagnostics
  
  fstat_s <- diag_s["Weak instruments", "statistic"]
  fstat_d <- diag_d["Weak instruments", "statistic"]
  
  # Stocker dans la liste
  iv_results[[suffix]] <- list(
    supply = list(model = m_iv_s, nw = nw_s, fstat = fstat_s),
    demand = list(model = m_iv_d, nw = nw_d, fstat = fstat_d)
  )
}
class(iv_results)
names(iv_results)
# Exemple d'accès :
# iv_results[["matched"]]$supply$nw      # Coefficients NW pour supply matched
# iv_results[["unmatched"]]$demand$fstat # F-statistic pour demand unmatched


# Préparer les modèles à mettre dans le tableau
all_models_supply <- list()
all_models_demand <- list()
column_labels_supply <- c()
column_labels_demand <- c()
F_stat_supply <- c()
F_stat_demand <- c()
for(suffix in names(iv_results)){
  
  # Supply
  all_models_supply <- c(all_models_supply, list(iv_results[[suffix]]$supply$nw))
  column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply  <- c(F_stat_supply , round(iv_results[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand <- c(all_models_demand, list(iv_results[[suffix]]$demand$nw))
  column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand <- c(F_stat_demand, round(iv_results[[suffix]]$demand$fstat, 2))
}
all_models_supply[[1]]

# Créer le tableau stargazer
stargazer(
  all_models_supply,
  type = "text",
  title = "Regression Results for All Fitted Supply/Demand Orthogonalized",
  #column.labels = column_labels_supply,
  column.labels = c("Delta","Unmatched FE", "Unmatched"),
  add.lines = list(c("First-stage F-stat", rep(F_stat_supply, each = 1))),
  keep = c("load_tot", "price")
)


stargazer(
  all_models_demand,
  type = "text",
  title = "Regression Results for All Fitted Supply/Demand Orthogonalized",
  #column.labels = column_labels_demand,
  column.labels = c("Delta","Unmatched FE", "Unmatched"),
  add.lines = list(c("First-stage F-stat", rep(F_stat_demand, each = 1))),
  keep = c("load_tot", "price")
)



# List of columns you want to lag
cols_to_lag <- c("delta_supply_orth", cols_fitted_supply_orth,
                 "delta_demand_orth", cols_fitted_demand_orth
)

price_indispo_merge_delta_clean$delta_supply_orth_lag
price_indispo_merge_delta_clean <- price_indispo_merge_delta_clean %>%
  mutate(across(all_of(cols_to_lag), ~ lag(.), .names = "{.col}_lag"))
price_indispo_merge_delta_clean %>%
  dplyr::select(all_of(c(cols_to_lag, paste0(cols_to_lag, "_lag")))) %>% 
  cor(use = "complete.obs")

View(price_indispo_merge_delta_clean %>% 
       mutate(delta_supply_orth_lag = lag(delta_supply_orth)) %>% 
       select(datetime,delta_supply_orth, delta_supply_orth_lag ))
iv_results_lag <- list()
for(s_col in c("delta_supply_orth", cols_fitted_supply_orth)){
  
  suffix <- sub("fit_", "", s_col)
  d_col  <- sub("supply", "demand", s_col)
  print(paste("Processing:", suffix))
  
  if(!d_col %in% names(price_indispo_merge_delta_clean)) next
  
  
  # lag_exog_s <- paste0("lag(", s_col, ", ", 1, ")")
  # lag_exog_d <- paste0("lag(", d_col, ", ", 1, ")")
  lag_exog_s <- paste0( s_col, "_lag")
  lag_exog_d <-paste0( d_col, "_lag")
  
  # Instruments supply/demand avec 12 lags
  s_instruments <- paste(c(s_col), collapse = " + ")
  d_instruments <- paste(c(d_col), collapse = " + ")
  
  # Formule IV supply
  iv_formula_s <- as.formula(paste(
    paste("log(load_tot) ~ log(price+1) +", paste(c(controls_vars, lag_exog_s), collapse = " + ")),
    paste(s_instruments, "+", paste(c(controls_vars, lag_exog_s), collapse = " + ")),
    sep = " | "
  ))
  
  # Formule IV demand
  iv_formula_d <- as.formula(paste(
    paste("log(load_tot) ~ log(price+1) +", paste(c(controls_vars, lag_exog_d), collapse = " + ")),
    paste(d_instruments, "+", paste(c(controls_vars, lag_exog_d), collapse = " + ")),
    sep = " | "
  ))
  
  # Estimation
  m_iv_s <- ivreg(iv_formula_s, data = price_indispo_merge_delta_clean)
  
  m_iv_d <- ivreg(iv_formula_d, data = price_indispo_merge_delta_clean)
  
  # SE Newey-West
  nw_se_s <- NeweyWest(m_iv_s, lag = 24, prewhite = FALSE, adjust = TRUE)
  nw_se_d <- NeweyWest(m_iv_d, lag = 24, prewhite = FALSE, adjust = TRUE)
  
  nw_s <- coeftest(m_iv_s, vcov. = nw_se_s)
  nw_d <- coeftest(m_iv_d, vcov. = nw_se_d)
  
  # Diagnostics
  diag_s <- summary(m_iv_s, diagnostics = TRUE)$diagnostics
  diag_d <- summary(m_iv_d, diagnostics = TRUE)$diagnostics
  
  fstat_s <- diag_s["Weak instruments", "statistic"]
  fstat_d <- diag_d["Weak instruments", "statistic"]
  
  # Stockage
  iv_results_lag[[suffix]] <- list(
    supply = list(model = m_iv_s, nw = nw_s, fstat = fstat_s),
    demand = list(model = m_iv_d, nw = nw_d, fstat = fstat_d)
  )
}



# Préparer les modèles à mettre dans le tableau
all_models_supply_lag <- list()
all_models_demand_lag <- list()

F_stat_supply_lag <- c()
F_stat_demand_lag <- c()
for(suffix in names(iv_results_lag)){
  
  # Supply
  all_models_supply_lag <- c(all_models_supply_lag, list(iv_results_lag[[suffix]]$supply$nw))
  #column_labels_supply  <- c(column_labels_supply , suffix)
  F_stat_supply_lag  <- c(F_stat_supply_lag , round(iv_results_lag[[suffix]]$supply$fstat, 2))
  
  # Demand
  all_models_demand_lag <- c(all_models_demand_lag, list(iv_results_lag[[suffix]]$demand$nw))
  #column_labels_demand <- c(column_labels_demand,  suffix)
  F_stat_demand_lag <- c(F_stat_demand_lag, round(iv_results_lag[[suffix]]$demand$fstat, 2))
}

# Créer le tableau stargazer
stargazer(
  all_models_supply_lag,
  type = "text",
  title = "Regression Results for All Fitted Supply/Demand Orthogonalized",
  #column.labels = column_labels_supply,
  column.labels = c("Delta","Unmatched FE", "Unmatched"),
  add.lines = list(c("First-stage F-stat", rep(F_stat_supply_lag, each = 1))),
  keep = c("load_tot", "price")
)


stargazer(
  all_models_demand_lag,
  type = "text",
  title = "Regression Results for All Fitted Supply/Demand Orthogonalized",
  #column.labels = column_labels_demand,
  column.labels = c("Delta","Unmatched FE", "Unmatched"),
  add.lines = list(c("First-stage F-stat", rep(F_stat_demand_lag, each = 1))),
  keep = c("load_tot", "price")
)


# --- extraire coefficients et std ---
get_coef_and_se <- function(models) {
  coef_vec <- sapply(models, function(m) coef(m)["log(price + 1)"])
  se_vec   <- sapply(models, function(m) m["log(price + 1)", "Std. Error"])
  list(coef = coef_vec, se = se_vec)
}

# Supply / Demand
supply_0     <- get_coef_and_se(all_models_supply)
demand_0     <- get_coef_and_se(all_models_demand)
supply_lag <- get_coef_and_se(all_models_supply_lag)
demand_lag <- get_coef_and_se(all_models_demand_lag)

# --- construire le tableau ---
coef_table <- data.frame(
  Model = rep(c("Delta","Unmatched_FE", "Unmatched"), 4),
  Coef_log_price = c(supply_0$coef, demand_0$coef, supply_lag$coef, demand_lag$coef),
  Std_Error      = c(supply_0$se,   demand_0$se,   supply_lag$se,   demand_lag$se),
  Type = rep(c("Supply","Demand","Supply","Demand"), each = 3),
  FE   = "YMWH",
  Lag  = rep(c("0","0","12","12"), each = 3)
)

write.csv(coef_table, "coef_table.csv")

coef_table_wide <- coef_table %>%
  filter(Model != "Unmatched") %>% 
  mutate(
    coef_se = sprintf("%.3f (%.3f)", Coef_log_price, Std_Error),
    Lag = paste0("Lag", Lag)  # pour nommer clairement les colonnes
  ) %>%
  select(Type, Lag, Model, coef_se) %>%
  pivot_wider(
    names_from = c(Lag, Model),
    values_from = coef_se
  )


library(gt)

gt_table <- coef_table_wide %>%
  gt(rowname_col = "Type") %>%
  tab_header(
    title = "Coefficient Estimates",
    subtitle = "Delta, Unmatched FE, and Unmatched for Lag 0 and Lag 12"
  ) %>%
  # Labels propres
  cols_label(
    Lag0_Delta = "Delta",
    Lag0_Unmatched_FE = "Unmatched FE",
    #Lag0_Unmatched = "Unmatched",
    Lag12_Delta = "Delta",
    Lag12_Unmatched_FE = "Unmatched FE",
    #Lag12_Unmatched = "Unmatched"
  ) %>%
  # Spanners (groupes de colonnes)
  tab_spanner(
    label = "Lag 0",
    columns = c(Lag0_Delta, Lag0_Unmatched_FE)#, Lag0_Unmatched)
  ) %>%
  tab_spanner(
    label = "Lag 12",
    columns = c(Lag12_Delta, Lag12_Unmatched_FE)#, Lag12_Unmatched)
  ) %>%
  # Mise en forme
  opt_table_outline() %>%
  tab_style(
    style = cell_fill(color = "lightgrey"),
    locations = cells_column_labels(columns = everything())
  )
gt_table

# Exemple d'accès :
# iv_results[["matched"]]$supply$nw      # Coefficients NW pour supply matched
# iv_results[["unmatched"]]$demand$fstat # F-statistic pour demand unmatched
monthly_data <- price_indispo_merge_delta_clean %>% 
  group_by(year, month) %>% 
  summarize(fit_supply_unmatched = mean(fit_supply_unmatched, na.rm= TRUE),
            fit_demand_unmatched = mean(fit_demand_unmatched, na.rm=TRUE),
            fit_supply_unmatched_orth = mean(fit_supply_unmatched_orth, na.rm= TRUE),
            fit_demand_unmatched_orth = mean(fit_demand_unmatched_orth, na.rm=TRUE),
            datetime = first(datetime))

my_plot <- ggplot(data = monthly_data, aes(x = datetime))+
  # geom_line(aes(y = fit_supply_unmatched, color = "Supply"))+
  # geom_line(aes(y = fit_demand_unmatched, color = "Demand"))+
  geom_line(aes(y = fit_supply_unmatched_orth, color = "Supply"))+
  geom_line(aes(y = fit_demand_unmatched, color = "Demand"))+
  theme_minimal()
my_plot


