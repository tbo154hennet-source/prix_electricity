
#get micro price counterfactual using the spot gas coefficient
# petit tableau avec la valeur des coefficents respectifs pour la France et l'Allemagne

#différence entre prédiction et effective

library(sandwich)
library(lmtest)

#en espagne


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/esp"
setwd(save_path_data)
price_indispo_merge_esp <- read_parquet("price_indispo_merge_esp.parquet")
price_indispo_merge_esp <- price_indispo_merge_esp %>%
  fill(spot_gas, .direction = "up") %>% 
  rename(renewable_day_ahead_forecast = day_ahead_forecast)

colnames_with_capacity <- colnames(price_indispo_merge_esp)[grepl("Capacity", colnames(price_indispo_merge_esp))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables" )


price_indispo_merge_esp[colnames_with_capacity] <- lapply(price_indispo_merge_esp[colnames_with_capacity], function(x) ifelse(is.na(x), 0, x))
#
price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  mutate(across(all_of(capacity_fields), 
                ~ . / 1000)) %>% 
  mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
         Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
         Uneffective.Unavailable.Capacity = Uneffective.Unavailable.Capacity - Uneffective.Unavailable.Nuclear.Capacity,
         temperature_mismatch = temperature_demand - temperature_supply 
  ) 

price_indispo_merge_esp <- price_indispo_merge_esp[complete.cases(price_indispo_merge_esp),]
price_indispo_merge_esp <- price_indispo_merge_esp[!rowSums(sapply(price_indispo_merge_esp, is.infinite)), ]
price_indispo_merge_esp$weekday <- weekdays(price_indispo_merge_esp$delivery_date)
price_indispo_merge_esp$creneau <- hour(price_indispo_merge_esp$datetime)

colnames(price_indispo_merge_esp)
#backfill spot gas
setwd("Z:/prix_electricite_gitlab")
gas_price_data <- read_excel("gas_daily.xlsx")
colnames(gas_price_data) <- c("time","price", "virgule")
gas_price_data <- gas_price_data %>%
  mutate(gas_price = as.numeric(paste0(price, ".", virgule)),
         time =as.Date(time)) %>% 
  dplyr::select(gas_price, time)


price_indispo_merge_esp <- merge(price_indispo_merge_esp,
                                 gas_price_data,
                                 by.x = "delivery_date", by.y ="time")

#Start date	June 15, 2022
iberian_cap <- data.frame(
  Month = c("2022-06", "2022-07", "2022-08", "2022-09", "2022-10", "2022-11",
            "2022-12", "2023-01", "2023-02", "2023-03",
            "2023-04", "2023-05", "2023-06", "2023-07", "2023-08", "2023-09",
            "2023-10", "2023-11", "2023-12"),
  gas_cap = c(40, 40, 40, 40, 40, 40, 40,
          45, 50, 55, 56.1, 57.2, 58.3,
          59.4, 60.6, 61.7, 62.8, 63.9,	65)
) %>%
  mutate(my_date = as.Date(paste0(Month, "-01")),
         year = as.character(year(my_date)),
         month =  as.integer(month(my_date)))
#gérrer facteur manquant

all_years <- sort(unique(price_indispo_merge_esp$year))  # toutes les années possibles



price_indispo_merge_esp <- price_indispo_merge_esp %>% 
  left_join(iberian_cap, by = c("year", "month")) %>% 
  mutate(gas_price_capped = ifelse(delivery_date >=  as.Date("2022-06-15" ),
                                  gas_cap, gas_price),
         year_factor = factor(year, levels = all_years),
         year_factor = relevel(year_factor, ref = "2022")
         
         )

price_indispo_merge_esp_pre_iberic <- price_indispo_merge_esp %>% 
  filter(delivery_date <=  as.Date("2022-06-15" ))

price_indispo_merge_esp_post_iberic <- price_indispo_merge_esp %>% 
  filter(delivery_date >=  as.Date("2022-06-15" ))




formula_str <- "price ~  Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity+
    #Uneffective.Unavailable.Capacity + 
    Published.Unavailable.Nuclear.Capacity+
    Unpublished.Unavailable.Nuclear.Capacity +
    #Uneffective.Unavailable.Nuclear.Capacity+
    Fore_Wind + Fore_Solar +nuclear+
    thermal_tot + hydro_tot +
     log(gen_tot) + unpredicted_renewables + intraday_update_renewables + 
    temperature_demand +
    precipitation_demand + 
    relative_humidity_demand  +
    year_factor + as.factor(month)+
    + as.factor(weekday) + as.factor(creneau)"

equation <- as.formula(paste(formula_str, "+ gas_price"))
equation_cap <- as.formula(paste(formula_str, "+ gas_price_capped"))

m_esp_pre_cap <- lm(
  equation_cap,
  data= price_indispo_merge_esp_pre_iberic)

m_esp_post_cap <- lm(
  equation_cap,
  data= price_indispo_merge_esp_post_iberic)


m_esp_pre <- lm(
  equation,
  data= price_indispo_merge_esp_pre_iberic)

m_esp_post <- lm(
  equation,
  data= price_indispo_merge_esp_post_iberic)
summary(m_esp_pre)

# Matrice de variance-covariance clusterisée

vcov_esp_pre_cap  <-  NeweyWest(m_esp_pre_cap, lag = 12, prewhite = FALSE, adjust = TRUE)
cl_se_esp_pre_cap <- sqrt(diag(vcov_esp_pre_cap))

vcov_esp_post_cap  <- NeweyWest(m_esp_post_cap, lag = 12, prewhite = FALSE, adjust = TRUE)
cl_se_esp_post_cap <- sqrt(diag(vcov_esp_post_cap))

vcov_esp_pre  <- NeweyWest(m_esp_pre, lag = 12, prewhite = FALSE, adjust = TRUE)
cl_se_esp_pre <- sqrt(diag(vcov_esp_pre))

vcov_esp_post  <- NeweyWest(m_esp_post, lag = 12, prewhite = FALSE, adjust = TRUE)
cl_se_esp_post <- sqrt(diag(vcov_esp_post))




#summary(m_esp_pre)
reg_table <- stargazer(
  m_esp_pre_cap, m_esp_post_cap, m_esp_pre, m_esp_post,  
  type = "text",
  se = list(cl_se_esp_pre_cap, cl_se_esp_post_cap, cl_se_esp_pre, cl_se_esp_post),
  title = "Regression Results",
  column.labels = c("Pre Cap", "Post Cap", "Pre", "Post"),
  keep = c("gas_price", "gas_price_capped")
)

View(price_indispo_merge_esp %>% 
       dplyr::select(datetime, delivery_date, price, gen_tot, gas_price, gas_price_capped))


#en france


save_path_data <- "Z:/prix_electricite_gitlab/day_ahead/outputs/fr"
setwd(save_path_data)
price_indispo_merge_fr <- read_parquet("price_indispo_merge_fr.parquet")
price_indispo_merge_fr <- price_indispo_merge_fr %>%
  fill(spot_gas, .direction = "up") %>% 
  rename(renewable_day_ahead_forecast = day_ahead_forecast)

colnames_with_capacity <- colnames(price_indispo_merge_fr)[grepl("Capacity", colnames(price_indispo_merge_fr))]
#get monthly avg of unavailabilities
capacity_fields <- 
  c(colnames_with_capacity,
    "renewable_day_ahead_forecast",  "unpredicted_renewables", "intraday_update_renewables" )


price_indispo_merge_fr[colnames_with_capacity] <- lapply(price_indispo_merge_fr[colnames_with_capacity], function(x) ifelse(is.na(x), 0, x))
#
price_indispo_merge_fr <- price_indispo_merge_fr %>% 
  mutate(across(all_of(capacity_fields), 
                ~ . / 1000)) %>% 
  mutate(Published.Unavailable.Capacity = Published.Unavailable.Capacity - Published.Unavailable.Nuclear.Capacity,
         Unpublished.Unavailable.Capacity = Unpublished.Unavailable.Capacity - Unpublished.Unavailable.Nuclear.Capacity,
         Uneffective.Unavailable.Capacity = Uneffective.Unavailable.Capacity - Uneffective.Unavailable.Nuclear.Capacity
         #temperature_mismatch = temperature_demand - temperature_supply 
  ) 

price_indispo_merge_fr <- price_indispo_merge_fr[complete.cases(price_indispo_merge_fr),]
price_indispo_merge_fr <- price_indispo_merge_fr[!rowSums(sapply(price_indispo_merge_fr, is.infinite)), ]
price_indispo_merge_fr$weekday <- weekdays(price_indispo_merge_fr$delivery_date)
price_indispo_merge_fr$creneau <- hour(price_indispo_merge_fr$datetime)

colnames(price_indispo_merge_fr)
#backfill spot gas

price_indispo_merge_fr <- merge(price_indispo_merge_fr,
                                gas_price_data,
                                by.x = "delivery_date", by.y ="time") %>% 
  rename(temperature_demand = temperature_2m,
  precipitation_demand = precipitation,
  relative_humidity_demand = relative_humidity_2m) %>% 
  mutate(
         year_factor = factor(year, levels = all_years),
         year_factor = relevel(year_factor, ref = "2022")
)


#Start date	June 15, 2022


price_indispo_merge_fr_pre_iberic <- price_indispo_merge_fr %>% 
  filter(delivery_date <=  as.Date("2022-06-15" ))

price_indispo_merge_fr_post_iberic <- price_indispo_merge_fr %>% 
  filter(delivery_date >=  as.Date("2022-06-15" ))
colnames(price_indispo_merge_fr)



formula_str_fr <- "price ~  Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity+
    #Uneffective.Unavailable.Capacity + 
    Published.Unavailable.Nuclear.Capacity+
    Unpublished.Unavailable.Nuclear.Capacity +
    #Uneffective.Unavailable.Nuclear.Capacity+
    Fore_Wind + Fore_Solar +
     log(gen_tot) + unpredicted_renewables + intraday_update_renewables + 
    temperature_demand +
    precipitation_demand + 
    relative_humidity_demand  +
    year_factor + as.factor(month)+
    + as.factor(weekday) + as.factor(creneau)"

equation_fr <- as.formula(paste(formula_str_fr, "+ gas_price"))

m_fr_pre <- lm(
  equation_fr,
  data= price_indispo_merge_fr_pre_iberic)

m_fr_post <- lm(
  equation_fr,
  data= price_indispo_merge_fr_post_iberic)


# Matrice de variance-covariance clusterisée
# France
vcov_fr_pre  <- NeweyWest(m_fr_pre, lag = 12, prewhite = FALSE, adjust = TRUE)
cl_se_fr_pre <- sqrt(diag(vcov_fr_pre))

vcov_fr_post  <- NeweyWest(m_fr_post, lag = 12, prewhite = FALSE, adjust = TRUE)
cl_se_fr_post <- sqrt(diag(vcov_fr_post))

#summary(m_fr_pre)


reg_table <- stargazer(
    m_fr_pre, m_fr_post,
  type = "text",
  se = list( cl_se_fr_pre, cl_se_fr_post),
  title = "Regression Results",
  column.labels = c( "Pre ES", "Post ES", "Pre Cap ES", "Post Cap ES", "Pre FR", "Post FR"),
  keep = c("gas_price", "gas_price_capped")
)

library(broom)   # pour tidy()

# mettre tous tes modèles dans une liste nommée
models <- list(
  "ES_Pre"       = m_esp_pre,
  "ES_Post"      = m_esp_post,
  "ES_Pre_Cap"   = m_esp_pre_cap,
  "ES_Post_Cap"  = m_esp_post_cap,
  "FR_Pre"       = m_fr_pre,
  "FR_Post"      = m_fr_post
)

# liste de SE NW correspondant aux modèles
se_list <- list(
  "ES_Pre"       = cl_se_esp_pre,
  "ES_Post"      = cl_se_esp_post,
  "ES_Pre_Cap"   = cl_se_esp_pre_cap,
  "ES_Post_Cap"  = cl_se_esp_post_cap,
  "FR_Pre"       = cl_se_fr_pre,
  "FR_Post"      = cl_se_fr_post
)

# fonction pour extraire coef + SE déjà calculés
extract_coef <- function(model, se_vec, var){
  coefs <- coef(model)
  
  if(var %in% names(coefs)){
    return(tibble(
      term = var,
      estimate = coefs[var],
      se = se_vec[var]
    ))
  } else {
    return(tibble(term = var, estimate = NA, se = NA))
  }
}

# toutes les variables d’intérêt
vars <- c("gas_price", "gas_price_capped")

# extraction
results <- imap_dfr(models, function(mod, name){
  se_vec <- se_list[[name]]   # récupérer le vecteur de SE NW correspondant
  map_dfr(vars, ~extract_coef(mod, se_vec, .x)) %>%
    mutate(model = name)
})

# séparer infos du nom du modèle
results <- results %>%
  separate(model, into = c("Country","Period","Cap"), sep = "_", fill = "right") %>%
  mutate(
    Cap = ifelse(is.na(Cap), "NoCap", "Cap"),
    Country = ifelse(Country=="ES", "Spain", "France"),
    Period = ifelse(Period=="Pre","Pre","Post")
  )
results_stars <- results %>%
  filter(!is.na(estimate)) %>% 
  mutate(
    tval = estimate / se,
    pval = 2 * (1 - pnorm(abs(tval))),   # test bilatéral
    stars = case_when(
      pval < 0.01 ~ "***",
      pval < 0.05 ~ "**",
      pval < 0.1  ~ "*",
      TRUE        ~ ""
    ),
    est_str = sprintf("%.3f%s (%.3f)", estimate, stars, se)
  ) %>%
  dplyr::select(Country, Period, Cap, term, est_str) %>%
  pivot_wider(
    id_cols = c(Country, Cap, term),
    names_from = Period,
    values_from = est_str
  ) %>%
  arrange(Country, Cap, term)

library(gt)

table_latex <-results_stars %>%
  dplyr::select(-Cap) %>% 
  mutate(term = case_when(
    term == "gas_price" ~ "Gas",
    term == "gas_price_capped" ~ "Gas capped",
    TRUE ~ term
  )) %>% 
  gt(rowname_col = "term") %>%
  tab_header(
    title = "Gas Price Pass-through"
  ) %>%
  cols_label(
    Country = "Country",
    #Cap = "Cap Scheme",
    Pre = "Pre Period",
    Post = "Iberic Exception"
  ) %>%
  fmt_missing(everything(), missing_text = "") %>%  # si reste NA → vide
  tab_options(
    table.font.size = "small",
    table.align = "center"
  )

table_latex 

setwd("Z:/Overleaf_elec/images/latex_tables")
# exporter en LaTeX
gtsave(table_latex, "table_gas_pass.tex")


#gérrer facteur manquant





# Recupérer les niveaux utilisés dans le modèle
data_2022 <- price_indispo_merge_esp %>% 
  filter(year == "2022")

pred_post <- predict(
  m_esp_pre,
  newdata = data_2022,
  se.fit = TRUE  # pour obtenir l'intervalle de confiance
  , interval = "prediction")


# Extraire fit et SE
fit_values <- pred_post$fit
se_values  <- pred_post$se.fit


# Optionnel : intervalle 95%
# upper <- fit_values + 1.96 * se_values
# lower <- fit_values - 1.96 * se_values

# Créer un data.frame pour inspection ou plotting
pred_post_df <- data.frame(
  year = data_2022$year,
  month = data_2022$month,
  date = data_2022$delivery_date,
  price = data_2022$price,
  gen_tot = data_2022$gen_tot,
  predicted_price = fit_values
)




#by hand to have factor level 2023
# Extraire coefficients
data_2022_2023 <- price_indispo_merge_esp %>% 
  filter(year >= "2022")


coef_pre  <- coef(m_esp_pre)
coef_post <- coef(m_esp_post)
names_pre <- names(coef_pre)
# Si le facteur year2023 existe dans post mais pas dans pre :
coef_pre["year_factor2023"] <- coef_post["year_factor2023"]

# 2. Matrice de design
X_new <- model.matrix(formula(m_esp_pre), data = data_2022_2023)

# Liste des noms de colonnes manquantes
missing_cols <- setdiff(names_pre, colnames(X_new))

# Pour chaque colonne manquante, rajouter un vecteur de 0
for (col in missing_cols) {
  X_new <- cbind(X_new, 0)
  colnames(X_new)[ncol(X_new)] <- col
}

# R�ordonner les colonnes de X_new pour matcher l'ordre des coefficients
X_new <- X_new[, names(coef_pre)]

# setdiff(names(coef_pre), colnames(X_new))
# dim(X_new)
# length(coef_pre)
# 3. Point estimate
yhat <- as.numeric(X_new %*% coef_pre)

# 4. Variance-covariance des betas
vcov_beta_pre <- vcov(m_esp_pre)
vcov_beta_post <- vcov(m_esp_post)


# noms des coefficients
missing_coef <- c("year_factor2023")
# par ex. "as.factor(year)2023"
# �tendre la matrice de variance-covariance de pre
p <- length(names_pre)
new_size <- p + length(missing_coef)

# nouvelle matrice pleine de 0
vcov_beta_pre_expanded <- matrix(0, nrow = new_size, ncol = new_size)
rownames(vcov_beta_pre_expanded) <- colnames(vcov_beta_pre_expanded) <- c(names_pre, missing_coef)

# recopier l'ancienne
vcov_beta_pre_expanded[names_pre, names_pre] <- vcov_beta_pre

# pour la variance/covariance du coef manquant : 
# on peut copier la valeur de post
vcov_beta_pre_expanded[missing_coef, missing_coef] <- vcov_beta_post[missing_coef, missing_coef]

# (optionnel) copier aussi les covariances avec les autres variables si tu veux les prendre de post :
# compl�ter covariances avec les autres coefficients
other_coefs <- setdiff(names_pre, missing_coef)

names_factors_pre <- names(coef_pre[grep("year_factor", names(coef_pre))])[c(-5)]
for (mc in missing_coef) {
  vcov_beta_pre_expanded[mc, setdiff(names_pre, names_factors_pre )] <- vcov_beta_post[mc, setdiff(names_pre, names_factors_pre )]
  vcov_beta_pre_expanded[setdiff(names_pre, names_factors_pre ), mc] <- vcov_beta_post[setdiff(names_pre, names_factors_pre ), mc]
  vcov_beta_pre_expanded[names_factors_pre , mc] <- 0.5 #arbitraire
  vcov_beta_pre_expanded[ mc, names_factors_pre] <- 0.5
  vcov_beta_pre_expanded[mc, mc] <- vcov_beta_post[mc, mc]
}
# View(vcov_beta_pre_expanded)
# View(vcov_beta_pre_expanded[ ,names(coef_pre[grep("year_factor", names(coef_pre))])])

# 5. Standard errors of fit
se_fit <- sqrt(diag(X_new %*% vcov_beta_pre_expanded %*% t(X_new)))
# 4. Replace missing se_fit by 0
se_fit[is.na(se_fit)] <- 0


# 6. R�siduelle sigma^2
sigma2 <- summary(m_esp_pre)$sigma^2

#for missing se_fit fit just put zero
#anyway the confidence bands are not driven by uncertain on the reg parameters
#but uncertainty in the regression

# 7. Standard errors for prediction interval
se_pred <- sqrt(se_fit^2 + sigma2)


# 8. Quantile de Student
alpha <- 0.05
tval <- qt(1 - alpha/2, df = m_esp_pre$df.residual)

# 9. IC et PI
ci_lower <- yhat - tval * se_fit
ci_upper <- yhat + tval * se_fit

pi_lower <- yhat - tval * se_pred
pi_upper <- yhat + tval * se_pred

# 10. Stocker r�sultats
pred_post_2022_2023 <- data.frame(
  predicted_price = yhat,
  se.fit = se_fit,
  lwr.CI = ci_lower,
  upr.CI = ci_upper,
  lwr.PI = pi_lower,
  upr.PI = pi_upper,
  year = data_2022_2023$year,
  month = data_2022_2023$month,
  date = data_2022_2023$delivery_date,
  datetime = data_2022_2023$datetime,
  price = data_2022_2023$price,
  gen_tot = data_2022_2023$gen_tot
)


pred_post_df_plot <- pred_post_df %>% 
  #filter(year == "2022") %>% 
  group_by(date) %>% 
  summarise(price = mean(price,na.rm =TRUE),
            predicted_price = mean(predicted_price.fit,na.rm =TRUE),
            lower = mean(predicted_price.lwr,na.rm =TRUE),
            upper = mean(predicted_price.upr,na.rm =TRUE)) %>% 
  ungroup()



pred_post_df_plot_2022_2023 <- pred_post_2022_2023 %>%
  group_by(date) %>%
  # mutate(n = n()) %>% 
  # filter(n >= 20) %>% 
  summarise(
    n = n(),
    datetime = first(datetime),
    price = mean(price),
    predicted_price = mean(predicted_price),
    var_fit = sum(se.fit^2) / n()^2,       # variance de la moyenne
    #var_pred = 
    .groups = "drop"
  ) %>%
  mutate(
    se_fit_month = sqrt(var_fit),
    se_pred_month = sqrt(se_fit_month^2 + sigma2),
    ci_lower = predicted_price - tval * se_fit_month,
    ci_upper = predicted_price + tval * se_fit_month,
    pi_lower = predicted_price - tval * se_pred_month,
    pi_upper = predicted_price + tval * se_pred_month
  )

weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}


monthly_pred <- pred_post_2022_2023 %>%
  group_by(year, month) %>%
  summarise(
    n = n(),
    datetime = median(datetime),
    price = weighted_mean(price, gen_tot),
    predicted_price = weighted_mean(predicted_price, gen_tot),
    gen_tot = mean(gen_tot),
    var_fit = weighted_mean(se.fit^2, gen_tot) / n(),       # variance de la moyenne
    #var_pred = sum(se_pred^2) / n()^2,     # variance pr�diction
    .groups = "drop"
  ) %>%
  mutate(
    se_fit_month = sqrt(var_fit),
    se_pred_month = sqrt(se_fit_month^2 + sigma2),
    ci_lower = predicted_price - tval * se_fit_month,
    ci_upper = predicted_price + tval * se_fit_month,
    pi_lower = predicted_price - tval * se_pred_month,
    pi_upper = predicted_price + tval * se_pred_month
  )


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs")
write_parquet(pred_post_df, "prediction_gas_price_counterfactual_2022.parquet")
write_parquet(pred_post_2022_2023 %>% 
                dplyr::select(-c(lwr.CI, upr.CI)), "prediction_gas_price_counterfactual.parquet")
write_parquet(monthly_pred , "prediction_gas_price_counterfactual_monthly.parquet")

#on confirme les deux pr�dictions donnent la m�me chose
#j'ai bien r�pliqu� la fonction predict

save_var_path <- 'Z:/prix_electricite_gitlab/Data_Structuring/output'
setwd(save_var_path)
IV_data_esp <-  read_parquet('IV_data_esp_elas.parquet')

save_var_path <- 'Z:/prix_electricite_gitlab/Data_Structuring/output'
setwd(save_var_path)
IV_data_fr <-  read_parquet('IV_data_fr_elas.parquet')

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/fr")
price_indispo_merge <- read_parquet("price_indispo_merge_delta_supply_demand_fr.parquet")


library(latex2exp)

colnames(price_indispo_merge_fr)

price_indispo_merge_fr_plot <- price_indispo_merge_fr %>% 
  group_by(delivery_date) %>% 
  summarise(price = mean(price, na.rm = TRUE),
            gas_price = mean(gas_price, na.rm = TRUE),
            year = first(year)
            )

price_plot <- ggplot( ) +
  # Observé
  geom_line(data = pred_post_df_plot_2022_2023, aes(x =date,y = price, color ="Elec ES", linetype = "Observed"), size = 0.7) +
  # Prédiction
  #geom_line(data = pred_post_df_plot, aes(x =date, y = predicted_price, color ="Elec", linetype = "Counterfactual"), color = "blue", size = 0.7) +
  # Intervalle de confiance
  #geom_ribbon(data = pred_post_df_plot, aes(x =date, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  # Prédiction
  geom_line(data = pred_post_df_plot_2022_2023, aes(x =date, y = predicted_price, color ="Elec ES", linetype = "Counterfactual"), size = 0.7) +
  # Intervalle de confiance
  geom_ribbon(data = pred_post_df_plot_2022_2023, aes(x =date, ymin = pi_lower, ymax = pi_upper, fill = "Elec 95% CI"), alpha = 0.2)+#,show.legend = FALSE ) +
  
  geom_line(data = IV_data_esp %>% filter(observation_date >= as.Date("2022-05-01")), aes(x = observation_date, y = spot_gas_capped, color ="Gas TTF", linetype = "Observed"))+
  geom_line(data = price_indispo_merge_fr_plot %>% filter(year >= 2022), aes(x = delivery_date, y = gas_price, color ="Gas TTF",  linetype = "Counterfactual"))+
  geom_line(data = price_indispo_merge_fr_plot %>% filter(year >= 2022), aes(x = delivery_date, y = price, color ="Elec FR", linetype = "Observed"))+
  
  labs(
    #title = "Prix prédit vs. observé (Post 2022)",
    x = "Date",
    y = expression(paste("Price (", "\u20AC","/Mwh)"))
  ) +
  scale_fill_manual(
    values = c(
      "Gas" = "blue",
      "Elec 95% CI" = "orange")
    )+
  scale_color_manual(
        values = c(
          "Gas TTF" = "blue",
          "Elec ES" = "orange",
          "Elec FR" = "red"
        ))+
  scale_linetype_manual(
    values = c(
      "Observed" = "solid",
      "Counterfactual" = "dashed"
    )
  )+
  theme(legend.text = element_text(size = 15),      # taille du texte
        legend.title = element_text(size = 16),     # taille du titre
        legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
        axis.title.x = element_text(size = 14),   # taille du titre axe x
        axis.title.y = element_text(size = 14),   # taille du titre axe y
        axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
        axis.text.y  = element_text(size = 12)    # taille des ticks y
        )

price_plot

save_plot_path <- 'Z:/Overleaf_elec/images/policy'
setwd(save_plot_path)
ggsave("iberian_exception_plot_price_counterfactual.png", plot = price_plot, width = 14 , height = 6, dpi = 300)

  




#check construction correcte des intervalles de confiance

# price_plot <- ggplot( ) +
#   #geom_line(data = pred_post_df_plot, aes(x =date, y = predicted_price, color ="Elec", linetype = "Counterfactual"), color = "blue", size = 0.7) +
#   # Intervalle de confiance
#   #geom_ribbon(data = pred_post_df_plot, aes(x =date, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
#   # Prédiction
#   geom_line(data = pred_post_2022_2023, aes(x =datetime, y = predicted_price, color ="hourly"), size = 0.7) +
#   # Intervalle de confiance
#   geom_ribbon(data = pred_post_2022_2023, aes(x =datetime, ymin = lwr.PI, ymax =  upr.PI, fill ="hourly"), alpha = 0.2)+#,show.legend = FALSE ) +
#   
#   geom_line(data = pred_post_df_plot_2022_2023, aes(x =datetime, y = predicted_price, color ="daily"), size = 0.7) +
#   # Intervalle de confiance
#   geom_ribbon(data = pred_post_df_plot_2022_2023, aes(x =datetime, ymin = pi_lower, ymax = pi_upper, fill = "daily"), alpha = 0.2)+#,show.legend = FALSE ) +
#   
#   geom_line(data = monthly_pred, aes(x =datetime, y = predicted_price, color ="monthly"), size = 0.7) +
#   # Intervalle de confiance
#   geom_ribbon(data = monthly_pred, aes(x =datetime, ymin = pi_lower, ymax = pi_upper, fill = "monthly"), alpha = 0.2)+#,show.legend = FALSE ) +
#   labs(
#     #title = "Prix prédit vs. observé (Post 2022)",
#     x = "Date",
#     y = expression(paste("Price (", "\u20AC","/Mwh)"))
#   ) +
#   scale_fill_manual(
#     values = c(
#       "hourly" = "blue",
#       "daily" = "red",
#       "monthly" = "green")
#   )+
#   scale_color_manual(
#     values = c(
#       "hourly" = "blue",
#       "daily" = "red",
#       "monthly" = "green"))+
#   # scale_linetype_manual(
#   #   values = c(
#   #     "Observed" = "solid",
#   #     "Counterfactual" = "dashed"
#   #   )
#   # )+
#   theme(legend.text = element_text(size = 15),      # taille du texte
#         legend.title = element_text(size = 16),     # taille du titre
#         legend.key.size = unit(1.5, "cm") ,          # taille des symboles/rectangles,
#         axis.title.x = element_text(size = 14),   # taille du titre axe x
#         axis.title.y = element_text(size = 14),   # taille du titre axe y
#         axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),  # taille et rotation des ticks x
#         axis.text.y  = element_text(size = 12)    # taille des ticks y
#   )
# 
# price_plot



