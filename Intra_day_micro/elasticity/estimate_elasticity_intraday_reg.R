library(AER)
library(stargazer)
library(dplyr)
library(tidyr)
library(purrr)
library(arrow)

# nota bene gen_da est une prédiction
# alors que j'obtiens le day ahead réalisé avec mes auction data
#potentiellement un instrument supplémentaire voir benati

#with renewables

save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)
auction_reg_renewable_data <- read_parquet( "auction_reg_renewable_data.parquet") %>% 
  mutate(Fore_Solar = Fore_Solar /1000,
         Fore_Wind = Fore_Wind /1000,
         `Wind Onshore`= `Wind Onshore`/1000,
         intraday_Wind = intraday_Wind /1000,
         update_wind = intraday_Wind - Fore_Wind,
         update_solar = intraday_Solar - Fore_Solar,
         unpredicted_wind = `Wind Onshore` -intraday_Wind,
         unpredicted_solar = Solar - intraday_Solar
  )


#se débarrasser des jours avec des sessions d'enchère incomplètes
auction_reg_renewable_data %>% 
  group_by(number_auctions) %>% 
  summarise(n = n())
colnames(auction_reg_renewable_data)
#je me restreins aux jours complets 
# l faudrait détailler par auction

auction_reg_renewable_data <- auction_reg_renewable_data %>%
  filter(
    (creneau <= 4  & number_auctions == 3) |
      (creneau <= 7  & number_auctions == 4) |
      (creneau <= 12 & number_auctions == 5) |
      (creneau > 12  & number_auctions == 6)
  )%>% 
  distinct()


# Créer une version "nettoyee
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
controls_clean <- c( "year", "month", "weekday", "creneau", "spot_gas")
controls_others <- c("Fore_Solar", "Fore_Wind" ,
                     "temperature_supply" , "precipitation_supply",
                     "relative_humidity_supply", "price_da",
                      "load_tot_da"#, "gen_pred_da","gen_exch_spot"
                     )

present_vars <- c(
  "qty_1to5",  "qty_5", "qty_first_1",   "price_first_1",            # dependent variable
  "price_diff_0to1" , "price_diff_5",  "price_diff_1to5",           # needed to build right_term
  "load_tot_da", "load_forecast_da", "price_da",   # needed for right_term too
  "QTY", "PRICE", "update_wind", "update_solar",
  controls_clean,              # raw versions of controls_vars
  setdiff(controls_others, c("price_da", "load_tot_da", "load_forecast_da")) # others
)

library(fixest)
library(dplyr)

# données
wind_reg_data <- auction_reg_renewable_data %>%
  dplyr::select(all_of(c(present_vars, "datetime"))) %>%
  filter(price_da > 1) %>%
  mutate(right_term = price_diff_1to5 * load_tot_da / price_da) %>%
  filter(complete.cases(.)) %>% 
  mutate(year = factor(year), month = factor(month),
         creneau = factor(creneau)
         )

# RHS OLS (tu enlèves price_da et load_tot_da comme tu faisais)
rhs_ols <- c(
  "right_term",
  controls_vars,
  setdiff(controls_others, c("price_da", "load_tot_da"))
)


fml_ols <- as.formula(paste("qty_1to5 ~", paste(rhs_ols, collapse = " + ")))

m_ols_wind_fx <- feols(
  fml_ols,
  data = wind_reg_data,
  vcov = DK(24) ~ datetime
)
rhs_exog <- c(controls_clean, controls_others)

fml_iv <- as.formula(paste(
  "qty_1to5 ~",
  paste(c(rhs_exog), collapse = " + "),
  "| right_term ~ update_wind"   # <-- IV fixest
))

m_iv_wind_fx <- feols(
  fml_iv,
  data = wind_reg_data,
  vcov = DK(24) ~ datetime
)
etable(
  m_ols_wind_fx,
  m_iv_wind_fx,
  se = "standard",          # SE déjà définies via vcov DK
  dict = c(right_term = "right_term"),
  keep = "right_term",
  headers = c("OLS (DK24)", "IV (DK24)")
)




# Fonction pour extraire coef + IVF
extract_coef_ivf <- function(model, var_name = "fit_log(price + 1)", IV = TRUE) {
  # Si model est une liste (supply/demand), prendre le premier élément
  if (is.list(model) && inherits(model[[1]], "fixest")) {
    mod <- model[[1]]
  } else if (inherits(model, "fixest")) {
    mod <- model
  } else {
    stop("Model must be a fixest object or a list of fixest objects")
  }
  
  # Coefficient
  
  if (IV){
    coef_val <- mod$coefficients[paste0("fit_",var_name)]
    se_val <-mod$se[paste0("fit_",var_name)]
    # IV-F statistic
    ivf_val <- fitstat(summary(mod, stage = 1), type = "ivf")
    
    # Retourne un tibble
    return(tibble(
      Model = deparse(substitute(model)),  # nom du modèle
      Coef = coef_val,
      Se = se_val,
      IVF = ivf_val$ivf1$stat
    ))
    
  }else{
    
    coef_val <- mod$coefficients[var_name]
    se_val <-mod$se[var_name]
    return(tibble(
      Model = deparse(substitute(model)),  # nom du modèle
      Coef = coef_val,
      Se = se_val,
      IVF = NA_real_
    ))
  }
  
}

coef_se_ols_wind  <- extract_coef_ivf(model = m_ols_wind_fx, var_name ="right_term", IV = FALSE)
coef_se_iv_wind  <- extract_coef_ivf(model =m_iv_wind_fx, var_name ="right_term")


df_wind_id <- data.frame(
  Model = c("Intraday OLS", 'Intraday IV'),
  Estimate = c(coef_se_ols_wind["Coef"], coef_se_iv_wind["Coef"] ),
  StdError = c(coef_se_ols_wind["Se"], coef_se_iv_wind["Se"]  )
)

df_wind_id$Elasticity_CI <- paste0(
  round(df_wind_id$Estimate, 5), " (", round(df_wind_id$StdError, 5), ")"
)


df_wind_id <- df_wind_id %>%
  dplyr::select(Model,Elasticity_CI ) %>% 
  mutate(
    Instrument = case_when(
      grepl("OLS", Model) ~ "OLS",
      grepl("IV", Model) ~ "IV",
      grepl("Bonte", Model) ~ "Bonte et al (2015)"
    ),
    Type = "Intraday"
  )



library(stringr)
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
write.csv(df_wind_id, "df_wind_intrday")



# Day ahead forecast 18:00 on day before delivery
# That is between the first and second auction of the intraday market
# 
# Intraday forecast at 8.00: at least one update to be published at 8.00 Brussels time on the day of actual delivery for intra-day. 
# This value is the most recent forecast at 8:00 the day of delivery



library(fixest)

# Dépendantes à estimer
dep_vars <- c("price_diff_1to5", "price_diff_0to1", "price_diff_5")

# RHS commun
rhs_vars <- c("update_wind", controls_vars, controls_others)

# time var pour DK (à adapter)
time_var <- "datetime"  # ou "auction_date_plus_one" si c'est ta vraie dimension temps

# (Optionnel) FE si tu en as: fe_vars <- c("creneau", "month", "weekday") etc.
# Si pas de FE: fe_part <- ""
# Si FE: fe_part <- paste(fe_vars, collapse = " + ")
fe_part <- ""  # <- mets ici tes FE si tu veux: "creneau + weekday" etc.

# Fonction pour faire une formule fixest
make_feols_fml <- function(y, xvars, fe_part = "") {
  rhs <- paste(xvars, collapse = " + ")
  if (nzchar(fe_part)) {
    as.formula(paste0(y, " ~ ", rhs, " | ", fe_part))
  } else {
    as.formula(paste0(y, " ~ ", rhs))
  }
}

# Estimation en batch
models_fs <- setNames(lapply(dep_vars, function(y) {
  fml <- make_feols_fml(y, rhs_vars, fe_part = fe_part)
  feols(fml, data = wind_reg_data, vcov = as.formula(paste0("DK(24) ~ ", time_var)))
}), dep_vars)

# Afficher un tableau
etable(models_fs)


