
#ajout d'une estimation de la demand elasticity
#en considérant forecast error 
#pour les jours proche d'une annonce covid


#Load errors are predominantly demand driven
# Est ce que l'on peut les expliquer en demand + renewables
# en contrôlant par les update de wind et sun


# Assure-toi que tes FE sont bien des facteurs (tu l'as déjà fait plus haut)
wind_reg_data <- wind_reg_data %>%
  mutate(
    year    = factor(year),
    month   = factor(month),
    weekday = factor(weekday),
    creneau = factor(creneau)
  ) %>%
  mutate(
    right_term_demand_0to1 = price_diff_0to1 * load_tot_da / price_da,
    price_diff_0to5        = price_diff_0to1 + price_diff_1to5,
    right_term_demand_0to5 = price_diff_0to5 * load_tot_da / price_da,
    qty_0to5               = qty_1to5 + qty_first_1,
    load_error             = load_tot_da - load_forecast_da
  )

fe_part <- "year + month + weekday + creneau"

vars_to_resid <- c(
  "qty_0to5",
  "right_term_demand_0to5",
  "price_diff_0to5",
  "update_wind", "update_solar",
  "load_error",
  controls_others
)

fe_models <- setNames(lapply(vars_to_resid, function(v) {
  fml <- as.formula(paste0(v, " ~ 1 | ", fe_part))
  feols(fml, data = wind_reg_data)
}), vars_to_resid)


covid_auction_data <- wind_reg_data %>%
  filter(map_lgl(datetime, ~ any(.x >= lockdowns$start - days(1) &
                                   .x <= lockdowns$start + days(4))))
for (v in vars_to_resid) {
  covid_auction_data[[paste0(v, "_res")]] <-
    covid_auction_data[[v]] - predict(fe_models[[v]], newdata = covid_auction_data)
}



build_ols_formula <- function(y, x) {
  as.formula(paste0(y, " ~ ", paste(x, collapse = " + ")))
}

build_iv_formula <- function(y, x_exog, endog, instr) {
  # FE block = 0 because you already residualized
  as.formula(paste0(
    y, " ~ ", paste(c(x_exog), collapse = " + "),
    " | ", endog, " ~ ", instr
  ))
}
y      <- "qty_0to5_res"
endog  <- "right_term_demand_0to5_res"
instr  <- "load_error_res"

specs <- list(
  OLS = list(
    type = "ols",
    x = c(
      endog,
      paste0(c("price_da", "load_tot_da", "update_wind", "update_solar"), "_res")
    )
  ),
  IV_full = list(
    type = "iv",
    x_exog = paste0(c(controls_others, "update_wind", "update_solar"), "_res")
  ),
  IV_bis = list(
    type = "iv",
    x_exog = paste0(c("price_da", "load_tot_da", "update_wind", "update_solar"), "_res")
  ),
  IV_ter = list(
    type = "iv",
    x_exog = c("1")  # no exogenous controls besides the endogenous regressor
  )
)

estimate_models <- function(data, specs, y, endog, instr, time_var = "datetime", dk_lag = 24) {
  
  vc <- as.formula(paste0("DK(", dk_lag, ") ~ ", time_var))
  
  models <- lapply(names(specs), function(nm) {
    sp <- specs[[nm]]
    
    if (sp$type == "ols") {
      fml <- build_ols_formula(y, sp$x)
    } else if (sp$type == "iv") {
      fml <- build_iv_formula(y, sp$x_exog, endog, instr)
    } else {
      stop("Unknown spec type: ", sp$type)
    }
    
    feols(fml, data = data, vcov = vc)
  })
  
  setNames(models, names(specs))
}

models_fx <- estimate_models(
  data = covid_auction_data,
  specs = specs,
  y = y,
  endog = endog,
  instr = instr,
  time_var = "datetime",  # or "day" if you prefer DK on days
  dk_lag = 24
)

etable(
  models_fx,
  keep = endog,
  headers = c("OLS", "IV (1)", "IV (2)", "IV (3)")
)


coef_se_ols_for_err_fx <- extract_coef_ivf(models_fx$OLS, var_name =endog ,IV = FALSE)
coef_se_iv_for_err_fx  <- extract_coef_ivf(models_fx$IV_full ,var_name =endog, IV = TRUE)

df_for_err_id <- tibble(
  Model = c("Intraday OLS", "Intraday IV"),
  Estimate = c(coef_se_ols_for_err_fx$Coef, coef_se_iv_for_err_fx$Coef),
  StdError = c(coef_se_ols_for_err_fx$Se,   coef_se_iv_for_err_fx$Se)
) %>%
  mutate(
    Elasticity_CI = paste0(round(Estimate, 5), " (", round(StdError, 5), ")"),
    Instrument = c("OLS", "IV"),
    Type = "Intraday"
  ) %>%
  dplyr::select(Model, Elasticity_CI, Instrument, Type)


setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
write.csv(df_for_err_id, "df_for_err_intrday")






covid_abnormal_data <-  merge(wind_reg_data,
                             df_2020 %>% 
                               filter(as.numeric(month) >= 3, as.Date(datetime) <= as.Date("2020-05-15")) %>% 
                               filter(load_error_abnormal) %>% 
                               select(datetime),
                             by = "datetime")

for (v in vars_to_resid) {
  covid_abnormal_data[[paste0(v, "_res")]] <- covid_abnormal_data[[v]] - predict(fe_fits[[v]], newdata = covid_abnormal_data)
}

m_iv_load_abnormal_err <- ivreg(iv_formula_str_load_err , data = covid_abnormal_data)
summary(m_iv_load_abnormal_err)

m_iv_load_err_abnormal_bis <- ivreg(iv_formula_str_load_err_bis , data = covid_abnormal_data)
summary(m_iv_load_err_abnormal_bis)

m_iv_load_err_abnormal_ter<- ivreg(iv_formula_str_load_err_ter, data = covid_abnormal_data)
summary(m_iv_load_err_abnormal_ter)


