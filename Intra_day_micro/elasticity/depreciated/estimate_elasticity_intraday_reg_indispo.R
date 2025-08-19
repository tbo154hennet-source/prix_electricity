


save_path_entsoe_output <- 'Z:/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)

auction_reg_data <- read_parquet("auction_reg_data.parquet") %>%
  group_by(datetime) %>%
  mutate(
    price_diff = case_when(
      auction_before == 1 ~ PRICE - price_da,
      TRUE ~ PRICE - lag(PRICE),
      right_term = price_diff*gen_exch_da/price_da
      # ,
      # Fore_Solar = Fore_Solar /1000,
      # Fore_Wind = Fore_Wind /1000,
      # `Wind Onshore`= `Wind Onshore`/1000,
      # intraday_Wind = intraday_Wind /1000,
      # update_wind = intraday_Wind - Fore_Wind,
      # unpredicted_wind = `Wind Onshore` -intraday_Wind
    )
  ) %>% 
  ungroup()


auction_reg_data %>% 
  filter(Published.Predicted.Unavailable.Capacity>0) %>% 
  group_by(auction) %>% 
  summarise(n = n())


colnames(auction_reg_data)

# View(auction_reg_data %>% 
#        select(datetime, gen_tot,  price_diff,
#               gen_pred_da, gen_exch_da, PRICE,
#               price_da, auction_before, auction, Published.Predicted.Unavailable.Capacity))


#se débarrasser des jours avec des sessions d'enchère incomplètes

# Créer une version "nettoyee
controls_vars <- c("spot_gas",    
                   "as.factor(year)" , "as.factor(month)",
                   "as.factor(weekday)", "as.factor(creneau)"
)
controls_clean <- c( "year", "month", "weekday", "creneau", "spot_gas")
controls_others <- c( "day_ahead_renewable_forecast",
                      "unpredicted_renewables"  ,"intraday_update_renewables",
                     "temperature_supply" , "precipitation_supply",
                     "relative_humidity_supply", "price_da",
                     "gen_exch_da"
                     
                     
                     #, "gen_pred_da","gen_exch_spot"
)


present_vars <- intersect(
                          c(grep("Capacity", colnames(auction_reg_data), value = TRUE) %>%
                              setdiff(grep("Uneffective|Nuclear", colnames(auction_reg_data), value = TRUE)),
                            controls_clean, controls_others, "price_diff",
                        "auction",  "gen_pred_da","gen_exch_spot", "QTY", "PRICE", "right_term")
                , colnames(auction_reg_data))

reg_data <- auction_reg_data %>%
  #enlever plein de zéros
  #garder ceux où il y a un évènement d'indispo
  # group_by(datetime) %>% 
  # mutate(dummy = sum(Published.Predicted.Unavailable.Capacity, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # filter(dummy >0) %>% 
  
  
  dplyr::select(all_of(present_vars))%>%
  filter(PRICE > 1 & price_da >1) %>% 
  mutate(right_term = price_diff*gen_pred_da/PRICE) %>%
  filter(complete.cases(.))

# Build the formula as a string
ols_formula_str_indispo <- as.formula(
  paste("QTY", "~",  
        paste(c("right_term",  controls_vars 
                ,setdiff(controls_others, c("price_da",  "gen_exch_da"))
        ),
        collapse = " + ")),
)

m_ols_indispo <- ivreg(
  ols_formula_str_indispo,
  data =  reg_data
)

ols_formula_str_indispo_int <- as.formula(
  paste("QTY", "~",  
        paste(c("right_term*as.numeric(auction)",  controls_vars,
                setdiff(controls_others, c("price_da",  "gen_exch_da"))
        ),
        collapse = " + ")),
)
m_ols_indispo_int <- ivreg(
  ols_formula_str_indispo_int,
  data = reg_data
)

# auction_reg_renewable_data %>%
#   dplyr::select(all_of(rev(present_vars)))%>% 
#   filter(price_da <= 0)
# 
# test = auction_reg_renewable_data %>%
#   dplyr::select(all_of(present_vars))%>% 
#   mutate(right_term = price_diff_1to5*gen_exch_da/price_da) %>%
#   filter(complete.cases(.))
# View(test)

# Build the formula as a string
iv_formula_str_indispo <- as.formula(paste(
  paste("QTY", "~",  paste(c("right_term", controls_vars, controls_others), collapse = " + ")),
  paste(paste(c("Published.Predicted.Unavailable.Capacity", controls_vars, controls_others), collapse = " + ")),
  sep = " | "
))

m_iv_indispo <- ivreg(iv_formula_str_indispo , data = reg_data)


iv_formula_str_indispo_int <- as.formula(paste(
  paste("QTY", "~",  paste(c("right_term*as.numeric(auction)", controls_vars, controls_others), collapse = " + ")),
  paste(paste(c("Published.Predicted.Unavailable.Capacity*as.numeric(auction)", controls_vars, controls_others), collapse = " + ")),
  sep = " | "
))


m_iv_indispo_int <- ivreg(
  iv_formula_str_indispo_int,
  data = reg_data 
)


# Create LaTeX code with regression results
reg_table <- stargazer(
  m_ols_indispo,
  m_iv_indispo,
  m_ols_indispo_int,
  m_iv_indispo_int,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS",  "IV", "OLS",  "IV"),#,
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("log_price", "right_term" )
)











