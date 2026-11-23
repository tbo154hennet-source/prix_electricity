

#relevance for standard day ahead specification

controls_vars <- c( "year", "month", "weekday", "creneau")
controls_clean <- controls_vars[!grepl("^as\\.factor\\(", controls_vars)]

setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/day_ahead/outputs/esp")
price_indispo_merge_bronte_clean <- read_parquet( "price_indispo_merge_bronte_clean.parquet")
colnames(price_indispo_merge_bronte_clean)
price_indispo_merge_bronte_clean <- price_indispo_merge_bronte_clean %>% 
  mutate(Fore_Wind = Fore_Wind /1000,
         intraday_Wind = intraday_Wind /1000,
         `Wind Onshore` = `Wind Onshore`/1000,
         update_wind = intraday_Wind - Fore_Wind ,
         unpredicted_wind = `Wind Onshore` -intraday_Wind ,
  )

price_indispo_merge_bronte_clean %>% 
  select(wind_speed_100m, 
         Fore_Wind, 
         update_wind,
         unpredicted_wind
  ) %>%
  cor( use = "pairwise.complete.obs")

#on remarque que l'update est parfaitement orthogonale ? la premi?re pr?diction


# FSref_formula_str <- as.formula(
#   paste("log(price+1)", "~",  paste(c(
#     "wind_speed_100m", "update_wind", "unpredicted_wind",
#     controls_clean), collapse = " + ")),
# )
#m_FSref <-  ivreg(FSref_formula_str , data = price_indispo_merge_bronte_clean)
FSref_formula_str_bis <- as.formula(
  paste("log(price+1)", "~",  paste(c(
    "Fore_Wind", "update_wind","unpredicted_wind",
    controls_clean), collapse = " + ")),
)



m_FSrefbis <-  ivreg(FSref_formula_str_bis , data = price_indispo_merge_bronte_clean)








#relevance for intraday




library(AER)

save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)

auction_reg_renewable_data <- read_parquet( "auction_reg_renewable_data.parquet") %>% 
  mutate(Fore_Solar = Fore_Solar /1000,
         Fore_Wind = Fore_Wind /1000,
         `Wind Onshore`= `Wind Onshore`/1000,
         intraday_Wind = intraday_Wind /1000,
         update_wind = intraday_Wind - Fore_Wind,
         unpredicted_wind = `Wind Onshore` -intraday_Wind,
         gen_pred_da = gen_pred_da/1000,
         right_term_1to5 = price_diff_1to5*gen_exch_da/price_da,
         right_term_5 = price_diff_5*gen_exch_da/price_da
         
  )%>%
  filter(
    (creneau <= 4  & number_auctions == 3) |
      (creneau <= 7  & number_auctions == 4) |
      (creneau <= 12 & number_auctions == 5) |
      (creneau > 12  & number_auctions == 6)
  )%>% 
  filter(price_da >1) %>% 
  distinct()

auction_reg_renewable_data_bis <- read_parquet( "auction_reg_renewable_data_bis.parquet") %>% 
  mutate(Fore_Solar = Fore_Solar /1000,
         Fore_Wind = Fore_Wind /1000,
         `Wind Onshore`= `Wind Onshore`/1000,
         intraday_Wind = intraday_Wind /1000,
         update_wind = intraday_Wind - Fore_Wind,
         unpredicted_wind = `Wind Onshore` -intraday_Wind,
         gen_pred_da = gen_pred_da/1000,
         right_term = price_diff_1to5*gen_exch_da/price_da
  ) %>% 
  rename(price_diff = price_diff_1to5)%>%
  filter(
    (creneau <= 4  & number_auctions == 3) |
      (creneau <= 7  & number_auctions == 4) |
      (creneau <= 12 & number_auctions == 5) |
      (creneau > 12  & number_auctions == 6)
  )%>% 
  filter(price_da >1) %>% 
  distinct()

# auction_reg_data$intraday_update_renewables[10000:nrow(auction_reg_data)]
# auction_reg_data %>% 
#   filter(Published.Predicted.Unavailable.Capacity >0) %>% 
#   pull(Published.Predicted.Unavailable.Capacity, datetime)
# Créer une version "nettoyee
controls_vars <- c( "year", "month", "weekday", "creneau")
controls_clean <- controls_vars[!grepl("^as\\.factor\\(", controls_vars)]
controls_others <- c("Fore_Solar", "Fore_Wind" ,
                     "temperature_supply" , "precipitation_supply",
                     "relative_humidity_supply", "price_da", "gen_pred_da")


#first stage
#check for relevance

FS15_formula_str <- as.formula(
  paste("right_term_1to5", "~",  paste(c("update_wind", "unpredicted_wind",
                                             controls_clean, controls_others), collapse = " + ")),
)

FS5_formula_str <- as.formula(
  paste("right_term_5", "~",  paste(c("update_wind","unpredicted_wind",
                                          controls_clean, controls_others), collapse = " + ")),
)
FSall_formula_str <- as.formula(
  paste("right_term", "~",  paste(c("update_wind","unpredicted_wind",
                                          controls_clean, controls_others), collapse = " + ")),
)

# View( auction_reg_renewable_data %>% 
#         select(datetime,log_price_diff_5, log_price_diff_1to5, price_da,
#                update_wind,unpredicted_wind, Fore_Wind
#                ) %>% 
#         na.omit())

m_FS15 <- ivreg(FS15_formula_str , data = auction_reg_renewable_data[20:nrow(auction_reg_renewable_data),] )


m_FS5 <-  ivreg(FS5_formula_str , data = auction_reg_renewable_data[20:nrow(auction_reg_renewable_data),])

m_FSall <-  ivreg(FSall_formula_str , data = auction_reg_renewable_data_bis[20:nrow(auction_reg_renewable_data_bis),])







#on fait les indispos




save_path_entsoe_output <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/Intra_day_micro/outputs'
setwd(save_path_entsoe_output)

auction_reg_data <- read_parquet("auction_reg_data.parquet") %>%
  group_by(datetime) %>%
  mutate(
    price_diff = case_when(
      auction_before == 1 ~ PRICE - price_da,
      TRUE ~ PRICE - lag(PRICE)
      # ,
      # Fore_Solar = Fore_Solar /1000,
      # Fore_Wind = Fore_Wind /1000,
      # `Wind Onshore`= `Wind Onshore`/1000,
      # intraday_Wind = intraday_Wind /1000,
      # update_wind = intraday_Wind - Fore_Wind,
      # unpredicted_wind = `Wind Onshore` -intraday_Wind
    ),
    right_term = price_diff*gen_exch_da/price_da
  ) %>% 
  ungroup()

# View(auction_reg_data %>% 
#        select(datetime, gen_tot,  price_diff,
#               gen_pred_da, gen_exch_da, PRICE,
#               price_da, auction_before, auction, Published.Predicted.Unavailable.Capacity))

colnames(auction_reg_data)
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
    "gen_pred_da","gen_exch_spot", "QTY", "PRICE", "right_term")
  , colnames(auction_reg_data))


#first stage
#check for relevance

FSindispo_formula_str <- as.formula(
  paste("right_term", "~",  paste(c("Published.Predicted.Unavailable.Capacity", 
                                    "Unpublished.Predicted.Unavailable.Capacity",
                                             controls_clean, controls_others), collapse = " + ")),
)
FSindispo_formula_str_int <- as.formula(
  paste("right_term", "~",  paste(c("Published.Predicted.Unavailable.Capacity*as.numeric(auction)", 
                                    controls_clean, controls_others), collapse = " + ")),
)

View( auction_reg_data %>%
        select(datetime,price_diff, right_term, price_da,
               Published.Predicted.Unavailable.Capacity, 
               Unpublished.Predicted.Unavailable.Capacity
               ) %>%
        na.omit())

m_FSindispo <- ivreg(FSindispo_formula_str , 
                data = auction_reg_data %>%
                  filter(price_da >1) %>% 
                  na.omit() )
m_FSindispo_int <- ivreg(FSindispo_formula_str_int , 
                     data = auction_reg_data %>%
                       filter(price_da >1) %>% 
                       na.omit() )

summary(m_FSindispo)
summary(m_FSindispo_int)



# Create LaTeX code with regression results
reg_table_relevance <- stargazer(
  #m_FS,
  m_FSrefbis,
  m_FSall,
  m_FS15,
  m_FS5,
  m_FSindispo,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("Day-ahead", "Intraday Wind",  "Intraday Wind Pre 5", "Intraday Wind Post 5", "Intraday Indispo"),#,
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("gen_tot", "price", 
           "intraday_update_renewables", "Fore_Wind", 
           "Published.Predicted.Unavailable.Capacity",
           "Unpublished.Predicted.Unavailable.Capacity",
           "update_wind", "unpredicted_wind"
           
  )
)



save_latex_path <- "C:/Users/DBT3KD/Desktop/Etude_macro_elec/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)

output_file <- file.path(save_latex_path, "elasticity_relevance_table.tex")

stargazer(
  m_FSrefbis,
  m_FSall,
  m_FS15,
  m_FS5,
  m_FSindispo,
  type = "latex",
  title = "Regression Results",
  column.labels = c("Day-ahead", "Intraday Wind",  "Intraday Wind Pre 5", "Intraday Wind Post 5", "Intraday Indispo"),#,
  keep = c("gen_tot", "price", 
           "intraday_update_renewables", "Fore_Wind", 
           "Published.Predicted.Unavailable.Capacity",
           "Unpublished.Predicted.Unavailable.Capacity",
           "update_wind", "unpredicted_wind"
  ),
  out = output_file
)


#enelver indispo inutilies et ajouter macro elasticity


library(AER)

#elasticity at the monthly level
#colnames(IV_data_log_diff_esp)
#colnames(IV_data_log_diff_fr)

#wind_forecast

save_var_path <- 'C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/depreciated/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_diff_esp <-  read_parquet('IV_data_esp_log_diff_elas.parquet')

# correct for the Iberian exception
colnames(IV_data_log_diff_esp)
IV_data_log_diff_esp$observation_date <- as.Date(IV_data_log_diff_esp$observation_date)
IV_data_log_diff_esp <- IV_data_log_diff_esp %>%
  mutate(
    PC1_supply = ifelse(observation_date >= as.Date("2022-06-01") & observation_date <= as.Date("2023-05-31"),
                        0, PC1_supply),
    PC1_supply = ifelse(observation_date == as.Date("2022-06-01"), -0.5, PC1_supply)
  ) %>% 
  rename(
    cold_excess_instrument = cold_excess_supply,
    heat_excess_instrument = heat_excess_supply,
    temp_excess_instrument = temp_excess_supply
  )
IV_data_log_diff_esp <- IV_data_log_diff_esp %>% 
  mutate(
    diff_cbf = (cbf_es - lag(as.numeric(cbf_es)))/1000,
    ffr_diff = ffr - lag(as.numeric(ffr))
  )%>%
  fill(ffr_diff, diff_cbf, .direction = "up")



controls_vars <- c("Spot_gas",    
                   "as.factor(year)")

FSmacro_formula <- as.formula(paste(
  paste("log(spot_price +1)", "~",  paste(c("Fore_Wind", controls_vars), collapse = " + "))))  

m_FSmacro <- ivreg(FSmacro_formula , data = IV_data_log_diff_esp %>% 
                     mutate(Fore_Wind = wind_forecast/1000))
summary(m_FSmacro)




reg_table_relevance <- stargazer(
  m_FSmacro,
  m_FSrefbis,
  m_FSall,
  m_FS15,
  m_FS5,
  #m_FSindispo,
  type = "text",
  title = "Regression Results",
  column.labels = c( "Monthly", "Day-ahead", "Intraday Wind",  "Intraday Wind Pre 5", "Intraday Wind Post 5"),#, "Intraday Indispo"),#,
  keep = c("gen_tot", "price", 
           "intraday_update_renewables", "Fore_Wind", 
           # "Published.Predicted.Unavailable.Capacity",
           # "Unpublished.Predicted.Unavailable.Capacity",
           "update_wind", "unpredicted_wind"
  ))


output_file_bis <- file.path(save_latex_path, "elasticity_relevance_table_without_indispo.tex")

stargazer(
  m_FSmacro,
  m_FSrefbis,
  m_FSall,
  m_FS15,
  m_FS5,
  #m_FSindispo,
  type = "latex",
  title = "Regression Results",
  column.labels = c( "Monthly", "Day-ahead", "Intraday Wind",  "Intraday Wind Pre 5", "Intraday Wind Post 5"),#, "Intraday Indispo"),#,
  keep = c("gen_tot", "price", 
           "intraday_update_renewables", "Fore_Wind", 
           # "Published.Predicted.Unavailable.Capacity",
           # "Unpublished.Predicted.Unavailable.Capacity",
           "update_wind", "unpredicted_wind"
  ),
  out = output_file_bis
)

