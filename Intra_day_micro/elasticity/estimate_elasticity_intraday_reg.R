library(AER)
library(stargazer)

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
         unpredicted_wind = `Wind Onshore` -intraday_Wind
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
                      "gen_exch_da"#, "gen_pred_da","gen_exch_spot"
                     )


present_vars <- c(
  "qty_1to5",                  # dependent variable
  "price_diff_1to5",           # needed to build right_term
  "gen_exch_da", "price_da",   # needed for right_term too
  "QTY", "PRICE", "update_wind",
  controls_clean,              # raw versions of controls_vars
  setdiff(controls_others, c("price_da", "gen_exch_da")) # others
)

# Build the formula as a string
ols_formula_str_wind <- as.formula(
  paste("qty_1to5", "~",  
  paste(c("right_term",  controls_vars 
          ,setdiff(controls_others, c("price_da",  "gen_exch_da"))
          ),
        collapse = " + ")),
)
m_ols_wind <- ivreg(
  ols_formula_str_wind,
  data = auction_reg_renewable_data %>%
    dplyr::select(all_of(present_vars))%>%
    filter(price_da >1) %>% 
    mutate(right_term = price_diff_1to5*gen_exch_da/price_da) %>%
    filter(complete.cases(.)) 
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
iv_formula_str_wind <- as.formula(paste(
  paste("qty_1to5", "~",  paste(c("right_term", controls_vars, controls_others), collapse = " + ")),
  paste(paste(c("update_wind", controls_vars, controls_others), collapse = " + ")),
  sep = " | "
))

m_iv_wind <- ivreg(iv_formula_str_wind , data = auction_reg_renewable_data %>%
                  dplyr::select(all_of(present_vars))%>%
                  filter(price_da >1) %>% 
                  mutate(right_term = price_diff_1to5*gen_exch_da/price_da) %>%
                  filter(complete.cases(.)))


# Create LaTeX code with regression results
reg_table_renwables_bis <- stargazer(
  m_ols_wind,
  m_iv_wind,
  #m_renew_plant, 
  type = "text",
  title = "Regression Results",
  column.labels = c("OLS",  "IV"),#,
  #add.lines = list(c("Adjusted F-stat",  NULL, fstat_adj))
  keep = c("log_price", "right_term" )
)



extract_coef_se <- function(model, variable = "log(gen_tot)") {
  s <- summary(model)
  est <- s$coefficients[variable, "Estimate"]
  se  <- s$coefficients[variable, "Std. Error"]
  return(c(est = est, se = se))
}

coef_se_ols_wind  <- extract_coef_se(model = m_ols_wind, variable="right_term")
coef_se_iv_wind  <- extract_coef_se(m_iv_wind, "right_term")


df_wind_id <- data.frame(
  Model = c("Intraday OLS", 'Intraday IV'),
  Estimate = c(coef_se_ols_wind["est"], coef_se_iv_wind["est"] ),
  StdError = c(coef_se_ols_wind["se"], coef_se_iv_wind["se"]  )
)

df_wind_id$Elasticity_CI <- paste0(
  round(df_wind_id$Estimate, 5), " (", round(df_wind_id$StdError, 5), ")"
)


df_wind_id <- df_wind_id %>%
  select(Model,Elasticity_CI ) %>% 
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

df_clean_da <- read.csv("df_clean_da.csv") %>% 
  filter(Type =="Demand") %>% 
  mutate(Type = "Day-ahead")

df_clean_macro <- read.csv("df_clean_macro.csv")%>% 
  filter(Type =="Demand")%>% 
  mutate(Type = "Monthly")


# Step 1: Combine the three dataframes
df_all <- bind_rows(df_wind_id, df_clean_da, df_clean_macro)

# Step 2: Keep only OLS and IV instruments
df_plot <- df_all %>%
  filter(Instrument %in% c("OLS", "IV"))

# Step 3: Ensure 'Type' is a factor with the desired order
df_plot <- df_plot %>%
  mutate(Type = factor(Type, levels = rev(c("Intraday", "Day-ahead", "Monthly"))))

# Step 4: Extract numeric Elasticity and SE from Elasticity_CI
df_plot <- df_plot %>%
  mutate(
    Elasticity = as.numeric(str_extract(Elasticity_CI, "^-?\\d+\\.?\\d*")),
    SE = as.numeric(str_extract(Elasticity_CI, "(?<=\\()\\-?\\d+\\.?\\d*(?=\\))"))
  )

# Step 5: Plot
plot_supply_elas <- ggplot(df_plot, aes(x = Type, y = Elasticity, color = Instrument)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = Elasticity - SE, ymax = Elasticity + SE),
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(
    x = NULL,
    y = "Elasticity",
    #title = "Time Evolution of Supply Elasticities"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("OLS" = "blue", "IV" = "red"))

save_latex_path <- "Z:/Overleaf_elec/images/latex_tables"
setwd(save_latex_path)

# Save the plot as a PDF (or PNG)
ggsave(
  filename = "plot_supply_elas.png",   # or "plot_supply_elas.png"
  plot = plot_supply_elas,
  width = 10,    # width in inches
  height = 6,   # height in inches
  units = "in",
  device = "png" # or "png"
)





