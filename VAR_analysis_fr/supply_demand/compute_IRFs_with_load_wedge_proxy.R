
IV_data_log_esp$wedge_iber_exception <- IV_data_log_esp$spot_gas - IV_data_log_esp$spot_gas_capped
IV_data_log_esp<- IV_data_log_esp %>% 
  mutate(wedge_iber_exception= ifelse(wedge_iber_exception > 10, wedge_iber_exception, 0 ))


IV_data_log_esp$year <- year(IV_data_log_esp$observation_date)
fs_data <- IV_data_log_esp[, c(headers_var_spot_first_mp_gas, 
                               "delta_demand","delta_supply", "year", "wedge_iber_exception")][-1:-p, ]
fs_data$res_price <- res[,1]
m_wedge <- lm(
  "res_price ~ delta_supply  + wedge_iber_exception + factor(year)",
  data = fs_data
)
s <- summary(m_wedge)

# lignes à garder (tout sauf factor(year))
keep <- !grepl("^factor\\(year\\)", rownames(s$coefficients))

s$coefficients[keep, ]

m_wedge_bis <- lm(
  "res_price ~ delta_supply  + wedge_iber_exception",
  data = fs_data %>% filter(year== 2022)
)
s_bis <- summary(m_wedge_bis)
s_bis






b <- coef(m_wedge)[c("delta_supply", "wedge_iber_exception")]
IV_data_log_esp$delta_supply_incorpo <- with(
  IV_data_log_esp,
  b["delta_supply"] * delta_supply +
    b["wedge_iber_exception"] * wedge_iber_exception
)

write_parquet(IV_data_log_esp, 'IV_data_esp_log_elas_sd_2026_pc.parquet')
getwd()


ggplot(data = IV_data_log_esp)+
  geom_line(aes(x= observation_date, y= delta_supply/1000000, color = "Initial"))+
  geom_line(aes(x= observation_date, y= delta_supply_incorpo, color = "With Wedge"))+
  labs(x= "Time", y ="Shock")
  theme_minimal()



controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_supply, p, type = "none")
} else {
  vars::VAR(x_supply, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)

proxies <- ts(IV_data_log_esp[, c("delta_supply")[[1]]], frequency=12)
theta_0_supply <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0

lambda_0_log_supply <- get_lambda_0(C, theta_0_supply)
# nboot = 1000

proxies_wedge <- ts(IV_data_log_esp[, c("delta_supply_incorpo")], frequency=12)
theta_0_supply_wedge <- first_var_estimation_bis(y, res, p, proxies_wedge, irhor, controls =controls, elas = 0)$theta_0
lambda_0_log_supply_wedge <- get_lambda_0(C, theta_0_supply_wedge)

theta_0_supply
theta_0_supply_ng
theta_0_supply_wedge

lambda_0_log_supply
lambda_0_log_supply_ng
lambda_0_log_supply_wedge

VAR_supply_pc_wedge <-SVARIV(ydata = x_supply, z = IV_data_log_esp$delta_supply_incorpo,p,
                       confidence,NWlags,
                       norm,scale,irhor,instrument_name="supply")


