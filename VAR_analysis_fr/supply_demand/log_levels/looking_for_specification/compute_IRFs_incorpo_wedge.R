
#lorsque je l'ajoute comme deuxième proxy, on voit que tout seul pas relevant
#obtiens exactement le même theta_0_supply qui n'est pas du obn signe


headers_var_gen_first_mp_exp <- c(headers_var_gen_first_mp, "spot_gas_log")
headers_var_spot_first_mp_exp <- c(headers_var_spot_first_mp, "spot_gas_log")

vars_demand_wedge<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp)], frequency=12)
vars_supply_wedge<- ts(IV_data_log_esp[, c(headers_var_spot_first_mp)], frequency=12)

p = 12
irhor = 20
x_demand_wedge =ts(vars_demand_wedge , start=c(2015,1), end=c(2025,9), frequency=12)
x_supply_wedge =ts(vars_supply_wedge , start=c(2015,1), end=c(2025,9), frequency=12)
y =vars_demand_wedge[-1:-p, ]
n <- ncol(vars_demand_wedge)
k <- 1
#nbre d'instruments
l <- 2


controls <- NULL
# Fit the VAR model
vl <- if (is.null(controls)) {
  vars::VAR(x_demand_wedge, p, type = "none")
} else {
  vars::VAR(x_demand_wedge, p, type = "none", exogen = controls)
}

coef = coef(vl)
bet = do.call(cbind, lapply(1:n, function(x) coef[[x]][,1]))
res = residuals(vl)
colnames(IV_data_log_esp)
x= y
proxies <- ts(IV_data_log_esp[, c("delta_supply_ng", "wedge_iber_exception")], frequency=12)
proxies <- ts(IV_data_log_esp[, c("delta_supply_ng")], frequency=12)
theta_0_supply_wedge_gen <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0
theta_0_supply_wedge_gen


#
#on sait que wedge affecte le prix de l'elec
#par l'intermédiaire de l'élasticité estimée -0.3 affecte de manière équivalente la qté

#de combien delta_supply fait-il bouger la quantité
vars_demand_wedge<- ts(IV_data_log_esp[, c(headers_var_gen_first_mp_exp)], frequency=12)
vars_supply_wedge<- ts(IV_data_log_esp[, c(headers_var_spot_first_mp_exp)], frequency=12)

p = 12
irhor = 20
x_demand_wedge =ts(vars_demand_wedge , start=c(2015,1), end=c(2025,9), frequency=12)
x_supply_wedge =ts(vars_supply_wedge , start=c(2015,1), end=c(2025,9), frequency=12)
y =vars_demand_wedge[-1:-p, ]


IV_data_log_esp$year <- year(IV_data_log_esp$observation_date)
fs_data <- IV_data_log_esp[, c(headers_var_gen_first_mp_gas, 
                               "delta_dem_ng","delta_supply_ng", "year")][-1:-p, ]
fs_data$res_gen <- res[,1]
fs_data$res_price <- res[,2]
m_wedge <- lm(
  "res_gen ~ delta_supply_ng + wedge_iber_exception + factor(year)",
  data = fs_data
)
s <- summary(m_wedge)

# lignes à garder (tout sauf factor(year))
keep <- !grepl("^factor\\(year\\)", rownames(s$coefficients))

s$coefficients[keep, ]

m_wedge_bis <- lm(
  "res_gen ~ delta_supply_ng + wedge_iber_exception",
  data = fs_data %>% filter(year== 2022)
)
s_bis <- summary(m_wedge_bis)

m_wedge_ter <- lm(
  "res_price ~ delta_supply_ng + wedge_iber_exception",
  data = fs_data
)
s_ter <- summary(m_wedge_ter)

ggplot(data = IV_data_log_esp, aes(x= observation_date))+
  geom_line(aes(y = spot_price))+
  geom_line(aes(y = gen_tot_da/100))+
  geom_line(aes(y = wind_forecast/10))


b <- coef(m_wedge)[c("delta_supply_ng", "wedge_iber_exception")]
IV_data_log_esp$delta_supply_incorpo <- with(
  IV_data_log_esp,
  b["delta_supply_ng"] * delta_supply_ng +
    b["wedge_iber_exception"] * wedge_iber_exception
)



ggplot(data = IV_data_log_esp)+
  geom_line(aes(x= observation_date, y= b["delta_supply_ng"] * delta_supply_ng, color = "Initial"))+
  geom_line(aes(x= observation_date, y= delta_supply_incorpo, color = "With Wedge"))+
  theme_minimal()


proxies <- ts(IV_data_log_esp[, c("delta_supply_ng")], frequency=12)
theta_0_supply_wedge_gen <- first_var_estimation_bis(y, res, p, proxies, irhor, controls =controls, elas = 0)$theta_0
theta_0_supply_wedge_gen


