
dim(proxies)
dim(proxies_kanzig)
proxies <- unlist(proxies)
class(proxies)

proxies <- ts(unlist(IV_data_log_diff_esp[, c(headers_proxies_supply)]), frequency=12)

VAR_test <-SVARIV(x,proxies,p,
                  confidence,NWlags,
                  norm,scale,irhor,instrument_name="test")


irf_data <- VAR_test$irfs
irf_data <- irf_data %>%
  filter(confi_type == "msw" )#| confi_type == "msw_cum")

lower_68 <-make_bandwith_obj(irf_data, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
upper_68 <-make_bandwith_obj(irf_data, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))

lower_90 <-make_bandwith_obj(irf_data, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
upper_90 <-make_bandwith_obj(irf_data, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))




save_var_path <- 'Z:/prix_electricite_gitlab/VAR_analysis_fr/VAR_output'
setwd(save_var_path)
IV_data_log_esp <-  read_parquet('IV_data_esp_log.parquet')

# correct for the Iberian exception
colnames(IV_data_log_esp)
IV_data_log_esp$observation_date <- as.Date(IV_data_log_esp$observation_date)


#   
headers_proxies_supply = c(
  "wind_speed_100m_log"#,
  #"delta_d_total"
)


headers_var_spot_first <- c("spot_price_log",
                            "gen_tot_log",
                            "IPI_log",
                            "HICP_log")



proxies <- ts(unlist(IV_data_log_esp[, c(headers_proxies_supply)]), frequency=12)

vars<- ts(IV_data_log_esp[, c(headers_var_spot_first)], frequency=12)

p = 12
irhor = 20
x =ts(vars , start=c(2015,1), end=c(2023,12), frequency=12)
y =vars[-1:-p, ]
T = nrow(y)
#variables dans VAR
n <- ncol(y)
#nbre de variables instrumentees
k <- 1
#nbre d'instruments
l <- ncol(proxies)

VAR_test_log <-SVARIV(x,proxies,p,
                  confidence,NWlags,
                  norm,scale,irhor,instrument_name="test")



irf_data <- VAR_test_log$irfs
irf_data_cum <- irf_data%>%
  filter(confi_type == "msw" )#| confi_type == "msw_cum")

lower_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
upper_68 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))

lower_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
upper_90 <-make_bandwith_obj(irf_data_cum, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))



series <- vars[,1]
# Appliquer à toutes les colonnes
results <- lapply(as.data.frame(vars), stationarity_tests)

# Compiler en table
stationarity_table_elec <- bind_rows(results, .id = "Series")
stationarity_table_elec$Series <- colnames(vars)

stationarity_table

stationarity_table_elec 

headers_var_spot_first <- c("spot_price_log",
                            "gen_tot_log",
                            "IPI_log",
                            "HICP_log")
vars<- ts(IV_data_log_esp[, c(headers_var_spot_first)], frequency=12)


# Appliquer à toutes les colonnes
results <- lapply(as.data.frame(vars), stationarity_tests)

# Compiler en table
stationarity_table_elec_log <- bind_rows(results, .id = "Series")
stationarity_table_elec_log$Series <- colnames(vars)

stationarity_table

stationarity_table_elec 

stationarity_table_elec_log
