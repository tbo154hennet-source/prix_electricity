
library(varexternal)

p           = 12    #Number of lags in the VAR model
NWlags      = 0;  # Newey-West lags(if it is neccessary to account for time series autocorrelation)
norm        = 1; # Variable used for normalization
scale       = 1; # Scale of the shock
horizons    = 20; #Number of horizons for the Impulse Response Functions(IRFs)
confidence=c(0.68,0.95);

VAR_test <-SVARIV(data$data,proxies_kanzig,p,
                  confidence,NWlags,norm,scale,horizons,instrument_name="test")

sh.col<-      c("#E41A1C")
names(sh.col)<-c("test")
pretty_irf(data=list(VAR_test$irfs),
           shock_names="test",
           pretty_names=c("a","b","c"),
           manual_color=sh.col,title="subheading")



kanzig_dates <- unlist(data$sampleDates)

date_year = as.numeric(substr(kanzig_dates, 1, 4))
date_month = as.numeric(substr(kanzig_dates, 6, 7))
date <- as.Date(paste(date_year, date_month, 1, sep = "-"))


library(sovereign)
# estimate VAR

VAR_data_log_sov<- data.frame(date, data$data)
VAR_data_log_sov_proxies <- data.frame(date, data$data, proxies_kanzig)
colnames(VAR_data_log_sov_proxies)

my_var =
  sovereign::VAR(
    data = VAR_data_log_sov,
    horizon = 10,
    freq = 'month',
    p = 4
    # lag.ic = 'BIC',
    # lag.max = 4
    # structure = "IV",
    # instrument = "proxies",
    # instrumented = "deflat_last_spot_price_log"
    
  )

my_var_IV =
  sovereign::VAR(
    data = VAR_data_log_sov_proxies,
    horizon = 10,
    freq = 'month',
    p = 4,
    # lag.ic = 'BIC',
    # lag.max = 4,
    structure = "IV",
    instrument = "proxies_kanzig",
    instrumented = "X1"
    
  )

# impulse response functions
var.irf = sovereign::var_irf(my_var)

plot_irf(var.irf, shocks = "X1", responses = NULL, verticle = FALSE)

# impulse response functions
var.irf.iv = sovereign::var_irf(my_var_IV)

plot_irf(var.irf.iv,  shocks = "X1", responses = NULL, verticle = FALSE)

View(data$data)
