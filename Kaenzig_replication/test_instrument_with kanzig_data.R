
colnames(VAR_data_log_sov_proxies)
VAR_data_log_sov$date
colnames(VAR_data_log)
VAR_data_log$observation_date

test_VAR <- merge(VAR_data_log_sov_proxies ,
                  VAR_data_log%>% 
                    dplyr::select(observation_date, PC1_OPEC,
                                  PC1_Supply,PC1_Demand),
                  
      by.x = "date", by.y ="observation_date")



my_var_supply =
  sovereign::VAR(
    data = test_VAR %>% 
      select(-PC1_OPEC, -PC1_Demand, -proxies_kanzig),
    horizon = 10,
    freq = 'month',
    p = 4,
    # lag.ic = 'BIC',
    # lag.max = 4,
    structure = "IV",
    instrument = "PC1_Supply",
    instrumented = "X1"
    
  )
my_var_kanzig =
  sovereign::VAR(
    data = test_VAR %>% 
      select(-PC1_OPEC, -PC1_Demand, -PC1_Supply),
    horizon = 10,
    freq = 'month',
    p = 4,
    # lag.ic = 'BIC',
    # lag.max = 4,
    structure = "IV",
    instrument = "proxies_kanzig",
    instrumented = "X1"
    
  )

my_var_tot =
  sovereign::VAR(
    data = test_VAR %>% 
      mutate(PC1_tot = PC1_OPEC + PC1_Supply + PC1_Demand) %>% 
      select(-PC1_OPEC, -PC1_Demand, -PC1_Supply, -proxies_kanzig),
    horizon = 10,
    freq = 'month',
    p = 4,
    # lag.ic = 'BIC',
    # lag.max = 4,
    structure = "IV",
    instrument = "PC1_tot",
    instrumented = "X1"
    
  )

var.irf.kanzig = sovereign::var_irf(my_var_kanzig)

plot_irf(var.irf.kanzig, shocks = "X1", responses = NULL, verticle = FALSE)

# impulse response functions
var.irf.OPEC = sovereign::var_irf(my_var_OPEC)

plot_irf(var.irf.OPEC, shocks = "X1", responses = NULL, verticle = FALSE)

var.irf.tot = sovereign::var_irf(my_var_tot)

plot_irf(var.irf.supply, shocks = "X1", responses = NULL, verticle = FALSE)

