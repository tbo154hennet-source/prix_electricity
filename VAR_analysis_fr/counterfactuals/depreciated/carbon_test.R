
#petit test sur le carbon price

formula_str <- "price ~  Published.Unavailable.Capacity + Unpublished.Unavailable.Capacity+
    #Uneffective.Unavailable.Capacity + 
    Published.Unavailable.Nuclear.Capacity+
    Unpublished.Unavailable.Nuclear.Capacity +
    #Uneffective.Unavailable.Nuclear.Capacity+
    Fore_Wind + Fore_Solar +nuclear+
    thermal_tot + hydro_tot +
     log(gen_tot) + unpredicted_renewables + intraday_update_renewables + 
    temperature_demand +
    precipitation_demand + 
    relative_humidity_demand  +
    #year_factor +
    as.factor(month)+
    + as.factor(weekday) + as.factor(creneau)"

equation <- as.formula(paste(formula_str, "+ gas_price + carbon_price"))

m_esp<- lm(
  equation,
  data= price_indispo_merge_esp)
summary(m_esp)
