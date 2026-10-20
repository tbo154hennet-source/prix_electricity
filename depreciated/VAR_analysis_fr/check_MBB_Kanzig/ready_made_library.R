
library(varexternal)

p           = 12    #Number of lags in the VAR model
NWlags      = 0;  # Newey-West lags(if it is neccessary to account for time series autocorrelation)
norm        = 1; # Variable used for normalization
scale       = 1; # Scale of the shock
horizons    = 50; #Number of horizons for the Impulse Response Functions(IRFs)
confidence=c(0.68,0.90);

VAR_test <-SVARIV(data$data,proxies_kanzig,p,
                  confidence,NWlags,norm,scale,horizons,instrument_name="test")
unique(VAR_test$irfs$confi_type)

irf_data <- VAR_test$irfs
irf_data <- irf_data %>%
  filter(confi_type == "msw" )#| confi_type == "msw_cum")

dim(bandwith_68)

View(irf_data %>% 
  filter((confi_level == 0.68) &
         (type        == "lower")))

make_bandwith_obj <- function(irf_data, my_confi_level, my_type, template_dim){
  
  # 1) filtrer et passer en large
  wide_tbl <- irf_data %>% 
    filter(confi_level == my_confi_level&
           type        == my_type) %>%
    dplyr::select(variable, horizon, value) %>% 
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    arrange(horizon) %>% 
    dplyr::select(-horizon)
  
  # 2) extraire les deux éléments (array 3D)
  mat12 <- apply(wide_tbl, c(1, 2), function(x) x)   # list matrix
  # On transforme en array 3D [horizon × variable × 2]
  #arr <- array(NA, dim = c(dim(mat12), 1))
  
  # # Extraire structure à 2 valeurs
  # # mat1 = premier élément, mat2 = second élément
  # mat1 <- apply(wide_tbl, c(1, 2), function(x) x[[1]])
  #mat2 <- apply(wide_tbl, c(1, 2), function(x) x[[2]])
  
  # # 3) reconstruire new_obj avec même structure que bandwith_68
  # new_obj <- array(NA, dim = template_dim)
  # 
  # new_obj[,,1] <- mat1[1,1:50,]
  # new_obj[,,2] <- mat1[2,1:50,]
  
  #return( mat12[1:50,])
  return( mat12)
}

lower_68 <-make_bandwith_obj(irf_data, my_confi_level=  0.68, my_type="lower", dim(bandwith_68))
dim(lower_68)
lower_68[1,1]
upper_68 <-make_bandwith_obj(irf_data, my_confi_level=  0.68, my_type="upper", dim(bandwith_68))

lower_90 <-make_bandwith_obj(irf_data, my_confi_level=  0.90, my_type="lower", dim(bandwith_90))
upper_90 <-make_bandwith_obj(irf_data, my_confi_level=  0.90, my_type="upper", dim(bandwith_90))





dim(new_obj)

sh.col<-      c("#E41A1C")
names(sh.col)<-c("test")

pretty_irf(data=list(VAR_test$irfs),
           shock_names="test",
           pretty_names=c("a","b","c"),
           manual_color=sh.col,title="subheading")

unique(irf_data$confi_type)
unique(irf_data$confi_level)





