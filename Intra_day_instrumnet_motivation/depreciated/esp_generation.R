
setwd('C:/Users/nchu/Desktop/DATA/ENTSOE_python_data/esp')

updated_final_indispo <- read_parquet('updated_final_indispo.parquet')
updated_final_indispo <- updated_final_indispo %>% 
  mutate(datetime = as.POSIXct(paste(electricity_delivery_date, creneau-1)
                               , format="%Y-%m-%d %H", tz = 'UTC'))

first_stage_data <- left_join(price_diff_data, updated_final_indispo,
                              by= c('auction_before', 'auction_after',
                                    'datetime', 'creneau'))


first_stage_data <- first_stage_data %>% 
  select("price", 'price_diff', "delivery_date", "log_price_diff",           
         "auction_before", "auction_after", 'creneau','centrale_unavailability',
         "real_unavailability", "theoretical_unavailability",'datetime' )






setwd("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/esp")

generation_by_fuel <-  read_parquet('generation_by_fuel.parquet')


generation_by_fuel <- generation_by_fuel %>% 
  rowwise() %>% 
  mutate(gen_tot = sum(c_across(-`__index_level_0__`), na.rm = TRUE),
         gen_uncontrolled = sum(`('Solar', 'Actual Aggregated')`,
                                `('Wind Onshore', 'Actual Aggregated')`,
                                `('Wind Offshore', 'Actual Aggregated')`)) %>% 
  select(`__index_level_0__`, gen_tot, gen_uncontrolled)


first_stage_data <- merge(first_stage_data, generation_by_fuel,
                          by.x= 'datetime', by.y= '__index_level_0__')

# Drop rows with NA in log_price column
first_stage_data  <- 
  first_stage_data[!is.na(first_stage_data$log_price_diff), ]%>%
  # simple normalization
  # daily avg max generation 40 GW
  # in percentage
  mutate(real_unavailability = 
           ifelse(is.na(real_unavailability), 0, real_unavailability ),
         real_unavailability_share = real_unavailability/gen_tot,
         theoretical_unavailability = 
           ifelse(is.na(theoretical_unavailability), 0,theoretical_unavailability),
         theoretical_unavailability_share = theoretical_unavailability/gen_tot,
         centrale_unavailability = 
           ifelse(is.na(centrale_unavailability), 0, centrale_unavailability ),
         centrale_unavailability_share = centrale_unavailability/gen_tot,
  )

# get clean_data to perform regression
my_reg_data <- first_stage_data[complete.cases(first_stage_data), ]
my_reg_data <- my_reg_data %>%
  filter(is.finite(log_price_diff) & 
           is.finite(real_unavailability_share))
# create unique creneau auction code for clustering
my_reg_data$idcode <- as.character(paste(my_reg_data$creneau,
                                         my_reg_data$auction_before ))

# test <- my_reg_data %>% filter(real_unavailability_share>0)
# View(test[, c('real_unavailability_share','centrale_unavailability_share')])
setwd('C:/Users/nchu/Desktop/DATA/merged_data')

write_parquet(my_reg_data, 'updated_first_stage_data.parquet')


