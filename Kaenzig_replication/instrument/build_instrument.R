# Load required libraries
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(tidyr)     # For reshaping data
library(purrr)     # For functional programming
library(pracma)    # For PCA (similar to MATLAB's pca function)

# Define the sample
start_year <- 1983
end_year <- 2017

sample_dates_proxy <- paste0(rep(start_year:end_year, each = 12), 
                             "M", sprintf("%02d", rep(1:12, end_year - start_year + 1)))
sample_dates_num <- seq(start_year + (1 - 1) / 12, 
                        end_year + (12 - 1) / 12, by = 1 / 12)

sample_dates_proxy <- sample_dates_proxy[-(1:3)]
sample_dates_num <- sample_dates_num[-(1:3)]

sample_dates_proxy_q <- paste0(rep(start_year:end_year, each = 4), 
                               "Q", rep(1:4, end_year - start_year + 1))[-(1:1)]

# Get announcement dates
setwd("C:/Users/DBT3KD/Desktop/uncertainty_shocks/uncertainty_materials_gitlab/Kaenzig_replication/instrument")
opec_announcement <- read_excel('../data/OPECannouncements.xlsx', sheet = "Daily", skip =1)
statement_dates <- opec_announcement[[1]]
statement_dates_char <- as.character(statement_dates)
statement_months <- paste0(substr(statement_dates_char, 1, 4), "M", substr(statement_dates_char, 6, 7))
statement_quarters <- paste0(substr(statement_dates_char, 1, 4), 
                             "Q", ceiling(as.numeric(substr(statement_dates_char, 6, 7)) / 3))

statement_dates_trading_days <- opec_announcement[[1]]
statement_info <- opec_announcement[, c(3, 4)]

# Get futures data
# Oil futures (WTI contracts)
oil_futures <- read_excel('../data/Oilfutures.xlsx', sheet = "POIL", skip =1)
p_wti <- log(oil_futures[, 2:25]) * 100
futures_dates <- oil_futures[[1]][-1]

# Volume
volume_futures <- read_excel('../data/Oilfutures.xlsx', sheet = "VOL", skip =1)
v_wti <- volume_futures[, 2:25]

# Construct oil price proxies
contract_max <- 13
wf <- 1 # Days after announcement
wb <- 0 # Days before announcement

# Initialize proxies
oil_proxies_wti_futures <- matrix(0, nrow = length(statement_dates), ncol = contract_max)
proxy_volumes_wti <- array(0, dim = c(length(statement_dates), contract_max, 2))


library(lubridate)
get_index <- function(jj) {
  date_ind <- which(futures_dates == statement_dates[jj])
  date <- statement_dates[jj]
  
  while (length(date_ind) == 0) {
    date <- date  + days(1)
    date_ind <- which(futures_dates == date)
  }
  return(date_ind)
}
date_ind <- get_index(1)
date_ind <- get_index(5)
date_ind <- get_index(7)
p_wti[date_ind, 1:contract_max]
p_wti[date_ind +1, 1:contract_max]
# Compute surprises
jj=7
for (jj in seq_along(statement_dates)) {
  date_ind <- get_index(jj)
  oil_proxies_wti_futures[jj, ] <- as.numeric(p_wti[date_ind + wf, 1:contract_max] - p_wti[date_ind - wb, 1:contract_max])
  proxy_volumes_wti[jj, , 1] <- as.numeric(v_wti[date_ind + wf, 1:contract_max])
  proxy_volumes_wti[jj, , 2] <- as.numeric(v_wti[date_ind - wb, 1:contract_max])
}

oil_proxies_wti_futures[is.na(oil_proxies_wti_futures)] <- 0

# Principal components
proxy_sel <- oil_proxies_wti_futures[, 2:13]
pca_res <- prcomp(proxy_sel, scale. = TRUE)
oil_proxies_pc <- pca_res$x[, 1]
oil_proxies_pc <- oil_proxies_pc / sd(oil_proxies_pc) * mean(apply(proxy_sel, 2, sd))

# Aggregate proxies
oil_proxies_wti <- cbind(oil_proxies_wti_futures, oil_proxies_pc)

# Aggregate to monthly frequency
unique_sm <- unique(statement_months)
nmax <- max(table(statement_months))

oil_proxies_wti_m <- matrix(0, nrow = length(sample_dates_proxy), ncol = ncol(oil_proxies_wti))
statement_info_m <- list(decision = matrix(NA, nrow = length(sample_dates_proxy), ncol = nmax),
                         type = matrix(NA, nrow = length(sample_dates_proxy), ncol = nmax))
statement_mind <- rep(0, length(sample_dates_proxy))

jj <- 1
for (ii in seq_along(sample_dates_proxy)) {
  #ii=1
  relevant_indices <- which(statement_months == sample_dates_proxy[ii])
  if (length(relevant_indices) == 1) {
    oil_proxies_wti_m[ii, ] <- oil_proxies_wti[relevant_indices, ]
    statement_info_m$decision[ii, 1:length(relevant_indices)] <- as.numeric(statement_info[relevant_indices, 1])
    statement_info_m$type[ii, 1:length(relevant_indices)] <-  as.numeric(statement_info[relevant_indices, 2])
    statement_mind[ii] <- 1
    jj <- jj + 1
  } else if (length(relevant_indices) > 1) {
    oil_proxies_wti_m[ii, ] <- colSums(oil_proxies_wti[relevant_indices, , drop = FALSE])
    statement_info_m$decision[ii, 1:length(relevant_indices)] <- as.numeric(t(statement_info[relevant_indices, 1]))
    statement_info_m$type[ii, 1:length(relevant_indices)] <-  as.numeric(t(statement_info[relevant_indices, 2]))
    statement_mind[ii] <- 1
    jj <- jj + length(relevant_indices)
  }
}

# Save results
saveRDS(list(sample_dates_proxy = sample_dates_proxy, oil_proxies_wti = oil_proxies_wti, 
             oil_proxies_wti_m = oil_proxies_wti_m, statement_info_m = statement_info_m, 
             statement_mind = statement_mind), file = "OilSurprisesMLog.rds")


library(R.matlab)
# Replace "your_file.mat" with the path to your .mat file
mat_data <- readMat("OilSurprisesMLog.mat")

mat_data$sampleDatesProxy
View(mat_data$oilProxiesWTI)
View(mat_data$oilProxiesWTIM)
# Explore the data
str(mat_data) 

# Extracting each element from the list
sampleDatesProxy <- sapply(mat_data$sampleDatesProxy, function(x) x[[1]])

# Convert to character vector if needed
sampleDatesProxy <- as.character(sampleDatesProxy)


