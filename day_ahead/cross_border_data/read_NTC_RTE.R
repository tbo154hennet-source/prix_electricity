#remotes::install_github("dreamRs/rte.data")

#ylpSsT5Z 


library(readr)
library(dplyr)
library(readxl)
library(arrow)
library(lubridate)
library(rte.data)
# To create a token you can use id_client and id_secret
id_client <- "cc84e271-2131-4ad9-ba0c-bbaf68942ef7"
id_secret <- "bc89150f-7869-456d-b4a9-b75b4856842b"
token <- get_token(
  key = list(id_client = id_client, id_secret = id_secret)
)

# Voir les fonctions disponibles
ls("package:rte.data")

# Créer une séquence de dates par intervalles de 7 jours
start_dates <- seq(ymd("2015-01-01"), ymd("2024-12-31"), by = "7 days")

start_date1 <- "2022-01-01T00:00:00Z"
end_date1   <- "2022-01-06T00:00:00Z"

ntc_data <- get_open_api(
    api = "ntc",
    resource = "ntc",
    start_date = start_date1,
    end_date = end_date1,
    token = token
  )

# Fonction pour télécharger un batch
get_ntc_batch <- function(start_date, border = "FR-GB") {
  end_date <- start_date + days(6)
  
  if (day(start_date) == 1 && month(start_date) == 1) {
    print(start_date)
  }
  tryCatch({
    get_open_api(
      api = "ntc",
      resource = "ntc",
      start_date = paste0(start_date, "T00:00:00Z"),
      end_date = paste0(end_date, "T23:59:59Z"),
      token = token
    )
  }, error = function(e) {
    message("Erreur pour la période : ", start_date, " - ", end_date)
    return(NULL)
  })
}

#start_date <- start_dates[1]
# Boucle pour collecter les données (avec purrr ou base R)
ntc_all <- do.call(rbind, lapply(start_dates, get_ntc_batch))


colnames(ntc_all)
unique(ntc_all$type)

my_ntc_data <- ntc_all %>%
  filter(type == "D-1") %>% 
  select(receiver_country_name, sender_country_name, value, start_date, end_date)

my_ntc_data_2 <- ntc_all %>%
  filter(type == "WEEKLY" & (sender_country_name=="Germany" |
                               receiver_country_name =="Germany"|
                               sender_country_name == "Belgium" |
                               receiver_country_name == "Belgium"     )) %>% 
  select(receiver_country_name, sender_country_name, value, start_date, end_date)

my_ntc_data_tot <- rbind(my_ntc_data, my_ntc_data_2)

#change headings
my_ntc_data_tot <- my_ntc_data_tot %>%
  mutate(receiver_country_code = case_when(
    receiver_country_name == "France"       ~ "FR",
    receiver_country_name == "Belgium"      ~ "BE",
    receiver_country_name == "Spain"        ~ "ES",
    receiver_country_name == "Italy"        ~ "IT",
    receiver_country_name == "Germany"      ~ "DE",
    receiver_country_name == "Switzerland"  ~ "CH",
    receiver_country_name == "England-IFA"  ~ "GB1",
    receiver_country_name == "England-IFA2" ~ "GB2",
    TRUE ~ receiver_country_name  # fallback if unmatched
  ),
  sender_country_name = case_when(
    sender_country_name == "France"       ~ "FR",
    sender_country_name == "Belgium"      ~ "BE",
    sender_country_name == "Spain"        ~ "ES",
    sender_country_name == "Italy"        ~ "IT",
    sender_country_name == "Germany"      ~ "DE",
    sender_country_name == "Switzerland"  ~ "CH",
    sender_country_name == "England-IFA"  ~ "GB1",
    sender_country_name == "England-IFA2" ~ "GB2",
    TRUE ~ sender_country_name  # fallback if unmatched
  ),
  code = paste0(sender_country_name, "_", receiver_country_code),
  )
colnames(my_ntc_data_tot)

my_ntc_data_wider <- my_ntc_data_tot %>%
  select(code, value, start_date) %>%
  pivot_wider(
    names_from = code,  # ou `code`
    values_from = value,
    values_fn = mean,    # Évite les list-columns
    id_cols = start_date # ou `date`, selon ton nom
  )

#nota bene nouvelle interconnexion IFA 2 augmente substantiellement la capacité avec l'Angleterre
my_ntc_data_wider <- my_ntc_data_wider %>%
  mutate(
    FR_GB = rowSums(cbind(FR_GB1, FR_GB2), na.rm = TRUE),
    GB_FR = rowSums(cbind(GB1_FR, GB2_FR), na.rm = TRUE)
  ) %>%
  select(-GB1_FR, -GB2_FR, -FR_GB1, -FR_GB2)


getwd()
setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/cross_border")
write_parquet(my_ntc_data_wider, "NTC_tot_data.parquet")

#read physical flows
# balance <- get_physical_flows(start_date = Sys.Date() - 60, end_date = Sys.Date())
id_client_bis <- "c90d9e89-d536-43c4-ad46-adba0f581644"
id_secret_bis <- "bc01e121-ac1b-47db-b12d-93f9c410b1b6"
token_bis <- get_token(
  key = list(id_client = id_client_bis, id_secret = id_secret_bis)
)

#
unavailabilities_data <- get_open_api(
  api = "unavailability_additional_information",
  resource = "generation_unavailabilities",
  start_date = start_date1,
  end_date = end_date1,
  token = token_bis
)
tz(start_date1)

