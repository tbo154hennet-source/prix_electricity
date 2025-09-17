setwd("Z:/code_supply_demand")
reconstruct_full_curve_shift_qty_demand_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_demand_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_demand_matched)

test <- reconstruct_full_curve_shift_qty_demand_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_demand_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_demand_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_demand_matched<- reconstruct_full_curve_shift_qty_demand_matched[complete.cases(reconstruct_full_curve_shift_qty_demand_matched)]


#fusion
reconstruct_full_curve_shift_qty_demand_matched_local <- merge(
  reconstruct_full_curve_shift_qty_demand_matched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


# Filtrage par groupe

reconstruct_full_curve_shift_qty_demand_matched_local_bis <- reconstruct_full_curve_shift_qty_demand_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]





reconstruct_full_curve_shift_qty_supply_matched<- read_parquet( 'reconstruct_full_curve_shift_qty_supply_matched.parquet')

# Convert to data.table if not already
setDT(reconstruct_full_curve_shift_qty_supply_matched)

test <- reconstruct_full_curve_shift_qty_supply_matched[0:10000,]
# Add year column
reconstruct_full_curve_shift_qty_supply_matched[, year := year(DATE)]
reconstruct_full_curve_shift_qty_supply_matched[, month := month(DATE)]

# Supprimer les lignes contenant des NA dans n'importe quelle colonne
reconstruct_full_curve_shift_qty_supply_matched<- reconstruct_full_curve_shift_qty_supply_matched[complete.cases(reconstruct_full_curve_shift_qty_supply_matched)]

#fusion
reconstruct_full_curve_shift_qty_supply_matched_local <- merge(
  reconstruct_full_curve_shift_qty_supply_matched,
  price_indispo_merge_esp[, .(delivery_date, hour, price, gen_tot, gen_tot_da)],
  by.x = c("DATE", "Hour"),
  by.y = c("delivery_date", "hour")
)


reconstruct_full_curve_shift_qty_supply_matched_local_bis <- reconstruct_full_curve_shift_qty_supply_matched_local[
  , .SD[order(abs(PRICE - price))][1:10], 
  by = .(DATE, Hour)
]



library(fixest)



reconstruct_full_curve_shift_qty_supply_matched_local_bis$Hour_Date <- 
  reconstruct_full_curve_shift_qty_supply_matched_local_bis$DATE + reconstruct_full_curve_shift_qty_supply_matched_local_bis$Hour

local_supply_FE <- feols(log(QTY) ~ log(PRICE) | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_supply_matched_local_bis)


reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour_Date <- 
  reconstruct_full_curve_shift_qty_demand_matched_local_bis$DATE + reconstruct_full_curve_shift_qty_demand_matched_local_bis$Hour

# Global model with fixed effects
local_demand_FE <- feols(log(QTY) ~ log(PRICE) | Hour_Date, 
                         data = reconstruct_full_curve_shift_qty_demand_matched_local_bis)

etable(local_supply_FE, local_demand_FE )



describe_local_slopes <- function(local_slopes, slope_col = "slope", 
                                  se_col = "se_slope", intercept_col = "intercept",
                                  mean_col = "mean_log_price") {
  x <- local_slopes[[slope_col]]
  se_x <- local_slopes[[se_col]]
  intercept_x <- local_slopes[[intercept_col]]
  p_x <- local_slopes[[mean_col]]
  
  
  # Centrage
  p_x_centered <- p_x - mean(p_x, na.rm = TRUE)
  
  # Poids OLS
  w <- (p_x_centered  * p_x) / sum(p_x_centered ^2, na.rm = TRUE)
  
  
  #w <- if(!is.null(w_col)) local_slopes[[w_col]] else NULL
  
  
  # enlever NA
  valid <- !is.na(x) & !is.na(se_x)
  x <- x[valid]
  se_x <- se_x[valid]
  if (!is.null(w)) w <- w[valid]
  
  n <- length(x)
  if (n == 0) return(data.frame())
  
  # stats simples
  mean_x <- mean(x)
  median_x <- median(x)
  sd_x <- sd(x)
  se_mean <- sqrt(sum(se_x^2)) / n       # propagation de l'erreur
  min_x <- min(x)
  max_x <- max(x)
  quantiles <- quantile(x, probs = c(0.25, 0.5, 0.75))
  
  # moyenne pond?r?e simple
  if (!is.null(w)) {
    w_mean <- weighted.mean(x, w)
    se_w_mean <- sqrt(sum((w^2) * (se_x^2))) / sum(w)   # propagation de l'erreur pond?r?e
  } else {
    w_mean <- NA
    se_w_mean <- NA
  }
  
  alpha = cor(intercept_x, p_x)
  
  
  data.frame(
    mean = mean_x,
    se_mean = se_mean,
    w_mean = w_mean,
    alpha = alpha,
    se_w_mean = se_w_mean,
    median = median_x,
    sd = sd_x,
    min = min_x,
    max = max_x,
    Q1 = quantiles[1],
    Q2 = quantiles[2],
    Q3 = quantiles[3],
    n = n
    
  )
}

# ?tape 1 : calculer slope et son SE localement
local_slopes_demand <- reconstruct_full_curve_shift_qty_demand_matched_local_bis[
  , {
    if (.N > 1) {
      fit <- lm(log(QTY) ~ log(PRICE))
      w <- PRICE^2
      .(
        slope = coef(fit)["log(PRICE)"],
        se_slope = summary(fit)$coefficients["log(PRICE)", "Std. Error"], # SE
        intercept = coef(fit)["(Intercept)"],
        mean_log_price = mean(log(PRICE), na.rm = TRUE),
        w_mean_log_price = weighted.mean(log(PRICE), w, na.rm = TRUE)  # moyenne pond?r?e
      )
    } else {
      .(
        slope = NA_real_,
        se_slope = NA_real_,
        intercept = NA_real_,
        mean_log_price = NA_real_,
        w_mean_log_price = NA_real_
      )
    }
  },
  by = .(DATE, Hour)
]
library(arrow)
setwd("Z:/code_supply_demand")
write_parquet(local_slopes_demand, "local_slopes_demand.parquet")

local_slopes_demand <-  read_parquet( "local_slopes_demand.parquet")


# Exemple d'utilisation
my_demand <- describe_local_slopes(local_slopes_demand)


# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_demand$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_demand$slope, probs = 0.975, na.rm = TRUE)

# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_demand[, slope_w := pmin(pmax(slope, q_low), q_high)]

# V?rifier les r?sultats

# On calcule la moyenne pond?r?e des pentes
w_mean_demand <- my_demand$w_mean + my_demand$alpha





#je suis en train de faire ma r?gression OLS
#alors que je voudrais la pente locale


local_slopes_supply <- reconstruct_full_curve_shift_qty_supply_matched_local_bis[
  , {
    if (.N > 1) {
      fit <- lm(log(QTY) ~ log(PRICE+1))
      w <- PRICE^2
      .(
        slope = coef(fit)["log(PRICE + 1)"],
        se_slope = summary(fit)$coefficients["log(PRICE + 1)", "Std. Error"], # SE
        intercept = coef(fit)["(Intercept)"],
        mean_log_price = mean(log(PRICE+1), na.rm = TRUE),
        w_mean_log_price = weighted.mean(log(PRICE+1), w, na.rm = TRUE)  # moyenne pond?r?e
      )
    } else {
      .(
        slope = NA_real_,
        se_slope = NA_real_,
        intercept = NA_real_,
        mean_log_price = NA_real_,
        w_mean_log_price = NA_real_
      )
    }
  },
  by = .(DATE, Hour)
]

write_parquet(local_slopes_supply, "local_slopes_supply.parquet")
local_slopes_supply <- read_parquet("local_slopes_supply.parquet")

colnames(local_slopes_supply)
# Exemple d'utilisation



# Calcul des quantiles pour le seuil 95%
q_low  <- quantile(local_slopes_supply$slope, probs = 0.025, na.rm = TRUE)
q_high <- quantile(local_slopes_supply$slope, probs = 0.975, na.rm = TRUE)

# Winsorisation
# Cr?ation d'un nouvel objet winsoris?
local_slopes_supply[, slope_w := pmin(pmax(slope, q_low), q_high)]

my_supply <- describe_local_slopes(local_slopes_supply)

w_mean_supply <- my_supply$w_mean + my_supply$alpha



# On calcule la moyenne pond?r?e des pentes
# w_mean_supply <- weighted.mean(
#   local_slopes_supply$slope,
#   local_slopes_supply$mean_log_price^2,  # poids = p^2
#   na.rm = TRUE
# )

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
df_clean <- read.csv( "df_clean_da.csv")


setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
df_clean <- read.csv( "df_clean_da_seminar.csv")

setwd("Z:/prix_electricite_gitlab/day_ahead/outputs/esp")
df_clean_bis <- read.csv( "df_clean_da_seminar_no_unavail.csv")

#df_clean <- read.csv( "df_clean_da.csv")

df_clean_supply <- df_clean %>% 
  filter(Model == "OLS" | grepl("Supply", Model))
df_clean_demand <- df_clean %>% 
  filter(Model == "OLS" | grepl("Demand", Model))


# cr?er un y num?rique pour les instruments

# Définir l'ordre personnalisé
model_order <- c("OLS", "Bonte et al (2015)", "IV", "IV Ortho")

df_clean_demand <- df_clean_demand %>%
  mutate(Instrument = factor(Instrument, levels = model_order)) %>%  # transformer en facteur
  arrange(Instrument) %>%                                       # trier selon le facteur
  mutate(y_numeric = 1:n()) 
#colnames(df_clean_demand)

df_clean <- full_join(df_clean,
                      df_clean_demand %>% 
                        dplyr::select(Instrument, y_numeric),
                      by = c("Instrument"))%>%
  mutate(Type = ifelse(Instrument == "OLS", "OLS", Type),
         # Instrument =ifelse(Instrument == "IV", "IV SD", Type),
         # Instrument =ifelse(Instrument == "IV Ortho", "IV SD Orth", Type),
         # 
  )

# position du label ? droite (juste au-dessus du max de l'histogramme)
x_max <- max(local_slopes_supply$slope_w, na.rm = TRUE) - 0.5


# Plot avec trait vertical et texte
my_elas <- ggplot() +
  geom_histogram(
    data = local_slopes_demand,
    aes(x = slope_w, y = ..count../sum(..count..)*10, fill = "Demand"),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7
  )+ 
  geom_segment(
    aes(x = w_mean_demand, xend = w_mean_demand, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "red",
    size = 1
  )+
  # geom_vline(
  #   xintercept = w_mean_demand,
  #   linetype = "dashed",
  #   color = "red",
  #   size = 1
  # ) +
  annotate(
    "text",
    x = w_mean_demand - 0.2,
    y = -0.5,
    #y = max(table(cut(local_slopes_demand$slope_w, breaks = 50)))/sum(table(cut(local_slopes_demand$slope_w, breaks = 50))) - 0.02,  # proportion
    label = paste0("w_mean = ", round(w_mean_demand, 3)),
    color = "red",
    vjust = -0.5,
    hjust = 0,
    size = 5   # augmente ou réduit la taille du texte
  ) +
  geom_histogram(
    data = local_slopes_supply,
    aes(x = slope_w, y = ..count../sum(..count..)*10, fill = "Supply"),  # <-- proportion
    bins = 50,
    color = "white",
    alpha = 0.7
  )+ 
  geom_segment(
    aes(x = w_mean_supply, xend = w_mean_supply, y = 0, yend = 5),  # yend = hauteur max souhait?e
    linetype = "dashed",
    color = "blue",
    size = 1
  )+
  annotate(
    "text",
    x = w_mean_supply - 0.2,
    y = -0.5,
    #y = max(table(cut(local_slopes_supply$slope_w, breaks = 50)))/sum(table(cut(local_slopes_supply$slope_w, breaks = 50))) - 0.02,  # proportion
    label = paste0("w_mean = ", round(w_mean_supply, 3)),
    color = "blue",
    vjust = -0.5,
    hjust = 0,
    size = 5   # augmente ou réduit la taille du texte
  ) +
  geom_point(data = df_clean, aes(x = Estimate, y = y_numeric, color = Type), size = 3) +
  geom_errorbarh(data = df_clean, 
                 aes(xmin = Estimate - StdError, xmax = Estimate + StdError, y = y_numeric, color = Type),
                 height = 0.2) +
  # labels instruments ? droite
  geom_text(data = df_clean,
            aes(x = x_max , y = y_numeric, label = Instrument),
            hjust = 0, size = 5) +
  labs(x = "Elasticity", y = "Proportion", color = "Type") +
  theme(
    #legend.key.size = unit(1.5, "cm") , 
    legend.position = "bottom",   
    legend.text = element_text(size = 15),
    #axis.text.x = element_text(size = 14, angle = 30, hjust =1),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14),   # taille du titre axe y
    axis.text.y  = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_manual(
    values = c(
      "Demand" = "red",
      "Supply" = "blue",
      "OLS" = "grey"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Demand" = "red",
      "Supply" = "blue",
      "OLS" = "grey"
    ),
    guide = "none"  # supprime la l?gende pour le fill
  )


my_elas

save_latex_path <- "Z:/Overleaf_elec/images/intra_day"
setwd(save_latex_path)
ggsave(my_elas,
       filename = "elasticity_check.png" , width = 12, height = 6, dpi = 300)
# ggsave(my_elas,
#        filename = "elasticity_check_intra_day.png" , width = 12, height = 6, dpi = 300)
