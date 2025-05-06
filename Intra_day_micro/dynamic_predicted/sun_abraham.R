unique(publi_all_matched_indispo$psr_resource_name)

unique(actual_OMIE_all_indispo$psr_resource_name)

unique(filtered_indispo$production_resource_name)


library(fixest)

# Génère les régresseurs de Sun & Abraham
fixest::sunab(
  cohort,
  period,
  ref.c = NULL,  # vector of references for the cohort
  ref.p = -1,    # vector of references for the relative period
  bin,           # list of values to be grouped (cohort + relative periods)
  bin.rel,       # vector defining which relative periods to bin
  bin.c,         # vector defining which cohorts to bin
  bin.p,         # vector defining which periods to bin
  att = FALSE,   # if TRUE, total ATT is computed instead of for each rel
  no_agg = FALSE # if TRUE, estimate all cohort x time_to_treatment coeff
)

# A injecter dans une régression, par exemple 
fixest::feols(
  y ~ fixest::sunab(event, t_rel, ref.p = -4) | ind_fe + t_fe,
  data = my_data)


library(fixest)
library(data.table)

set.seed(123)

# Création de données simulées
n <- 1000
T <- 10
dt <- CJ(id = 1:n, time = 1:T)

# Génère des événements aléatoires à partir de la période 3
event_time_start <- sample(3:8, n, replace = TRUE)
dt[, event_time := time - event_time_start[id]]

# Traitement continu : augmente après l'événement
dt[, treat_cont := ifelse(event_time >= 0, runif(.N, 1, 5), 0)]

# Variable dépendante : effet croissant avec traitement et temps
dt[, y := 0.5 * treat_cont * (event_time == 0) +
     0.8 * treat_cont * (event_time == 1) +
     1.0 * treat_cont * (event_time == 2) +
     rnorm(.N) + id + time]

# Crée les interactions entre traitement continu et event_time
est <- feols(y ~ i(event_time, treat_cont, ref = -1) | id + time, data = dt)

# Résumé
summary(est)

iplot(est,
      main = "Event Study – Traitement continu",
      xlab = "Temps relatif à l'événement",
      ylab = "Effet marginal du traitement continu")
# Suppose:
#   - 'event_time' = time relative to treatment (0 = first treated period)
#   - 'treat_cont' = continuous treatment intensity (0 if untreated)
#   - 'cohort' = time of first treatment (0 if never treated)

# Estimate with fixest, including cohort-specific interactions:
est <- feols(y ~ sunab(cohort, time, treat_cont) | id + time, data = dt)
iplot(est,
      main = "Event Study: Continuous Treatment",
      xlab = "Event Time",
      ylab = "Marginal Effect per Unit of Treatment")

