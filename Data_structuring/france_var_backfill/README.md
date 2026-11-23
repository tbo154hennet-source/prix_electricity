# France : rétro-prolongement du VAR mensuel à 2010

Produit `VAR_analysis/data/IV_data_fr_log_elas_sd_2010.parquet` (2010-01 à
2025-09), utilisé par l'étape `france2010` du pipeline VAR.

## Commandes

Depuis la racine du dépôt :

    Rscript Data_structuring/france_var_backfill/download_france_sources.R
    Rscript Data_structuring/france_var_backfill/build_france_var_2010.R

Le premier script écrit de petits CSV mensuels dans `sources/`. Les réponses
horaires Open-Meteo sont mises en cache hors du dépôt
(`OPEN_METEO_CACHE`, par défaut `C:/Users/DBT3KD/Documents/france_var_backfill_cache`).
Le quota gratuit d'Open-Meteo impose environ une minute par point de vent.

## Variables du VAR

À partir de 2015-01 : niveaux bruts du benchmark
(`Data_structuring/output/IV_data_fr_new.parquet`). Avant : sources publiques
raccordées par le ratio moyen benchmark / source sur 2015-2016, puis
transformations de `seasonal_adjustment_fr_sd.R` sur 2010-2025 (prix déflatés
par l'IPCH, X-13, log(x + 1)).

| Variable | Source 2010-2014 | Remarque |
|---|---|---|
| spot_price | EPEX Spot France, horaire (archive [ewoken/epex-spot-data](https://ewoken.github.io/epex-spot-data/)) | moyenne temporelle mensuelle, définition identique au benchmark ; égale au centime près au benchmark ENTSO-E de 2015-02 à 2016-12 |
| load_tot | RTE, consommation brute mensuelle (ODRÉ `equilibre-national-mensuel-prod-conso-brute`) | GWh convertis en MW moyens |
| gen_forecast | RTE, production totale mensuelle (même jeu) | le benchmark est la prévision ENTSO-E J-1, non disponible avant 2015 |
| spot_gas | TTF (`day_ahead/outputs/gas_price_formatted.parquet`) dès 2012, indice FMI « Natural gas, Europe » chaîné avant | même construction que l'Espagne |
| IPI | INSEE, IPI industrie manufacturière CVS-CJO base 2021 (DBnomics) | série du benchmark |
| HICP | FRED CP0000FRM086NEST | série du benchmark |
| ffr | FRED ECBMRRFR | sans rééchelonnement |

Les facteurs d'échelle et corrélations sur 2015-2016 sont dans
`splice_diagnostics_2010.csv`.

## Proxies d'offre : le vent

Les proxies valent NA tant qu'ils ne sont pas observés : `proxy_instrument()`
les met à 0 dans les moments sum(z_t u_t) et les exclut des premières étapes.
Ils n'apportent donc d'information d'identification que sur leur période
d'observation.

Facteur de charge météo (ERA5) : vent à 100 m (Open-Meteo, modèle `era5`) en
12 points des zones éoliennes, passé par une courbe de puissance générique
(3 / 12 / 25 m/s), pondéré par le parc régional RTE (parts 2012 avant 2012),
puis calibré linéairement sur le facteur de charge observé RTE (ODRÉ
`fc-tc-nationaux-mensuels-eolien-solaire`). Sur 2012-2025, corrélation 0,991
avec le facteur de charge RTE (R² 0,98). Il est exogène par construction :
il ne dépend ni des écrêtements à prix négatifs ni de la disponibilité.

- `wind_era5_cf_log_tadj` (**central**, 2010-) : log du facteur de charge
  ERA5, anomalie par mois calendaire, résidu sur les anomalies de degrés-jours
  de chauffage et de climatisation (Eurostat `nrg_chdd_m`).
- `wind_era5_cf_log` (2010-), `wind_rte_cf_log` (2012-) : idem sans la
  correction de température ; RTE = facteur de charge observé.
- `wind_era5_surprise`, `wind_rte_surprise` : capacité_t × (facteur de
  charge_t − moyenne du mois calendaire), en GW ; et leurs variantes `_tadj`.
- `Fore_Wind_log` (2015-) : proxy du benchmark, recopié tel quel. Il est en
  log de niveau et garde donc la tendance de capacité (le parc a plus que
  doublé depuis 2015), ce que les définitions en facteur de charge évitent.

Pourquoi la correction de température : les mois d'hiver venteux sont doux
(flux d'ouest), les mois calmes froids et anticycloniques. Le vent brut est
corrélé à environ 0,3 en hiver avec l'anomalie de température, et le choc
d'offre identifié par le vent brut fait monter la charge à l'impact (+0,13 à
+0,20 pour +1 de prix, +0,12 dans le benchmark 2015), ce qui contredit une
demande décroissante. La correction réduit cette réponse (+0,08) sans
l'annuler, et fait baisser la relevance (F HAC de 12,1 à 8,1).

La capacité est interpolée linéairement entre les valeurs de fin d'année
(ODRÉ `parc-national-annuel-prod-eolien-solaire`). Corrélations entre proxies,
avec les degrés-jours et calibration ERA5 → RTE : `wind_proxy_diagnostics_2010.csv`.

## Diagnostics de raccord (2015-2016)

Corrélations benchmark / source ≥ 0,998 en niveau et ≥ 0,988 en variations
mensuelles pour toutes les variables (`splice_diagnostics_2010.csv`). Le prix
EPEX de l'archive est égal au centime près à la série ENTSO-E du benchmark de
2015-02 à 2016-12. Point faible : avant 2012, le gaz vient de l'indice FMI
(indexé pétrole), corrélé à 0,38 seulement avec le TTF sur 2012-2014, comme
pour l'Espagne. L'IPCH FRED est désormais en base 2025 (facteur 1,244).

## Proxy de demande : température

`temperature_discomfort` est la moyenne mensuelle de |T| horaire au point
Open-Meteo (50.17875 N, 4.077917 E). C'est le seul point qui entre dans les
données françaises 2015- : `distinct(datetime, .keep_all = TRUE)` dans
`day_ahead/fr/prepare_regress_day_ahead_fr_new.R` garde le premier des
49 points population au lieu de leur moyenne. La définition est reproduite à
l'identique pour rester comparable au benchmark ; à corriger dans le
benchmark si l'on veut une température nationale.
