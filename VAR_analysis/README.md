# VAR analysis

Chaîne reproductible centrée sur le benchmark espagnol. VAR_analysis/ contient
le pipeline macro maintenu, les deux producteurs de tableaux supplémentaires dans
supplementary/, et leurs entrées propres dans data/. L’ancienne arborescence
VAR_analysis_fr/ est archivée sous depreciated/VAR_analysis_fr/.

## Exécution

Depuis la racine du dépôt :

    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R

Une étape peut être relancée séparément ; ses dépendances sont chargées
automatiquement :

    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=irfs
    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=relevance
    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=contributions
    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=counterfactuals
    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=robustness
    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=france
    C:\INSEE\R\R-4.5.1\bin\Rscript.exe VAR_analysis/main.R --stage=france2010

Plusieurs étapes peuvent être séparées par des virgules. Les modèles sont mis
en cache dans output/models. Variables d’environnement utiles :

- VAR_FORCE=true : ignore les caches et réestime les modèles ;
- VAR_EXPORT_OVERLEAF=false : désactive l’écriture dans Overleaf_elec ;
- VAR_USE_MSW_INFERENCE=false : calcule uniquement les estimations ponctuelles ;
- VAR_LAGS et VAR_HORIZON : règlent le nombre de retards et l’horizon ;
- VAR_CF_BOOT : nombre de tirages bootstrap du scénario éolien (200 par défaut) ;
- VAR_CF_BLOCK : longueur des blocs bootstrap (automatique si 0).
- VAR_GAS_SUPPLY_SHOCKS : chemin du classeur GasSupplyShocks.xlsx ; la feuille
  2025update est utilisée comme instrument externe Alessandri-Gazzani.

## Échantillon espagnol

Le VAR espagnol est estimé sur 2010-01 à 2025-09
(data/IV_data_esp_log_elas_sd_2010.parquet). Les sept variables du VAR sont
prolongées avant 2015 par des sources publiques raccordées par ratio sur
2015-2016 : prix OMIE, demande et production REE, IPI INE, HICP et taux BCE
(FRED), gaz TTF et, avant 2012, indice gaz Europe du FMI. Elles sont ensuite
désaisonnalisées (X-13) sur toute la période. Le fichier est produit dans le
dépôt source par Data_structuring/spain_var_backfill/build_spain_var_2010.R.

Les instruments micro ne sont observés qu’à partir de 2015-01 et valent NA
avant. Ils valent 0 dans les moments sum(z_t u_t), où ces mois n’ajoutent rien,
de sorte que la normalisation theta = Gamma / Gamma_norm est celle des mois
observés ; premières étapes, F de relevance et centrages n’utilisent que les
mois observés. Le contrefactuel éolien 2030 et ses ratios d’écart-type portent
sur 2015-2025, période du scénario micro.

## France étendue à 2010 (étape `france2010`)

Le VAR français (mêmes sept variables) est aussi estimé sur 2010-01 à 2025-09
(data/IV_data_fr_log_elas_sd_2010.parquet, produit par
Data_structuring/france_var_backfill/, voir son README). Avant 2015 : prix
EPEX Spot France (archive horaire, identique au benchmark sur 2015-2016),
consommation et production RTE, gaz TTF puis indice FMI, IPI INSEE, IPCH et
taux FRED, raccordés par ratio sur 2015-2016 puis désaisonnalisés sur toute la
période.

Le proxy d'offre est le vent, observé dès 2010 grâce à un facteur de charge
météo (ERA5, pondéré par le parc régional, calibré sur le facteur de charge
RTE). Proxy central : `wind_era5_cf_log_tadj`, net des anomalies de
degrés-jours, car le vent brut est corrélé à la température en hiver. Les
variantes (vent brut, facteur de charge RTE dès 2012, prévision ENTSO-E dès
2015, surprises en GW) sont listées dans `config.R` (`france_2010`).

Sorties uniquement locales, sans export Overleaf : output/france_2010/
(IRF, comparaison benchmark / échantillon étendu, relevance, couverture des
proxies, contributions IV et Mertens-Ravn).

## Identification centrale : SVAR bayésien à élasticités (étape `elasticity`)

Le module `R/09_elasticity_svar.R` identifie les chocs d'offre et de demande
d'électricité sans restriction de signe sur les réponses et sans a priori sur
la valeur des pentes :

- forme réduite : a priori Minnesota + sum-of-coefficients + single-unit-root,
  hyperparamètres au mode a posteriori de la vraisemblance marginale
  (Giannone, Lenza et Primiceri, 2015) ;
- bloc électrique sur les innovations prix/charge, nettes de l'innovation gaz
  (TTF exogène dans le mois) : courbe d'offre de pente eps_s, courbe de
  demande de pente eps_d, chocs orthogonaux (Baumeister et Hamilton) ;
- seuls les signes eps_s > 0 et eps_d < 0 sont imposés (eps_s tiré sur
  [0,01 ; 1,5]) ;
- proxies physiques mensuels (`attach_physical_proxies`), construits
  directement au pas du VAR (anomalies : résidus sur indicatrices de mois
  calendaire et tendance) :
  - offre (`phys_supply`) : énergie non fossile disponible = anomalies
    standardisées du vent réalisé à 100 m et des précipitations cumulées sur
    6 mois (hydraulicité), orthogonalisées sur les anomalies de froid et de
    chaleur ;
  - demande (`phys_demand`) : anomalie des degrés-jours d'inconfort
    (`cold_excess` + `heat_excess`), orthogonalisée sur le vent et
    l'hydraulicité.
  Comme dans un proxy-SVAR à deux proxies, ils entrent dans la vraisemblance
  par deux restrictions d'exclusion.

Les élasticités micro servent de validation : les pentes implicites des
proxies au premier stage (eps_d = -0,038 pour l'offre, eps_s = 0,236 pour la
demande) retrouvent l'élasticité horaire de l'offre (0,237) et l'élasticité
journalière de la demande (-0,04).

Robustesses présentées :
- `*_elasticity_benchmark.png` : mêmes proxies avec a priori sur les pentes
  (eps_s ~ N(0,20 ; 0,04), eps_d ~ N(-0,04 ; 0,03)) ; ancienne spécification
  centrale (shifts synthétiques horaires agrégés à la Kilian + a priori) et sa
  version sans a priori ;
- `*_elasticity_robustness_identification.png` : a priori VAR diffus,
  innovations non nettes du gaz, a priori sur les pentes, Mertens-Ravn appliqué
  à chaque tirage a posteriori (proxies physiques + élasticité micro de
  demande ; médiane seule, bandes trop larges) ;
- `*_elasticity_robustness_proxies.png` : proxies résidualisés sur les retards
  du VAR (exogénéité lead-lag), proxy d'offre seul, proxy de demande seul ;
- `*_elasticity_robustness_temperature.png` : la température déplace aussi
  l'offre (courbes horaires : froid -161 MW d'offre pour +180 MW de demande).
  (a) restriction d'exclusion corrigée : cov(z, v_s) = kappa cov(z, v_d), kappa
  = -0,66 estimé sur les déplacements mensuels des courbes dus à la
  température (`attach_temperature_shifts`) ; (b) proxy de demande issu des
  courbes (`curve_demand`), purgé du déplacement d'offre dû à la température ;
- `*_elasticity_robustness_synthetic.png` : famille des shifts synthétiques
  (avec et sans a priori, éolien + température, wedge ibérique).

Autres variantes estimées et reportées dans `elasticity_svar_specifications.csv` :
p = 6, Minnesota seul, a priori d'offre centré sur l'élasticité horaire, sans
proxy (a priori seuls), shifts synthétiques en niveaux, un seul shift
synthétique, composante température seule, prévision éolienne seule.

Sorties dans `output/elasticity/` (IRF, tableau des spécifications, validation
par les proxies, figures benchmark et robustesse, décompositions historiques,
contributions avec bandes a posteriori, contrefactuels ; le choc gaz
Alessandri-Gazzani est identifié sur la même forme réduite). Ces sorties sont
exportées vers Overleaf par défaut (`VAR_ELASTICITY_OVERLEAF=false` pour
désactiver).

## Ordre du pipeline

1. Chargement et validation des données mensuelles Espagne et France.
2. IRF benchmark Espagne : proxies PCA d’offre et de demande, VAR avec gaz.
3. Relevance : premières étapes, F classique et F HAC Newey-West lorsque les
   dépendances correspondantes sont disponibles.
4. Contributions de l’offre et de la demande à l’inflation et à la
   désinflation.
5. Deux contrefactuels fondés sur les trajectoires micro sources :
   - exception ibérique sans subvention à la production électrique au gaz ;
   - fluctuations macro avec la capacité éolienne espagnole prévue en 2030
     (62 GW).
6. Robustesse des proxies : benchmark, vent, proxy augmenté du wedge.
7. Robustesse d’identification :
   - Cholesky avec ordering prix puis quantité ;
   - Mertens-Ravn à deux instruments ;
   - restriction de courbe de demande appliquée au choc d’offre :
     réponse quantité / réponse prix = élasticité micro ;
   - élasticité-prix de la demande lue automatiquement dans
     supply_demand_curve/estimate_elasticity/outputs/elasticity_standard.csv, ligne Supply sans retard (IV horaire instrumentée par la prévision de vent, pente de la demande).
8. France :
   - IV vent pour l’offre et température pour la demande ;
   - identification Mertens-Ravn avec la même élasticité micro ;
   - IRF, relevance et contributions.

Toutes les figures d’IRF suivent l’ordre du Cholesky : prix de l’électricité,
charge, production, production industrielle, IPCH, taux directeur, prix du gaz.

Les fenêtres de contributions à l'IPCH sont centralisées dans config.R :

- inflation : 2021T2-2022T3 ;
- désinflation : 2022T3-2023T2.

La mesure principale est désormais la contribution à l'inflation cumulée :
variation du niveau d'IPCH attribuable au choc entre le mois précédant la
fenêtre et son dernier mois. Sa part est calculée avec l'inflation cumulée
observée sur exactement la même fenêtre. Les variations de contribution au
glissement annuel restent disponibles comme diagnostics dans les colonnes
Change_in_yoy_contribution_pp et Episode_explained_yoy_change_pp. L'ancienne
somme annualisée des contributions aux niveaux est conservée dans la colonne
Legacy_annualized_percent.

Le tableau énergétique présente d'abord l'inflation observée, puis quatre
composantes indentées : offre de gaz, offre électrique, demande électrique et
autres facteurs. Deux sous-lignes décomposent l'offre de gaz entre effet direct
et transit par le prix électrique ; elles ne sont pas recomptées dans le total.
La période figure une seule fois dans l'en-tête de chaque épisode.

La variation cumulée du prix de l'électricité est également décomposée entre
les chocs d'offre électrique, de demande électrique et d'offre de gaz. La somme
des trois chocs identifiés est complétée par un résidu `Other/unidentified
factors`, de sorte que le tableau se réconcilie exactement avec la variation
observée du prix électrique sur chaque fenêtre.

Pour tenir compte du décalage entre le pic du prix électrique et celui de
l'inflation, le tableau du prix électrique utilise ses propres fenêtres :

- hausse du prix électrique : 2021T2-2022T1 ;
- baisse du prix électrique : 2022T2-2023T1.

## Choc structurel d'offre de gaz

La spécification Espagne conserve le même VAR à sept variables et ajoute une
identification proxy-SVAR séparée du choc d'offre de gaz. L'instrument mensuel
`Shocks` est lu dans la troisième feuille (`2025update`) de
`GasSupplyShocks.xlsx`, puis aligné exactement sur les dates du VAR. Le choc est
normalisé pour augmenter `spot_gas_log` à l'impact : il s'interprète donc comme
une contraction adverse de l'offre de gaz.

La seconde décomposition historique contient les chocs d'offre électrique, de
demande électrique et d'offre de gaz. Pour la sous-composante « gas supply via
electricity price », la trajectoire du prix électrique provoquée par un choc gaz
est déconvoluée avec l'IRF de prix du choc d'offre électrique. La séquence de
chocs d'offre électrique équivalents ainsi obtenue est ensuite propagée à
l'HICP avec l'IRF d'offre électrique. L'effet direct du gaz est défini comme
l'effet gaz total moins cette composante électrique. Les deux sous-composantes
sont donc exactement additives. Les agrégats sont définis ainsi :

- Total elec = offre électrique + composante du gaz via le prix électrique ;
- Total energy = offre électrique + choc d'offre de gaz total.

Principaux exports :

- `gas_supply_shock_spain_irf.png` ;
- `historical_decomposition_spain_three_shocks.csv` et `.png` ;
- `historical_decomposition_spain_extended_gas_channels.csv` et `.png` ;
- `historical_gas_induced_electricity_supply_shocks.csv` ;
- `historical_decomposition_spain_gas_channels.csv` ;
- `irfs_gas_supply_path_decomposition.csv` ;
- `contributions_spain_energy_three_shocks.csv` ;
- `gas_supply_path_decomposition_diagnostics.csv`.

## Contrefactuels

Le contrefactuel ibérique reprend la trajectoire estimée par
gas_regression_micro.R en remplaçant le gaz plafonné par le prix TTF non
subventionné. La cible mensuelle va de juin 2022 à février 2023. L’écart de prix
est déconvolué par ridge avec l’IRF d’offre identifiée par
delta_supply_incorpo, puis propagé à toutes les variables du VAR. L’intervalle
de prédiction micro à 95 % est également propagé à l’inflation.

Le scénario éolien reprend price_variance_counterfactual_esp_ter.parquet,
construit avec une capacité de 62 GW. La trajectoire ajustée en moyenne est la
spécification principale ; la trajectoire brute est conservée comme diagnostic.
La cible de prix est inversée conjointement avec les pré-chocs requis par
l’horizon de l’IRF. Les intervalles des ratios de volatilité proviennent d’un
bootstrap moving-block apparié des résidus du VAR et du proxy.

## Exports

Tous les résultats sont d’abord écrits dans output. Lorsque l’export Overleaf
est actif, les noms historiques et les chemins Overleaf_elec sont conservés,
notamment :

- supply_msw_main_plot.png
- demand_msw_main_plot.png
- supply_msw_wind_plot.png
- supply_msw_wedge_plot.png
- irf_wind_delta_supply_demand_spain.png
- supply_shock_spain_irf_compare.png
- demand_shock_spain_irf_compare.png
- supply_shock_france_irf_compare.png
- demand_shock_france_irf_compare.png
- wind_supply_shock_fr.png
- iberian_shock.png
- iberian_exception_plot_price_counterfactual.png
- iberian_exception_plot_inflation_counterfactual.png
- wind_counterfactuals.png
- macro_fluct.png

Les tableaux counterfactual_iberian_summary.csv,
counterfactual_volatility_ratios.csv et
counterfactual_reconstruction_diagnostics.csv documentent les hypothèses, les
effets et la précision de reconstruction.
