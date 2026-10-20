# VAR analysis

Chaîne reproductible centrée sur le benchmark espagnol. VAR_analysis/ contient
le pipeline macro maintenu, les deux producteurs de tableaux supplémentaires dans
supplementary/, et leurs entrées propres dans data/. L’ancienne arborescence
Dans le dépôt source, VAR_analysis_fr/ est archivée sous
depreciated/VAR_analysis_fr/ et n’est pas incluse dans ce package minimal.

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
     day_ahead/outputs/esp/df_clean_da.csv, ligne IV Demand.
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
