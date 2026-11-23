# Espagne : rétro-prolongement des données du VAR mensuel

**Objectif :** obtenir `load_gwh` (demande), `gen_tot_gwh` (production) et `spot_price_eur_mwh` (OMIE Espagne) de janvier 2010 à décembre 2014. Les proxies sont exportés comme 0 uniquement pour faciliter l'intégration dans un fichier rectangulaire, avec un indicateur `proxy_observed=0` : ne pas traiter ces zéros comme de vraies observations.

## 1. Prérequis

Python >= 3.10, bibliothèque standard uniquement. Connexion Internet nécessaire pour télécharger les données REE et le miroir OMIE.

## 2. Commandes

```bash
python backfill_spain_macro.py --start-year 2010 --end-year 2014 --output spain_macro_electricity_backfill.csv
python -m unittest test_backfill.py -v
```

Sans accès réseau, la série de prix effective 2010-2014 jointe peut être convertie ainsi :

```bash
python backfill_spain_macro.py --price-file omie_es_price_2010_2014.csv --price-only --output spain_price_2010_2014.csv
```

Les JSON REE téléchargés ailleurs peuvent aussi être traités hors ligne en créant `load_2010.json`, `generation_2010.json`, etc. dans un dossier, puis en ajoutant `--ree-json-dir CHEMIN`.

**Important :** les données mensuelles de REE sont demandées par année car la taille maximale acceptée par l'API varie selon le widget. Le périmètre par défaut est `peninsular` (plus cohérent avec le day-ahead espagnol), et peut être changé en `--scope national` si cela correspond mieux à ton `gen_tot` actuel.

## 3. Dictionnaire de variables

- `date` : premier jour du mois (ISO YYYY-MM-DD)
- `load_gwh` : demande mensuelle d'électricité (GWh), *REE demanda/evolucion*
- `gen_tot_gwh` : somme des technologies de production mensuelle (GWh), *REE generacion/estructura-generacion*
- `spot_price_eur_mwh` : moyenne mensuelle du prix day-ahead espagnol (€/MWh), *OMIE source secondaire*
- `proxy_supply`, `proxy_demand` : 0 avant la date de début des proxies, **placeholders non observés**
- `proxy_observed` : 0 sur la période sans instruments; 1 à partir de leur période de mesure (à définir dans ton code de VAR)

## 4. Sources

- OMIE (compilation indépendante) : https://raw.githubusercontent.com/tiagofelicia/dados-energia/main/data/agregados/omie_es_mensal.csv
- REE API : https://www.ree.es/en/datos/apidata
- ENTSO-E (alternative historique pour load) : https://www.entsoe.eu/data/power-stats/

## 5. Contrôles à réaliser avant le VAR

1. Comparer les 12 mois de 2015 aux séries d'origine (prix et génération, même périmètre).
2. Vérifier le facteur d'unité GWh/MWh et si la génération concerne la péninsule ou tout le pays.
3. Vérifier la continuité mensuelle et l'absence de données manquantes (le script bloque si besoin).
4. Ne pas remplacer les instruments pré-2015 manquants par de véritables chocs nuls au moment de calculer les moments proxy ou leurs écarts-types.
5. Ajouter séparément les autres variables macro (inflation, IPI, taux, prix de l'énergie, etc.) à la période 2010-2014.

## 6. Limites de ce test

L'environnement ayant servi à produire ces scripts interdit les requêtes Internet depuis le terminal : le CSV OMIE 2010-2014 a été retranscrit depuis sa version publiquement consultable et validé en tests unitaires; les réponses de l'API REE ont été testées au moyen d'exemples JSON conformes à son schéma. **La disponibilité effective du téléchargement REE, les champs des réponses historiques et les 60 mois REE n'ont pas été vérifiés par requêtes réseau exécutées.**
