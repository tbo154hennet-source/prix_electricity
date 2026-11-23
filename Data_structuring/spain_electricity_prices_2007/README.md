# Spain day-ahead electricity prices from 2007

Install: `pip install pandas requests pdfplumber`

Run: `python download_spain_price_monthly.py --start 2007 --end 2025 --output spain_price_2007_2025.csv`

Verify official historical report extraction (network needed): `python download_spain_price_monthly.py --start 2007 --end 2009 --refresh-ree --output check_ree.csv`

Offline test with known 2010-14 prices: `python download_spain_price_monthly.py --start 2007 --end 2014 --mirror-file omie_es_price_2010_2014.csv --output test.csv`

TEST: `python test_download.py`

Definitions:
* 2007-2009: REE reports, monthly DAY-AHEAD market volume-weighted prices, peninsular, €/MWh.
* 2010 onward: OMIE Spanish bidding zone monthly arithmetic mean of spot market prices (secondary compilation derived from OMIE). Beware the 2025 15-minute reform.
* Two distinct price columns are kept by default. `--combine` generates a single mixed-definition series for EXPLORATION ONLY, not recommended for final academic specification. Missing data stays missing (never fill with zero).

Official sources:
* https://www.ree.es/sites/default/files/downloadable/inf_sis_elec_ree_2007_v3.pdf
* https://www.ree.es/sites/default/files/downloadable/inf_sis_elec_ree_2008_v4.pdf
* https://www.ree.es/sites/default/files/downloadable/inf_sis_elec_ree_2009.pdf
* https://www.omie.es/es/market-results-history/annual/daily-market/daily-prices?scope=annual&year=2007
* https://www.portal-asistencia.omie.es (ask for 2007-2009 Spanish marginal day-ahead hourly prices to create arithmetic monthly series homogeneous with 2010+)
* https://github.com/tiagofelicia/dados-energia/tree/main/data/agregados

Caveats:
* The OMIE public FILE repository currently provides only a six-year rolling history: hourly files `marginalpdbc_YYYYMMDD.1` cannot be assumed downloadable for 2007. Official historical annual reports are a different publication channel.
* 2008 full annual REE report shows 58.88 for March; another REE 2008 section PDF displays 59.88. Consult the underlying original table before using final data.
* A period spanning 2007-2025 should preferably be reconstructed with a homogeneous price statistic throughout. The mixed series has a known weighting break in January 2010.
