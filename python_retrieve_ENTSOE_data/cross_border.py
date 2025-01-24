import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'FR'  

#2016-03
# Define start and end dates
start_date = pd.Timestamp('20241001', tz='Europe/Brussels')
end_date = pd.Timestamp('20241001', tz='Europe/Brussels')
# Generate monthly date ranges
monthly_dates = pd.date_range(start=start_date, end=end_date, freq='MS')
import os
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_by_plant\fr'  # Remplacez par votre chemin
os.chdir(path)

# Changer le répertoire de travail
os.getcwd()

type_marketagreement_type = 'A01'
contract_marketagreement_type = "A01"
# balancing market A01
# intraday market A02
# day ahead A03
process_type = 'A51'	
#A34 contracted
#A16 realised

start = monthly_dates[0]
# Iterate over each month
end = start + pd.DateOffset(days=1)



imbalance_prices = client.query_imbalance_prices(country_code, start=start, end=end, psr_type=None)
imbalance_volumes = client.query_imbalance_volumes(country_code, start=start, end=end, psr_type=None)

reserve_prices = client.query_contracted_reserve_prices(country_code, type_marketagreement_type, 
														start=start, end=end, psr_type=None)
reserve_prices_procured_capacity = client.query_contracted_reserve_prices_procured_capacity(country_code, process_type = 'A47', 
														type_marketagreement_type = type_marketagreement_type, 
														start=start, end=end, psr_type=None)
#reserve_amount = client.query_contracted_reserve_amount(country_code, type_marketagreement_type, start=start, end=end, psr_type=None)

physical_crossborder = client.query_physical_crossborder_allborders(country_code, start, end, export=True)
generation_import = client.query_generation_import(country_code, start, end)

#type_marketagreement_type = 'A03'
#procured_balancing_capacity = client.query_procured_balancing_capacity(country_code, process_type, start=start, end=end, type_marketagreement_type=None)

generation_import.keys()
# Drop all rows where the first level of the MultiIndex is 'Generation'
generation_import_data = generation_import.loc[:, generation_import.columns.get_level_values(0) != 'Generation']
generation_import_data

country_code_from = 'FR'
country_code_to = 'ES'
transfer_capacity = client.query_net_transfer_capacity_dayahead(country_code_from=country_code_from,
            country_code_to=country_code_to, start=start, end=end)



spain_prices = client.query_day_ahead_prices('ES', start=start, end=end)
port_prices = client.query_day_ahead_prices('PT', start=start, end=end)
