from entsoe import EntsoePandasClient
import pandas as pd

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')

start = pd.Timestamp('20140101', tz='Europe/Brussels')
end = pd.Timestamp('20230101', tz='Europe/Brussels')
country_code = 'FR'  
country_code_from = 'FR'  # France
country_code_to = 'ESP'
type_marketagreement_type = 'A01'
contract_marketagreement_type = "A01"
process_type = 'A51'
print(client)

import os
os.getcwd()
# Import the pyarrow library
import pyarrow as pa
import pyarrow.parquet as pq

# Create a sample dataframe (replace this with your actual dataframe)
import pandas as pd
data = {'col1': [1, 2, 3], 'col2': ['a', 'b', 'c']}
df = pd.DataFrame(data)
# Convert the pandas dataframe to a pyarrow table
table = pa.Table.from_pandas(df)

# Spécifier le chemin absolu (par exemple, C:\Users\Username\Documents\test)
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA'  # Remplacez par votre chemin
os.chdir(path)
# Changer le répertoire de travail
os.getcwd()
# Write the table to a Parquet file
#df.to_csv('filename.csv', index=False) 
pq.write_table(table, 'example.parquet')


os.chdir('day_ahead_prices')
start = pd.Timestamp('20150101', tz='Europe/Brussels')
end = pd.Timestamp('20150201', tz='Europe/Brussels')
day_ahead_prices = client.query_day_ahead_prices(country_code, start=start, end=end)

day_ahead_prices_table = pa.Table.from_pandas(day_ahead_prices.to_frame())
# Write the table to a Parquet file
pq.write_table(day_ahead_prices_table, 'day_ahead_prices_2015_01')



