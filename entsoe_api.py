from entsoe import EntsoePandasClient
import pandas as pd

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')

start = pd.Timestamp('20171201', tz='Europe/Brussels')
end = pd.Timestamp('20180101', tz='Europe/Brussels')
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

# Define the path to save the Parquet file
parquet_file_path = '/content/drive/MyDrive/sample.parquet'

# Write the table to a Parquet file
pq.write_table(table, parquet_file_path)

# Print the path to the saved Parquet file
print("Parquet file saved at:", parquet_file_path)


