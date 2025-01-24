import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'ES'  

#2016-03
# Define start and end dates
start_date = pd.Timestamp('20200101', tz='Europe/Brussels')
end_date = pd.Timestamp('20201231', tz='Europe/Brussels')
# Generate monthly date ranges
monthly_dates = pd.date_range(start=start_date, end=end_date, freq='MS')
import os
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_by_plant\esp'  # Remplacez par votre chemin
os.chdir(path)

# Changer le répertoire de travail
os.getcwd()

# Iterate over each month
for date in monthly_dates:
    # Define the start and end dates for the first half of the current month
    start_month_first_half = date
    end_month_first_half = date + pd.DateOffset(days=15)

    # Define the start and end dates for the second half of the current month
    start_month_second_half = date + pd.DateOffset(days=15)
    end_month_second_half = date +  pd.offsets.MonthBegin(1)  # End of the current month

    # Query and save data for the first half of the month
    print(f"Processing first half of {start_month_first_half.strftime('%Y-%m')}")
    generation_first_half = client.query_generation_per_plant(country_code, start=start_month_first_half, end=end_month_first_half, psr_type=None, include_eic=False)

    # Define file path for the first half
    string_first_half = start_month_first_half.strftime('%Y-%m') + '-first-half.parquet'

    # Convert the pandas dataframe to a pyarrow table and write to Parquet file
    generation_table_first_half = pa.Table.from_pandas(generation_first_half)
    pq.write_table(generation_table_first_half, string_first_half)

    # Print a message indicating the successful query for the first half
    print(f"Data queried successfully for the first half: {start_month_first_half.strftime('%Y-%m')}")

    # Query and save data for the second half of the month
    print(f"Processing second half of {start_month_second_half.strftime('%Y-%m')}")
    generation_second_half = client.query_generation_per_plant(country_code, start=start_month_second_half, end=end_month_second_half, psr_type=None, include_eic=False)

    # Define file path for the second half
    string_second_half = start_month_second_half.strftime('%Y-%m') + '-second-half.parquet'

    # Convert the pandas dataframe to a pyarrow table and write to Parquet file
    generation_table_second_half = pa.Table.from_pandas(generation_second_half)
    pq.write_table(generation_table_second_half, string_second_half)

    # Print a message indicating the successful query for the second half
    print(f"Data queried successfully for the second half: {start_month_second_half.strftime('%Y-%m')}")
