import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient
import os
client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')

#path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_by_plant\fr'  # Remplacez par votre chemin
os.chdir(path)
os.getcwd()

country_code = 'ES'  
# Define start and end dates
start_date = pd.Timestamp('20150801', tz='Europe/Brussels')
#end_date = pd.Timestamp('20170101', tz='Europe/Brussels')

#manque 20150801
end_date = pd.Timestamp('20150801', tz='Europe/Brussels')
#end_date = pd.Timestamp('20150201', tz='Europe/Brussels')

# Generate monthly date ranges
monthly_dates = pd.date_range(start=start_date, end=end_date, freq='MS')

# Iterate over each month
for date in monthly_dates:
    # Define the start and end dates for the current month
    start_month = date
    end_month = date +  pd.offsets.MonthBegin(1) # End of the current month
    print(start_month)
    print(end_month)

    string = start_month.strftime('%Y-%m') + '.parquet'
    day_ahead_prices = client.query_day_ahead_prices(country_code, start=start_month, end=end_month)

    #generation_by_plant = client.query_generation_per_plant(country_code, start=start_month, end=end_month, psr_type=None, include_eic=False)

    table = pa.Table.from_pandas(day_ahead_prices.to_frame())
    #my_table = pa.Table.from_pandas(day_ahead_prices)
    # Write the table to a Parquet file
    pq.write_table(table, string)
    # Print a message indicating the successful query
    print(f"Data queried successfully for the month: {start_month.strftime('%Y-%m')}")


  