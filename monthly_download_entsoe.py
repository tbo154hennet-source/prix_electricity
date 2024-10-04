import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'FR'  
# Define start and end dates
start_date = pd.Timestamp('20150201', tz='Europe/Brussels')
#end_date = pd.Timestamp('20230101', tz='Europe/Brussels')
end_date = pd.Timestamp('20150301', tz='Europe/Brussels')

# Generate monthly date ranges
yearly_dates = pd.date_range(start=start_date, end=end_date, freq='YS')

# Iterate over each month
for date in yearly_dates:
    # Define the start and end dates for the current month
    start_month = date
    end_month = date +  pd.offsets.MonthBegin(1) # End of the current month
    print(start_month)
    print(end_month)


    # Query generation per plant data for the current month
    gen_unavailabilities = client.query_unavailability_of_generation_units(country_code, start=start_month, end=end_month, docstatus=None, periodstartupdate=None, periodendupdate=None)


    # Process the generation data as needed
    # For example, you can save it to a file or perform further analysis
    parquet_file_path = '/content/drive/MyDrive/outages/'

    string = parquet_file_path + start_month.strftime('%Y-%m') + '.parquet'
    # Convert the pandas dataframe to a pyarrow table
    gen_unav_table = pa.Table.from_pandas( gen_unavailabilities)
    # Write the table to a Parquet file
    pq.write_table(gen_unav_table, string)
    # Print a message indicating the successful query
    print(f"Data queried successfully for the month: {start_month.strftime('%Y-%m')}")