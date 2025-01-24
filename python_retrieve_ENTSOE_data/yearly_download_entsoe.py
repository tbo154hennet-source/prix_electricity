import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'ES'  
# Define start and end dates
start_date = pd.Timestamp('20240101', tz='Europe/Brussels')
#end_date = pd.Timestamp('20170101', tz='Europe/Brussels')
end_date = pd.Timestamp('20241231', tz='Europe/Brussels')
# Generate monthly date ranges
yearly_dates = pd.date_range(start=start_date, end=end_date, freq='YS')

import os
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\esp\gen_by_fuel'  # Remplacez par votre chemin
os.chdir(path)

# Changer le répertoire de travail
os.getcwd()


# Iterate over each month
for date in yearly_dates:
    # Define the start and end dates for the current month
    start_year = date
    end_year = date +  pd.offsets.YearBegin(1) # End of the current month
    print(start_year)
    print(end_year)
    string = start_year.strftime('%Y') + '.parquet'
    #gen_unavailabilities= client.query_unavailability_of_generation_units(
        #country_code, start=start_year, end=end_year, docstatus=None, periodstartupdate=None, periodendupdate=None)
    #wind_and_solar_forecast = client.query_wind_and_solar_forecast(country_code, start=start_year, end=end_year, psr_type=None)
    #intraday_wind_and_solar_forecast = client.query_intraday_wind_and_solar_forecast(country_code, start=start_year, end=end_year, psr_type=None)
    
    generation_tot = client.query_generation(country_code, start=start_year, end=end_year, psr_type=None, include_eic=False)
    
    #day_ahead_prices = client.query_day_ahead_prices(country_code, start=start_year, end=end_year)
    table = pa.Table.from_pandas(generation_tot)
    # Write the table to a Parquet file
    pq.write_table(table, string)
    # Print a message indicating the successful query
    print(f"Data queried successfully for the month: {start_year.strftime('%Y-%m')}")



