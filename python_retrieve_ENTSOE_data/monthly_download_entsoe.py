import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient
import os

# Define start and end dates
client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'FR'
#country_code_to = 'FR'   
# Define start and end dates
start_date = pd.Timestamp('20240101', tz='Europe/Brussels')
#end_date = pd.Timestamp('20170101', tz='Europe/Brussels')
end_date = pd.Timestamp('20260101', tz='Europe/Brussels')
# Generate monthly date ranges
monthly_dates = pd.date_range(start=start_date, end=end_date, freq='MS')
import os
#path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_tot\esp\by_fuel'  # Remplacez par votre chemin
#path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Wind_solar_forecast\PT'
#path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\day_ahead_prices\fr'
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_tot\fr'
os.chdir(path)
os.getcwd()
#date = monthly_dates[len(monthly_dates)-6]

# Iterate over each month
for date in monthly_dates:
    # Define the start and end dates for the current month
    start_month = date
    end_month = date +  pd.offsets.MonthBegin(1) # End of the current month
    print(start_month)
    print(end_month)
    string = start_month.strftime('%Y-%m')+ country_code + '.parquet'

    #generation_per_plant = client.query_generation_per_plant(country_code, 
    #      start=start_month, end=end_month, psr_type=None, include_eic=False)

    #gen_unavailabilities= client.query_unavailability_of_generation_units(
     #   country_code, start=start_month, end=end_month, docstatus=None, periodstartupdate=None, periodendupdate=None)
    #wind_and_solar_forecast = client.query_wind_and_solar_forecast(country_code, start=start_year, end=end_year, psr_type=None)
    #intraday_wind_and_solar_forecast = client.query_intraday_wind_and_solar_forecast(country_code, start=start_year, end=end_year, psr_type=None)
    
    generation_tot = client.query_generation(country_code, start=start_month, end=end_month, psr_type=None, include_eic=False)
    #withdrawn_unavailability_of_generation_units = client.query_withdrawn_unavailability_of_generation_units(country_code, start_year, end_year)
    #query_crossborder_flows = client.query_crossborder_flows(country_code_from, country_code_to, start_month, end_month)
    #client.query_scheduled_exchanges(country_code_from, country_code_to, start, end, dayahead=False)
    #client.query_net_transfer_capacity_dayahead(country_code_from, country_code_to, start, end)
    #client.query_net_transfer_capacity_weekahead(country_code_from, country_code_to, start, end)
    #client.query_net_transfer_capacity_monthahead(country_code_from, country_code_to, start, end)
    #client.query_net_transfer_capacity_yearahead(country_code_from, country_code_to, start, end)
    #table = pa.Table.from_pandas(query_crossborder_flows)
    #table = pa.Table.from_pandas(generation_per_plant)
    # Example: Convert a Series to a DataFrame
    #wind_and_solar_forecast = client.query_intraday_wind_and_solar_forecast(country_code, start=start_month, end=end_month, psr_type=None)
    
    # Convert to a PyArrow Table
    # day_ahead_prices = client.query_day_ahead_prices(country_code, start=start_month, end=end_month)
    # table = pa.Table.from_pandas(day_ahead_prices.to_frame(name="day_ahead_prices"))
    table = pa.Table.from_pandas(generation_tot)
    #my_table = pa.Table.from_pandas(day_ahead_prices)
    # Write the table to a Parquet file
    pq.write_table(table, string)
    # Print a message indicating the successful query
    print(f"Data queried successfully for the month: {start_month.strftime('%Y-%m')}")

#query_withdrawn_unavailability_of_generation_units
# day_ahead_prices.head()