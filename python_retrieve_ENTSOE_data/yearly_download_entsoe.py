import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'FR'
#country_code = 'PT'
# DE-AT-LU / CH/ IT ou IT-NORD, IT_NORD, LU
#country_code_from = 'ES'
#country_code_to = 'PT'   

# Define start and end dates
start_date = pd.Timestamp('20240101', tz='Europe/Brussels')
#end_date = pd.Timestamp('20170101', tz='Europe/Brussels')
end_date = pd.Timestamp('20251231', tz='Europe/Brussels')
# Generate monthly date ranges
yearly_dates = pd.date_range(start=start_date, end=end_date, freq='YS')

import os
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Wind_solar_forecast\FR\intraday'
#path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_tot\fr'  # Remplacez par votre chemin
#path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\cross_border_flows\actual_flows\ES_PT'
os.chdir(path)

# Changer le répertoire de travail
os.getcwd()
#date = start_date
# Iterate over each month
for date in yearly_dates:
    # Define the start and end dates for the current month
    start_year = date
    end_year = date +  pd.offsets.YearBegin(1) # End of the current month
    print(start_year)
    print(end_year)
    string = start_year.strftime('%Y')+ country_code + '.parquet'

    #day_ahead_prices = client.query_day_ahead_prices(country_code, start=start_year, end=end_year)
    
    #withdrawn_unavailability_of_generation_units = client.query_withdrawn_unavailability_of_generation_units(country_code, start_year, end_year)
    #string = country_code_from + '_'+ country_code_to + '_'+ start_year.strftime('%Y') + '.parquet'
    #gen_unavailabilities= client.query_unavailability_of_generation_units(
        #country_code, start=start_year, end=end_year, docstatus=None, periodstartupdate=None, periodendupdate=None)
    #wind_and_solar_forecast = client.query_wind_and_solar_forecast(country_code, start=start_year, end=end_year, psr_type=None)
    #generation_forecast = client.query_generation_forecast(country_code, start=start_year, end=end_year)
    intraday_wind_and_solar_forecast = client.query_intraday_wind_and_solar_forecast(country_code, start=start_year, end=end_year, psr_type=None)
    #generation_tot = client.query_generation(country_code, start=start_year, end=end_year, psr_type=None, include_eic=False)
    #crossborder_flows = client.query_crossborder_flows(country_code_from, country_code_to, start = start_year, end = end_year)
    #scheduled_exchanges = client.query_scheduled_exchanges(country_code_from, country_code_to,  start = start_year, end = end_year, dayahead=True)
    #scheduled_exchanges_bis = client.query_scheduled_exchanges(country_code_from, country_code_to,  start = start_year, end = end_year, dayahead=False)
    
    #net_transfer_capacity_dayahead = client.query_net_transfer_capacity_dayahead(country_code_from, country_code_to,  start = start_year, end = end_year)
    #net_transfer_capacity_weekahead= client.query_net_transfer_capacity_weekahead(country_code_from, country_code_to,  start = start_year, end = end_year)
    #net_transfer_capacity_monthahead= client.query_net_transfer_capacity_monthahead(country_code_from, country_code_to,  start = start_year, end = end_year)
    #net_transfer_capacity_yearahead = client.query_net_transfer_capacity_yearahead(country_code_from, country_code_to,  start = start_year, end = end_year)
    
    
    #table = pa.Table.from_pandas(generation_tot)
    table = pa.Table.from_pandas(intraday_wind_and_solar_forecast)
    #day_ahead_prices = client.query_day_ahead_prices(country_code, start=start_year, end=end_year)
    # Write the table to a Parquet file
    pq.write_table(table, string)
    # Print a message indicating the successful query
    print(f"Data queried successfully for the month: {start_year.strftime('%Y-%m')}")




