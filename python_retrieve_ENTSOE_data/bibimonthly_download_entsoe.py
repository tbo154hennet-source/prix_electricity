import pyarrow as pa
import pyarrow.parquet as pq
import pandas as pd
from entsoe import EntsoePandasClient

client = EntsoePandasClient(api_key='e8c7f878-ba9e-4b59-b7f1-7476e5e4f526')
country_code = 'PT'  

#2016-03
# Define start and end dates
start_date = pd.Timestamp('20170201', tz='Europe/Brussels')
end_date = pd.Timestamp('20171231', tz='Europe/Brussels')
# Generate monthly date ranges
monthly_dates = pd.date_range(start=start_date, end=end_date, freq='MS')
import os
path = r'C:\Users\DBT3KD\Desktop\Etude_macro_elec\DATA\Generation_by_plant\pt'  # Remplacez par votre chemin
os.chdir(path)

# Changer le répertoire de travail
os.getcwd()

#periodStart=202301282300&periodEnd=202301292300
# Iterate over each month
for date in monthly_dates:
    # Define the start and end dates for each quarter of the month
    start_month_first_quarter = date
    end_month_first_quarter = date + pd.DateOffset(days=7)
    
    start_month_second_quarter = end_month_first_quarter
    end_month_second_quarter = start_month_second_quarter + pd.DateOffset(days=7)
    
    start_month_third_quarter = end_month_second_quarter
    end_month_third_quarter = start_month_third_quarter + pd.DateOffset(days=7)
    
    start_month_fourth_quarter = end_month_third_quarter
    end_month_fourth_quarter = date + pd.offsets.MonthBegin(1)  # End of the month

    # Query and save data for each quarter
    for i, (start, end, label) in enumerate(
        [(start_month_first_quarter, end_month_first_quarter, "first-quarter"),
         (start_month_second_quarter, end_month_second_quarter, "second-quarter"),
         (start_month_third_quarter, end_month_third_quarter, "third-quarter"),
         (start_month_fourth_quarter, end_month_fourth_quarter, "fourth-quarter")]
    ):
        # Print which quarter is being processed
        print(f"Processing {label} of {start.strftime('%Y-%m')}")

        # Query data
        generation_data = client.query_generation_per_plant(
            country_code, start=start, end=end, psr_type=None, include_eic=False
        )

        # Define file path for each quarter
        file_name = f"{start.strftime('%Y-%m')}-{label}.parquet"

        # Convert the pandas dataframe to a pyarrow table and write to Parquet file
        generation_table = pa.Table.from_pandas(generation_data)
        pq.write_table(generation_table, file_name)

        # Print a success message
        print(f"Data queried successfully for the {label}: {start.strftime('%Y-%m')}")
