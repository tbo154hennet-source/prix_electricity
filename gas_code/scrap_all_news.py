
from GoogleNews import GoogleNews
import datetime
import math
from datetime import datetime, timedelta
import pandas as pd
import os
from gnews import GNews

#comme il faut prendre au moins deux dates consécutives
#on les automatise deux par deux

# à un moment date est repris par datetime

#

google_news = GNews()
    #datetime(2022, 9, 30)
google_news.start_date = (2022, 9, 29) # Search from 1st Jan 2020
google_news.end_date = (2022, 9, 30) # Search until 1st March 2020
top_news= google_news.get_news_by_topic("BUSINESS")
#"FINANCE"
#"ENERGY"
#demand_news.get_news('gas demand')
#supply_news.get_news('gas supply')
#uncertainty_news.get_news('gas uncertainty')

def get_gas_news(date_str):
    #date_str = my_datetime.strftime('%m/%d/%Y')
    #news_data = supply_news
    # Convert the string to a datetime object
    date_obj = datetime.strptime(date_str, '%m/%d/%Y')

    # Add one day
    date_obj += timedelta(days=1)
    
    # Convert back to American date format string
    new_date_str = date_obj.strftime('%m/%d/%Y')
    #new_date = date + timedelta(days=1)


    supply_news = GoogleNews(start=date_str,  end=new_date_str)
    supply_news.get_news('supply')
    demand_news = GoogleNews(start=date_str,  end=new_date_str)
    demand_news.get_news('demand')
    
    demand_news = pd.DataFrame(demand_news.results())
    supply_news = pd.DataFrame(supply_news.results())
    #uncertainty_news = pd.DataFrame(uncertainty_news.results())

    demand_news['event_type'] = 'demand'
    supply_news['event_type'] = 'supply'
    #uncertainty_news['event_type'] = 'uncertainty'
    # List of keywords
    #gas_keywords = ["gas", "Gazprom", "LNG", "TTF", "GAZP"]
    oil_keywords = ["oil", "OPEC", "fuel", "crude", "Brent", "WTI"]
    # Create a regex pattern by joining keywords with "|"
    pattern = r'\b(' + '|'.join(oil_keywords) + r')\b'

    # Filter rows where 'title' contains any of the keywords (case-insensitive)
    supply_filtered_news = supply_news[supply_news['title'].str.contains(pattern, case=False, na=False)]

    demand_filtered__news = demand_news[demand_news['title'].str.contains(pattern, case=False, na=False)]
    #filtered_demand_news['title'].iloc[0]

    df_gas_news = pd.concat([demand_filtered__news , supply_filtered_news])
    if df_gas_news.empty==False:
        df_gas_news = df_gas_news[['title', 'date', 'datetime', 'media', 'event_type']]
    
    #df_gas_news['datetime'] = pd.to_datetime(df_gas_news['datetime'])
    return(df_gas_news)

# sortie de Google News 25 janvier 2006 pour la partie américaine
store_news_data_all = pd.DataFrame(columns = ['title', 'date', 'datetime', 'media', 'event_type'])
# Search from 1st Jan 2000
# Search until 12th December 2023
#attention format de date américaine mm/dd/yyyy
jour_initial = "07/07/2022"
my_datetime = datetime.strptime(jour_initial, '%m/%d/%Y')

#test = get_gas_news(date)

while (my_datetime < datetime(2023, 12, 31)):
    
    # Convert back to American date format string
    date_str = my_datetime.strftime('%m/%d/%Y')
    if my_datetime.day ==1 or my_datetime.day == 2:
        print(f"{date_str}")
    date_news = get_gas_news(date_str)
    # Use pd.concat to append the new row
    store_news_data_all = pd.concat([store_news_data_all, date_news], ignore_index=True)
    # Add two days
    my_datetime += timedelta(days=2)
    

os.chdir("C:/Users/DBT3KD/Desktop/uncertainty_shocks/DATA/all_news")
os.getcwd()
store_news_data_all.to_csv('store_news_data_all_2022_2023.csv', index=False)

#Jul 7, 2022
store_news_data_all.keys()
store_news_data_all['date']



