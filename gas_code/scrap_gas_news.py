
from GoogleNews import GoogleNews
import datetime
import math
from datetime import datetime, timedelta
import pandas as pd
import os

#comme il faut prendre au moins deux dates consécutives
#on les automatise deux par deux

# à un moment date est repris par datetime


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

    demand_news = GoogleNews(start=date_str,  end=new_date_str)
    demand_news.get_news('gas demand')
    supply_news = GoogleNews(start=date_str,  end=new_date_str)
    supply_news.get_news('gas supply')
    uncertainty_news = GoogleNews(start=date_str,  end=new_date_str)
    uncertainty_news.get_news('gas uncertainty')
    
    demand_news = pd.DataFrame(demand_news.results())
    supply_news = pd.DataFrame(supply_news.results())
    uncertainty_news = pd.DataFrame(uncertainty_news.results())

    demand_news['event_type'] = 'demand'
    supply_news['event_type'] = 'supply'
    uncertainty_news['event_type'] = 'uncertainty'


    df_gas_news = pd.concat([demand_news, supply_news, uncertainty_news])
    if df_gas_news.empty==False:
        df_gas_news = df_gas_news[['title', 'date', 'datetime', 'media', 'event_type']]
    
    #df_gas_news['datetime'] = pd.to_datetime(df_gas_news['datetime'])
    return(df_gas_news)

# sortie de Google News 25 janvier 2006 pour la partie américaine
store_news_data = pd.DataFrame(columns = ['title', 'date', 'datetime', 'media', 'event_type'])

# Search from 1st Jan 2000
# Search until 12th December 2023
#attention format de date américaine mm/dd/yyyy
jour_initial = "02/24/2022"
my_datetime = datetime.strptime(jour_initial, '%m/%d/%Y')

#test = get_gas_news(date)

while (my_datetime < datetime(2022, 9, 30)):
    
    # Convert back to American date format string
    date_str = my_datetime.strftime('%m/%d/%Y')
    if my_datetime.day ==1 or my_datetime.day == 2:
        print(f"{date_str}")
    date_news = get_gas_news(date_str)
    # Use pd.concat to append the new row
    store_news_data = pd.concat([store_news_data, date_news], ignore_index=True)
    # Add two days
    my_datetime += timedelta(days=2)
    

os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()
store_news_data.to_csv('store_news_data_dix_neuf.csv', index=False)







demand_news = GoogleNews(start=date_str,  end=new_date_str, region='US')
demand_news.get_news('gas demand')
demand_news = pd.DataFrame(demand_news.results())

demand_news_UK = GoogleNews(start=date_str,  end=new_date_str, region='GB')
demand_news_UK.get_news('gas demand')
demand_news_UK = pd.DataFrame(demand_news_UK.results())

demand_news_FR = GoogleNews(start=date_str,  end=new_date_str, region = 'FR')
demand_news_FR.get_news('gas')
demand_news_FR = pd.DataFrame(demand_news_FR.results())

print(demand_news)
print(demand_news_UK)
print(demand_news_FR)