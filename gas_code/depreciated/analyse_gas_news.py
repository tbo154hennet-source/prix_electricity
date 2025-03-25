import pandas as pd
import os
from mistralai import Mistral
import re
import json
import time

api_key = "nsjcr3GfVcGCQrvJKIv5VQqDZZAWEKEz"
model = "open-mistral-nemo"

client = Mistral(api_key=api_key)

chat_response = client.chat.complete(
    model= model,
    messages = [
        {
            "role": "user",
            "content": "What is the best French cheese?",
        },
    ]
)
#print(chat_response.choices[0].message.content)

instructions_content = (
    "You are an assistant reading the news. "
    "You will read through the list of news article titles provided and keep only those"
    "new information that is relevant to natural gas supply and demand"
    "and likely to disrupt the market."
    "In particular, you will disregard opinion pieces, or discussion articles and keep only statements that bring narrative information."
    "You will return a JSON format output gathering all the relevant articles"
    "This file should have two fields:"
    "The first named title includes the title of the article"
    "The second chooses the most appropriate label between supply and demand"
    "Nothing else than the JSON format is expected."
)

#day_news = first_group
# Sort the DataFrame by 'datetime' column in ascending order
os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()

#news_data = pd.read_parquet('labeled_news_data.parquet') #, engine='pyarrow')
news_data = pd.read_parquet('store_full_news_data.parquet') #, engine='pyarrow')
news_data['datetime'] =pd.to_datetime(news_data['date'], format='mixed', dayfirst=True, errors='coerce')
news_data.dropna(subset=['datetime'], inplace=True)
news_data = news_data.drop_duplicates()



def sort_news_data(day_news):#, month_context, previous_day_context, news_data):
    
    global sorted_news_data
    print(day_news['datetime'].iloc[0])

    #build news day sumamry and instrument dummies
    day_news_title = day_news['title'].astype(str)

    # Concatenate all titles with a blank line separator
    concatenated_titles = day_news_title.str.cat(sep='\n\n')

    
    completion =  client.chat.complete(
    model= model,
    messages=[
            {"role": "system",
              "content": instructions_content},
              {"role": "user",
              "content": "Here is the main news of the day.\n {text}".format(text=concatenated_titles)}
        ]
    )

    summary_output = completion.choices[0].message.content

    # Define a regex to extract JSON block
    json_pattern = re.compile(r'\[\s*{\s*"title":.*?}\s*\]', re.DOTALL)

    # Search for JSON in the text
    match = json_pattern.search(summary_output)

    if match:
        json_found = match.group()

    my_parsed_data = json.loads(json_found)


    #if not several news articles
    try:
        # Attempt to convert directly to DataFrame
        my_parsed_data = pd.DataFrame(my_parsed_data)
    except ValueError:
        # If it fails, wrap it in a list and try again
        my_parsed_data = pd.DataFrame([my_parsed_data])
    my_parsed_data['datetime'] = day_news['datetime'].iloc[0]
    my_parsed_data.columns = ['title', 'label', 'datetime']
    
    #my_parsed_data.keys()
    # Ajout du dictionnaire comme nouvelle ligne
    # Recommended way using pd.concat()
    sorted_news_data = pd.concat([sorted_news_data, my_parsed_data], ignore_index=True).drop_duplicates(subset=['title', 'datetime'], keep='first')
    sorted_news_data = sorted_news_data.reset_index(drop=True)
    #print(sorted_news_data.keys())

    # Delay for 1 second between requests
    time.sleep(3)

    return sorted_news_data


grouped_data = news_data.groupby(news_data['datetime'].dt.date)
first_group_key, first_group = list(grouped_data)[0]
first_group_key, first_group = list(grouped_data)[8]
day_news = first_group
sort_news_data(first_group)

import itertools
 # Option 1: Use itertools.islice to get the first ten groups
first_ten_groups = list(itertools.islice(grouped_data, 10))
# Get the first 10 group keys
first_ten_keys = list(grouped_data.groups.keys())[:10]
# Filter the groupby object to include only these keys
filtered_grouped = news_data[news_data['datetime'].dt.date.isin(first_ten_keys)].groupby(news_data['datetime'].dt.date)
first_group_key, first_group = list(filtered_grouped)[0]
filtered_grouped.apply(sort_news_data)

#trous 2016-11-19, 2016-12-18, 2019-01-27


sorted_news_data = pd.DataFrame(columns=['title', 'datetime'])
grouped_data.apply(sort_news_data)
# Filter groups based on the condition
filtered_data = grouped_data.filter(lambda x: (x['datetime'] >= '2019-01-28').any())
filtered_data = filtered_data.groupby(filtered_data['datetime'].dt.date)
type(filtered_data)
filtered_data.apply(sort_news_data)

first_group_key, first_group = list(filtered_data)[0]


final_news_data = sorted_news_data.reset_index(drop=True)

len(sorted_news_data)
sorted_news_data[['title', 'datetime']]

sorted_news_data[['title', 'datetime']].head(100)

os.getcwd()
sorted_news_data.to_parquet('sorted_news_data_LLM_2019.parquet')
