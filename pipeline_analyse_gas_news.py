import pandas as pd
import os
import numpy as np
from mistralai import Mistral
import re
import json
import time
import datetime
import requests


os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()

news_data = pd.read_parquet('labeled_news_data.parquet') #, engine='pyarrow')

news_data['datetime'] =pd.to_datetime(news_data['date'], format='mixed', dayfirst=True, errors='coerce')
news_data.dropna(subset=['datetime'], inplace=True)
# Correct way to drop a column
news_data.drop(columns='event_type', inplace=True)
news_data = news_data.drop_duplicates()

# Filter for February 2022
news_feb_2022 = news_data[(news_data['datetime'].dt.year == 2022) & 
                          (news_data['datetime'].dt.month == 2)]

api_key = "nsjcr3GfVcGCQrvJKIv5VQqDZZAWEKEz"
model = "open-mistral-nemo"

client = Mistral(api_key=api_key)

instructions_summary = ("You are an assistant reading the news."
                        "Summarize the news from the past few days in a single sentence with less than 70 characters.")

instructions_content_without_context = (
    "You are an assistant reading the news."
    "You will read through the list of news article titles provided and generate a single output "
    "in a structured format. Please return the results in JSON format with three fields. \n"
    "You will disregard opinion pieces or reports and focus on statements that bring narrative information."
    "You will consider only news relevant to gas supply and demand in the European market."
    "You will disregard news that are simply a repetition of previous information or if they are new"
    "or significant new developments to the context supplied as input.\n"
    "The first one is a sentence summarizing the day overall main news. It should not exceed 70 characters. \n "
    "The second entitled gas_supply_news_shock is a dummy variable corresponding to whether or not this news corresponds "
    "to a significant gas supply news shock affecting the European market."  
    "The 1 or 0 response should account for the absolute number of articles "
    "relative to past days as well as the share of articles mentioning the main information"
    "and whether or not it is new with regards to the context."
    "The labelling should be such that shocks are rare. \n"
    "The third field entitled gas_demand_news_shock is a dummy variable as well corresponding to whether or not this news corresponds "
    "to a significant gas demand news shock. The same guidelines should be used."
)

date = datetime.datetime(2022, 2, 8)
#pd.to_datetime('2022-02-01')

print(type(date))


def make_request_with_backoff():
    retries = 0
    max_retries = 5
    while retries < max_retries:
        try:
            response = requests.get("your_api_url")
            response.raise_for_status()  # If status code is not 2xx, raise error
            return response
        except requests.exceptions.HTTPError as err:
            if err.response.status_code == 429:
                retries += 1
                wait_time = min(2 ** retries + random.uniform(0, 1), 60)  # Exponential backoff
                print(f"Rate limit exceeded, retrying in {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                raise  # Raise other HTTP errors
    return None  # If max retries reached, return None or handle accordingly

response = make_request_with_backoff()


def analyse_news_data(day_news):#, month_context, previous_day_context, news_data):

    global previous_day_context  # Declare the global variable here
    global few_days_context
    global month_context
    global parsed_news_data

    print(previous_day_context)
    date = day_news['datetime'].iloc[0]

    #build few days context

    date_5 = date - pd.DateOffset(days=5)
    summary_news_past_month = parsed_news_data[(parsed_news_data['datetime'] < date)]
    summary_news_past_month = summary_news_past_month[(date_5 <= summary_news_past_month['datetime'])]
    
    summary_news_past_month = summary_news_past_month['summary'].astype(str)
    if (len(summary_news_past_month)>4):
        concatenated_days = summary_news_past_month.str.cat(sep='\n\n')

        completion =  client.chat.complete(
        model= model,
        messages=[
                {"role": "system",
                "content": instructions_summary},
                {"role": "user",
                "content": "Here is the news from the past few days.\n {text}".format(text=concatenated_days)}
            ]
        )
        month_context = completion.choices[0].message.content

    time.sleep(3)
    #build news day sumamry and instrument dummies
    day_news_title = day_news['title'].astype(str)

    # Concatenate all titles with a blank line separator
    concatenated_titles = day_news_title.str.cat(sep='\n\n')

    instructions_content_with_context = (
     instructions_content_without_context +
    "\nThe context was such that the past month, " +
         month_context +
         "Yesterday,"+
        previous_day_context)
    
    completion =  client.chat.complete(
    model= model,
    messages=[
            {"role": "system",
              "content": instructions_content_with_context},
              {"role": "user",
              "content": "Here is the main news of the day.\n {text}".format(text=concatenated_titles)}
        ]
    )

    summary_output = completion.choices[0].message.content
    parsed_data = json.loads(summary_output)
    #uniormiser le nom de la 2ème clé
    # Récupérer la deuxième clé et renommer dynamiquement
    keys = list(parsed_data.keys())  # Convertir les clés en liste
    first_key, second_key, third_key = keys[0], keys[1], keys[2]
    # Renommer les clés
    parsed_data['summary'] = parsed_data.pop(first_key)  # Renommer la deuxième clé
    parsed_data['gas_supply_news_shock'] = parsed_data.pop(second_key)  # Renommer la deuxième clé
    parsed_data['gas_demand_news_shock'] = parsed_data.pop(third_key)  # Renommer la deuxième clé
    
    parsed_data['datetime'] = date
    # Ajout du dictionnaire comme nouvelle ligne
    # Recommended way using pd.concat()
    parsed_news_data = pd.concat([parsed_news_data, pd.DataFrame([parsed_data])], ignore_index=True)
    
    previous_day_context = parsed_data['summary']

    # Delay for 1 second between requests
    time.sleep(3)

    return parsed_data


month_context = ""
previous_day_context = ""
grouped_data = news_data.groupby(news_data['datetime'].dt.date)
grouped_data = news_feb_2022.groupby(news_data['datetime'].dt.date)

#first_group_key, first_group = list(grouped_data)[0]

parsed_news_data = pd.DataFrame(columns=['summary', 'gas_supply_news_shock',
                                          'gas_demand_news_shock', 'datetime'])
parsed_news_data = grouped_data.apply(analyse_news_data)
parsed_news_data.keys()
parsed_news_data['gas_supply_news_shock'].value_counts()
parsed_news_data['gas_demand_news_shock'].value_counts()

for i in range(len(parsed_news_data)):
    print(parsed_news_data['summary'].iloc[i])

parsed_news_data['summary'].iloc[0]
news_data.groupby('datetime')

news_data['datetime'].head()
news_data['datetime'].tail()
news_data['date'].tail(50)
news_data.keys()








