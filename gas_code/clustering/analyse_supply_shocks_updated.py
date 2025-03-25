
import os
import pandas as pd
from mistralai import Mistral
import re
import json
import time


data_path = "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data"
os.chdir(data_path)

news_data = pd.read_parquet('labeled_news_distinct.parquet') #, engine='pyarrow')
news_data['datetime'] =pd.to_datetime(news_data['date'], format='mixed', dayfirst=True, errors='coerce')
news_data.dropna(subset=['datetime'], inplace=True)
news_data = news_data.drop_duplicates()

data_path_bis = "C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news"
os.chdir(data_path_bis)
all_info_days_type= pd.read_csv("all_info_days_type.csv")

all_info_days_type.shape

api_key = "nsjcr3GfVcGCQrvJKIv5VQqDZZAWEKEz"
model = "open-mistral-nemo"
model_mistral = "open-mixtral-8x7b"

client = Mistral(api_key=api_key)

# instructions_content = (
#     "You are an assistant reading the news. "
#     "I will give you a list of titles from news articles."
#     "You will generate a structured output in JSON format with three fields."
#     "The first one is a sentence summarizing the day overall main news. It should not exceed 50 characters."
#     "If there is no single new that stands out, typically mentioned by at least half of the articles, you can write 'No single news stands out'."
#     "The second field is a dummy variable equal to 1 if the news results from a scheduled announcement and -1 if it was unscheduled."
#     "You shall answer 0 if the answer is ambiguous."
#     "The third field is a dummy variable equal to 1 if the news is likely to increase supply and -1 it is likely to decrease it." 
#     "You shall answer 0 if the answer is ambiguous."
# )

instructions_content = (
    "You are an assistant reading the news. "
    "I will give you a list of titles from news articles."
    "You will generate a structured output in JSON format with two fields."
    "The first one is a sentence summarizing the day overall main news. It should not exceed 50 characters."
    "If there is no single new that stands out, typically mentioned by at least half of the articles, you can write 'No single news stands out'."
    "The second field indicates whether this piece of news is expected to increase or decrease oil prices."
    "The value should be 1 if the news is expected to increase oil prices, -1 if it is expected to decrease oil prices, and 0 if the answer is ambiguous."
)



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

    try:
        # Try to parse the object using json.loads()
        my_parsed_data = json.loads(summary_output)

    except (ValueError, TypeError):
        # If parsing fails, it's not a valid JSON string
        # Regular expression to extract JSON part between the ```json and ```
        json_match = re.search(r'```json\n(.*?)\n```', summary_output, re.DOTALL)
        json_part = json_match.group(1)  # The JSON part without the surrounding markdown
        my_parsed_data = json.loads(json_part)
    

    #if not several news articles
    # If it fails, wrap it in a list and try again
    my_parsed_data = pd.DataFrame([my_parsed_data])

    my_parsed_data['datetime'] = day_news['datetime'].iloc[0]
    my_parsed_data['n_articles'] = len(day_news)
    
    #my_parsed_data.columns = ['summary', 'expectation', 'shock_sign', 'datetime', 'n_articles']
    my_parsed_data.columns = ['summary', 'oil_price_effect', 'datetime', 'n_articles']
    
    #my_parsed_data.keys()
    # Ajout du dictionnaire comme nouvelle ligne
    # Recommended way using pd.concat()
    sorted_news_data = pd.concat([sorted_news_data, my_parsed_data], ignore_index=True).drop_duplicates(subset=['summary', 'datetime'], keep='first')
    sorted_news_data = sorted_news_data.reset_index(drop=True)
    #print(sorted_news_data.keys())

    # Delay for 1 second between requests
    time.sleep(5)

    return sorted_news_data

all_info_days_type.keys()


selected_dates = pd.to_datetime(all_info_days_type['datetime'])
news_data['datetime'] = pd.to_datetime(news_data['datetime'], format='mixed', dayfirst=True, errors='coerce')
all_info_days_type['datetime'] = selected_dates
selected_news_data = news_data[news_data['datetime'].isin(selected_dates)]

#news_data[news_data['datetime']== "2016-09-06"]

grouped_data = selected_news_data.groupby(selected_news_data['datetime'].dt.date)


# Get the name of the first group from the grouped object
first_group_name = list(grouped_data.groups.keys())[2]

# Get all rows for the first group
first_group_rows = grouped_data.get_group(first_group_name)

# Display the rows of the first group
print(first_group_rows)

sorted_news_data = pd.DataFrame(columns=['summary', 'datetime'])
day_news = first_group_rows
test = sort_news_data(day_news)


grouped_data.apply(sort_news_data)

# Convert datetime column in both dataframes to datetime64[ns]
all_info_days_type['datetime'] = pd.to_datetime(all_info_days_type['datetime'])
sorted_news_data['datetime'] = pd.to_datetime(sorted_news_data['datetime'])

# Now merge safely
merged_data = pd.merge(all_info_days_type, sorted_news_data, on='datetime', how='left')

#merged_data.loc[merged_data['OPEC'] == True, 'expectation'] = 1
#merged_data.loc[merged_data['OPEC'] == True, ['summary','datetime']] = 1
os.chdir(data_path_bis)
os.getcwd()
merged_data.to_csv('LLM_description_instrument_days.csv')
sorted_news_data.to_csv('dominating_news_data.csv')

