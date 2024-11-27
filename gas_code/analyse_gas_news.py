
import os
from mistralai import Mistral

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
    "new information that is relevant to gas supply and demand"
    "and likely to disrupt the market."
    "In particular, you will disregard opinion pieces, or discussion articles and keep only statements that bring narrative information."
    "For each selected article, you will return a JSON format ouput with a single key title including the title of the article "
)

#day_news = first_group
# Sort the DataFrame by 'datetime' column in ascending order
news_data = news_data.sort_values(by='datetime', ascending=True)

day_news = list(grouped_data)[18][1]
print(type(day_news))

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

    # Regular expression to extract JSON part between the ```json and ```
    json_match = re.search(r'```json\n(.*?)\n```', summary_output, re.DOTALL)

    # If a match is found, extract and print the JSON part
    if json_match:
        json_part = json_match.group(1)  # The JSON part without the surrounding markdown

    my_parsed_data = json.loads(json_part)
    #if not several news articles
    try:
        # Attempt to convert directly to DataFrame
        my_parsed_data = pd.DataFrame(my_parsed_data)
    except ValueError:
        # If it fails, wrap it in a list and try again
        my_parsed_data = pd.DataFrame([my_parsed_data])
    my_parsed_data['datetime'] = day_news['datetime'].iloc[0]
    my_parsed_data.columns = ['title', 'datetime']
    
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
grouped_data = news_feb_2022.groupby(news_data['datetime'].dt.date)

news_feb_2022_bis =  news_feb_2022[(news_feb_2022['datetime'].dt.day > 19)]
grouped_data_bis = news_feb_2022_bis.groupby(news_data['datetime'].dt.date)

list(grouped_data)[0]
list(grouped_data)[1]
list(grouped_data)[5]
first_group_key, first_group = list(grouped_data)[0]
second_group_key, second_group = list(grouped_data)[1]



sorted_news_data = pd.DataFrame(columns=['title', 'datetime'])

sorted_news_data = grouped_data.apply(sort_news_data)
sorted_news_data = grouped_data_bis.apply(sort_news_data)
sorted_news_data.drop_duplicates(subset=['title', 'datetime'], keep='first').sort_values(by='datetime', ascending=True)

print(type(sorted_news_data))
final_news_data = sorted_news_data.reset_index(drop=True)

my_parsed_data.keys()
len(sorted_news_data)
sorted_news_data[['title', 'datetime']]

sorted_news_data[['title', 'datetime']].head(100)

os.getcwd()
sorted_news_data.to_parquet('sorted_news_data.parquet')
