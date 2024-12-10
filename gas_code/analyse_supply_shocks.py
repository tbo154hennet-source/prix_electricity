
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
    "I will give you a list of titles from news articles."
    "You will assess whether or not one main piece of news stands out,"
    "typically if the same news is mentioned in more than half of the articles."
    "you will generate a structured output in JSON format with two fields."
    "The first one is a sentence summarizing the day overall main news."
    "The second one corresponds to one of the appropriate label from the following list:"
    "1. Gas supply news shock"
    "2. Gas demand news shock"
    "3. No main news"
)

day_news = first_group


# Sort the DataFrame by 'datetime' column in ascending order
#news_data = news_data.sort_values(by='datetime', ascending=True)

day_news = list(grouped_data)[18][1]
print(type(day_news))
day_news['title']
test <- sort_news_data(day_news)

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
    
    my_parsed_data.columns = ['summary', 'label', 'datetime', 'n_articles']
    
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
