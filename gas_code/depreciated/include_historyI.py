import os
import datetime
from GoogleNews import GoogleNews
import math
from datetime import datetime, timedelta
import pandas as pd

os.chdir("C:/Users/DBT3KD/Desktop")
os.getcwd()

date_str = "02/24/2022"
#date_str = "02/25/2022"
date_obj = datetime.strptime(date_str, '%m/%d/%Y')
# Add one day
date_obj += timedelta(days=1)
# Convert back to American date format string
new_date_str = date_obj.strftime('%m/%d/%Y')
#new_date = date + timedelta(days=1)

supply_news = GoogleNews(start=date_str,  end=new_date_str)
supply_news.get_news('gas supply')
supply_news = pd.DataFrame(supply_news.results())
supply_news_title = supply_news['title']
print(type(supply_news_title))
# Ensure that the series is of type str
supply_news_title = supply_news['title'].astype(str)

# Concatenate all titles with a blank line separator
concatenated_titles_24 = supply_news_title.str.cat(sep='\n\n')
concatenated_titles_25 = supply_news_title.str.cat(sep='\n\n')

print(concatenated_titles_24)
print(concatenated_titles_25)


def generate_summary(input_text, ):
    # Make request to chat GPT
    completion =  client.chat.complete(
    model= model,
    messages=[
            {"role": "system", "content": "You are an assistant reading the news. You need to parse available information and determine whether there is one main news corresponding to a gas supply news shock."},
            {"role": "user", "content": "Here is the main news of the day.\n {text}".format(text=input_text)}
        ]
    )

    # Get the summary of the inptu text output from Chat GPT
    summary_output = completion.choices[0].message.content

    return summary_output





