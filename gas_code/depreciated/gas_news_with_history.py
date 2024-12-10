
parsed_news_data = pd.DataFrame(columns=['summary', 'gas_supply_shock'])

instructions_content_history = (
    "You are an assistant reading the news. "
    "You will read through the list of news article titles provided and generate a single output "
    "in a structured format. Please return the results in JSON format with two fields. "
    "The first one is a sentence summarizing the day overall main news. "
    "The second one is a dummy variable corresponding to whether or not this news corresponds "
    "to a significant gas supply news shock."  
    "The 0 or 1 response should account for the absolute number of articles "
    "of the day relative to past days as well as the share of articles mentioning this news."
)
input_text = concatenated_titles_24

completion_24 =  client.chat.complete(
    model= model,
    messages=[
            {"role": "system",
              "content": instructions_content},
              {"role": "user",
              "content": "Here is the main news of the day.\n {text}".format(text=input_text)}
        ]
    )

import re
import json

summary_output_24 = completion_24.choices[0].message.content

# Regular expression to match the JSON part
json_match_24 = re.search(r'```json\n({.*?})\n```', summary_output_24, re.DOTALL)

# If a match is found, extract and print the JSON part
if json_match_24:
    json_part = json_match_24.group(1)  # The JSON part without the surrounding markdown
    print(json_part)

parsed_data = json.loads(json_part)
print(type(parsed_data))

# Ajout du dictionnaire comme nouvelle ligne
parsed_news_data = parsed_news_data.append(parsed_data, ignore_index=True)

day_context = parsed_data['summary']

month_context = "European supply is uncertain"

#analyse past month summary to get general overview on the last month
#last_30_rows = parsed_news_data.tail(30)


#context to have a sense of memory running individual api requests
instructions_content_without_context = (
    "You are an assistant reading the news. "
    "You will read through the list of news article titles provided and generate a single output "
    "in a structured format. Please return the results in JSON format with two fields. "
    "The first one is a sentence summarizing the day overall main news. "
    "The second one is a dummy variable corresponding to whether or not this news corresponds "
    "to a significant gas supply news shock."  
    "The 0 or 1 response should account for the absolute number of articles "
    "of the day relative to past days as well as the share of articles mentioning this news"
    "and whether or not it is new information to the context."
)
instructions_content_with_context = (instructions_content_without_context +
 "\nThe context was such that the past month, " +
         month_context +
         "Yesterday,"+
        day_context) 

input_text_25 = concatenated_titles_25

prompt = ("Here is the main news of the day.\n" + 
          input_text_25)

completion_25 =  client.chat.complete(
    model= model,
    messages=[
            {"role": "system",
              "content": instructions_content_with_context},
              {"role": "user",
              "content": prompt}
        ]
    )

summary_output_25 = completion_25.choices[0].message.content

# Regular expression to match the JSON part
json_match_25 = re.search(r'```json\n({.*?})\n```', summary_output_25, re.DOTALL)

# If a match is found, extract and print the JSON part
if json_match_25:
    json_part = json_match_25.group(1)  # The JSON part without the surrounding markdown
    print(json_part)

parsed_data_25 = json.loads(json_part)



