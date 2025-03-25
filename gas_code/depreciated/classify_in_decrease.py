
import os
os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()

import pyarrow.parquet as pq

# Read the Parquet file
bert_embeddings = pq.read_table("bert_embeddings.parquet")

bert_embeddings

from transformers import pipeline

# Load zero-shot classification pipeline
classifier = pipeline("zero-shot-classification", model="facebook/bart-large-mnli")
# Load sentiment-analysis pipeline
sentiment_classifier = pipeline("sentiment-analysis")


# Define your labels
labels = ["supply", "demand", "ambiguous"]

# Classify an example article
text = "OPEC cuts oil production, causing supply concerns globally."
# Example text
text_sentiment = "Global oil demand is surging due to economic recovery."

ambiguous_text = "The oil market is in a state of flux."

result = classifier(text, candidate_labels=labels)
result_sentiment = classifier(text_sentiment, candidate_labels=labels)
result_ambiguous = classifier(ambiguous_text, candidate_labels=labels)

print(result)
print(result_sentiment)
print(result_ambiguous)

#sample_news_2021 = news_data[(news_data['datetime'].dt.year == 2021)]
os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()

#news_data = pd.read_parquet('labeled_news_data.parquet') #, engine='pyarrow')
news_data = pd.read_parquet('store_full_news_data.parquet') #, engine='pyarrow')

# Define keywords for filtering
gas_keywords = ["gas", "Gazprom", "LNG", "TTF", "GAZP"]
exclude_keywords = ["edible", "food", "biofuel", "\\?", "station", "gasoline", "diesel"]

# Create patterns
gas_pattern = "|".join(gas_keywords)  # Combine gas keywords into a single regex
exclude_pattern = "|".join(exclude_keywords)  # Combine exclude keywords into a single regex

# Filter rows containing gas_keywords in the title
filtered_news = news_data[news_data["title"].str.contains(gas_pattern, case=False, na=False)]

# Exclude rows containing exclude_keywords in the title
filtered_news = filtered_news[~filtered_news["title"].str.contains(exclude_pattern, case=False, na=False)]


len(filtered_news)
filtered_news['title'][0]



test = filtered_news.sample(20)

# Define your labels
labels_positive_negative = ["supply increase", "supply_decrease", 
                             "demand increase", "demand decrease", "ambiguous"]


data_labels = test['title'].apply(lambda x: classifier(x, candidate_labels=labels_positive_negative))

data_labels = filtered_news['title'].apply(lambda x: classifier(x, candidate_labels=labels))

for label in data_labels:
    print(label)
