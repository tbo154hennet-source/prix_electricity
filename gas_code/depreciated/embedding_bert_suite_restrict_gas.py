from transformers import BertTokenizer, BertModel
import torch
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import numpy as np
import pandas as pd
from mistralai import Mistral
import re
import json
import time
import datetime
import requests
import os


#sample_news_2021 = news_data[(news_data['datetime'].dt.year == 2021)]
os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()

#news_data = pd.read_parquet('labeled_news_data.parquet') #, engine='pyarrow')
news_data = pd.read_parquet('store_full_news_data.parquet') #, engine='pyarrow')

news_data['datetime'] =pd.to_datetime(news_data['date'], format='mixed', dayfirst=True, errors='coerce')
news_data.dropna(subset=['datetime'], inplace=True)
# Correct way to drop a column
#news_data.drop(columns='event_type', inplace=True)
news_data = news_data.drop_duplicates()

#len(news_data)
gas_keywords = ["gas", "Gazprom", "LNG", "TTF", "GAZP"]

# Create a regex pattern by joining keywords with "|"
pattern = r'\b(' + '|'.join(gas_keywords) + r')\b'

# Filter rows where 'title' contains any of the keywords (case-insensitive)
filtered_news = news_data[news_data['title'].str.contains(pattern, case=False, na=False)]

# Define the keywords you want to exclude
exclude_keywords = ["edible", "food", "biofuel", "\\?", "station", "gasoline", "diesel"]

# Create a single pattern from the keywords
exclude_pattern = "|".join(exclude_keywords)

# Filter out rows where 'title' contains any of the exclude keywords
filtered_news = filtered_news[~filtered_news['title'].str.contains(exclude_pattern, case=False, na=False, regex=True)]

len(filtered_news)
news_articles = list(filtered_news['title'].values)
#len(news_articles)

# Load pre-trained BERT model and tokenizer
tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertModel.from_pretrained('bert-base-uncased')

# Convert the text to BERT embeddings
def get_bert_embedding(text):
    inputs = tokenizer(text, return_tensors='pt', truncation=True, padding=True, max_length=512)
    with torch.no_grad():
        outputs = model(**inputs)
    # Use the embeddings from the last hidden layer (the mean of all token embeddings)
    return outputs.last_hidden_state.mean(dim=1).squeeze().numpy()

# Convert articles to BERT embeddings
article_bert_vectors = np.array([get_bert_embedding(article) for article in news_articles])

#save bert embeddings
df = pd.DataFrame(article_bert_vectors)
df.to_parquet("bert_embeddings_gas_specific.parquet", engine="pyarrow", index=False)

# Step 5: Apply K-means Clustering
small_num_clusters = 7 # Specify the number of clusters
kmeans_bert_small = KMeans(n_clusters=small_num_clusters, random_state=42)
kmeans_bert_small.fit(article_bert_vectors)

# Step 5: Apply K-means Clustering
high_num_clusters = 13  # Specify the number of clusters
kmeans_bert_high = KMeans(n_clusters= high_num_clusters, random_state=42)
kmeans_bert_high.fit(article_bert_vectors)

# Step 6: Assign the Textual Labels to the Articles
clustered_articles_bert = pd.DataFrame({
    'article': filtered_news['title'],
    'small_cluster': kmeans_bert_small.labels_,
    'high_cluster': kmeans_bert_high.labels_
})

clustered_articles_bert_with_dates = pd.merge(
    filtered_news,        # First dataframe
    clustered_articles_bert,      # Second dataframe
    left_on="title",         # Column from the first dataframe
    right_on="article",      # Column from the second dataframe
    how="inner"              # Merge type: inner, outer, left, or right
)

#probleme si je lui donne l'ensemble des nouvelles
#prompt avec beaucoup trop de tokens
#a la place je prends juste un sample des news de chaque catégorie



api_key = "nsjcr3GfVcGCQrvJKIv5VQqDZZAWEKEz"
instructions_clusters = ("I have clustered these news articles according to different themes."
    "Read the titles of articles from each cluster and give a appropriate label putting the emphasis"
    "on the differences between clusters.The output shall be in json format where each key is the cluster number and its unique argument is the label"
    "You will return as many labels as the number of clusters.")
model_mistral = "open-mistral-nemo"
model_mistral = "open-mixtral-8x7b"
client = Mistral(api_key=api_key)


def get_cluster_labels(size, clustered_articles_bert_with_dates):
    #size="high"
    if size == "small":
        num_clusters = small_num_clusters
    else:
        num_clusters = high_num_clusters

    concatenated_clusters = ""
    for i in range(1, num_clusters):
        concatenated_clusters = concatenated_clusters + "This is a new cluster"
        cluster_i = clustered_articles_bert_with_dates[clustered_articles_bert_with_dates[size+'_cluster'] == i].sample(n=100, random_state=42)
        cluster_i = cluster_i['title'].astype(str)
        cluster_i = cluster_i.str.cat(sep='\n\n')
        concatenated_clusters = concatenated_clusters + str(i)+ "th Cluster : {text}\n\n".format(text=cluster_i)

    
    completion =  client.chat.complete(
        model= model_mistral,
        messages=[
                {"role": "system",
                "content": instructions_clusters},
                {"role": "user",
                "content": concatenated_clusters}
            ]
        )
    summary_output = completion.choices[0].message.content

    try:
        # Try to parse the object using json.loads()
        data_labels = json.loads(summary_output)

    except (ValueError, TypeError):
        # If parsing fails, it's not a valid JSON string
        # Regular expression to extract JSON part between the ```json and ```
        json_match = re.search(r'```json\n(.*?)\n```', summary_output, re.DOTALL)
        json_part = json_match.group(1)  # The JSON part without the surrounding markdown
        data_labels = json.loads(json_part)

    df_labels = pd.DataFrame(data_labels.keys(), data_labels.values())
    df_labels.reset_index(inplace=True)
    df_labels.columns = [size+'_cluster_label', size+'_cluster' ]
    df_labels[size+'_cluster'] = df_labels[size+'_cluster'].apply(lambda x : int(re.search(r'\d+', x).group())) # Finds sequences of digits
    return(df_labels)



small_cluster_labels = get_cluster_labels("small", clustered_articles_bert_with_dates)
high_cluster_labels = get_cluster_labels("high", clustered_articles_bert_with_dates)

#import copy
#sauvegarde = copy.deepcopy(small_cluster_labels)
#high_cluster_labels = df_labels

# Sample string
#text = "1th Cluster 23"
# Extract the first number
#match = re.search(r'\d+', text)  # Finds the first sequence of digits
#int(match.group())
#clustered_articles_bert_with_dates.value_counts('high_cluster')

clustered_articles_bert_final = pd.merge(
    clustered_articles_bert_with_dates,        # First dataframe
    small_cluster_labels,      # Second dataframe
    on='small_cluster',     # Column from the second dataframe
    how="inner"              # Merge type: inner, outer, left, or right
)

clustered_articles_bert_final = pd.merge(
    clustered_articles_bert_final,        # First dataframe
    high_cluster_labels,      # Second dataframe
    on='high_cluster',     # Column from the second dataframe
    how="inner"              # Merge type: inner, outer, left, or right
)

#question est ce que les labels s'intersectent

table = clustered_articles_bert_final[['small_cluster_label','high_cluster_label'] ].value_counts()
table = pd.DataFrame(table).reset_index()
table.sort_values(by='small_cluster_label', inplace=True)
print(table.head(24))

os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data/clustered_news")
os.getcwd()
clustered_articles_bert_final.to_csv("full_news_double_clustering_gas_restrict.csv", index=False)



#clustered_articles_bert_with_dates['cluster'] = clustered_articles_bert_with_dates['cluster'].astype(int)
