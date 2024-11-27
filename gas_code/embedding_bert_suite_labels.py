from transformers import BertTokenizer, BertModel
import torch
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
import numpy as np
import pandas as pd

# Step 1: Sample List of News Articles
news_articles = [
    "Oil prices rise after OPEC decision",
    "Government policies to curb carbon emissions",
    "The oil market is booming. What is next ?",
    "Climatosceptics are gaining ground, our carbon emissions as well",
    "Carbon emissions are at an all-time high",
    "The future of oil prices is uncertain"
]

#sample_news_2021 = news_data[(news_data['datetime'].dt.year == 2021)]
os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/DATA/gas_news_data")
os.getcwd()

news_data = pd.read_parquet('labeled_news_data.parquet') #, engine='pyarrow')

news_data['datetime'] =pd.to_datetime(news_data['date'], format='mixed', dayfirst=True, errors='coerce')
news_data.dropna(subset=['datetime'], inplace=True)
# Correct way to drop a column
news_data.drop(columns='event_type', inplace=True)
news_data = news_data.drop_duplicates()

news_articles = list(news_data['title'].values)

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

# Step 5: Apply K-means Clustering
num_clusters = 12  # Specify the number of clusters
kmeans_bert = KMeans(n_clusters=num_clusters, random_state=42)
kmeans_bert.fit(article_bert_vectors)

# Step 6: Assign the Textual Labels to the Articles
clustered_articles_bert = pd.DataFrame({
    'article': news_articles,
    'cluster': kmeans_bert.labels_
})


clustered_articles_bert_with_dates = pd.merge(
    news_data,        # First dataframe
    clustered_articles_bert,      # Second dataframe
    left_on="title",         # Column from the first dataframe
    right_on="article",      # Column from the second dataframe
    how="inner"              # Merge type: inner, outer, left, or right
)


concatenated_clusters = ""
for i in range(1, num_clusters):
    cluster_i = clustered_articles_bert_with_dates[clustered_articles_bert_with_dates['cluster'] == i]
    cluster_i = cluster_i['title'].astype(str)
    cluster_i = cluster_i.str.cat(sep='\n\n')
    concatenated_clusters = concatenated_clusters + str(i)+ "th Cluster : {text}\n\n".format(text=cluster_i)

instructions_clusters = ("I have clustered these news articles according to different themes."
"Read the titles of articles from each cluster and give a appropriate label putting the emphasis"
"on the differences between clusters.The output shall be in json format where each key is the cluster number and its unique argument is the label"
)
model_mistral = "open-mistral-nemo"
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

# Regular expression to extract JSON part between the ```json and ```
json_match = re.search(r'```json\n(.*?)\n```', summary_output, re.DOTALL)

# If a match is found, extract and print the JSON part
if json_match:
    json_part = json_match.group(1)  # The JSON part without the surrounding markdown

data_labels = json.loads(json_part)
df_labels = pd.DataFrame(data_labels.keys(), data_labels.values())
df_labels.reset_index(inplace=True)
df_labels.columns = ['cluster_label', 'cluster' ]


# Sample string
#text = "1th Cluster 23"
# Extract the first number
#match = re.search(r'\d+', text)  # Finds the first sequence of digits
#int(match.group())


df_labels['cluster'] = df_labels['cluster'].apply(lambda x : int(re.search(r'\d+', x).group())) # Finds sequences of digits

clustered_articles_bert_final = pd.merge(
    clustered_articles_bert_with_dates,        # First dataframe
    df_labels,      # Second dataframe
    on='cluster',     # Column from the second dataframe
    how="inner"              # Merge type: inner, outer, left, or right
)

clustered_articles_bert_final['cluster_label'].value_counts()
clustered_articles_bert_final['cluster'].value_counts()
os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/gas_code")
os.getcwd()
clustered_articles_bert_final.to_csv("clustered_articles_bert_labeled_full_sample.csv", index=False)



#clustered_articles_bert_with_dates['cluster'] = clustered_articles_bert_with_dates['cluster'].astype(int)
