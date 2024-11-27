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

'''
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
num_clusters = 2  # Specify the number of clusters
kmeans = KMeans(n_clusters=num_clusters, random_state=42)
kmeans.fit(article_bert_vectors)

# Clustered articles with BERT embeddings
clustered_articles_bert = pd.DataFrame({
    'article': news_articles,
    'cluster': kmeans.labels_
})

print(clustered_articles_bert)
'''


import nltk
from collections import Counter
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.preprocessing import StandardScaler
#nltk.download('stopwords')
from nltk.corpus import stopwords

#sample_news_2021 = news_data[(news_data['datetime'].dt.year == 2021)]
news_articles = list(news_data['title'].values)

# Step 1: Vectorize the articles using TF-IDF
vectorizer = TfidfVectorizer(stop_words=stopwords.words('english'))
X = vectorizer.fit_transform(news_articles)

# Step 2: Apply KMeans clustering
num_clusters = 12
kmeans = KMeans(n_clusters=num_clusters, random_state=42)
kmeans.fit(X)

# Step 3: Extract keywords for each cluster
def get_cluster_keywords(cluster_centers, feature_names, n_keywords=4):
    keywords = []
    for center in cluster_centers:
        # Get the indices of the top n features for each cluster
        sorted_indices = center.argsort()[-n_keywords:][::-1]
        cluster_keywords = [feature_names[i] for i in sorted_indices]
        keywords.append(cluster_keywords)
    return keywords

# Get feature names (words) from the TF-IDF vectorizer
feature_names = vectorizer.get_feature_names_out()

# Get the top 5 keywords for each cluster
cluster_keywords = get_cluster_keywords(kmeans.cluster_centers_, feature_names)

# Step 4: Assign meaningful labels to clusters based on keywords
cluster_labels = []
for i, keywords in enumerate(cluster_keywords):
    cluster_labels.append(f"Cluster {i+1}: " + ", ".join(keywords))

# Step 5: Assign these textual labels to the articles
clustered_articles = pd.DataFrame({
    'article': news_articles,
    'cluster': kmeans.labels_
})

# Assign textual labels to each article based on their cluster
clustered_articles['cluster_label'] = clustered_articles['cluster'].apply(lambda x: cluster_labels[x])

clustered_articles.sort_values(by="cluster", inplace=True)

os.chdir("C:/Users/DBT3KD/Desktop/Etude_macro_elec/prix_electricite_gitlab/gas_code")
os.getcwd()
clustered_articles.to_csv("clustered_articles_vdif.csv", index=False)



#labels par cluster proposés par chatgpt
cluster_labels = {
    0: "Global Gas Trade and Regional Cooperation",
    1: "Major Gas Supply Agreements",
    2: "Supply Chain and Infrastructure Challenges in Oil & Gas",
    3: "European Gas Crisis and Russia’s Role",
    4: "Geopolitics of Gas in the Middle East and Asia",
    5: "Gas Pipeline Disruptions and Cybersecurity",
    6: "New Pipeline Projects and Developments",
    7: "Energy Crisis and Gas Price Volatility",
    8: "Natural Gas Market Trends and Demand Forecasts",
    9: "Pipeline Projects in Developing Regions",
    10: "Supply Chain and Emergency Response",
    11: "Infrastructure Expansion and Gas Production"
}

clustered_articles['cluster_label'] = clustered_articles['cluster'].map(cluster_labels)

clustered_articles['cluster_label'].value_counts()
clustered_articles[clustered_articles['cluster']==1]

type(clustered_articles)

sample_news_2021.keys()
clustered_articles.keys()
# Merge two dataframes based on different columns
clustered_articles_with_dates = pd.merge(
    sample_news_2021,        # First dataframe
    clustered_articles,      # Second dataframe
    left_on="title",         # Column from the first dataframe
    right_on="article",      # Column from the second dataframe
    how="inner"              # Merge type: inner, outer, left, or right
)
len(clustered_articles_with_dates)
len(clustered_articles)
clustered_articles_with_dates.head(1)
clustered_articles_with_dates.to_csv("clustered_articles_vdif_labeled.csv", index=False)
