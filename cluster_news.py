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

sample_news_2021 = news_data[(news_data['datetime'].dt.year == 2021)]
news_articles = list(sample_news_2021['title'].values)

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
num_clusters = 10  # Specify the number of clusters
kmeans = KMeans(n_clusters=num_clusters, random_state=42)
kmeans.fit(article_bert_vectors)

# Step 4: Extract Keywords for Each Cluster (Using TF-IDF or Frequency Analysis)
def get_cluster_keywords(cluster_centers, feature_names, n_keywords=5):
    keywords = []
    for center in cluster_centers:
        # Get the indices of the top n features for each cluster
        sorted_indices = center.argsort()[-n_keywords:][::-1]
        cluster_keywords = [feature_names[i] for i in sorted_indices]
        keywords.append(cluster_keywords)
    return keywords

# You can use the TF-IDF vectorizer to get the feature names (important words)
vectorizer = TfidfVectorizer(stop_words=stopwords.words('english'))
X_tfidf = vectorizer.fit_transform(news_articles)
feature_names = vectorizer.get_feature_names_out()

# Get the top 5 keywords for each cluster
cluster_keywords = get_cluster_keywords(kmeans.cluster_centers_, feature_names)

# Step 5: Assign Meaningful Textual Labels Based on Keywords
cluster_labels = []
for i, keywords in enumerate(cluster_keywords):
    cluster_labels.append(f"Cluster {i+1}: " + ", ".join(keywords))

# Step 6: Assign the Textual Labels to the Articles
clustered_articles = pd.DataFrame({
    'article': news_articles,
    'cluster': kmeans.labels_
})

# Assign textual labels to each article based on their cluster
clustered_articles['cluster_label'] = clustered_articles['cluster'].apply(lambda x: cluster_labels[x])

# Display the result
print(clustered_articles)
clustered_articles['cluster_label'].value_counts()

clustered_articles[['cluster', 'cluster_label']]


