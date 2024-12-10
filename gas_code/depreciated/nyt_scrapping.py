
from pynytimes import NYTAPI
#Then you can simply add your API key (get your API key from The New York Times Dev Portal):

nyt = NYTAPI("AGElr60ezvBARu7Ywh9lkci6aNNT6q3Z", parse_dates=True)


NYTAPI.article_search(query=None, dates=None, options=None, results=10)

import datetime

articles_obama = nyt.article_search(
    query = "Obama", # Search for articles about Obama
    results = 30, # Return 30 articles
    # Search for articles in January and February 2019
    dates = {
        "begin": datetime.datetime(2019, 2, 27),
        "end": datetime.datetime(2019, 2, 28)
    }
)


articles = nyt.article_search(
    query = "gas", # Search for articles about Obama
    #results = 30, # Return 30 articles
    # Search for articles in January and February 2019
    dates = {
        "begin": datetime.datetime(2019, 1, 31),
        "end": datetime.datetime(2019, 2, 28)
    }
)
articles = nyt.article_search(
    query = "gas", # Search for articles about Obama
    results = 20, # Return 30 articles
    # Search for articles in January and February 2019
    dates = {
        "begin": datetime.datetime(2019, 1, 31),
        "end": datetime.datetime(2019, 2, 28)
    }
)
articles = nyt.article_search(
    query = "gas", # Search for articles about Obama
    results = 50, # Return 30 articles
    # Search for articles in January and February 2019
    dates = {
        "begin": datetime.datetime(2023, 2, 27),
        "end": datetime.datetime(2023, 2, 28)
    },
    options = {
        # Return articles from the following four sources
        "sources": [
            "New York Times",
            "AP",
            "Reuters",
            "International Herald Tribune"
        ]
    }
)
len(articles_obama)
for article in articles_obama:
    print(article["source"])
    print(article["pub_date"])
article.keys()
article = articles[0]
article.keys()

