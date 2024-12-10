
import requests
from bs4 import BeautifulSoup
from urllib.parse import urljoin

def get_news():
    base_url = 'https://www.ft.com/commodities?page=3'
    html_text = requests.get(base_url).text
    soup = BeautifulSoup(html_text, 'lxml')

    news_container = soup.find_all('article', class_='listing-item-blog')

    my_news = []
    prev_publish_date = None  # Store the previous news publish date
    for each_news in news_container:
        new_subpage = each_news.find('a', class_="post-title post-url")['href']
        subpage_html = requests.get(new_subpage).text
        subpage_soup = BeautifulSoup(subpage_html, 'lxml')

        news_title = subpage_soup.find('span', class_="post-title").text

        news_publish_date_element = each_news.find(
            'time', class_="post-published updated")
        news_publish_date = news_publish_date_element['datetime']

        if prev_publish_date and news_publish_date > prev_publish_date:
            # Skip this news as its publish date is not greater than the previous one
            continue

        div_element = subpage_soup.find(
            'div', class_='entry-content clearfix single-post-content')
        p_elements = div_element.find_all('p')

        paragraphs = []
        for p_element in p_elements:
            paragraph_text = p_element.get_text(strip=True).replace(
                '�', '')  # Remove unsupported character '�'
            paragraphs.append(paragraph_text)

        # Extract the image URL
        image_element = subpage_soup.find(
            'a', class_="post-thumbnail open-lightbox")
        if image_element:
            image_url = image_element['href']
            news_data = {
                'Title': news_title,
                'Publish Date': news_publish_date,
                'Paragraphs': paragraphs,
                'Image': image_url,
                'link': new_subpage
            }
        else:
            news_data = {
                'Title': news_title,
                'Publish Date': news_publish_date,
                'Paragraphs': paragraphs,
                'link': new_subpage
            }

        my_news.append(news_data)

    # Update the previous publish date for the next iteration
    if my_news:
        prev_publish_date = my_news[-1]['Publish Date'][0]


    return my_news


get_news()

# Trouver les articles
articles = soup.find_all('li', class_='o-header__drawer-menu-item')
# Extraire les informations des articles
extracted_articles = []
for article in articles:
    link = article.find('a', class_='o-header__drawer-menu-link')
    if link:
        title = link.text.strip()
        url = link['href']
        extracted_articles.append({'title': title, 'url': url})

# Afficher les résultats
for article in extracted_articles:
    print(f"Title: {article['title']}, URL: {article['url']}")

results = soup.find(id="ResultsContainer")
job_cards = results.find_all("div", class_="card-content")

# Extract category
category = soup.find('a', class_='o-teaser__tag').get_text(strip=True)

category_all = soup.find_all('a', class_='o-teaser__tag')#.get_text(strip=True)
print(type(category_all))
category_all[0]
category_all[1].get_text(strip=True)


heading_all = soup.find_all('a', class_='js-teaser-heading-link')#.get_text(strip=True)
heading_all[1].get_text(strip=True)

for heading in heading_all:
    print(heading.get_text(strip=True))

dates_all = soup.find_all('time', class_='o-date')#.get_text(strip=True)
dates_all[1].get_text(strip=True)

for date in dates_all:
    print(date.get_text(strip=True))

len(category_all)
len(heading_all)
len(dates_all)


# Extract article title
title = soup.find('a', class_='js-teaser-heading-link').get_text(strip=True)

# Extract URL (append domain if necessary)
url = soup.find('a', class_='js-teaser-heading-link')['href']

# Extract description/standfirst
description = soup.find('p', class_='o-teaser__standfirst').get_text(strip=True)

# Combine extracted data
data = {
    'Category': category,
    'Title': title,
    'URL': url,
    'Description': description
}





