# WordCloud
####################################################TASK-1################################################
pip install WordCloud

from wordcloud import WordCloud, STOPWORDS
import matplotlib.pyplot as plt

with open("C:\\Datasets\\shilpa\\user\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\oneplusseven.txt","r",encoding="utf-8") as sw:
    oneplus = sw.read()
    
other_stopwords_to_remove = ['\\n', 'n', '\\', '>', 'nLines', 'nI',"n'", "hi"]
STOPWORDS = STOPWORDS.union(set(other_stopwords_to_remove))
stopwords = set(STOPWORDS)
text = str(oneplus)
wordcloud = WordCloud(width = 1800, height = 1800, 
                background_color ='white', 
                max_words=200,
                stopwords = stopwords, 
                min_font_size = 10).generate(text)
plt.imshow(wordcloud, interpolation='bilinear')
plt.axis("off")
plt.show()


# Dependency Parse Tree and Named Entities - Visualizations
# Install the required libraries from Python console

# pip install spacy
# python -m spacy download en

# The visualizations will work in jupyter 
################
# import spacy
from spacy import displacy
import en_core_web_sm
nlp = en_core_web_sm.load()

doc = nlp('God helps those who help themselves.')
displacy.serve(doc, style='dep')

displacy.render(doc, style='dep')

text = 'Once upon a time there lived a programmer named Sharat Chandra. \
        He along with his close friend Bharani Kumar who was also known as Star \
        who is the founder of 360DigiTMG and AiSPRY, trained people on NLP.'
doc2 = nlp(text)
displacy.render(doc2, style='ent', jupyter=True)
################


# Building pipelines for NLP projects
from sklearn.pipeline import Pipeline
from sklearn.feature_extraction.text import TfidfTransformer
# from sklearn import tree
from sklearn.datasets import fetch_20newsgroups
from sklearn.feature_extraction.text import CountVectorizer
import pandas as pd

email_data = pd.read_csv("C:/Datasets_BA/Data Mining-Supervised/sms_raw_NB.csv", encoding = "ISO-8859-1")
data = email_data.text
data_categories = ['ham', 'spam']

categories = ['misc.forsale', 'sci.electronics', 'talk.religion.misc']
news_data = fetch_20newsgroups(subset='train', categories=categories, shuffle=True, random_state=42, download_if_missing=True)

text_classifier_pipeline = Pipeline([('vect', CountVectorizer()), ('tfidf', TfidfTransformer())])
text_classifier_pipeline.fit(news_data.data, news_data.target) 

res = pd.DataFrame(text_classifier_pipeline.fit_transform(news_data.data, news_data.target).todense()).head()


# Saving and Loading models

# pip install joblib
import pickle
from joblib import dump, load
from sklearn.feature_extraction.text import TfidfVectorizer

corpus = [
'Data Science is the most in demand job role in the current market',
'It is combination of both Maths and Business skills at a time',
'Natural Language Processing is a part of Data Science'
]
tfidf_model = TfidfVectorizer()
print(tfidf_model.fit_transform(corpus).todense())

dump(tfidf_model, 'tfidf_model.joblib')

import os
os.getcwd()

tfidf_model_loaded = load('tfidf_model.joblib')

print(tfidf_model_loaded.fit_transform(corpus).todense())

pickle.dump(tfidf_model, open("tfidf_model.pickle.dat", "wb")) #Save the model
loaded_model = pickle.load(open("tfidf_model.pickle.dat", "rb")) #To load the saved model

print(loaded_model.fit_transform(corpus).todense())



###############################################TASK-2################################################
import requests   # Importing requests to extract content from a url
from bs4 import BeautifulSoup as bs # Beautifulsoup is for web scrapping...used to scrap specific content 
import re 

import matplotlib.pyplot as plt
from wordcloud import WordCloud

# creating empty reviews list 
oneplus_reviews=[]


for i in range(1,21):
  ip=[]  
  url="https://www.amazon.in/OnePlus-Display-Storage-4000mAH-Battery/product-reviews/B07HGJK535/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber="+str(i)
  response = requests.get(url)
  soup = bs(response.content,"html.parser")# creating soup object to iterate over the extracted content 
  reviews = soup.find_all("span",attrs={"class","a-size-base review-text review-text-content"})# Extracting the content under specific tags  
  for i in range(len(reviews)):
    ip.append(reviews[i].text)  
 
  oneplus_reviews=oneplus_reviews+ip  # adding the reviews of one page to empty list which in future contains all the reviews

# writng reviews in a text file 
with open("oneplus.txt","w",encoding='utf8') as output:
    output.write(str(oneplus_reviews))
	

# Joinining all the reviews into single paragraph 
ip_rev_string = " ".join(oneplus_reviews)

import nltk
# from nltk.corpus import stopwords


# Removing unwanted symbols incase if exists
ip_rev_string = re.sub("[^A-Za-z" "]+"," ", ip_rev_string).lower()
ip_rev_string = re.sub("[0-9" "]+"," ", ip_rev_string)

# words that contained in iphone XR reviews
ip_reviews_words = ip_rev_string.split(" ")

#TFIDF
from sklearn.feature_extraction.text import TfidfVectorizer
vectorizer = TfidfVectorizer(ip_reviews_words, use_idf=True,ngram_range=(1, 3))
X = vectorizer.fit_transform(ip_reviews_words)

with open("C:\\Datasets_BA\\360DigiTMG\\DS_India\\360DigiTMG DS India Module wise PPTs\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\stop.txt","r") as sw:
    stop_words = sw.read()
    
stop_words = stop_words.split("\n")

stop_words.extend(["oneplus","mobile","time","android","phone","device","screen","battery","product","good","day","price"])

ip_reviews_words = [w for w in ip_reviews_words if not w in stop_words]

# Joinining all the reviews into single paragraph 
ip_rev_string = " ".join(ip_reviews_words)

# WordCloud can be performed on the string inputs.
# Corpus level word cloud

wordcloud_ip = WordCloud(
                      background_color='White',
                      width=1800,
                      height=1400
                     ).generate(ip_rev_string)

plt.imshow(wordcloud_ip)

# positive words # Choose the path for +ve words stored in system
with open("C:\\Datasets_BA\\360DigiTMG\\DS_India\\360DigiTMG DS India Module wise PPTs\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\positive-words.txt","r") as pos:
  poswords = pos.read().split("\n")

# Positive word cloud
# Choosing the only words which are present in positive words
ip_pos_in_pos = " ".join ([w for w in ip_reviews_words if w in poswords])

wordcloud_pos_in_pos = WordCloud(
                      background_color='White',
                      width=1800,
                      height=1400
                     ).generate(ip_pos_in_pos)
plt.figure(2)
plt.imshow(wordcloud_pos_in_pos)

# negative words Choose path for -ve words stored in system
with open("C:\\Datasets_BA\\360DigiTMG\\DS_India\\360DigiTMG DS India Module wise PPTs\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\negative-words.txt", "r") as neg:
  negwords = neg.read().split("\n")

# negative word cloud
# Choosing the only words which are present in negwords
ip_neg_in_neg = " ".join ([w for w in ip_reviews_words if w in negwords])

wordcloud_neg_in_neg = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(ip_neg_in_neg)
plt.figure(3)
plt.imshow(wordcloud_neg_in_neg)


# wordcloud with bigram
nltk.download('punkt')
from wordcloud import WordCloud, STOPWORDS

WNL = nltk.WordNetLemmatizer()

# Lowercase and tokenize
text = ip_rev_string.lower()

# Remove single quote early since it causes problems with the tokenizer.
text = text.replace("'", "")

tokens = nltk.word_tokenize(text)
text1 = nltk.Text(tokens)

# Remove extra chars and remove stop words.
text_content = [''.join(re.split("[ .,;:!?‘’``''@#$%^_&*()<>{}~\n\t\\\-]", word)) for word in text1]

# Create a set of stopwords
stopwords_wc = set(STOPWORDS)
customised_words = ['price', 'great'] # If you want to remove any particular word form text which does not contribute much in meaning

new_stopwords = stopwords_wc.union(customised_words)

# Remove stop words
text_content = [word for word in text_content if word not in new_stopwords]

# Take only non-empty entries
text_content = [s for s in text_content if len(s) != 0]

# Best to get the lemmas of each word to reduce the number of similar words
text_content = [WNL.lemmatize(t) for t in text_content]

nltk_tokens = nltk.word_tokenize(text)  
bigrams_list = list(nltk.bigrams(text_content))
print(bigrams_list)

dictionary2 = [' '.join(tup) for tup in bigrams_list]
print (dictionary2)

# Using count vectoriser to view the frequency of bigrams
from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(ngram_range=(2, 2))
bag_of_words = vectorizer.fit_transform(dictionary2)
vectorizer.vocabulary_

sum_words = bag_of_words.sum(axis=0)
words_freq = [(word, sum_words[0, idx]) for word, idx in vectorizer.vocabulary_.items()]
words_freq =sorted(words_freq, key = lambda x: x[1], reverse=True)
print(words_freq[:100])

# Generating wordcloud
words_dict = dict(words_freq)
WC_height = 1000
WC_width = 1500
WC_max_words = 200
wordCloud = WordCloud(max_words=WC_max_words, height=WC_height, width=WC_width, stopwords=new_stopwords)
wordCloud.generate_from_frequencies(words_dict)

plt.figure(4)
plt.title('Most frequently occurring bigrams connected by same colour and font size')
plt.imshow(wordCloud, interpolation='bilinear')
plt.axis("off")
plt.show()

##################################################TASK2-B###################################
# Text cleaning and Tokenization



#####################################################
import re

sentence5 = 'Sharat tweeted, "Witnessing 70th Republic Day of India from Rajpath, \
New Delhi. Mesmerizing performance by Indian Army! Awesome airshow! @india_official \
@indian_army #India #70thRepublic_Day. For more photos ping me sharat@photoking.com :)"'

re.sub(r'([^\s\w]|_)+', ' ', sentence5).split()


# Extracting n-grams
# n-grams can be extracted from 3 different techniques:
# listed below are:
# 1. Custom defined function
# 2. NLTK
# 3. TextBlob

# Extracting n-grams using customed defined function
import re
def n_gram_extractor(input_str, n):
    tokens = re.sub(r'([^\s\w]|_)+', ' ', input_str).split()
    for i in range(len(tokens)-n+1):
        print(tokens[i:i+n])

n_gram_extractor('The cute little boy is playing with the kitten.', 2)

n_gram_extractor('The cute little boy is playing with the kitten.', 3)


# Extracting n-grams with nltk
from nltk import ngrams
list(ngrams('The cute little boy is playing with the kitten.'.split(), 2))

list(ngrams('The cute little boy is playing with the kitten.'.split(), 3))


# Extracting n-grams using TextBlob
# TextBlob is a Python library for processing textual data.

# pip install textblob

from textblob import TextBlob
blob = TextBlob("The cute little boy is playing with the kitten.")

blob.ngrams(n=2)

blob.ngrams(n=3)


# Tokenizing texts with different packages: Keras, Textblob

sentence5 = 'Sharat tweeted, "Witnessing 70th Republic Day of India from Rajpath, New Delhi. Mesmerizing performance by Indian Army! Awesome airshow! @india_official @indian_army #India #70thRepublic_Day. For more photos ping me sharat@photoking.com :)"'

# Tolenization with Keras
from keras.preprocessing.text import text_to_word_sequence
text_to_word_sequence(sentence5)

# Tokenization with TextBlob
from textblob import TextBlob
blob = TextBlob(sentence5)
blob.words

# Tokenize sentences using other nltk tokenizers:
# 1. Tweet Tokenizer
# 2. MWE Tokenizer (Multi-Word Expression)
# 3. Regexp Tokenizer
# 4. Whitespace Tokenizer
# 5. Word Punct Tokenizer


# 1. Tweet tokenizer
from nltk.tokenize import TweetTokenizer
tweet_tokenizer = TweetTokenizer()
tweet_tokenizer.tokenize(sentence5)

# 2. MWE Tokenizer (Multi-Word Expression)
from nltk.tokenize import MWETokenizer
mwe_tokenizer = MWETokenizer([('Republic', 'Day')]) # Declaring set of words that are to be treated as one entity
mwe_tokenizer.add_mwe(('Indian', 'Army')) # Adding more words to the set

mwe_tokenizer.tokenize(sentence5.split()) #  Indian Army' should be treated as a single token. But here "Army!" is treated as a token. 

mwe_tokenizer.tokenize(sentence5.replace('!','').split()) # "Army!" will be treated as Army 


# 3. Regexp Tokenizer
from nltk.tokenize import RegexpTokenizer
reg_tokenizer = RegexpTokenizer('\w+|\$[\d\.]+|\S+')
reg_tokenizer.tokenize(sentence5)


# 4. Whitespace Tokenizer
from nltk.tokenize import WhitespaceTokenizer
wh_tokenizer = WhitespaceTokenizer()
wh_tokenizer.tokenize(sentence5)


# 5. WordPunct Tokenizer
from nltk.tokenize import WordPunctTokenizer
wp_tokenizer = WordPunctTokenizer()
wp_tokenizer.tokenize(sentence5)


# Regexp Stemmer
sentence6 = "I love playing Cricket. Cricket players practice hard."
from nltk.stem import RegexpStemmer
regex_stemmer = RegexpStemmer('ing$')

' '.join([regex_stemmer.stem(wd) for wd in sentence6.split()])

# Porter Stemmer
sentence7 = "Before eating, it would be nice to sanitize your hands with a sanitizer"
from nltk.stem.porter import PorterStemmer
ps_stemmer = PorterStemmer()
' '.join([ps_stemmer.stem(wd) for wd in sentence7.split()])



# Lemmatization
import nltk
from nltk.stem import WordNetLemmatizer
from nltk import word_tokenize

nltk.download('wordnet')
lemmatizer = WordNetLemmatizer()

sentence8 = "The codes executed today are far better than what we execute generally."

' '.join([lemmatizer.lemmatize(word) for word in word_tokenize(sentence8)])


# Singularize & Pluralize words
from textblob import TextBlob
sentence9 = TextBlob('She sells seashells on the seashore')
sentence9.words

sentence9.words[2].singularize()

sentence9.words[5].pluralize()


# Language Translation
# From Spanish to English

from textblob import TextBlob
en_blob = TextBlob(u'muy bien')
en_blob.translate(from_lang='es', to='en') 


# Custom Stop words removal
from nltk import word_tokenize
sentence9 = "She sells seashells on the seashore"
custom_stop_word_list = ['she', 'on', 'the', 'am', 'is', 'not']
' '.join([word for word in word_tokenize(sentence9) if word.lower() not in custom_stop_word_list])


# Extracting general features from raw texts

# Number of words
# Detect presence of wh words
# Polarity
# Subjectivity
# Language identification

import pandas as pd
df = pd.DataFrame([['The vaccine for covid-19 will be announced on 1st August.'],
                   ['Do you know how much expectation the world population is having from this research?'],
                   ['This risk of virus will end on 31st July.']])
df.columns = ['text']
df

# Number of words
from textblob import TextBlob
df['number_of_words'] = df['text'].apply(lambda x : len(TextBlob(str(x)).words))
df['number_of_words']

# Detect presence of wh words
wh_words = set(['why', 'who', 'which', 'what', 'where', 'when', 'how'])
df['is_wh_words_present'] = df['text'].apply(lambda x : True if len(set(TextBlob(str(x)).words).intersection(wh_words))>0 else False)
df['is_wh_words_present']


# Polarity
df['polarity'] = df['text'].apply(lambda x : TextBlob(str(x)).sentiment.polarity)
df['polarity']

# Subjectivity
df['subjectivity'] = df['text'].apply(lambda x : TextBlob(str(x)).sentiment.subjectivity)
df['subjectivity']

# Language of the sentence
df['language'] = df['text'].apply(lambda x : TextBlob(str(x)).detect_language())
df['language']


# Bag of Words
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
corpus = ['At least seven Indian pharma companies are working to develop a vaccine against coronavirus',
'the deadly virus that has already infected more than 14 million globally.',
'Bharat Biotech, Indian Immunologicals, are among the domestic pharma firms working on the coronavirus vaccines in India.'
]

bag_of_words_model = CountVectorizer()
print(bag_of_words_model.fit_transform(corpus).todense()) # bag of words

bag_of_word_df = pd.DataFrame(bag_of_words_model.fit_transform(corpus).todense())
bag_of_word_df.columns = sorted(bag_of_words_model.vocabulary_)
bag_of_word_df.head()

# Bag of word model for top 5 frequent terms
bag_of_words_model_small = CountVectorizer(max_features=5)
bag_of_word_df_small = pd.DataFrame(bag_of_words_model_small.fit_transform(corpus).todense())
bag_of_word_df_small.columns = sorted(bag_of_words_model_small.vocabulary_)
bag_of_word_df_small.head()

# TFIDF
from sklearn.feature_extraction.text import TfidfVectorizer

tfidf_model = TfidfVectorizer()
print(tfidf_model.fit_transform(corpus).todense())

tfidf_df = pd.DataFrame(tfidf_model.fit_transform(corpus).todense())
tfidf_df.columns = sorted(tfidf_model.vocabulary_)
tfidf_df.head()

# TFIDF for top 5 frequent terms
tfidf_model_small = TfidfVectorizer(max_features=5)
tfidf_df_small = pd.DataFrame(tfidf_model_small.fit_transform(corpus).todense())
tfidf_df_small.columns = sorted(tfidf_model_small.vocabulary_)
tfidf_df_small.head()


# Feature Engineering (Text Similarity)
from nltk import word_tokenize
from nltk.stem import WordNetLemmatizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
lemmatizer = WordNetLemmatizer()

pair1 = ["Do you have Covid-19","Your body temperature will tell you"]
pair2 = ["I travelled to Malaysia.", "Where did you travel?"]
pair3 = ["He is a programmer", "Is he not a programmer?"]

def extract_text_similarity_jaccard (text1, text2):
    words_text1 = [lemmatizer.lemmatize(word.lower()) for word in word_tokenize(text1)]
    words_text2 = [lemmatizer.lemmatize(word.lower()) for word in word_tokenize(text2)]
    nr = len(set(words_text1).intersection(set(words_text2)))
    dr = len(set(words_text1).union(set(words_text2)))
    jaccard_sim = nr/dr
    return jaccard_sim

extract_text_similarity_jaccard(pair1[0], pair1[1])
extract_text_similarity_jaccard(pair2[0], pair2[1])
extract_text_similarity_jaccard(pair3[0], pair3[1])

tfidf_model = TfidfVectorizer()

# Creating a corpus which will have texts of pair1, pair2 and pair 3 respectively
corpus = [pair1[0], pair1[1], pair2[0], pair2[1], pair3[0], pair3[1]]

tfidf_results = tfidf_model.fit_transform(corpus).todense()
# Note: Here tfidf_results will have tf-idf representation of 
# texts of pair1, pair2 and pair3 in the given order.

# tfidf_results[0], tfidf_results[1] represents pair1
# tfidf_results[2], tfidf_results[3] represents pair2
# tfidf_results[4], tfidf_results[5] represents pair3

#cosine similarity between texts of pair1
cosine_similarity(tfidf_results[0], tfidf_results[1])

#cosine similarity between texts of pair2
cosine_similarity(tfidf_results[2], tfidf_results[3])

#cosine similarity between texts of pair3
cosine_similarity(tfidf_results[4], tfidf_results[5])


##################################################Task-2################################################

import requests   # Importing requests to extract content from a url
from bs4 import BeautifulSoup as bs # Beautifulsoup is for web scrapping...used to scrap specific content 
import re 

import matplotlib.pyplot as plt
from wordcloud import WordCloud

# creating empty reviews list 
oneplus_reviews=[]


for i in range(1,21):
  ip=[]  
  url="https://www.amazon.in/OnePlus-Display-Storage-4000mAH-Battery/product-reviews/B07HGJK535/ref=cm_cr_getr_d_paging_btm_next_3?ie=UTF8&reviewerType=all_reviews&pageNumber="+str(i)
  response = requests.get(url)
  soup = bs(response.content,"html.parser")# creating soup object to iterate over the extracted content 
  reviews = soup.find_all("span",attrs={"class","a-size-base review-text review-text-content"})# Extracting the content under specific tags  
  for i in range(len(reviews)):
    ip.append(reviews[i].text)  
 
  oneplus_reviews=oneplus_reviews+ip  # adding the reviews of one page to empty list which in future contains all the reviews

# writng reviews in a text file 
with open("oneplus.txt","w",encoding='utf8') as output:
    output.write(str(oneplus_reviews))
	

# Joinining all the reviews into single paragraph 
ip_rev_string = " ".join(oneplus_reviews)

import nltk
# from nltk.corpus import stopwords


# Removing unwanted symbols incase if exists
ip_rev_string = re.sub("[^A-Za-z" "]+"," ", ip_rev_string).lower()
ip_rev_string = re.sub("[0-9" "]+"," ", ip_rev_string)

# words that contained in iphone XR reviews
ip_reviews_words = ip_rev_string.split(" ")

#TFIDF
from sklearn.feature_extraction.text import TfidfVectorizer
vectorizer = TfidfVectorizer(ip_reviews_words, use_idf=True,ngram_range=(1, 3))
X = vectorizer.fit_transform(ip_reviews_words)

with open("C:\\Datasets_BA\\360DigiTMG\\DS_India\\360DigiTMG DS India Module wise PPTs\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\stop.txt","r") as sw:
    stop_words = sw.read()
    
stop_words = stop_words.split("\n")

stop_words.extend(["oneplus","mobile","time","android","phone","device","screen","battery","product","good","day","price"])

ip_reviews_words = [w for w in ip_reviews_words if not w in stop_words]

# Joinining all the reviews into single paragraph 
ip_rev_string = " ".join(ip_reviews_words)

# WordCloud can be performed on the string inputs.
# Corpus level word cloud

wordcloud_ip = WordCloud(
                      background_color='White',
                      width=1800,
                      height=1400
                     ).generate(ip_rev_string)

plt.imshow(wordcloud_ip)

# positive words # Choose the path for +ve words stored in system
with open("C:\\Datasets_BA\\360DigiTMG\\DS_India\\360DigiTMG DS India Module wise PPTs\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\positive-words.txt","r") as pos:
  poswords = pos.read().split("\n")

# Positive word cloud
# Choosing the only words which are present in positive words
ip_pos_in_pos = " ".join ([w for w in ip_reviews_words if w in poswords])

wordcloud_pos_in_pos = WordCloud(
                      background_color='White',
                      width=1800,
                      height=1400
                     ).generate(ip_pos_in_pos)
plt.figure(2)
plt.imshow(wordcloud_pos_in_pos)

# negative words Choose path for -ve words stored in system
with open("C:\\Datasets_BA\\360DigiTMG\\DS_India\\360DigiTMG DS India Module wise PPTs\\Module 18 Text Mining_Natural Language Processing (NLP)\\Data\\negative-words.txt", "r") as neg:
  negwords = neg.read().split("\n")

# negative word cloud
# Choosing the only words which are present in negwords
ip_neg_in_neg = " ".join ([w for w in ip_reviews_words if w in negwords])

wordcloud_neg_in_neg = WordCloud(
                      background_color='black',
                      width=1800,
                      height=1400
                     ).generate(ip_neg_in_neg)
plt.figure(3)
plt.imshow(wordcloud_neg_in_neg)


# wordcloud with bigram
nltk.download('punkt')
from wordcloud import WordCloud, STOPWORDS

WNL = nltk.WordNetLemmatizer()

# Lowercase and tokenize
text = ip_rev_string.lower()

# Remove single quote early since it causes problems with the tokenizer.
text = text.replace("'", "")

tokens = nltk.word_tokenize(text)
text1 = nltk.Text(tokens)

# Remove extra chars and remove stop words.
text_content = [''.join(re.split("[ .,;:!?‘’``''@#$%^_&*()<>{}~\n\t\\\-]", word)) for word in text1]

# Create a set of stopwords
stopwords_wc = set(STOPWORDS)
customised_words = ['price', 'great'] # If you want to remove any particular word form text which does not contribute much in meaning

new_stopwords = stopwords_wc.union(customised_words)

# Remove stop words
text_content = [word for word in text_content if word not in new_stopwords]

# Take only non-empty entries
text_content = [s for s in text_content if len(s) != 0]

# Best to get the lemmas of each word to reduce the number of similar words
text_content = [WNL.lemmatize(t) for t in text_content]

nltk_tokens = nltk.word_tokenize(text)  
bigrams_list = list(nltk.bigrams(text_content))
print(bigrams_list)

dictionary2 = [' '.join(tup) for tup in bigrams_list]
print (dictionary2)

# Using count vectoriser to view the frequency of bigrams
from sklearn.feature_extraction.text import CountVectorizer
vectorizer = CountVectorizer(ngram_range=(2, 2))
bag_of_words = vectorizer.fit_transform(dictionary2)
vectorizer.vocabulary_

sum_words = bag_of_words.sum(axis=0)
words_freq = [(word, sum_words[0, idx]) for word, idx in vectorizer.vocabulary_.items()]
words_freq =sorted(words_freq, key = lambda x: x[1], reverse=True)
print(words_freq[:100])

# Generating wordcloud
words_dict = dict(words_freq)
WC_height = 1000
WC_width = 1500
WC_max_words = 200
wordCloud = WordCloud(max_words=WC_max_words, height=WC_height, width=WC_width, stopwords=new_stopwords)
wordCloud.generate_from_frequencies(words_dict)

plt.figure(4)
plt.title('Most frequently occurring bigrams connected by same colour and font size')
plt.imshow(wordCloud, interpolation='bilinear')
plt.axis("off")
plt.show()

##########



