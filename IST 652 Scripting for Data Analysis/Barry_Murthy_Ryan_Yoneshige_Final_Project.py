# Name of file: Barry_Murthy_Ryan_Yoneshige_Final_Project.py
# Created by: Barry Murthy and Ryan Yoneshige
# Course: IST652
#
# Importing the libraries and modules
import os
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import itertools
import collections
from datetime import datetime as dt
import re
from bs4 import BeautifulSoup as bs
import lxml
import requests
import nltk
from nltk.corpus import stopwords
#
# Reading in the Olympics data from Kaggle into a data frame
df = pd.read_csv('C:\\Users\\ryany\\OneDrive\\Desktop\\Olympics_Tokyo_tweets.csv')
print(df.head(5))
#
# Changing columns that should hold int values to int type
cols_to_int = ['retweet_count', 'favorite_count', 'user_followers', 'user_friends']
df.dropna(inplace = True)
for col in cols_to_int:
    df[col] = df[col].astype(float).round().astype(int)
#
# Changing the date columns to date type
df['user_created_at'] = [dt.strptime(date, '%Y-%m-%d %H:%M:%S') for date in df.user_created_at]
df['date'] = [dt.strptime(date, '%Y-%m-%d %H:%M:%S') for date in df.date]
#
# Reading in the medals data
medals_tables = pd.read_html('https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/medal-standings.htm')
medals = medals_tables[0]
#
# Renaming the columns in the medals table
medals.rename(columns={'Unnamed: 2': 'gold', 'Unnamed: 3': 'silver', 'Unnamed: 4': 'bronze'}, inplace=True)
#
# Scraping the wikipedia page for the names of the summer olympic sports
events_url = 'https://en.wikipedia.org/wiki/Summer_Olympic_Games'
response = requests.get(events_url)
soup = bs(response.content, 'lxml')
info_box = soup.find(class_ = 'infobox hlist nowraplinks')
box_li = info_box.find_all('li')
#
# Storing the sport names into a list
sports = []
for li in box_li[33:]:
    sports.append(li.find('a').get_text())
print(sports)
#
# Storing the tweet text into a list, removing the urls, changing the text to lowercase, and splitting the text
tweets_text = [tweet for tweet in df.text]
def remove_url(txt):
    return " ".join(re.sub("([^0-9A-Za-z \t])|(\w+:\/\/\S+)", "", txt).split())
tweets_text_no_url = [remove_url(tweet) for tweet in tweets_text]
words_in_tweets = [tweet.lower().split() for tweet in tweets_text_no_url]
#
# Removing stop words and flattening the list of tweet text words
stop_words = set(stopwords.words('english'))
tweet_words_nsw = [[word for word in tweet_words if not word in stop_words] for tweet_words in words_in_tweets]
#
# Creating a counter for the tweet words
all_words_nsw = list(itertools.chain(*tweet_words_nsw))
word_counts_nsw = collections.Counter(all_words_nsw)
#
# Converting the sport words to lowercase, splitting, and flattening the list
sport_words = [sport.lower().split() for sport in sports]
sport_words_flat = list(itertools.chain(*sport_words))
#
# Creating a list of words that appear in the sport words as well as the tweets
int_words = list(set(sport_words_flat).intersection(all_words_nsw))
#
# Creating a sorted (decreasing) dictionary of the counts of sport words in the tweets
word_counts_dict = dict(word_counts_nsw)
sport_words_counts = {item for item in word_counts_dict.items() if item[0] in int_words}
sport_words_counts = dict(sport_words_counts)
sorted_sports_dict = {word: count for word, count in sorted(sport_words_counts.items(), key = lambda item: item[1], reverse = True)}
print(sorted_sports_dict)
#
# Creating a plot of the sorted sports dictionary
plt.figure(figsize = (10,10), dpi = 100)
plt.barh(range(len(sorted_sports_dict)), list(sorted_sports_dict.values()), align = 'center')
plt.yticks(range(len(sorted_sports_dict)), list(sorted_sports_dict.keys()))
plt.title('Word Frequencies in Tweets by Sport')
plt.xlabel('Word Frequency')
plt.ylabel('Word in Tweet')
#plt.savefig('olympic_sport_words.png')
plt.show()
#
# Creating the followers bins
followers_bins = [0, 1000000, 10000000, 100000000]
bin_labels = [1,2,3]
df['followers_bin'] = pd.cut(df['user_followers'], bins = followers_bins, labels = bin_labels)
print(df.groupby('followers_bin').count()['id'])
#
# Creating a column for the length of a tweet
df['tweet_length'] = [len(tweet) for tweet in df.text]
#
# Inspecting the means of the numeric columns grouped by followers bin
print(df.groupby('followers_bin').mean())
#
# Splitting the data frame by followers bin column
fol1_df = df[df['followers_bin'] == 1]
fol2_df = df[df['followers_bin'] == 2]
fol3_df = df[df['followers_bin'] == 3]
#
# Looking at the differences in words used between the three followers bins
tweets_text_1 = [tweet for tweet in fol1_df.text]
tweets_text_no_url_1 = [remove_url(tweet) for tweet in tweets_text_1]
words_in_tweets_1 = [tweet.lower().split() for tweet in tweets_text_no_url_1]
tweet_words_nsw_1 = [[word for word in tweet_words_1 if not word in stop_words] for tweet_words_1 in words_in_tweets_1]
all_words_nsw_1 = list(itertools.chain(*tweet_words_nsw_1))
word_counts_nsw_1 = collections.Counter(all_words_nsw_1)
word_counts_dict_1 = dict(word_counts_nsw_1)
sorted_words_dict_1 = {word: count for word, count in sorted(word_counts_dict_1.items(), key = lambda item: item[1], reverse = True)}
fol1_top30 = dict(list(sorted_words_dict_1.items())[:30])
#
# Creating a bar graph for the fol1 top 30 words
plt.figure(figsize = (10,10), dpi = 100)
plt.barh(range(len(fol1_top30)), list(fol1_top30.values()), align = 'center')
plt.yticks(range(len(fol1_top30)), list(fol1_top30.keys()))
plt.title('Word Frequencies in Tweets from Users in Followers Bin 1')
plt.xlabel('Word Frequency')
plt.ylabel('Word in Tweet')
#plt.savefig('fol1_words.png')
plt.show()
#
# Looking at the followers bin 2 tweets
tweets_text_2 = [tweet for tweet in fol2_df.text]
tweets_text_no_url_2 = [remove_url(tweet) for tweet in tweets_text_2]
words_in_tweets_2 = [tweet.lower().split() for tweet in tweets_text_no_url_2]
tweet_words_nsw_2 = [[word for word in tweet_words_2 if not word in stop_words] for tweet_words_2 in words_in_tweets_2]
all_words_nsw_2 = list(itertools.chain(*tweet_words_nsw_2))
word_counts_nsw_2 = collections.Counter(all_words_nsw_2)
word_counts_dict_2 = dict(word_counts_nsw_2)
sorted_words_dict_2 = {word: count for word, count in sorted(word_counts_dict_2.items(), key = lambda item: item[1], reverse = True)}
fol2_top30 = dict(list(sorted_words_dict_2.items())[:30])
#
# Creating a bar graph for the fol2 top 30 words
plt.figure(figsize = (10,10), dpi = 100)
plt.barh(range(len(fol2_top30)), list(fol2_top30.values()), align = 'center')
plt.yticks(range(len(fol2_top30)), list(fol2_top30.keys()))
plt.title('Word Frequencies in Tweets from Users in Followers Bin 2')
plt.xlabel('Word Frequency')
plt.ylabel('Word in Tweet')
#plt.savefig('fol2_words.png')
plt.show()
#
# Looking at the followers bin 3 tweets
tweets_text_3 = [tweet for tweet in fol3_df.text]
tweets_text_no_url_3 = [remove_url(tweet) for tweet in tweets_text_3]
words_in_tweets_3 = [tweet.lower().split() for tweet in tweets_text_no_url_3]
tweet_words_nsw_3 = [[word for word in tweet_words_3 if not word in stop_words] for tweet_words_3 in words_in_tweets_3]
all_words_nsw_3 = list(itertools.chain(*tweet_words_nsw_3))
word_counts_nsw_3 = collections.Counter(all_words_nsw_3)
word_counts_dict_3 = dict(word_counts_nsw_3)
sorted_words_dict_3 = {word: count for word, count in sorted(word_counts_dict_3.items(), key = lambda item: item[1], reverse = True)}
fol3_top30 = dict(list(sorted_words_dict_3.items())[:30])
#
# Creating a bar graph for the fol3 top 30 words
plt.figure(figsize = (10,10), dpi = 100)
plt.barh(range(len(fol3_top30)), list(fol3_top30.values()), align = 'center')
plt.yticks(range(len(fol3_top30)), list(fol3_top30.keys()))
plt.title('Word Frequencies in Tweets from Users in Followers Bin 3')
plt.xlabel('Word Frequency')
plt.ylabel('Word in Tweet')
#plt.savefig('fol3_words.png')
plt.show()
#
# Splitting the data frame into tweets including an "@" and those that do not
hashtag = df[df['text'].str.contains("@")]
nohashtag = df[~df['text'].str.contains("@")]
#
# Looking at the differences in words used between the tweets including an @ and those that do not
hash_text = [tweet for tweet in hashtag.text]
hash_text_no_url = [remove_url(tweet) for tweet in hash_text]
words_in_hashtweets = [tweet.lower().split() for tweet in hash_text_no_url]
hashtweet_words_nsw = [[word for word in hashtweet_words if not word in stop_words] for hashtweet_words in words_in_hashtweets]
all_hashwords_nsw = list(itertools.chain(*hashtweet_words_nsw))
hashword_counts_nsw = collections.Counter(all_hashwords_nsw)
hashword_counts_dict = dict(hashword_counts_nsw)
sorted_hashwords_dict = {word: count for word, count in sorted(hashword_counts_dict.items(), key = lambda item: item[1], reverse = True)}
hash_top30 = dict(list(sorted_hashwords_dict.items())[:30])
print(hash_top30)
#
nohash_text = [tweet for tweet in nohashtag.text]
nohash_text_no_url = [remove_url(tweet) for tweet in nohash_text]
words_in_nohashtweets = [tweet.lower().split() for tweet in nohash_text_no_url]
nohashtweet_words_nsw = [[word for word in nohashtweet_words if not word in stop_words] for nohashtweet_words in words_in_nohashtweets]
all_nohashwords_nsw = list(itertools.chain(*nohashtweet_words_nsw))
nohashword_counts_nsw = collections.Counter(all_nohashwords_nsw)
nohashword_counts_dict = dict(nohashword_counts_nsw)
sorted_nohashwords_dict = {word: count for word, count in sorted(nohashword_counts_dict.items(), key = lambda item: item[1], reverse = True)}
nohash_top30 = dict(list(sorted_nohashwords_dict.items())[:30])
print(nohash_top30)
#
# Reading in the data about which countries participated in this year's olympic games
country_tables = pd.read_html('https://www.whereig.com/olympics/summer-olympics-participating-countries.html')
countries = country_tables[2]
#
# Creating a data frame that combines the countries and numbers of participants into single columns
countries_list = pd.concat([countries['Country (A-G)'], countries['Country (G-N)'], countries['Country (N-Z)']], ignore_index = True).tolist()
participants_list = pd.concat([countries['No. of Athletes'], countries['No. of Athletes.1'], countries['No. of Athletes.2']], ignore_index = True).tolist()
countries_df = pd.DataFrame(list(zip(countries_list, participants_list)), columns =['country', 'participants'])
#
# Cleaning up the countries dataframe
countries_df['country'] = countries_df['country'].str.lower()
countries_df.dropna(axis = 0, how = 'all', inplace = True)
countries_df.reset_index(drop = True, inplace = True)
#
# Reading in the data to create a dataframe consisting of the abbreviations for countries
abbr_tables = pd.read_html('https://www.olympiandatabase.com/index.php?id=1670&L=1')
abbr = abbr_tables[0]
abbr.dropna(axis = 0, how = 'all', inplace = True)
abbr.drop(labels = [0, 1, 3, 125], axis = 0, inplace = True)
abbr.dropna(axis = 1, how = 'all', inplace = True)
abbr.reset_index(drop = True, inplace = True)
#
# Creating a dataframe that combines the countries and abbreviations into single columns
abbr_list = pd.concat([abbr[0], abbr[3]], ignore_index = True).tolist()
names_list = pd.concat([abbr[1], abbr[4]], ignore_index = True).tolist()
abbr_df = pd.DataFrame(list(zip(abbr_list, names_list)), columns =['abbreviation', 'country_name'])
abbr_df.drop_duplicates(inplace = True, ignore_index = True)
#
# Cleaning up the abbreviations dataframe
abbr_df['abbreviation'] = abbr_df['abbreviation'].str.lower()
abbr_df['country_name'] = abbr_df['country_name'].str.lower()
#
# Joining the participating countries dataframe with the abbreviations dataframe
countries_abbr_df = countries_df.join(abbr_df.set_index('country_name'), on = 'country')
#
# Checking for NA values in the abbreviation column
na_abbr = countries_abbr_df.loc[countries_abbr_df['abbreviation'].isna()]
print(na_abbr)
#
# Manually entering in the abbreviations for the countries whose abbreviations did not transfer
countries_abbr_df.loc[countries_abbr_df.country == 'antigua and barbuda', 'abbreviation'] = 'ant'
countries_abbr_df.loc[countries_abbr_df.country == 'bosnia and herzegovina', 'abbreviation'] = 'bih'
countries_abbr_df.loc[countries_abbr_df.country == 'eswatini', 'abbreviation'] = 'swz'
countries_abbr_df.loc[countries_abbr_df.country == 'federated states of micronesia', 'abbreviation'] = 'fsm'
countries_abbr_df.loc[countries_abbr_df.country == 'the gambia', 'abbreviation'] = 'gam'
countries_abbr_df.loc[countries_abbr_df.country == 'guinea-bissau', 'abbreviation'] = 'gbs'
countries_abbr_df.loc[countries_abbr_df.country == 'ivory coast', 'abbreviation'] = 'civ'
countries_abbr_df.loc[countries_abbr_df.country == 'refugee olympic team', 'abbreviation'] = 'eor'
countries_abbr_df.loc[countries_abbr_df.country == 'republic of the congo', 'abbreviation'] = 'drc'
countries_abbr_df.loc[countries_abbr_df.country == 'saint vincent and the grenadines', 'abbreviation'] = 'vin'
countries_abbr_df.loc[countries_abbr_df.country == 'são tomé and príncipe', 'abbreviation'] = 'stp'
countries_abbr_df.loc[countries_abbr_df.country == 'chinese taipei', 'abbreviation'] = 'tpe'
countries_abbr_df.loc[countries_abbr_df.country == 'united states', 'abbreviation'] = 'usa'
#
# Preparing the medals dataframe to be joined with the countries abbreviations dataframe
medals['NOCCode'] = medals['NOCCode'].str.lower()
medals['Team/NOC'] = medals['Team/NOC'].str.lower()
#
# Joining the medals df with the countries abbreviations df on abbreviation
countries_perf_df = countries_abbr_df.join(medals.set_index('NOCCode'), on = 'abbreviation')
#
# Cleaning the countries performance df
countries_perf_df['Rank'] = countries_perf_df['Rank'].fillna(94)
countries_perf_df['RankbyTotal'] = countries_perf_df['RankbyTotal'].fillna(94)
countries_perf_df['Team/NOC'] = countries_perf_df['Team/NOC'].fillna('')
countries_perf_df['gold'] = countries_perf_df['gold'].fillna(0)
countries_perf_df['silver'] = countries_perf_df['silver'].fillna(0)
countries_perf_df['bronze'] = countries_perf_df['bronze'].fillna(0)
countries_perf_df['Total'] = countries_perf_df['Total'].fillna(0)
#
# Creating a list of words in the tweets that relate to specific countries
country_words = []
for word in all_words_nsw:
    if word in countries_perf_df['country'].tolist():
        country_words.append(word)
    elif word in countries_perf_df['abbreviation'].tolist():
        country_words.append(word)
    elif word in countries_perf_df['Team/NOC'].tolist():
        country_words.append(word)
    else:
        continue
#        
# Counting the frequencies of the country words
country_words_counts = collections.Counter(country_words)
#
# Converting the list of tuples to a dictionary to prepare fot plotting
country_top30 = dict(country_words_counts.most_common(30))
#
# Creating a bar graph for the top 30 country words
plt.figure(figsize = (10,10), dpi = 100)
plt.barh(range(len(country_top30)), list(country_top30.values()), align = 'center')
plt.yticks(range(len(country_top30)), list(country_top30.keys()))
plt.title('Word Frequencies in Tweets Referencing Countries')
plt.xlabel('Word Frequency')
plt.ylabel('Word in Tweet')
#plt.savefig('country_words_top30.png')
plt.show()
#
# Creating a dictionary of the country words and their counts
country_words_dict = dict(country_words_counts)
#
# Creating a medals proportion column in the countries performance df
countries_perf_df['medals_prop'] = (countries_perf_df['Total']/sum(countries_perf_df['Total'])).round(4)
#
# Sorting the countries perf df by rank and country
sorted_countries_perf = countries_perf_df.sort_values(['Rank', 'country'])
sorted_countries_perf.reset_index(drop = True, inplace = True)
#
# Creating a dictionary from the coutry words and calculating proportions of each country word used
country_prop_dict = {word: round(count/total, 4) for total in (sum(country_words_dict.values()),) for word, count in country_words_dict.items()}
country_prop_dict = {word: count for word, count in sorted(country_prop_dict.items(), key = lambda item: item[1], reverse = True)}
#
# Inspecting India's rank in the olympics
print(sorted_countries_perf.loc[sorted_countries_perf['country'] == 'india'])