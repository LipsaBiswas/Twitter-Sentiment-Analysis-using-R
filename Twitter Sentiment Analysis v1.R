#ALT + O to collapse  and ALT+SHIFT+O to expand this document

#Objective  ####
'Objective - we will retrieve a set of tweets for a  search text and do the sentiment analysis of 
the tweets . we will find the ratio of positive , negative, neutral polarity tweets in the dataset.

You may ask what is polarity ? In short, Polarity is the emotions expressed in a sentence.
Initially,I was baffled with the realization that whatever I am calling a negative or most negative tweet is not a negative tweet
but its a tweet with high negative polarity  .Its very much possible , and happens many a times that two different persons
talk about the same thing with different tones , words , and emotions. One may express with lots of positive words and happy
expressions and another with negative words, sentiment and depressing expressions. so , one person''s talk has "positive polarity"
and other person''s "negative polarity"..The syuzhet packages get_nrc_sentiment implements Saif Mohammad''s NRC Emotion lexicon.
According to Mohammad, "the NRC emotion lexicon is a list of words and their associations with eight emotions 
(anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive).
There are other challenges in sentiment analysis , like sarcasm. " Oh she is so great , loving  , helpful lady !!" .
its a real praise or sarcasm ? So, yes , sentiment analysis does not seem to be full proof yet but several research going on to
overcome these hurdles.

"

'
#Methodology ####
'
1. Retrieve tweets using twitter API
2. Cleanse tweets to remove URLs (,stop words, mention tags etc)
3. Retrieve sentiment score for each tweet using functions of syuzhet package
4. Combine tweets and their individual sentiment scores in one data frame
5. Calculate the overall polarity of the tweet , if its positive/negative/neutral
6. Create a pie chart to show the percentage ratio of positive, negative, neutral polarity tweets
'
#Create an app in twitter to access twitter APIs#####
# Visit https://developer.twitter.com/en/apps
# You need to have a developer account in twitter to access twitter APIs . It may take some time to get twitter approval
# for a developer account.Once your developer account is approved and created in twitter ,
# take note of the following values from the Twitter app page: "API key", "API secret", "Access token", and "Access token secret".


#Install and Load packages ####
install.packages("NLP")
install.packages("tm")
install.packages("twitterR")
install.packages("tidyverse")
install.packages("syuzhet")

library(NLP) #Natural Language Processing
library(tm) # text mining
library(tidyverse) # contains ggplot2 , dplyr , purrr, stringr,readr etc
library(twitteR) # helps access Twitter API
library(syuzhet) # Sentiment analysis

#Twitter Authentication####
#Note - you have to provide your developer auth details here. In case you dont have a dev account yet,
#you can search internet and use available twitter datasets
consumer_key = 'XXXXXXXXXXXXXXXX'
consumer_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
access_token = 'XXXXXXXXXXXXXXXXXXXX'
access_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXX'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#search text in twitter####
search_text<-"sushmaswaraj"
#Note - explore searchTwitter function to know what arguments it can take
#tweets = twitteR::searchTwitter(search_text, n = 1e4, since = '2019-08-06', retryOnRateLimit = 1e3)
tweets = twitteR::searchTwitter(search_text, n = 200) #returns last 200 tweets for the searched_text
#View(tweets)
class(tweets)#list
tweets_count <- length(tweets)
tweets_count
#As the name suggests ,twListToDF function converts twitteR lists to data.frames
tweets_df <- twListToDF(tweets) 
#View(tweets_df)

#Tweets cleaning #####
#Lets remove the URLs mentioned in the tweets as part of data cleansing
#Note - you can cleanse the tweets as per your requirements . can remove @ / mention tags ,stop words, URLs etc
tweets_df_clean <- gsub("http.*","",tweets_df$text)
tweets_df_clean <- gsub("https.*","",tweets_df$text)
tweets_df_clean

#Get Emotions and Valence from NRC Dictionary ####
#Calls the NRC sentiment dictionary to calculate the presence of eight different emotions and their corresponding valence in a text file.
#syuzhet package"
#?get_nrc_sentiment
#Convert the tweets data frame to character vecor as get_nrc_sentiment() accepts character vector
tweets_char_vector <- as.vector(tweets_df_clean)
#View(tweets_char_vector)

sentiments_df <- get_nrc_sentiment(tweets_char_vector)
head(sentiments_df)
# anger anticipation disgust fear joy sadness surprise trust negative positive
# 1     0            1       0    1   1       0        0     1        0        3
# 2     0            0       0    0   0       0        0     0        0        0
# 3     0            0       0    0   0       0        0     0        0        0
# 4     0            0       0    0   0       0        0     0        0        0
# 5     0            0       0    0   0       0        0     0        0        0
# 6     0            0       0    0   0       0        0     1        0        0
#For each tweets get_nrc_sentiment() is returning scores for eight emotions(angry...trust) and two sentiments(positive, negative)
#Note - we can do some interesting analysis with eight emotion scores as well
# create a new data frame by combining column of cleansed tweets dataframe(tweets_df_clean) 
#and sentiment score dataframe(sentiments_df). So , in one df we will have tweets and their respective sentiment scores(for each emotion and sentiment)
sentiments_tweets_df <- cbind(tweets_df_clean, sentiments_df) 
head(sentiments_tweets_df,2)
#View(head(sentiments_tweets_df))

#Get sentiment score of each tweet
#?get_sentiment -"Return value is a numeric vector of sentiment values, one value for each input sentence."
sentiment_score <- get_sentiment(tweets_char_vector)
sentiment_score
max(sentiment_score)
#check tweet with highest positive polarity , i.e. emotions expressed is highly positive
most_positive_polarity_tweet <- tweets_char_vector[sentiment_score == max(sentiment_score)]
most_positive_polarity_tweet
#check tweet with highest negative polarity , i.e. emotions expressed is highly negative
most_negative_polarity_tweet <- tweets_char_vector[sentiment_score == min(sentiment_score)]
most_negative_polarity_tweet

#Find the list of positive , negative , neutral (polarity) tweets####

#Find positive tweets
positive_tweets <- tweets_char_vector[sentiment_score > 0]
#head(positive_tweets)
#Find negative tweets
negative_tweets <- tweets_char_vector[sentiment_score < 0]
#View(negative_tweets)
#Find Neutral tweets
neutral_tweets <- tweets_char_vector[sentiment_score == 0]
#View(neutral_tweets)

#we will create a data frame of percentage of positive, nagtive, neutral tweets and feed the data to draw a pie chart 
#count of tweets
l_positive_tweets<-length(positive_tweets) 
l_negative_tweets<-length(negative_tweets)
l_neutral_tweets<-length(neutral_tweets)

total<-l_positive_tweets+l_negative_tweets+l_neutral_tweets
pct_positive_tweets <- (l_positive_tweets/total)*100
pct_negative_tweets <- (l_negative_tweets/total)*100
pct_neutral_tweets <- (l_neutral_tweets/total)*100

df <- data.frame("Polarity"=c("Negative","Positive","Neutral") , "polarity_pct"=c(pct_negative_tweets,pct_positive_tweets , pct_neutral_tweets))
df
# Polarity polarity_pct
# 1 Negative           15
# 2 Positive           15
# 3  Neutral           70

#Visual analysis of the insight derived####

#Create a pie chart to show the ratio of positive , negative, neutral tweets in the retrieved tweitter dataset for
# the searched text
#Note - you can take the dataframe (df ) and do any kind of (visual) analysis within R or ouside.
#e.g. , can feed the data to tableau or return to a calling web application . I have chosen ggplot R package
#to draw a pie chart. Check online resources to make cool visualizations
# Create a basic bar
polarity_pie<-NULL
polarity_pie = ggplot(df, aes(x="", y=polarity_pct, fill=Polarity)) + geom_bar(stat="identity", width=1)
polarity_pie
# Convert the basic bar to pie (polar coordinates) and add labels
polarity_pie = polarity_pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(polarity_pct,1), "%")), position = position_stack(vjust = 0.5))
polarity_pie
# Add custom color scale . check http://colorbrewer2.org for more color suggestions
polarity_pie = polarity_pie + scale_fill_manual(values=c("#fbb4ae","#b3cde3","#ccebc5"))

polarity_pie
# Remove labels and add title
polarity_pie = polarity_pie + labs(x = NULL, y = NULL, fill = NULL, title = paste0("Twitter sentiment analysis for text- '",search_text,"'"))
polarity_pie
# Tidy up the theme , e.g. title text color , remove axis lines and axis texts etc
polarity_pie = polarity_pie + theme_classic() + theme(axis.line = element_blank(),
                                                      axis.text = element_blank(),
                                                      axis.ticks = element_blank(),
                                                      plot.title = element_text(hjust = 0.5, color = "#666666"))
polarity_pie

#What next ???####
'Take it as baseline code for text mining , sentiment analysis. you can develop it further for creating
more useful and interesting use cases .e.g. Create an UI to accept dynamic search text and return analysis
check this link:https://twitter-sentiment-csv.herokuapp.com/ 
Create word cloud to show high frequency words in a document or any text data.
Why just twitter , pick any unstructured text form , do text mining , create interesting visuals.
Read some more about sentiment analysis here
https://hackernoon.com/text-processing-and-sentiment-analysis-of-twitter-data-22ff5e51e14c
--
Lipsa Biswas (https://www.linkedin.com/in/lipsa-biswas/)
'