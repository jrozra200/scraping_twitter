SCRAPING TWITTER

These scripts can be run via the twitter_analysis.R script or individually. 

I still need to tweak the twitter_analyis.R script to allow for the proper inputs (what do you want to search on, what kind of output do you want, etc.).

scraping_twitter.R grabs tweets from twitter and then pairs them down to what is important. By default, this searches for "comcast email" and does some cleaning based on the results I've seen there.

tweet_clean.R prepares the tweets for analysis by removing unneccessary words, spaces, punct, etc.

classify_polarity.R gives us the positive/negative on each tweet.

classify_emotion.R gives us the emotion on each tweet.

create_wordcloud.R creates a wordcloud for the tweets. I'd like to tweak this in the future to show positive and negative tweets differently.

create_piechart.R creates a piechart of the overall sentiment of the tweets. 