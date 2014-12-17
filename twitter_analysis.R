twitter_analysis <- function() {
        paredTweetList <- scraping_twitter()
        
        cleanTweets <- tweet_clean(paredTweetList)
        
        emotion <- classify_emotion(cleanTweets)
        polarity <- classify_polarity(cleanTweets)
        
        totalFinalOutput <- cbind(paredTweetList, emotion, polarity)
        colnames(totalFinalOutput) <- c("tweet", "created", "source", "username", "emotion", "polarity")
        
        totalFinalOutput
}