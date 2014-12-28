scraping_twitter <- function(SearchTerm = "comcast email", numTweets = 100, startDay = NULL, endDay = NULL) {
        library(twitteR) ##built in R package that does some of the Twitter API heavy lifting
                
        ##Authentication
        load("credentials.RData")  ##has my secret keys and shiz
        registerTwitterOAuth(twitCred) ##logs me in
        
        ##Get the tweets to work with
        tweetList <- searchTwitter(searchString = SearchTerm, n = numTweets, since = startDay, until = endDay) ##Searches twitter for anything with comcast and email in it
        
        tweetList <- twListToDF(tweetList) ##converts that data we got into a data frame
        
        fixemail <- grep("(fix.*email)", tweetList$text) ##finds the rows that have the phrase "fix ... email" in them
        comcastemail <- grep("[Cc]omcast.*email", tweetList$text) ##finds the rows that have the phrase "comcast ... email" in them
        noemail <- grep("no email", tweetList$text) ##finds the rows that have the phrase "no email" in them
        comcasttweet <- grep("[Cc]omcast", tweetList$screenName) ##finds the rows that originated from a Comcast twitter handle
        custserv <- grep("[Cc]ustomer [Ss]ervice.*email|email.*[Cc]ustomer [Ss]ervice", tweetList$text) ##finds the rows related to email and customer service
        
        smbiz <- grep("Comcast, I have no email. This is bad for my small business.  Their response", tweetList$text)##This "spam" tweet pops up frequently... Identifying it to remove it.
        
        ##combine all of the "good" tweets row numbers that we greped out above and then sorts them and makes sure they are unique
        combined <- c(fixemail, comcastemail, noemail, comcasttweet, custserv)
        uvals <- unique(combined)
        sorted <- sort(uvals)
        rem <- !(sorted %in% smbiz)
        finalvalues <- sorted[rem]
        
        ##pull the row numbers that we want, and with the columns that are important to us (tweet text, time of tweet, source, and username)
        paredTweetList <- tweetList[finalvalues, c(1, 5, 10, 11)] 
        
        ##make the device source look nicer
        paredTweetList$statusSource <- sub("<.*\">", "", paredTweetList$statusSource) 
        paredTweetList$statusSource <- sub("</a>", "", paredTweetList$statusSource)
        
        ##name the columns
        names(paredTweetList) <- c("Tweet", "Created", "Source", "ScreenName")
        
        ##write the output to a csv... commenting this out for now - will bring it back if we pull this into R for visualization
        ## write.table(paredTweetList, "testfile.csv", sep = ",", row.names = FALSE, qmethod = "double")
        
        paredTweetList
}