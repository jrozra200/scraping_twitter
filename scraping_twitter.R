scraping_twitter <- function(SearchTerm = "comcast email", numTweets = 100, startDay = NULL, endDay = NULL) {
        library(twitteR) ##built in R package that does some of the Twitter API heavy lifting
        library(stringr) ##Does some of the text editing
        library(wordcloud) ##Lets me do word clouds
        library(RColorBrewer) ##Allows for many different color pallets
        library(plotrix) ##Alows for pie charts
        
        ##Authentication
        load("credentials.RData")  ##has my secret keys and shiz
        registerTwitterOAuth(twitCred) ##logs me in
        
        ##Get the tweets to work with
        tweetList <- searchTwitter("comcast email", n = 1000) ##Searches twitter for anything with comcast and email in it
        
        tweetList <- twListToDF(tweetList) ##converts that data we got into a data frame
        
        fixemail <- grep("(fix.*email)", tweetList$text) ##finds the rows that have the phrase "fix ... email" in them
        comcastemail <- grep("[Cc]omcast.*email", tweetList$text) ##finds the rows that have the phrase "comcast ... email" in them
        noemail <- grep("no email", tweetList$text) ##finds the rows that have the phrase "no email" in them
        comcasttweet <- grep("[Cc]omcast", tweetList$screenName) ##finds the rows that originated from a Comcast twitter handle
        custserv <- grep("[Cc]ustomer [Ss]ervice.*email|email.*[Cc]ustomer [Ss]ervice", tweetList$text) ##finds the rows related to email and customer service
        
        combined <- c(fixemail, comcastemail, noemail)
        uvals <- unique(combined)
        sorted <- sort(uvals)
        
        paredTweetList <- tweetList[sorted, c(1, 5, 10, 11)]
        paredTweetList$statusSource <- sub("<.*\">", "", paredTweetList$statusSource)
        paredTweetList$statusSource <- sub("</a>", "", paredTweetList$statusSource)
        names(paredTweetList) <- c("Tweet", "Created", "Source", "ScreenName")
        
        write.table(paredTweetList, "testfile.csv", sep = ",", row.names = FALSE, qmethod = "double")
        
        
        ##NOW some sentiment analysis
        
        ##Cleaning up the data some more (just the text now...)
        
        text <- paredTweetList$Tweet
        
        # remove retweet entities
        text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
        # remove at people
        text <- gsub("@\\w+", "", text)
        # remove punctuation
        text <- gsub("[[:punct:]]", "", text)
        # remove numbers
        text <- gsub("[[:digit:]]", "", text)
        # remove html links
        text <- gsub("http\\w+", "", text)
        # remove unnecessary spaces
        #text <- gsub("[ \t]{2,}", "", text)
        #text <- gsub("^\\s+|\\s+$", "", text)
        
        # define "tolower error handling" function 
        try.error <- function(x)
        {
                # create missing value
                y <- NA
                # tryCatch error
                try_error <- tryCatch(tolower(x), error=function(e) e)
                # if not an error
                if (!inherits(try_error, "error"))
                        y <- tolower(x)
                # result
                return(y)
        }
        # lower case using try.error with sapply 
        text <- sapply(text, try.error)
        
        # remove NAs in text
        text <- text[!is.na(text)]
        names(text) <- NULL
        
        #load up word polarity list and format it
        afinn_list <- read.delim("AFINN-111.txt", header=FALSE, stringsAsFactors=FALSE)
        names(afinn_list) <- c('word', 'score')
        afinn_list$word <- tolower(afinn_list$word)
        
        vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
        negTerms <- afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1]
        posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1]
        vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]
        
        final_scores <- matrix('', 0, 5)
        
        for(i in text) {
                wordList <- str_split(i, '\\s+')
                words <- unlist(wordList)
                vPosMatches <- match(words, vPosTerms)
                posMatches <- match(words, posTerms)
                vNegMatches <- match(words, vNegTerms)
                negMatches <- match(words, negTerms)
                        
                #sum up number of words in each category
                vPosMatches <- sum(!is.na(vPosMatches))
                posMatches <- sum(!is.na(posMatches))
                vNegMatches <- sum(!is.na(vNegMatches))
                negMatches <- sum(!is.na(negMatches))
                score <- c(vNegMatches, negMatches, posMatches, vPosMatches)   
                                
                #add row to scores table
                newrow <- c(i, score)
                final_scores <- rbind(final_scores, newrow)
        }
        
        results <- as.data.frame(final_scores)
        results <- cbind(results, 'sentiment')
        
        colnames(results) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
        
        results[, 2] <- as.numeric(results[, 2]); results[, 2] <- results[, 2] - 1
        results[, 3] <- as.numeric(results[, 3]); results[, 3] <- results[, 3] - 1
        results[, 4] <- as.numeric(results[, 4]); results[, 4] <- results[, 4] - 1
        results[, 5] <- as.numeric(results[, 5]); results[, 5] <- results[, 5] - 1
        results[, 6] <- NA
        
        counter <- 1
        
        while(counter <= nrow(results)) {
                sentcount <- (results[counter, 2] * -2) + (results[counter, 3] * -1) + results[counter, 4] + (results[counter, 5] * 2)
                
                results[counter, 6] <- if(sentcount < -1) {
                        "negative"
                } else if(sentcount > 1) {
                        "positive"
                } else {
                        "neutral"
                }
                
                counter <- counter + 1
        }
        
        png(file = "wordcloud.png")
        wordcloud(results$sentence, scale=c(6, 2), random.order = FALSE, colors=brewer.pal(8, "Paired"))
        dev.off()
        
        postweets <- results[results$sentiment == "positive", ]
        neuttweets <- results[results$sentiment == "neutral", ]
        negtweets <- results[results$sentiment == "negative", ]
        
        total <- dim(results)[1]
        posnum <- dim(postweets)[1]
        negnum <- dim(negtweets)[1]
        neutnum <- dim(neuttweets)[1]
        
        posperc <- posnum/total
        negperc <- negnum/total
        neutperc <- neutnum/total
        
        slices <- c(posnum, negnum, neutnum)
        lbls <- c("Positive", "Negative", "Nuetral")
        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct)
        lbls <- paste(lbls, "%", sep = "")
        
        png(file = "sentimentpie.png")
        pie3D(slices, labels=lbls, explode=0.1, main="Overall Sentiment of Comcast Email on Twitter")        
        dev.off()
}