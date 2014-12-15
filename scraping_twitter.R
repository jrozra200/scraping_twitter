scraping_twitter <- function(SearchTerm = "comcast email", numTweets = 100, startDay = NULL, endDay = NULL) {
        library(twitteR)
        library(sentiment)
        library(plyr)
        library(stringr)
        library(ggplot2)
        library(wordcloud)
        library(RColorBrewer)
        
        load("credentials.RData")
        
        registerTwitterOAuth(twitCred)
        
        tweetList <- searchTwitter("comcast email", n = 1000)
        
        tweetList <- twListToDF(tweetList)
        
        fixemail <- grep("(fix.*email)", tweetList$text)
        comcastemail <- grep("[Cc]omcast.*email", tweetList$text)
        noemail <- grep("no email", tweetList$text)
        comcasttweet <- grep("[Cc]omcast", tweetList$screenName)
        custserv <- grep("[Cc]ustomer [Ss]ervice.*email|email.*[Cc]ustomer [Ss]ervice", tweetList$text)
        
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
        
        postweets <- results[results$sentiment == "positive", ]
        neuttweets <- results[results$sentiment == "neutral", ]
        negtweets <- results[results$sentiment == "negative", ]
        
        total <- dim(results)[1]
        posnum <- dim(postweets)[1]
        negnum <- dim(negtweets)[1]
        
        posperc <- posnum/total
        negperc <- negnum/total
        
        wordcloud(results$sentence, random.order = FALSE)
        
        histo <- c(posperc, negperc)
                
        hist(histo)
        
}