sentiment_analysis <- function(paredTweetList) {
        library(stringr) ##Does some of the text editing
        
        ##NOW some sentiment analysis
        
        ##Cleaning up the data some more (just the text now...) First grabbing only the text
        text <- paredTweetList
        
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
        # remove unnecessary spaces - NEED TO RELOOK AT THIS - didn't seem to work right
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
        
        ##segment the terms for evaluation by severity
        vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
        negTerms <- afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1]
        posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1]
        vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]
        
        ##create a matrix to store the scores and sentiment
        final_scores <- matrix('', 0, 5)
        
        ##go through each sentence in the matrix
        for(i in text) {
                ##pull out each word
                wordList <- str_split(i, '\\s+')
                words <- unlist(wordList)
                
                ##match every class of word
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
        
        ##rename the scored data and change it to a data frame
        results <- as.data.frame(final_scores)
        results <- cbind(results, "sentiment")
        
        ##name the columns
        colnames(results) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
        
        ##coerce the values to numerics (for some reason each value is incremented by 1, so adjusting for this as well)
        results[, 2] <- as.numeric(results[, 2]); results[, 2] <- results[, 2] - 1
        results[, 3] <- as.numeric(results[, 3]); results[, 3] <- results[, 3] - 1
        results[, 4] <- as.numeric(results[, 4]); results[, 4] <- results[, 4] - 1
        results[, 5] <- as.numeric(results[, 5]); results[, 5] <- results[, 5] - 1
        results[, 6] <- NA ##empting out the sentiment column, to which we will be adding our answer below
        
        ##counter for the while statment - we will go through each row in the matrix
        counter <- 1
        
        while(counter <= nrow(results)) {
                ##calculate the overall sentiment of the tweet
                sentcount <- (results[counter, 2] * -2) + (results[counter, 3] * -1) + results[counter, 4] + (results[counter, 5] * 2)
                
                ##assign the overall sentiment to the sentiment column
                results[counter, 6] <- if(sentcount < -1) {
                        "negative"
                } else if(sentcount > 1) {
                        "positive"
                } else {
                        "neutral"
                }
                
                counter <- counter + 1
        }
        
        results
}