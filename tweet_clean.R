tweet_clean <- function(paredTweetList) {
        library(stringr) ##Does some of the text editing
        
        ##Cleaning up the data some more (just the text now...) First grabbing only the text
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
        
        text
}