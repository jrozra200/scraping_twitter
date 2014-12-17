make_piechart <- function(results, toScreen = TRUE, filename = "sentimentpie.png") {
        library(plotrix) ##Alows for pie charts
        
        ##segment the matrix by sentiment
        postweets <- results[results$sentiment == "positive", ]
        neuttweets <- results[results$sentiment == "neutral", ]
        negtweets <- results[results$sentiment == "negative", ]
        
        ##get a value for the number of rows in each matrix
        total <- dim(results)[1]
        posnum <- dim(postweets)[1]
        negnum <- dim(negtweets)[1]
        neutnum <- dim(neuttweets)[1]
        
        ##calculate the percentage for each sentiment value
        posperc <- posnum/total
        negperc <- negnum/total
        neutperc <- neutnum/total
        
        ##create the pie chart slices, labels, and add the percentage
        slices <- c(posnum, negnum, neutnum)
        lbls <- c("Positive", "Negative", "Neutral")
        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct)
        lbls <- paste(lbls, "%", sep = "")
        
        if(toScreen == TRUE){ ##show the piechart on the screen
                pie3D(slices, labels=lbls, explode=0.1, main="Overall Sentiment of Comcast Email on Twitter")
        } else { ##create the piechart and write it to a png
                png(file = "sentimentpie.png")
                pie3D(slices, labels=lbls, explode=0.1, main="Overall Sentiment of Comcast Email on Twitter")        
                dev.off()
        }
        
}