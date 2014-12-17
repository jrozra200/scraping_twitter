make_wordcloud <- function(text, toScreen = TRUE, filename = "wordcloud.png") {
        library(wordcloud) ##Lets me do word clouds
        library(RColorBrewer) ##Allows for many different color pallets
        
        if(toScreen == TRUE){ ##write the wordcloud to the screen
                wordcloud(text, scale=c(6, 2), random.order = FALSE, colors=brewer.pal(8, "Paired"))
        } else { ##create the wordcloud and write it to a png
                png(file = filename)
                wordcloud(text, scale=c(6, 2), random.order = FALSE, colors=brewer.pal(8, "Paired"))
                dev.off()
        }
        
}