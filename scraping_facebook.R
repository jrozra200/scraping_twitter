scraping_facebook.R <- function() {
        library(Rfacebook)
        library(Rook)
        library(httpuv)
                
        load("fb_oauth")
                
        load("fbtoken")
        
        getNewsfeed(FBcred, n=10) ##the only thing I can get to work... 
}