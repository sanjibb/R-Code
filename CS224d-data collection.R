install.packages("streamR", "RCurl", "ROAuth", "RJSONIO")
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "FN5kpHlom1nH2MM05KWKKFqiv" # From dev.twitter.com
consumerSecret <- "UhGDSc7YxJP7Jojtve4sATsEGlMNMLpD94XRZOohCOsRxSJ8GS" # From dev.twitter.com

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

### STOP HERE!!! ###

# PART 2: Save the my_oauth data to an .Rdata file
save(my_oauth, file = "my_oauth.Rdata")

library(streamR)
load("my_oauth.Rdata")
filterStream(file.name = "tweets1.json", # Save tweets in a json file
             track = c("democrat","democratic party","Hilary Clinton", "Barny Sanders", "republic","republican"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
             language = "en",
             timeout = 10, # Keep connection alive for 60 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials

tweets.df <- parseTweets("tweets1.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
now<-Sys.time()

write.csv(tweets.df,paste(format(Sys.time(), "%Y-%m-%d-%I-%p"), "csv", sep = "."))

