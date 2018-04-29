
# Install packages
install.packages("twitteR")
install.packages("tm")
install.packages("topicmodels")
#install.packages("rgra")

# Load libraries
library(twitteR)
library(tm)
library(tidytext)
library(ggplot2)
library(stringi)
library(stringr)
library(wordcloud)

# Access Twitter Session
    # Go to url to setup access to twitter API https://apps.twitter.com/
        # Consumer Key (API Key)	Iv5IXRssSkiq6UyzhCfycX1tB
        # Consumer Secret (API Secret)	naCxYM7ylTCItqdxekBIsFhLZ045Or9B06ysAON9flKJuTt0qj
        # Access Token	93778529-7trF55u5hpOoT7slg5tCN8LjCKWpbsustMkBmPl9r
        # Access Token Secret	BleVM8M6qNv0SFZSnBQzL7HhxBXOl7Q2OnXgrt8BGTRHK

ConsumerKey = "Iv5IXRssSkiq6UyzhCfycX1tB"
ConsumerSecret = "naCxYM7ylTCItqdxekBIsFhLZ045Or9B06ysAON9flKJuTt0qj"
AccessToken = "93778529-7trF55u5hpOoT7slg5tCN8LjCKWpbsustMkBmPl9r"
AccessTokenSecret = "BleVM8M6qNv0SFZSnBQzL7HhxBXOl7Q2OnXgrt8BGTRHK"


setup_twitter_oauth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret)

# Search Twitter (returns 25 tweets by default)
tweet_raw <- searchTwitter(searchString = "#DataScience",n = 3000)

tweets<- strip_retweets(tweet_raw, strip_manual=TRUE, strip_mt=TRUE)
head(tweets,5)



# Search Twitter for a user
kdnuggets <-getUser(user = "kdnuggets")
kdnuggets$description
kdnuggets$getFollowersCount()
kdnuggets$getFriends(5)
kdnuggets$getFavorites(5)

# Convert the list of tweets into a data frame
tweetDF<-twListToDF(tweets)
head(tweetDF)

tweetDF<-as.tbl(tweetDF)
tweetDF


# Get source of tweets from statusSource
library(tidyr)
head(tweetDF$statusSource,20)

# Extract iPhone and Android updates
tweets_tbl <- tweetDF %>%
    select(id, statusSource, text, created) %>%
    tidyr::extract(statusSource, "source", "Twitter for (.*?)<")%>%
    filter(source %in% c("iPhone", "Android","iPad"))

tweets_tbl

# Transform the text of tweets into Document Term Matrix

# First clean the text
library(tm)

# Create a collection of documents with each tweet text is a row
# Remove some special characters like smilies to help cleaning
tweetDF$text<-sapply(tweetDF$text,function(x) iconv(x ,to="UTF-8-MAC",sub = "" ))
tweetCorpus <- Corpus(VectorSource(tweetDF$text))
inspect(tweetCorpus[1:5])

# Usual transformations for cleaning the test
tweetCorpus <- tm_map(tweetCorpus, tolower)
inspect(tweetCorpus[1:5])
tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
tweetCorpus <- tm_map(tweetCorpus, removeNumbers)

# Remove URLs using Regex
# https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html
    removeURL <- function(x) gsub("^https://[a-z A-Z 0-9 ./]*", "", x) # remove URLs
    tweetCorpus <- tm_map(tweetCorpus, removeURL)
inspect(tweetCorpus[1:5])

    twtrStopWords <- c(stopwords("english"),'datascience','data','science')
tweetCorpus <- tm_map(tweetCorpus, removeWords, twtrStopWords) # remove stop words


inspect(tweetCorpus[1:5])

# Create a Document Term Matrix
tweetDTM<-DocumentTermMatrix(tweetCorpus,list(termFreq=1))
inspect(tweetDTM)
saveRDS(tweetDTM,"./tweetDTM")

# Find frequent terms
freqTerms<-findFreqTerms(tweetDTM,lowfreq = 10)

# Find their frequencies
term.freq<-colSums(as.matrix(tweetDTM))
term.freq.df<-data.frame(term = names(term.freq),freq=term.freq)

# Wordcloud
wordcloud(words = freqTerms,
          freq = term.freq.df[term.freq.df$term %in% freqTerms,2],
          colors = T,random.color = T)


# Find word associations
term.association<-findAssocs(tweetDTM,terms = "sql",corlimit = 0.2)

# Plot it
term.assoc.freq <- rowSums(as.matrix(term.association$sql))
sqlDF <- data.frame(term=names(term.association$sql),freq=term.association$sql)

g<-ggplot(sqlDF,aes(x=term,y=freq)) +
    geom_bar(stat = "identity")+
    xlab("Terms")+
    ylab("Associations")+
    coord_flip()
g






