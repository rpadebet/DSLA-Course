# Topic Modeling using Latent Dirichlet Allocation (LDA)
library(topicmodels)

# Load the Tweet Document Term Matrix
tweetDTM<-readRDS(file="./DSLA Course/Week 4/tweetDTM")

# Model the topics
tweetDTM.new<-tweetDTM[rowSums(as.matrix(tweetDTM))>0,]
ldaTopics <- LDA(tweetDTM.new, k = 2,control=list(seed =1023))

# Model Parameters
ldaTopics
ldaTopics@terms[1:10]
ldaTopics@beta[,1:5]
ldaTopics@gamma[1:5,]
as.matrix(ldaTopics@wordassignments[1:10,1:10])

# First 6 terms of every topic
ldaTerms <- terms(ldaTopics, 6)
apply(ldaTerms, MARGIN = 2, paste, collapse = ", ")

# Another Tidy way to visualize the topics
library(tidytext)

lda_topics <- tidy(ldaTopics, matrix = "beta")
lda_topics

library(dplyr)

top_terms <- lda_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

# Assigning topics to new tweets
tweet_raw.new <- searchTwitter(searchString = "iot",n = 100)
tweets.new<- strip_retweets(tweet_raw.new, strip_manual=TRUE, strip_mt=TRUE)
tweetDF.new<-twListToDF(tweets.new)
tweetDF.new$text<-sapply(tweetDF.new$text,function(x) iconv(x ,to="UTF-8-MAC",sub = "" ))
tweetCorpus.new <- Corpus(VectorSource(tweetDF.new$text))

# Usual transformations on new test data
tweetCorpus.new <- tm_map(tweetCorpus.new, tolower)
tweetCorpus.new <- tm_map(tweetCorpus.new, removePunctuation)
tweetCorpus.new <- tm_map(tweetCorpus.new, removeNumbers)
tweetCorpus.new <- tm_map(tweetCorpus.new, removeURL)
tweetCorpus.new <- tm_map(tweetCorpus.new, removeWords, twtrStopWords)

inspect(tweetCorpus.new)

# Create a Document Term Matrix
tweetDTM.test<-DocumentTermMatrix(tweetCorpus.new,list(termFreq=1))
tweetDTM.test<-tweetDTM.test[rowSums(as.matrix(tweetDTM.test))>0,]
inspect(tweetDTM.test)

# Apply model to new test data using posterior function
tweetCorpus.new.topics <- posterior(object = ldaTopics,newdata = tweetDTM.test)

# Get probabilities of topic for each document
tweetCorpus.new.topics$topics
apply(tweetCorpus.new.topics$topics, 1, which.max)

tweetDF.new$text[c(27)]  
tweetDF.new$text[c(8,24,36,40)]  

