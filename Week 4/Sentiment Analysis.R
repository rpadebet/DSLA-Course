# Sentiment Analysis

# There are a variety of methods and dictionaries that exist for evaluating 
#'the opinion or emotion in text. The tidytext package contains several 
#'sentiment lexicons in the sentiments dataset.

# The lexicons are
    # AFINN from Finn Årup Nielsen,
    # bing from Bing Liu and collaborators, and
    # nrc from Saif Mohammad and Peter Turney.

sentiments

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# Note: that the size of the chunk of text that we use to 
    #' add up unigram sentiment scores can have an effect on an analysis.
    #' A text the size of many paragraphs can often have positive and negative sentiment 
    #' averaged out to about zero, while sentence-sized or paragraph-sized text often works better

# Let us analyze Jane Austen for "joy" words
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                   ignore_case = TRUE)))) %>%
    ungroup() %>%
    unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>% 
    filter(sentiment == "joy")

tidy_books %>%
    filter(book == "Emma") %>%
    inner_join(nrcjoy) %>%
    count(word, sort = TRUE)

# We can analyze how sentiment changes throughout the book. 
# We look at 80 lines at a time here to keep track of sentiment
library(tidyr)

janeaustensentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(book, index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

# Let us plot this and visualize the sentiment changes
library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free_x")




# Most common negative and positive words
bing_word_counts <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts

bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()

# Here miss could be referring to a unmarried women character but is categorized as negative.
# We could add this to our stop words dictionary to remove it's contributions

custom_stop_words <- bind_rows(data_frame(word = c("miss"), lexicon = c("custom")), 
                               stop_words)

custom_stop_words


## Word clouds: This give a visual representation of words frequently occuring

library(wordcloud)

tidy_books %>%
    anti_join(custom_stop_words) %>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100))



## If we want to find out which are the most negative chapters in a book?

# We need to unnest and organize our dataframe into sentences instead of words
# and then apply sentiment analysis on each chapter

# Example: Pride and Prejudice
PandP_sentences <- data_frame(text = prideprejudice) %>% 
    unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]

# We can also unnest into chapters using some regex
austen_chapters <- austen_books() %>%
    group_by(book) %>%
    unnest_tokens(chapter, text, token = "regex", 
                  pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
    ungroup()

austen_chapters$chapter[2]

# Find number of chapters in each book
austen_chapters %>% 
    group_by(book) %>% 
    summarise(chapters = n())


# most negative chapters
bingnegative <- get_sentiments("bing") %>% 
    filter(sentiment == "negative")

wordcounts <- tidy_books %>%
    group_by(book, chapter) %>%
    summarize(words = n())

wordcounts

tidy_books %>%
    semi_join(bingnegative) %>%
    group_by(book, chapter) %>%
    summarize(negativewords = n()) %>%
    left_join(wordcounts, by = c("book", "chapter")) %>%
    mutate(ratio = negativewords/words) %>%
    filter(chapter != 0) %>%
    top_n(3) %>%
    ungroup()