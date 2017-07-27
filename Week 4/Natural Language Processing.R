
## Introduction to Natural Language Processing

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

## Put this into a data frame
library(dplyr)
text_df <- data_frame(line = 1:4, text = text)

text_df

## Notice that this data frame containing text isn’t yet compatible with tidy text analysis, though. 
# We can’t filter out words or count which occur most frequently, 
# since each row is made up of multiple combined words. 
# We need to convert this so that it has one-token-per-document-per-row.

# Tokenization

library(tidytext)

text_df %>%
    unnest_tokens(term, text)

text_df<-unnest_tokens(text_df,word,text,token = "words")



## Tidying text of Jane Austen 
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                   ignore_case = TRUE)))) %>%
    ungroup()

head(original_books,20)

## Let us tokenize it
        # The default tokenizing is for words,
        #' but other options include characters, 
        #' n-grams, sentences, lines, paragraphs, 
        #' or separation around a regex pattern
        
tidy_books <- original_books %>%
    unnest_tokens(word, text)

tidy_books

#' STOP WORDS: Often in text analysis, we will want to remove stop words; 
#' stop words are words that are not useful for an analysis, 
#' typically extremely common words such as “the”, “of”, “to”, and so forth in English.
#' We can remove stop words (kept in the tidytext dataset stop_words) with an anti_join()

data(stop_words)

tidy_books <- tidy_books %>%
    anti_join(stop_words)


# Now find the most common words
tidy_books %>%
    count(word, sort = TRUE)


# Plot the most common words
library(ggplot2)

tidy_books %>%
    count(word, sort = TRUE) %>%
    filter(n > 600) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()


# Let us see how writing style of Jane Austen compares with some other books
library(gutenbergr)

# Books from H.G. Wells
hgwells <- gutenberg_download(c(35, 36, 5230, 159))

# Tidy it
tidy_hgwells <- hgwells %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

# Count common words
tidy_hgwells %>%
    count(word, sort = TRUE)


# Books by Bronte sister's
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidy_bronte %>%
    count(word, sort = TRUE)



# Let us now compare these three authors by binding the three data frames together
# We will analyze them by the frequency of words they use to that of Jane Austen

library(tidyr)


frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>% 
    select(-n) %>% 
    spread(author, proportion) %>% 
    gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

frequency

# Plot this

library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~author, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Jane Austen", x = NULL)

# Quantify this correlation for Bronte sisters vs Jane Austen
cor.test(data = frequency[frequency$author == "Brontë Sisters",],
         ~ proportion + `Jane Austen`)

# Quantify this correlation for H.G Wells vs Jane Austen
cor.test(data = frequency[frequency$author == "H.G. Wells",],
         ~ proportion + `Jane Austen`)



