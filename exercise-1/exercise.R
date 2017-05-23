# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# Load booksinto a dataframe using the austen_books() function
books <- austen_books()

# How many books are in the dataset?
num.books <- length(as.integer(unique(books$book)))

# Which book has the most lines?
longest.book <- books %>% group_by(book) %>% summarize(lines = n()) %>% filter(lines == max(lines))

# Use the unnest_tokens function to generate the full list of words
all.words <- books %>% unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
most.common.words <- all.words %>% group_by(word) %>% summarize(count = n()) %>% arrange(-count)

# Remove stop words by performing an anti_join with the stop_words dataframe
removed.stop.words <- all.words %>% anti_join(stop_words, by = "word")

# Which non stop-words are most common?
most.common.stop.words <- removed.stop.words %>% group_by(word) %>% summarize(count=n()) %>% arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
removed.stop.words %>% 
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
