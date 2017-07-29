library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(scales)
library(wordcloud)
library(reshape2)


tolstoy_books <- gutenberg_download(c(2600, 1399, 2142, 986))
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 2600] <- "War and Peace"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 1399] <- "Anna Karenina"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 2142] <- "Childhood"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 986] <- "Master and Man"

original_books <- tolstoy_books %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books


tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(gutenberg_id == "2600") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

tolstoy_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(gutenberg_id, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(tolstoy_sentiment, aes(index, sentiment, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_x")

war_and_peace <- tidy_books %>% 
  filter(gutenberg_id == "War and Peace")

war_and_peace

afinn <- war_and_peace %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(war_and_peace %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          war_and_peace %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative")) %>% 
count(sentiment)

get_sentiments("bing") %>% 
  count(sentiment)

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

custom_stop_words <- bind_rows(data_frame(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


tolstoy_chapters <- tolstoy_books %>%
  group_by(gutenberg_id) %>%
  unnest_tokens(chapter, text, token = "regex", 
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

tolstoy_chapters %>% 
  group_by(gutenberg_id) %>% 
  summarise(chapters = n())

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(gutenberg_id, chapter) %>%
  summarize(words = n())


tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(gutenberg_id, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("gutenberg_id", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()
