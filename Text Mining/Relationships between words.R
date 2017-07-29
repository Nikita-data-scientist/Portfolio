library(dplyr)
library(tidyr)
library(widyr)
library(tidytext)
library(igraph)
library(ggraph)
library(ggplot2)

tolstoy_books <- gutenberg_download(c(2600, 1399, 2142, 986))
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 2600] <- "War and Peace"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 1399] <- "Anna Karenina"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 2142] <- "Childhood"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 986] <- "Master and Man"

tolstoy_bigrams <- tolstoy_books %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

tolstoy_bigrams

tolstoy_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- tolstoy_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

tolstoy_books %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

bigrams_filtered %>%
  filter(word2 == "anna") %>%
  count(gutenberg_id, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(gutenberg_id, bigram) %>%
  bind_tf_idf(bigram, gutenberg_id, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

tolstoy_section_words <- tolstoy_books %>%
  filter(gutenberg_id == "War and Peace") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)



