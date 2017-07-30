library(dplyr)
library(tidytext)
library(gutenbergr)
library(ggplot2)


tolstoy_books <- gutenberg_download(c(2600, 1399, 2142, 986))
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 2600] <- "War and Peace"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 1399] <- "Anna Karenina"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 2142] <- "Childhood"
tolstoy_books$gutenberg_id[tolstoy_books$gutenberg_id == 986] <- "Master and Man"

book_words <- tolstoy_books %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE) %>%
  ungroup()

total_words <- book_words %>% 
  group_by(gutenberg_id) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

ggplot(book_words, aes(n/total, fill = gutenberg_id)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free_y")

freq_by_rank <- book_words %>% 
  group_by(gutenberg_id) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = gutenberg_id)) + 
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = gutenberg_id)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) + 
  scale_x_log10() +
  scale_y_log10()

book_words <- book_words %>%
  bind_tf_idf(word, gutenberg_id, n)

book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_austen %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = gutenberg_id)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

plot_austen %>% 
  group_by(gutenberg_id) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free") +
  coord_flip()

