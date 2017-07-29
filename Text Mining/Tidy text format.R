library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(gutenbergr)
library(scales)

tolstoy_books <- gutenberg_download(c(2600, 1399, 2142, 986))

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

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, col = factor(n))) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
  
twain_books <- gutenberg_download(c(74, 76, 1837, 86))

tidy_twain <- twain_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_twain %>%
  count(word, sort = TRUE)

tidy_twain %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, col = factor(n))) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

shakespeare_books <- gutenberg_download(c(1112, 1524, 2264, 2267))

tidy_shakespeare <- shakespeare_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_shakespeare %>%
  count(word, sort = TRUE)

tidy_shakespeare %>%
  count(word, sort = TRUE) %>%
  filter(n > 300) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, col = factor(n))) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

frequency <- bind_rows(mutate(tidy_twain, author = "Mark Twain"),
                       mutate(tidy_shakespeare, author = "William Shakespeare"), 
                       mutate(tidy_books, author = "Leo Tolstoy")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Mark Twain`:`William Shakespeare`)


ggplot(frequency, aes(x = proportion, y = `Leo Tolstoy`, color = abs(`Leo Tolstoy` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Leo Tolstoy", x = NULL)

cor.test(data = frequency[frequency$author == "Mark Twain",],
         ~ proportion + `Leo Tolstoy`)

cor.test(data = frequency[frequency$author == "William Shakespeare",],
         ~ proportion + `Leo Tolstoy`)

