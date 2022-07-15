library(tidyverse)
library(rms)
require(lubridate)
require(ggpubr)
require(tidytext)
require(tm)
require(stringr)
require(igraph)


##################
# Get stop_words #
##################
#* Create variable of stop words to filter text from

stopwords_filter = stop_words %>%
  filter(lexicon == 'SMART')


####################
# Assess two words #
####################

# collate biwords

Bigram = df %>%
  filter(!is.na(comments)) %>%
  select(patId, (comments)) %>%
  unnest_tokens(trigram, comments, token = "ngrams", n = 1) %>%
  separate(trigram, c("word1"), sep = " ") %>%
  filter(!word1 %in% stopwords_filter$word) %>%
  filter(!is.na(word1)) %>%
  group_by(patId) %>%
  summarise(words = str_c(word1, collapse = " ")) %>%
  ungroup() %>%
  unnest_tokens(trigram, words, token = "ngrams", n = 2) %>%
  mutate(biwords = trigram) %>%
  separate(trigram, c('word1', 'word2'), ' ')
  

# plot top 20 biwords

Bigram %>%
  count(biwords, sort = TRUE) %>% 
  filter(!is.na(biwords)) %>%
  slice(1:30) %>%
  ggplot(aes(x = reorder(biwords, n), y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab(NULL) +
  ylab('Count')



######################
# Assess three words #
######################

# collate triwords

triwords = df %>%
  filter(!is.na(comments)) %>%
  select(patId, (comments)) %>%
  unnest_tokens(trigram, comments, token = "ngrams", n = 1) %>%
  separate(trigram, c("word1"), sep = " ") %>%
  filter(!word1 %in% stopwords_filter$word) %>%
  filter(!is.na(word1)) %>%
  group_by(patId) %>%
  summarise(words = str_c(word1, collapse = " ")) %>%
  ungroup() %>%
  unnest_tokens(trigram, words, token = "ngrams", n = 3) %>%
  mutate(triword = trigram) %>%
  separate(trigram, c('word1', 'word2', 'word3'), ' ')


# plot top 20 
triwords %>%
  count(triword, sort = TRUE) %>% 
  filter(!is.na(triword)) %>%
  slice(1:20) %>%
  ggplot(aes(x = reorder(triword, n), y= n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  xlab(NULL) +
  ylab('Count')


# filter if contains word

triwords %>%
  filter(
    if_any(
      .cols = c('word1', 'word2', 'word3'),
      .fns = function(x) x == "face"
    )
  ) %>%
  count(triword, sort = TRUE)







############
# i graphs #
############
require(ggraph)

bigram_graph <- Bigram %>%
  count(biwords, sort = TRUE) %>%
  separate(biwords, c('word1', 'word2'), ' ') %>%
  filter(n > 7) %>%
  graph_from_data_frame()


a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
set.seed(1)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                 arrow = a, end_cap = circle(.07, 'inches'), check_overlap = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
  theme_void() +
  ggtitle("bigram network, n > 7")

