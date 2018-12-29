#Loading all the required packages, 
#Ensure to have dplyr at the end as we need to use count from dplyr
library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(devtools)
library(sentiment)
library(qdap)
library(reports)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)
library(RWeka)
library(dplyr)

setwd("C:/Users/dell/Anaconda2/ICTC")
jobs.df <- read.csv("test_use.csv",header=TRUE)

#Tokenizing by n-gram
desc_bigrams <- jobs.df %>%
  unnest_tokens(bigram, Description, token = "ngrams", n = 2)

#Couting and filtering n-grams
desc_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- desc_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
filter_words <- c("opportunity", "equal", "computer", "senior", "market", "des")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% filter_words) %>%
  filter(!word2 %in% filter_words)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#Creating a corpus of bigrams to get bigram frequency
myCorpus<- VCorpus(VectorSource(bigrams_united$bigram))
myCorpus = tm_map(myCorpus,removeWords,c(stopwords(),"s","de", "ve", "e", "g", "hse", "pr","i", "ii"))
myCorpus = tm_map(myCorpus,removePunctuation)
myCorpus = tm_map(myCorpus,removeNumbers)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(myCorpus,
                                control = list(tokenize = BigramTokenizer))

freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 40)

set.seed(1234)
wordcloud(words = freq.df$word, freq = freq.df$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.15, scale=c(2,0.5),
          colors=brewer.pal(8, "Dark2"))

bigram_tf_idf <- bigrams_united %>%
  count(Company, bigram) %>%
  bind_tf_idf(bigram, Company, n) %>%
  arrange(desc(tf_idf))

#Bigram Frequency Diagram
ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")


bigram_tf_idf

bigram_graph <- bigram_counts %>%
  filter(n > 15) %>%
  graph_from_data_frame()

bigram_graph

#Bigram relation diagram
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#Bigram directional relationship diagram
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

