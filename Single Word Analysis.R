
#### Installing all required Packages
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
library(dplyr)
library(reports)
library(tidytext)
library(tidyr)
library(igraph)
library(ggraph)

setwd("C:/Users/dell/Anaconda2/ICTC")
jobs.df <- read.csv("test_use.csv",header=TRUE)


# Create document corpus with Job Description text
myCorpus<- VCorpus(VectorSource(jobs.df$Description)) 

#Convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))

#Remove any links
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#Remove anything except the english language and space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#Remove Stopwords and some other unwanted words
myStopWords<- c((stopwords('english')),c("de", "bi", "en", "id", "isdst","ess", "eg", "et", "4", "18", "118", "0", "des", "les", "la", "donnees", "le"))
myCorpus<- tm_map(myCorpus,removeWords , myStopWords) 

#Remove any Single Words
removeSingle <- function(x) gsub(" . ", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))

#Remove Extra Whitespaces
myCorpus<- tm_map(myCorpus, stripWhitespace) 

#Create Term Document Matrix

tdm<- TermDocumentMatrix(myCorpus, control= list(wordLengths= c(1, Inf)))
tdm

#Find the terms used most frequently
(freq.terms <- findFreqTerms(tdm, lowfreq = 100))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 150)
df <- data.frame(term = names(term.freq), freq= term.freq)

#PLotting the graph of frequent items
ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 

#calculate the frequency of words and sort it by frequency and setting up the Wordcloud
word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 100)

# Identify and plot word correlations.
WordCorr <- apply_as_df(myCorpus[1:132], word_cor, word = "skills", r=.39)
plot(WordCorr)

qheat(vect2df(WordCorr[[1]], "word", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

#Find association with a specific keyword
findAssocs(tdm, "skills", 0.39)

#Topic Modelling to identify latent/hidden topics using LDA technique
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  jobs.df <- jobs.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtm, k = 6) # find 6 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
