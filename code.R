# Advanced Social Media Analytics Project

library(twitteR)
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(maps)
library(ggmap)
library(ROAuth)
library(RCurl)
library(magrittr)
library(tm)
library(stringi)
library(stringr)
library(wordcloud)
library(igraph)
library(sentimentr)
library(topicmodels) 
library(tidyverse) 
library(rvest) 
library(reshape2)

# download certification scheme
download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "/Users/Project/test.pem")

# define R objects from my consumer information
my.key <- "00jwRX7tZCA2X8fLRUnALN2d2"
my.secret <- "E1GagmM05Uv4qCIg52dLsMQ7yJL3KLfEPRrwwy9PRZ8Fzvttuc"
access.token <- "1323699258764263428-yyspcPCRzirjaeP3HRSR9z22vswvPz"
access.token.secret <- "fUQbKbU5qxT3utjD0zs4c3N9K3qNBIxQuLcA1OMLCQJk5"

# authentication
setup_twitter_oauth(my.key, my.secret, access.token, access.token.secret)

# read tweets
data <- search_tweets("SpaceX OR Spacex OR spaceX OR spacex", n = 3000)

# check data
View(data)
class(data)
head(data)
length(unique(data$location))

# remove special characters in non Latin languages
data$location2 <- iconv(data$location, to = "ASCII", sub = "")

# replace blank space with NA
data$location2[data$location2 == ""] <- NA_character_
data$location2[data$location2 == " "] <- NA_character_
data$location2[data$location2 == "  "] <- NA_character_
data$location2[data$location2 == "   "] <- NA_character_
data$location2[data$location2 == "    "] <- NA_character_
data$location2[data$location2 == "     "] <- NA_character_
data$location2[data$location2 == ", "] <- NA_character_

# plot the graph of Tiwtter users' locations
data[data$location2 != " /Special",] %>% count(location2, sort = T) %>% mutate(location2 = reorder(location2, n)) %>%
  na.omit() %>% top_n(10) %>% ggplot(aes(x = location2, y = n)) +
  geom_bar(stat = "identity") + geom_col() + coord_flip() +
  labs(x = "Location", y = "Count", title = "Twitter Users' Locations") + theme_light()

# plot the time series of tweets
ts_plot(data, "hours") + ggplot2::theme_minimal() + 
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(x = NULL, y = NULL, title = "Frequency of SpaceX Twitter Status", 
                subtitle = "Twitter Status Counts at 1-hour Interval",
                caption = "\nSource: Data from Twitter API")

# remove special characters in non latin language
usableText <- iconv(data$text, to = "ASCII", sub = "")
data_corpus <- Corpus(VectorSource(usableText))
data_corpus <- tm_map(data_corpus, tolower)
data_corpus <- tm_map(data_corpus, removePunctuation)
data_corpus <- tm_map(data_corpus, removeNumbers)
data_corpus <- tm_map(data_corpus, function(x)removeWords(x, stopwords()))

# data cleaning
data_corpus <- tm_map(data_corpus, function(x)removeWords(x, stopwords("french")))
data_corpus <- tm_map(data_corpus, function(x)removeWords(x, stopwords("italian")))
data_corpus <- tm_map(data_corpus, function(x)removeWords(x, stopwords("spanish")))
data_corpus <- tm_map(data_corpus, function(x)removeWords(x, stopwords("english")))
text_corpus <- tm_map(data_corpus, content_transformer(function(x) iconv(x, to = "ASCII", sub = "byte")))

# document-term matrix
data.tdm <- TermDocumentMatrix(data_corpus)
m <- as.matrix(data.tdm)
m[1:2, 1:5]

# most frequent terms in the matrix
v <- sort(rowSums(m), decreasing = T)
d <- data.frame(word = names(v), freq = v)
head(d)

# plot the graph of word frequency
barplot(d[1:20, ]$freq, las = 3, names.arg = d[1:20, ]$word, col = "blue3", main = "Most Frequent Words", ylab = "Frequency")

# find the frequent words that are used at least 50 times
findMostFreqTerms(data.tdm, lowfreq = 50)[1:10]

# create wordcloud for frequent words
wordcloud(words = d$word, freq = d$freq, min.freq = 40, max.words = 100, random.order = F, colors = brewer.pal(8, "Dark2"))

# remove sparse terms from the term-document matrix
data.tdm <- removeSparseTerms(data.tdm, sparse = 0.95)

# convert the term-document matrix into a data frame
data.df <- as.data.frame(as.matrix(data.tdm))

# scale the data
data.df.scale <- scale(data.df)

# create a distance matrix
data.dist <- dist(data.df.scale, method = "euclidean")

# cluster the data
data.fit <- hclust(data.dist, method = "ward.D2")

# plot the graph
plot(data.fit, main = "Cluster of SpaceX Data")

# plot clusters
groups <- cutree(data.fit, k = 5)
plot(data.fit, main = "Cluster of SpaceX Data")
rect.hclust(data.fit, k = 5, border = "red")

# define a tag extractor function
tags <- function(x) toupper(grep("#", strsplit(x, " +")[[1]], value = T))

# create a list of tag sets for each tweet
l <- nrow(data)/20
taglist <- vector(mode = "list", l)

# create an empty vector to store the tweet texts
texts <- vector(mode = "character", length = l)

# extract tweet texts from each tweet status
for (i in 1:l) texts[i] <- data$text[i]
texts <- iconv(texts, to = "ASCII", sub="")

# populate it
j <- 0
for(i in 1:l){
  if(is.na(str_match(texts[i],"#"))[1,1] == FALSE){
    j<-j+1
    taglist[[j]]<-str_squish(removePunctuation(tags(ifelse(is.na(str_match(texts[i], "\n")[1,1]) == T, texts[i], gsub("\n", " ", texts[i])))))
  }
}
alltags <- NULL
for (i in 1:l) alltags <- union(alltags, taglist[[i]])

# create an empty graph
hash.graph <- graph.empty(directed = T)
hash.graph <- hash.graph + vertices(alltags)

# populate it with edges
for (tags in taglist){
  if (length(tags) > 1){
    for (pair in combn(length(tags), 2, simplify=FALSE,
                       FUN = function(x) sort(tags[x]))){
      if (pair[1] != pair[2]) {
        if (hash.graph[pair[1], pair[2]]==0)
          hash.graph <- hash.graph + edge(pair[1], pair[2])
      }
    }
  }
}

# construct a network
V(hash.graph)$color <- "black"
E(hash.graph)$color <- "black"
V(hash.graph)$name <- paste("#", V(hash.graph)$name, sep = "")
V(hash.graph)$label.cex <- 0.5
V(hash.graph)$size <- 15
V(hash.graph)$size2 <- 2
hash.graph_simple <- delete.vertices(simplify(hash.graph), degree(hash.graph) <= 5)

# plot the graph 
plot(hash.graph_simple, edge.width = 2, edge.color = "black", vertex.color = "SkyBlue2", 
     vertex.frame.color = "black", label.color = "black", vertex.label.font = 2, edge.arrow.size = 0.3)

# sentiment analysis
plain.text <- vector()
for(i in 1:dim(data)[1]){
  plain.text[i]<-data_corpus[[i]][[1]]
}
sentence_sentiment <- sentiment(get_sentences(plain.text)) 
sentence_sentiment

# average sentiment
average_sentiment <- mean(sentence_sentiment$sentiment) 
average_sentiment

# standard deviation of sentiment
sd_sentiment <- sd(sentence_sentiment$sentiment) 
sd_sentiment

# extract sentiment terms
extract_sentiment_terms(get_sentences(plain.text))

# make a new document term matrix
text_corpus2 <- text_corpus[1:200]
doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(text_corpus2))) 
dtm <- DocumentTermMatrix(text_corpus2[doc.lengths > 0])

# pick a random seed for replication
SEED <-  sample(1:1000000, 1)

# start with 2 topics
k <- 2
Topics_results <- LDA(dtm, k = k, control = list(seed = SEED))

# common terms in each topic
terms(Topics_results,15)
topics(Topics_results)
tidy_model_beta <- tidy(Topics_results, matrix = "beta") 

# plot the graph of topic against beta score
tidy_model_beta %>% group_by(topic) %>%top_n(10, beta) %>%ungroup() %>%
  arrange(topic, -beta) %>% ggplot(aes(reorder(term, beta),beta,fill=factor(topic))) + 
  geom_col(show.legend = FALSE) + facet_wrap(~ topic, scales = "free") + scale_fill_viridis_d() +
  coord_flip() + labs(x = "Topic", y = "Beta Score", title = "Topic Modeling")
