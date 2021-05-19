library(tm)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(wordcloud)


mydata <- read.csv(file.choose(), header = T)
str(mydata)

#lets have quick look at the count of Sentiment.
sentiment <- mydata %>% group_by(sentiment) %>% summarise(count = n())

#ploting graph
ggplot(sentiment, aes(sentiment, count)) + geom_bar(stat = "Identity", fill = rainbow(3)) + theme_classic()

#Insights: Most of the tweets are neutral

corpus <- iconv(mydata$text, to = "UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[1:5])

removeUrL <- function(x) gsub('http[[:alnum:]]*', '',x)
cleanset <- tm_map(cleanset, content_transformer(removeUrL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c("according","company","s","will","said"))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'eur', 
                   replacement = 'euro')
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

#term Document Matrix:

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

w <- rowSums(tdm)
w <- subset(w, w>=50)
barplot(w,
        las = 2,
        col = rainbow(50))

#word cloud
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 100,
          random.order = F,
          colors = brewer.pal(8, "Dark2"))


















