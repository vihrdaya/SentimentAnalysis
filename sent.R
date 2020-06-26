library(rvest)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)

url <- 'https://markets.businessinsider.com/news/stocks/warren-buffett-90-billion-apple-stake-20-percent-berkshire-hathaway-2020-6-1029338684'
articledata <- read_html(url)
artickewords <- html_nodes(articledata, 'p')
words <- html_text(artickewords)
wordstring <- as.list(strsplit(words, ' '))
wordsresults <- unlist(wordstring)
wordsresults
onelist <- as.list(wordsresults)

x <- data.frame("word" = c(wordsresults))



positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

x %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)

bing <- get_sentiments("bing")
article_sentiments <- x %>%
  inner_join(bing) %>%
  count(word , sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

article <- c(rep(url, 2))
neg_sentiment <- sum(article_sentiments$negative)
pos_sentiment <- sum(article_sentiments$positive)
Sentiment <- c("Positive", "Negative")
value <- c(pos_sentiment, neg_sentiment)
datadf <- data.frame(article, Sentiment, value)



library(ggplot2)
ggplot(datadf, aes(fill = Sentiment, y = value, x = article)) + 
  geom_bar(position="fill", stat="identity")

