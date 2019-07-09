library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(tm)
library(wordcloud)
library(tidytext)
library(tidyr)
library(ggraph)
library(igraph)

source("functions.R")

books <- read_excel("books.xlsx")
reviews <- read.csv("reviews.csv")
texts <- read.csv("book_text.csv")


#####books graphs
Title <- unique(books$Title)
joy <- c()
trust <- c()
sadness <- c()
anger <- c()
wordcloud_reviews <- c()
wordcloud_text <- c()

for (i in 1:30){
  book_review <- filter_book(reviews, Title[i])
  book_text <- filter_book(texts, Title[i])
  
  wordcloud_reviews[i] <- paste(paste(strsplit(Title[i], split = " ")[[1]], collapse="_"), "wordcloud_reviews.png", sep="_")
  png(filename=paste("www/", wordcloud_reviews[i], sep=""))
  wordcld(book_review$Reviews, "Title", "Reviews")
  dev.off()
  
  wordcloud_text[i] <- paste(paste(strsplit(Title[i], split = " ")[[1]], collapse="_"), "wordcloud_text.png", sep="_")
  png(filename=paste("www/", wordcloud_text[i], sep=""))
  wordcld(book_text$Text, "Title", "Book")
  dev.off()
  
  joy[i] <- paste(paste("joy", Title[i], sep="_"), ".png", sep="")
  png(filename=paste("www/", joy[i], sep=""))
  count_feeling(reviews, "nrc", "joy", Title[i], 15, "Reviews", "Set1")
  dev.off()
  
  trust[i] <- paste(paste("trust", Title[i], sep="_"), ".png", sep="")
  png(filename=paste("www/", trust[i], sep=""))
  count_feeling(reviews, "nrc", "trust", Title[i], 15, "Reviews", "Set1")
  dev.off()
  
  sadness[i] <- paste(paste("sadness", Title[i], sep="_"), ".png", sep="")
  png(filename=paste("www/", sadness[i], sep=""))
  count_feeling(reviews, "nrc", "sadness", Title[i], 15, "Reviews", "Set1")
  dev.off()
  
  anger[i] <- paste(paste("anger", Title[i], sep="_"), ".png", sep="")
  png(filename=paste("www/", anger[i], sep=""))
  count_feeling(reviews, "nrc", "anger", Title[i], 15, "Reviews", "Set1")
  dev.off()
}

books$Wordcloud_Reviews <- wordcloud_reviews
books$Wordcloud_Text <- wordcloud_text
books$Trust_Reviews <- trust
books$Joy_Reviews <- joy
books$Anger_Reviews <- anger
books$Sadness_Reviews <- sadness

books$X__1 <- NULL

write.csv(books, "books_updated.csv", row.names = F)


#####authors graphs
Author <- unique(books$Author)
Networks <- c()
Bigram_Reviews <- c()
Bigram_Text <- c()

for(i in 1:5){
  Networks[i] <- paste(Author[i],".png", sep="")
  png(filename=paste("www/", Networks[i], sep=""))
  bigrams_network(reviews, "Reviews", "Author", Author[i], 15, "lightblue")
  dev.off()
  
  Bigram_Reviews[i] <- paste("reviews_", Author[i], ".png", sep="")
  png(filename=paste("www/", Bigram_Reviews[i], sep=""))
  bigrams_barplot(reviews, "Reviews", "Author", Author[i], "Set1", 10)
  dev.off()
  
  Bigram_Text[i] <- paste("text_", Author[i], ".png", sep="")
  png(filename=paste("www/", Bigram_Text[i], sep=""))
  bigrams_network(texts, "Text", "Author", Author[i], 15, "lightblue")
  dev.off()
}

authors <- as.data.frame(cbind(Author, Networks, Bigram_Reviews, Bigram_Text))

write.csv(authors, "authors.csv", row.names = F)

