library(rvest)
library(dplyr)
library(stringr)
library(readxl)
library(plyr)
scraping <- read_excel("for_scraping.xlsx")

scrap <- function(page, title, id){
  url <- paste("https://www.amazon.com/", title, "/product-reviews/", id, "/ref=cm_cr_arp_d_paging_btm_", page, "?ie=UTF8&reviewerType=all_reviews&pageNumber=", page, sep="")
  text <- read_html(url) %>%
    html_nodes(".review-text") %>%
    html_text()
  
  book <- as.data.frame(text)
  colnames(book) <- "Reviews"
  return(book)
}

final_data_reviews <- function(b_title, b_author, a_title, id){
  data <- c()
  for (page in 1:30) {
    data1 <- scrap(page, a_title, id)
    data[page] <- data1
  }
  data <- as.data.frame(unlist(data))
  author <- c()
  title <- c()
  for (i in 1:length(data)){
    author[i] <- b_author
    title[i] <- b_title
  }
  book <- as.data.frame(cbind(author, title, data))
  colnames(book) <- c("Author", "Title", "Reviews")
  return(book)
}


list <- c()
for (i in 1:30){
  list[[i]] <- final_data_reviews(scraping[i,1], scraping[i,2],
                                             scraping[i,3],scraping[i,4])
}

reviews <- ldply(list, data.frame)

######### Data manipulation #############
reviews$N_Words<-sapply(strsplit(reviews$Reviews, " "), length) #calculating # of words 
reviews<-reviews[-c(857, 4327, 4744, 4965, 7625), ] #deleting non-important reviews

########## Getting Polarity Scores ###########
sent_anal<-data.frame(reviews[, c("Reviews", "Author", "Title")], Count=nrow(reviews))
sent_anal$Reviews<-iconv(sent_anal$Reviews, to="ASCII", sub="")

sent_reviews<-polarity(text.var = sent_anal$Reviews, grouping.var = sent_anal$Count)
sent_scores_reviews<-scores(sent_reviews)

reviews$Average_Polarity<-sent_scores_reviews$ave.polarity

reviews$Average_Polarity2<-ifelse(reviews$Average_Polarity>0, "Posititive", 
                                  ifelse(reviews$Average_Polarity<0, "Negative", "Neutral"))

#write.csv(reviews, "reviews.csv", row.names = F)
