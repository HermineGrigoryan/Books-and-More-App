library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(ggrepel)
library(plotrix)
library(textmineR)
library(stringr)
library(tm)
library(wordcloud)
library(RWeka)

reviews <- read.csv("data/reviews.csv", stringsAsFactors = F)
authors <- read.csv("data/authors.csv", stringsAsFactors = F)
books <- read.csv("data/books_new.csv", stringsAsFactors = F)

filter_book <- function(df, title){
  df %>% 
    filter(Title == title)
}

filter_author <- function(df, author){
  df %>% 
    filter(Author == author)
}

tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "book", "books", "author", "page", "pages", "read"))
  return(corpus)
}

#books functions
##################################################################################### 
top <- function(df, type, var, size){
  group <- ifelse(type=="Title", "Books", "Authors")
  palette <- ifelse(type=="Title", "YlOrRd", "Greens")
  title_color <- ifelse(type=="Title", "#d10c0c", "#02913e")
  getPalette = colorRampPalette(brewer.pal(9, palette))
  
  df %>%
    arrange(desc(!! rlang::sym(var))) %>%
    head(size) %>%
    ggplot(aes(x=reorder(!! rlang::sym(type), !! rlang::sym(var)),
               y=!! rlang::sym(var))) +
    geom_bar(stat="identity", fill=getPalette(size)) + coord_flip()+
    labs(title=paste("Top", size, group, "with the Most", var),
         x=group) +
    theme(panel.background = element_rect(fill="#edeeff"),
          plot.title = element_text(face="italic", size=22, margin=margin(0,0,25,0), color=title_color),
          axis.text = element_text(size=12),
          axis.title = element_text(size=16, face="italic"))
  
}

#compare books functions

######################################################################################
comparecld <- function(df, title1, title2, size){
  book1 <- filter_book(df, title1)
  book2 <- filter_book(df, title2)
  all_book1 <- paste(book1$Reviews, collapse="")
  all_book2 <- paste(book2$Reviews, collapse="")
  all <- c(all_book1, all_book2)
  all_vs <- VectorSource(all)
  all_corpus <- VCorpus(all_vs)
  if(size=="2"){
    all_tdm <- TermDocumentMatrix(all_corpus, control=list(weighting = weightTfIdf, removeNumbers=T, 
                                                           removePunctuation=T, stopwords=T, stemming=T, tokenize=tokenizer))
  } else{
    all_tdm <- TermDocumentMatrix(all_corpus, control=list(weighting = weightTfIdf, removeNumbers=T, 
                                                           removePunctuation=T, stopwords=T, stemming=T))
  }
  colnames(all_tdm) <- c(title1, title2)
  all_m<-as.matrix(all_tdm)
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 6))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste("The Most Frequent Words in Reviews"), col="#990749", cex=3)
  comparison.cloud(all_m, colors=c("#23b9d1","#f94d7b"), max.words=100, random.order=F, scale=c(5,1))
}

#######################################################################################

dendrogram <- function(df, title1, title2, size){
  book1 <- filter_book(df, title1)
  book2 <- filter_book(df, title2)
  all_book1 <- paste(book1$Reviews, collapse="")
  all_book2 <- paste(book2$Reviews, collapse="")
  all <- c(all_book1, all_book2)
  all_vs <- VectorSource(all)
  all_corpus <- VCorpus(all_vs)
  all_clean <- clean_corpus(all_corpus)
  if(size=="2"){
    all_tdm <- TermDocumentMatrix(all_clean, control=list(tokenize=tokenizer))}
  else {
    all_tdm <- TermDocumentMatrix(all_clean)
  }
  all_m <- as.matrix(all_tdm)
  
  common_words <- subset(
    all_m,
    all_m[, 1] > 0 & all_m[, 2] > 0
  )
  
  # calc common words and difference
  difference <- abs(common_words[, 1] - common_words[, 2])
  common_words <- cbind(common_words, difference)
  common_words <- common_words[order(common_words[, 3],
                                     decreasing = T), ]
  top25_df <- data.frame(x = common_words[1:25, 1],
                         y = common_words[1:25, 2],
                         labels = rownames(common_words[1:25, ]))
  
  pyramid.plot(top25_df$x, top25_df$y,
               labels = top25_df$labels, 
               main = "Review Words in Common",
               gap = 28,
               laxlab = NULL,
               raxlab = NULL, 
               unit = NULL,
               top.labels = c(title1,
                              "Words",
                              title2)
  )
  
}

#######################################################################################



#######################################################################################

############## function for constructing barblot based on polarity ###############
barplot_polarity <- function(df, author){
  data <- filter_author(df, author)
  
  ggplot(data, aes(x=Average_Polarity2)) +
    geom_bar(aes(fill=Title)) + facet_wrap(.~Title)+
    scale_fill_manual(values=colorRampPalette(brewer.pal(8, "Set1"))(length(unique(data$Title)))) +
    labs(title=paste("The Distribution of Average Polarities of Book Reviews", "by", author),
         x="Average Polarity", y="Number of Reviews") +
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="purple", size=0.09),
          plot.title = element_text(face="italic", size=22, margin=margin(20,0,20,0), color="#441963"),
          axis.text = element_text(face = "bold", size=12),
          axis.title = element_text(face="italic", size=16),
          strip.text = element_text(size=12),
          legend.position = "none")
}
#####################################################################################


polarity_dist <- function(df, author){
  data <- filter_author(df, author)
  
  ggplot(data, aes(x=Average_Polarity_Score)) +
    geom_histogram(binwidth=.2, color="white", aes(fill=Title)) +
    labs(title=paste("Polarity Score Distributions of Books by ", author, sep=""), x="Polarity Score", y="Number of Reviews")+
    facet_wrap(.~Title, scales="free") +
    scale_x_continuous(breaks = seq(-2,3,0.5)) +
    scale_fill_manual(values=c("#7f7fff", "#8B4789", "#4c4cff", 
                               "#B272A6", "#9999ff", "#7A378B")) +
    theme(panel.background = element_rect(fill="white"),
          panel.grid.major = element_line(color = "#9999ff"),
          legend.position = "none",
          plot.title = element_text(face="italic", size=20, color="#ab71d6", margin=margin(20,0,20,0)),
          axis.title = element_text(face="italic", size=16),
          axis.text = element_text(face="bold", size=12),
          strip.text = element_text(size=12))
}
#############################################################################################################################


important_words_authors <- function(data, rev_text, author, number, palette){
  text_df<-as.data.frame(cbind(1:nrow(data),data))

  review_words <- text_df %>%
    unnest_tokens(word, !! rlang::sym(rev_text)) %>%
    count(Author, word, sort = TRUE) %>%
    ungroup()
  plot_reviews <- review_words %>%
    bind_tf_idf(word, Author, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    mutate(Author = factor(Author, levels = c("Agatha Christie", "Jane Austen",
                                              "Dr. Seuss", "Stephen King",
                                              "J.R.R. Tolkien"))) %>%
    mutate(word = reorder(word, tf_idf)) %>%
    group_by(Author) %>%
    top_n(number, tf_idf) %>%
    ungroup() %>%
    mutate(word = reorder(word, tf_idf))
  plot_data<-plot_reviews[plot_reviews$Author==author,]

  plot<-ggplot(plot_data, aes(word, tf_idf, fill = word)) +
    scale_fill_manual(values =colorRampPalette(brewer.pal(8, palette))(number))+
    geom_col(show.legend = FALSE) +
    labs(title=paste("The Most Important Words Used in the", rev_text, "of Books by", author),
         x = NULL, y = "The Importance of Words") +
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.09),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12))+
    coord_flip()
  return(plot)
}


#############################################################################
bigrams_barplot<-function(df, rev_text, auth_title, variable, palette, number){
  reviews_bigrams <- df %>%
    unnest_tokens(bigram, !! rlang::sym(rev_text), token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)%>%
    unite(bigram, word1, word2, sep = " ")%>%
    count(!! rlang::sym(auth_title), bigram) %>%
    bind_tf_idf(bigram, !! rlang::sym(auth_title), n) %>%
    arrange(desc(tf_idf))
  
  reviews_bigrams$bigram<-gsub('[[:punct:] ]+',' ',reviews_bigrams$bigram)
  
  data<-reviews_bigrams[reviews_bigrams[,auth_title]==variable,]
  data_plot<-data %>%
    arrange(desc(tf_idf)) %>%
    head(number)
  plot<-ggplot(data_plot, aes(reorder(bigram, tf_idf), tf_idf, fill = bigram)) + 
    scale_fill_manual(values =colorRampPalette(brewer.pal(8, palette))(number))+
    geom_col(show.legend = FALSE) +
    labs(title=paste("The Most Important Phrases Used in the", rev_text, "(", variable, ")"),
         x = NULL, y = "The Importance of Words") + 
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.09),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12))+
    coord_flip()
  return(plot)
}

######################################################################################

bigrams_network <- function(data, rev_text, title_auth, variable, density, color) {
  df <- data[data[,title_auth]==variable,]
  bigrams_count <- df %>%
    unnest_tokens(bigram, !! rlang::sym(rev_text), token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n > density)
  set.seed(123)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams_count %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = color, size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    labs(subtitle = paste("Directed graph of common bigrams in 
                          the", rev_text, "of '", variable, "', showing those 
                          that occurred more than", density, "times")) +
    theme_graph(subtitle_face = "italic")
}

###################################################################################

pos_neg_review <- function(df, title, count, return){
  data <- filter_book(df, title)
  positive <- data %>%
    arrange(desc(Average_Polarity_Score)) %>%
    select(Reviews) %>%
    head(count)
  negative <- data %>%
    arrange(Average_Polarity_Score) %>%
    select(Reviews) %>%
    head(count)
  if(return=="positive"){return(positive)}
  else if(return=="negative"){return(negative)}
}

