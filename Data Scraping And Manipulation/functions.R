filter_book <- function(df, title){
  df %>% 
    filter(Title == title)
}

filter_author <- function(df, author){
  df %>% 
    filter(Author == author)
}

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "book", "books", "author", "page", "pages", "read"))
  return(corpus)
}

######################################################################################
##type - title or author, col - review or content  
wordcld <- function(text, type, col){
  palette <- ifelse(type=="Title", "Dark2", "Paired")
  title_color <- ifelse(type=="Title", "#540f91", "#116d0d")
  
  text <- iconv(text, to="ASCII", sub="")
  #creating vector source, corpus and tdm.
  vector_source <- VectorSource(text)
  corpus <- VCorpus(vector_source)
  corpus_cln <- clean_corpus(corpus)
  tdm <- TermDocumentMatrix(corpus_cln)
  tdm_mat <- as.matrix(tdm)
  
  freqs <- rowSums(tdm_mat)
  df_freq <- data.frame(terms=rownames(tdm_mat), freq=freqs, stringsAsFactors = F)
  df_freq <- df_freq[order(df_freq$freq, decreasing = T),]
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste("The Most Frequent Words in the", col), col=title_color, cex=2)
  set.seed(1)
  wordcloud(words = df_freq$terms, freq = df_freq$freq, min.freq = 1,
            max.words = 250, random.order = F, scale=c(3,0.8), colors = brewer.pal(8, palette),
            main="Title")
  
}

####################################################################################

count_feeling<-function(data, lexicon, feeling, title, number, book_review, palette){
  text_df<-as.data.frame(cbind(1:nrow(data),data))
  
  tidy_books<-text_df %>%
    unnest_tokens(word, !! rlang::sym(book_review))
  
  feeling_data <- get_sentiments(lexicon) %>% 
    filter(sentiment == feeling)
  
  data<-tidy_books %>%
    filter(Title == title) %>%
    inner_join(feeling_data) %>%
    count(word, sort = TRUE) %>%
    head(number)
  barplot<-ggplot(data, aes(x=reorder(word, n), y=n))+
    geom_bar(stat="identity", aes(fill=word))+
    coord_flip()+
    scale_fill_manual(values =colorRampPalette(brewer.pal(8, palette))(number))+
    labs(title=paste("Top ", number, " Words Used in  ", book_review, " of  '", title, "'  Associated with  '", feeling, "' Emotion", sep=""), 
         y="Frequency of Words", x="Words")+
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.008),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12),
          legend.position = "none")
  return(barplot)
} 

# There are 3 types of lexicons: bing(positive, negative), 
# nrc(trust, fear, negative, sadnes..), afinn (shows scores from -5 to 5. with + positive, with - negative)
# get_sentiments("nrc")

######################################################################################

important_words_authors<-function(data, rev_text, author, number, palette){
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
    labs(title=paste("Most Important Words Used in the", rev_text, "of Books by", author),
         x = NULL, y = "The Importance of Words") + 
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.008),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12))+
    coord_flip()
  return(plot)
}

###################################################################################


important_words_books<-function(data, rev_text, author, title, number, palette){
  data<-data[data$Author==author,]
  text_df<-as.data.frame(cbind(1:nrow(data),data))
  
  review_words <- text_df %>%
    unnest_tokens(word, !! rlang::sym(rev_text)) %>%
    count(Title, word, sort = TRUE) %>%
    ungroup()
  plot_reviews <- review_words %>%
    bind_tf_idf(word, Title, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    mutate(Title = factor(Title, levels = unique(Title))) %>% 
    mutate(word = reorder(word, tf_idf)) %>%
    group_by(Title) %>% 
    top_n(number, tf_idf) %>% 
    ungroup() %>%
    mutate(word = reorder(word, tf_idf))
  plot_data<-plot_reviews[plot_reviews$Title==title,]
  
  plot<-ggplot(plot_data, aes(word, tf_idf, fill = word)) + 
    scale_fill_manual(values =colorRampPalette(brewer.pal(8, palette))(number))+
    geom_col(show.legend = FALSE) +
    labs(title=paste("Most Important Words Used in the", rev_text, "of '", title, "' by",  author),
         x = NULL, y = "The Importance of Words") + 
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.008),
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
    labs(title=paste("Most Important Phrases Used in the", rev_text, "(", variable, ")"),
         x = NULL, y = "The Importance of Words") + 
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.008),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12))+
    coord_flip()
  return(plot)
}


######################################################################################


bigrams_network <- function(data, rev_text, title_auth, variable, density, color) {
  df<-data[data[,title_auth]==variable,]
  bigrams_count<-df %>%
    unnest_tokens(bigram, !! rlang::sym(rev_text), token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n > density)
  set.seed(123)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  graph<-bigrams_count %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = color, size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    labs(subtitle = paste("Directed graph of common bigrams in 
                          the", rev_text, "of '", variable, "', showing those 
                          that occurred more than", density, "times"))+
    theme_graph(subtitle_face = "italic")
  return(graph)
}