library(shiny)
library(shinythemes)
library(dplyr)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(plotrix)
library(textmineR)
library(tm)
library(wordcloud)
library(RWeka)
library(DT)


number_ticks <- function(n) {function(limits) pretty(limits, n)}

var_vs_year<-function(data, page_rating, author){
  data<-data[data$Author==author,]
  if(page_rating=="Pages"){lab<-"Number of Pages"}
  else if(page_rating=="Number_of_reviews"){lab<-"Number of Reviews"}
  else{lab<-"Rating"}
  plot<-ggplot(data, aes(x=Year, y=!! rlang::sym(page_rating)))+ geom_point(size=5, aes(color=Title))+
    geom_line(size=0.5, color="blue")+
    labs(title=paste("Publication Year vs", lab, "(", author, ")"), x="Publication Year", y=lab,
         color="Title")+
    scale_y_continuous(breaks = number_ticks(8))+
    scale_x_continuous(breaks = number_ticks(10))+
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="grey", size=0.09),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12),
          legend.position = "bottom",
          legend.text = element_text(face="italic", size=12))
  return(plot)
}


barplot_count <- function(data, author, y_axis){
  data<-data[data$Author==author,]
  if(y_axis=="Avg_rating"){lab<-"Average Rating"}
  else if(y_axis=="Pages"){lab<-"Number of Pages"}
  else{lab<-"Number of Reviews"}
  data %>%
    arrange_at(y_axis, funs(desc(.))) %>%
    ggplot(aes(x=reorder(Title, !! rlang::sym(y_axis)), y=!! rlang::sym(y_axis)))+
    scale_fill_manual(values =colorRampPalette(brewer.pal(6, "RdPu"))(length(unique(data$Title))))+
    scale_y_continuous(breaks = number_ticks(8))+
    geom_bar(stat="identity", aes(fill=Title))+coord_flip()+
    labs(x="Book Title", y=lab, title=paste("Top 6 Authors by", lab))+
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="grey", size=0.09),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12),
          legend.position = "none")
    
}


boxplt <- function(){
  ggplot(reviews, aes(x=Author, y=N_Words, label=Title))+
  geom_boxplot(aes(fill=Author))+
  geom_point(size=ifelse(reviews$N_Words>1200, 3, 0),
             color=ifelse(reviews$N_Words>1200, "red", "black"))+
  scale_fill_manual(values =colorRampPalette(brewer.pal(8, "Set1"))(6))+
  geom_text_repel(data=subset(reviews, N_Words>1200))+
  coord_flip()+
  scale_y_continuous(breaks = number_ticks(8))+
  labs(title="The Distribution of the Number of Words in Reviews for Each Author", y="Number of Words in Reviews", x="Author")+
  theme(panel.background = element_rect(fill="white"),
        panel.grid = element_line(color="#ccccff", size=0.09),
        title = element_text(face="italic", size=15),
        axis.text = element_text(face = "italic", size=12),
        legend.position = "none")
}

average_stats<-function(data, y_axis){
  if(y_axis=="Avg_rating"){lab<-"Average Rating"}
  else if(y_axis=="Pages"){lab<-"Average Number of Pages"}
  else{lab<-"Average Number of Reviews"}
  data %>%
    group_by_at("Author") %>%
    summarise_at(y_axis, mean) %>%
    ggplot(aes(x=reorder(Author, !! rlang::sym(y_axis)), y=!! rlang::sym(y_axis)))+
    geom_bar(stat="identity", aes(fill=Author))+
    scale_fill_manual(values =colorRampPalette(brewer.pal(8, "Set1"))(6))+
    coord_flip()+labs(title=paste(lab, "for Each Author"),
                      x="Author", y=lab)+
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.09),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12),
          legend.position = "none")
}

top_new<-function(data, x_axis, y_axis, number){
  getPalette = colorRampPalette(brewer.pal(9, "YlOrRd"))
  if(y_axis=="Avg_rating"){lab<-"Average Rating"}
  else if(y_axis=="Pages"){lab<-"Number of Pages"}
  else if(y_axis=="Year"){lab<-"Year"}
  else{lab<-"Number of Reviews"}
  data<-data %>%
    arrange_at(y_axis, funs(desc(.))) %>%
    head(number)
  ggplot(data, aes(x=reorder(!! rlang::sym(x_axis), !! rlang::sym(y_axis)), y=!! rlang::sym(y_axis)))+
    geom_bar(stat="identity", fill=getPalette(number))+
    coord_flip()+labs(title=paste("Top", number, "Books with the Highest", lab),
                      x="Author", y=lab)+
    theme(panel.background = element_rect(fill="white"),
          panel.grid = element_line(color="#ccccff", size=0.09),
          plot.title = element_text(face="italic", size=22, margin=margin(25,0,25,0), color="#d10c0c"),
          axis.title = element_text(size=16, face="italic"),
          title = element_text(face="italic", size=15),
          axis.text = element_text(face = "italic", size=12),
          legend.position = "none")
}