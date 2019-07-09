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


reviews <- read.csv("data/reviews.csv", stringsAsFactors = F)
reviews[,sapply(reviews,is.character)] <- sapply(
  reviews[,sapply(reviews,is.character)],
  iconv,"WINDOWS-1252","UTF-8")
authors <- read.csv("data/authors.csv", stringsAsFactors = F)
authors[,sapply(authors,is.character)] <- sapply(
  authors[,sapply(authors,is.character)],
  iconv,"WINDOWS-1252","UTF-8")
books <- read.csv("data/books_new.csv", stringsAsFactors = F)
books[,sapply(books,is.character)] <- sapply(
  books[,sapply(books,is.character)],
  iconv,"WINDOWS-1252","UTF-8")

source("ui/BooksTab.R")
source("ui/AuthorsTab.R")
source("ui/AboutTab.R")
source("general/functions2.R")
source("general/quantitative_analysis.R")

ui <- #tagList(
 # shinythemes::themeSelector(),
 navbarPage(
  theme=shinytheme("journal"),
  title = "Books & More",
  Tab1,
  Tab2,
  Tab3
)


server <- function(input, output){
#####books tab  
  ####all books
  output$topBooks <- renderPlot(
    top_new(books, "Title", input$var1, input$size1)
  )
  
  ####book details
  book <- reactive({
    filter_book(books, input$book)
  })
  
  output$coverPhoto <- renderUI(
    HTML(paste('<img src="',book()$Image,'", height="300", width="220", align="middle" hspace="40">', sep=""))
  )
  
  output$details <- renderUI(
    HTML(
      paste('<p style="font-size:120%;"> <strong>Title:</strong> ', book()$Title, '<p>', sep=""),
      paste('<p style="font-size:120%;"> <strong>Author:</strong> ', book()$Author, '<p>', sep=""),
      paste('<p style="font-size:120%;"> <strong>Average Rating:</strong> ', book()$Avg_rating, '<p>', sep=""),
      paste('<p style="font-size:120%;"> <strong>Number of Pages:</strong> ', book()$Pages, '<p>', sep=""),
      paste('<p style="font-size:120%;"> <strong>Publication Year:</strong> ', book()$Year, '<p>', sep=""),
      paste('<p style="font-size:120%;"> <strong>Genre:</strong> ', book()$Genre, '<p>', sep="")
    )
  )
  
 
  output$table <- DT::renderDataTable(
    if(input$outputType == "table"){
      datatable(pos_neg_review(reviews, input$book, input$reviewCount, input$reviewType),
                    options=list(dom="t"))
    }
  )
  
  output$plot <- renderImage(
    if(input$outputType == "graph"){
      
      if(input$graphType == "wordcloud"){
        if(input$textType == "review"){
        return(list(
          src = paste("www/", book()$Wordcloud_Reviews, sep=""),
          contentType = "image/png",
          alt = ""
        ))
      } else{
        return(list(
          src = paste("www/", book()$Wordcloud_Text, sep=""),
          contentType = "image/png",
          alt = ""
        ))
      }
     
      } else{
          if(input$emotion == "Joy"){
            return(list(
              src = paste("www/", book()$Joy_Reviews, sep=""),
              contentType = "image/png",
              alt = ""
            ))
          } else if (input$emotion == "Trust"){
            return(list(
              src = paste("www/", book()$Trust_Reviews, sep=""),
              contentType = "image/png",
              alt = ""
            ))
          } else if (input$emotion == "Sadness"){
            return(list(
              src = paste("www/", book()$Sadness_Reviews, sep=""),
              contentType = "image/png",
              alt = ""
            ))
          } else {
            return(list(
              src = paste("www/", book()$Anger_Reviews, sep=""),
              contentType = "image/png",
              alt = ""
            ))
          }
        }
    
  } else {
      return(list(
        src = "www/empty.png",
        contentType = "image/png",
        alt = ""
      ))
    },  deleteFile = FALSE
  )
  
  ####compare books
  
  output$comparePlot <- renderPlot(
    if(input$graphType2 == "dendrogram"){
      dendrogram(reviews, input$book1, input$book2, input$size)
    } else if(input$graphType2 == "comparison"){
      comparecld(reviews, input$book1, input$book2, input$size)
    } else {inp
      
    }
  )
  
  
  
#####authors tab
  ####all authors
  output$authorsPlot1 <- renderPlot(
    if(input$chartType == 'Distributions'){
      boxplt()
    } else {
      average_stats(books, input$var2)
    }
  )
  ####author details
  author <- reactive({
    filter_author(authors, input$author)
  })
  
  output$plot1 <- renderPlot(
    if(input$chartType2 == 'top'){
      barplot_count(books, input$author, input$var3)
    } else if(input$chartType2 == 'progress'){
      var_vs_year(books, input$var3, input$author)
    } else {
      if(input$polType == 'scores'){
        polarity_dist(reviews, input$author)
      } else {
        barplot_polarity(reviews, input$author)
      }
    }
  )
  
  output$plot2 <- renderImage(
    if(input$chartType3 == 'network'){
      return(list(
        src = paste("www/", author()$Networks, sep=""),
        contentType = "image/png",
        alt = ""
      ))
    } else if (input$chartType3 == 'bigram'){
      if(input$textType2 == 'review'){
        return(list(
          src = paste("www/", author()$Bigram_Reviews, sep=""),
          contentType = "image/png",
          alt = ""
        ))
      } else {
        return(list(
          src = paste("www/", author()$Bigram_Text, sep=""),
          contentType = "image/png",
          alt = ""
        ))
      }
    } else {
      return(list(
        src ="datark.png",
        contentType = "image/png",
        alt = ""
      ))
    }, deleteFile = F
  )
  
}

shinyApp(ui=ui, server=server)
