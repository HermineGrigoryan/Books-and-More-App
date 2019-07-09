library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(plotrix)
library(textmineR)
library(tm)
library(wordcloud)
library(RWeka)
library(DT)

books <- read.csv("data/books_new.csv", stringsAsFactors = F)

Tab1 <- tabPanel("Books",
         br(), br(), br(),
         sidebarLayout(
           sidebarPanel(
             conditionalPanel(condition="input.bookTabSelected==1",
                              selectInput(inputId = "var1", label = "Select the Category",
                                          choices = list("Average Rating" = "Avg_rating",
                                                         "Number of Pages" = "Pages",
                                                         "Numer of Reviews" = "Number_of_reviews"), selected = "Number_of_reviews"),
                              br(),
                              sliderInput(inputId = "size1", label = "Size:",
                                          min = 5, max = 30, value = 15)),
             
             conditionalPanel(condition="input.bookTabSelected==2", 
                              selectInput(inputId = "book", label = em("Select a book:"),
                                          choices = books$Title, selected = "Murder on the Orient Express"),
                              br(),
                              
                              radioButtons(inputId = "outputType", label = NULL,
                                           choices = list("Visualization" ="graph", "Data Summary" = "table"),
                                           selected = "table"),
                              br(),
                              
                              conditionalPanel(
                                condition = "input.outputType == 'table'",
                                radioButtons(inputId = "reviewType", label = "Select the Review Type:",
                                             choices = list("Positive Reviews" ="positive", 
                                                            "Negative Reviews" = "negative"),
                                             selected = "positive"),
                                sliderInput(inputId = "reviewCount", label="Select the Number of Reviews:",
                                            min=1, max=10, value=5)),
                              
                              conditionalPanel(
                                condition = "input.outputType == 'graph'",
                                selectInput(inputId = "graphType", label = "Select the Graph:",
                                            choices = list("The Most Frequent Words" = "wordcloud",
                                                           "The Most Emotional Words" = "barplot"),
                                            selected = "wordcloud")),
                              
                              br(),
                              conditionalPanel(
                                condition = "input.graphType == 'wordcloud' & input.outputType == 'graph'",
                                radioButtons(inputId = "textType", label = NULL,
                                             choices = list("In the Book Reviews" ="review", 
                                                            "In the Book Text" = "text"),
                                             selected = "review")),
                              
                                
                              br(),
                              conditionalPanel(
                                condition = "input.graphType == 'barplot' & input.outputType == 'graph'",
                                radioButtons(inputId = "emotion", label = "Select the Emotion:",
                                             choices = list("Joy", "Trust", "Sadness", "Anger"),
                                             selected = "Joy"))
                              
                              ),
             
             conditionalPanel(condition="input.bookTabSelected==3", 
                              selectInput(inputId = "book1", label = em("Select the First Book:"),
                                          choices = books$Title, selected = "Pride and Prejudice"),
                              br(),
                              
                              selectInput(inputId = "book2", label = em("Select the Second Book:"),
                                          choices = books$Title, selected = "The Shining"),
                              br(),
                              
                              radioButtons(inputId = "graphType2", label = em("Select the Plot:"),
                                           choices = list("Dendrogram" = "dendrogram",
                                                          "Comparison Cloud" ="comparison"),
                                           selected = "dendrogram"),
                              br(),
                              conditionalPanel(
                                condition = "input.graphType2 == 'dendrogram' | input.graphType2 == 'comparison'",
                                radioButtons(inputId = "size", label = em("Select the word count:"),
                                             choices = list("One word" ="1", "Two words" = "2"),
                                             selected = "1"))),
             width=3),
             
                              
           
           mainPanel(
             tabsetPanel(
               tabPanel("All Books", value=1, 
                        plotOutput("topBooks")),
               
               tabPanel("Book Details", value=2, br(), br(),
                        column(5, htmlOutput("coverPhoto")),
                        column(7, br(), htmlOutput("details")),
                        column(12, br(), br(), hr(), br()),
                        column(12, dataTableOutput("table")), 
                        column(12, imageOutput("plot")),
                        column(12, br(), br(), br())),
               
               tabPanel("Compare Books", value=3,
                       plotOutput("comparePlot", height="650px")),
               id = "bookTabSelected"
             )
           )
         )
)
