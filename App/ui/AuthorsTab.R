Tab2 <- tabPanel("Authors",
                 br(), br(), br(),
                 sidebarLayout(
                   sidebarPanel(
                     conditionalPanel(condition="input.authorTabSelected==1",
                                      radioButtons(inputId = "chartType", label = "Select the Chart:",
                                                   choices = list("Distributions", "Top Authors"),
                                                   selected = "Distributions"),
                                      
                                      conditionalPanel(
                                        condition = "input.chartType=='Top Authors'",
                                        selectInput(inputId = "var2", label = "Select the Category",
                                                    choices = list("Average Rating" = "Avg_rating",
                                                                   "Number of Pages" = "Pages",
                                                                   "Numer of Reviews" = "Number_of_reviews"),
                                                                   selected = "Avg_rating")
                      
                                                       )),
  
                                   
                     conditionalPanel(condition="input.authorTabSelected==2",
                                      selectInput(inputId = "author", label = em("Select an author:"),
                                                  choices = unique(books$Author), selected = "Agatha Christie"),
                                      br(),
                                      selectInput(inputId = "chartType2", label = em("Select the First Chart:"),
                                                  choices = list("Progress Over the Years" = "progress",
                                                                 "Top Books by Author" = "top",
                                                                 "Polarity Distributions" = "polarity"),
                                                  selected = "polarity"),
                                      br(),
                                      
                                      conditionalPanel(
                                        condition = "input.chartType2=='polarity'",
                                        radioButtons(inputId = "polType", label = NULL,
                                                     choices = list("Polarity Scores" ="scores", 
                                                                    "Polarity Labels" = "label"),
                                                     selected = "label")
                                      ),
                                      
                                      conditionalPanel(
                                        condition = "input.chartType2=='top' | input.chartType2=='progress'",
                                        selectInput(inputId = "var3", label = em("Select the Category"),
                                                    choices = list("Average Rating" = "Avg_rating",
                                                                   "Number of Pages" = "Pages",
                                                                   "Numer of Reviews" = "Number_of_reviews"),
                                                    selected = "Avg_rating")
                                      ),
                                      
                                      br(),
                                      selectInput(inputId = "chartType3", label = em("Select the Second Chart:"),
                                                  choices = list("Word Netwroks" = "network",
                                                                 "Important Phrases" = "bigram"),
                                                  selected = "network"),
                                      br(),
                                      conditionalPanel(
                                        condition = "input.chartType3=='bigram'",
                                        radioButtons(inputId = "textType2", label = NULL,
                                                     choices = list("In the Book Reviews" ="review", 
                                                                    "In the Book Text" = "text"),
                                                     selected = "review")
                                      )),
                                      
                     width=3),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("All Authors", value=1,
                                br(),
                                plotOutput("authorsPlot1")),
                       tabPanel("Author Details", value=2, 
                                column(12, plotOutput("plot1", height="400px")),
                                column(12, br(), br(), hr()),
                                column(12, imageOutput("plot2"))),
                       id = "authorTabSelected"
                     )
                   )
                 )
)
