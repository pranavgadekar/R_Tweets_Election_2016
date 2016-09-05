library(shiny)

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
      tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                      "))
    ),
    
    headerPanel("Tweet Analysis for USA Presidential Elections"),
            
    sidebarLayout(
      sidebarPanel(
        
       selectInput("Candidates","Please select a candidate: ",
                   choices = c("All","Barack Obama","Donald Trump","George W. Bush","Marco Rubio","Ted Cruz")),
       actionButton("do", "Fetch Tweets")
       ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Analysis of stored tweets", plotOutput("sentiment")),
          tabPanel("Analysis of live tweets", plotOutput("sentimentA")),
          tabPanel("Summary", verbatimTextOutput("summary"))
        ),
        plotOutput("pie")
      )
      
      )
    
  )
  
)