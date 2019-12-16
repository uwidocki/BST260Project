library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(purrr)
library(RColorBrewer)
library(ggthemes)
library(ggimage)
library(rgdal)
library(png)
library(stringr)
library(tm)



ui = fluidPage(
  
  titlePanel("Analysis of the Fifth Democratic Debate"),
  tabsetPanel(
        tabPanel("Tweets in Word Clouds",
             sidebarLayout(
               sidebarPanel(
                 p("The slider below generates a word cloud from the tweets at each timestamp. Timestamp 1 corresponds to 9:59pm EST on 11/20/2019 and Timestamp 35 corresponds to 11:28pm EST the same day."),
                 br(),
                 sliderInput("timestamp", "Timestamp:",
                             min = 1,
                             max = 35,
                             value = 1,
                             ticks = T, animate = T)
                 
                 ),
               mainPanel(
                 plotOutput("wordcloud")
                 
               )
               )
        )
  )
)



server <- function(input, output) {
  
  
  output$wordcloud <- renderPlot({
 
    d <- as.data.frame(lst[[input$timestamp]])
    set.seed(538)
    wordcloud(words = d$word, freq = d$freq, max.words=1000, random.order=FALSE, rot.per=0.25, 
              colors=brewer.pal(8, "Blues"))
    
  })
  
}


shinyApp(ui, server)
