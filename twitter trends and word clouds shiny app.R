library(ggplot2)
library(plyr)
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
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#### Hi Heather, please run wordcloud, Twitter trends and transcript analysis before running this shiny app!

ui = fluidPage(
  
  titlePanel("Analysis of the Fifth Democratic Debate"),
  tabsetPanel(
    tabPanel("Twitter Trends",
             sidebarLayout(
               sidebarPanel(
                 p("The interactive graph shows Twitter trends for each of the top democratic candidates and Donald Trump. Choose a politician from the dropdown menu to investigate their Twitter trends during the airing of the debate."),
                 br(),
                 
                 selectInput("politician", label = "Select a Politician",
                             choices = as.list(levels(df$politician)))
               ),
               mainPanel(
                 plotOutput("scatterPlot")
                 
               )
             )
    ),
    
    tabPanel("Tweets in Wordclouds",
             sidebarLayout(
               sidebarPanel(
                 p("To study the relationship between life expectancy and fertility rate, we will look at yearly scatter plots
                   . In the plot, click a data point to receive additional information.
                   Start by choosing a year in the slider below."),
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
  ),
  tabPanel("Tweets in Wordclouds",
           sidebarLayout(
             sidebarPanel(
               p("input later."),
               br(),
               
               selectInput("Candidate", label = "Select a Candidate",
                           choices = as.list(candidates))
             ),
             mainPanel(
               plotOutput("wordcloud1")
               
             )
           )
  )
)
)





server <- function(input, output) {
  output$scatterPlot = renderPlot({
    
    berning <- df %>% filter( politician == "Bernie Sanders")
    biden <- df %>% filter(politician == "Joe Biden")
    warren <- df %>% filter(politician == "Elizabeth Warren")
    pete <- df %>% filter(politician == "Pete Buttigieg")
    trump <- df %>% filter(politician == "Donald Trump")
    
    
    if(input$politician == "Donald Trump"){
      trends <- data.frame(x = trump$tcreated,
                           y = trump$trend,
                           image = sample("https://external-preview.redd.it/eQq8vs_OxRGKhgBl-t5ZOhAdy7pJ0dYMCQEX1nDrKbc.png?auto=webp&s=bc1a2db7a7e8428d4b12542528217072eae70405",
                                          size=35, replace = TRUE))
    }
    
    if(input$politician == "Bernie Sanders"){
      trends <- data.frame(x = berning$tcreated,
                           y = berning$trend,
                           image = sample("https://i.ya-webdesign.com/images/bernie-sanders-face-png-2.png",
                                          size=35, replace = TRUE))
    }
    
    if(input$politician == "Joe Biden"){
      trends <- data.frame(x = biden$tcreated,
                           y = biden$trend,
                           image = sample("https://www.washingtonpost.com/graphics/2018/politics/2020-presidential-hopefuls/img/cutouts/biden.png",
                                          size=35, replace = TRUE))
    }
    
    
    if(input$politician == "Elizabeth Warren"){
      trends <- data.frame(x = warren$tcreated,
                           y = warren$trend,
                           image = sample("https://www.washingtonpost.com/graphics/2018/politics/2020-presidential-hopefuls/img/cutouts/warren.png",
                                          size=35, replace = TRUE))
    }
    
    if(input$politician == "Pete Buttigieg"){
      trends <- data.frame(x = pete$tcreated,
                           y = pete$trend,
                           image = sample("https://www.washingtonpost.com/graphics/2018/politics/2020-presidential-hopefuls/img/cutouts/buttigieg.png",
                                          size=35, replace = TRUE))
    }
    
    
    trends %>% ggplot(aes(x,y), position = position_jitter(w = 0.05, h = 0)) + 
      xlab("Time (in GMT)") +
      ylab("Proportion of Tweets at Time t") +
      geom_line(aes(y = y), size = 1) +
      geom_image(aes(image=image), size=.055) +
      ggtitle("Twitter Trends During the Fifth Democratic Debate") +
      theme_fivethirtyeight() +
      theme(axis.title = element_text()) 
    
  })
  
  
  output$wordcloud <- renderPlot({
    
    d <- as.data.frame(lst[[input$timestamp]])
    set.seed(538)
    wordcloud(words = d$word, freq = d$freq, max.words=1000, random.order=FALSE, rot.per=0.25, 
              colors=brewer.pal(8, "Blues"))
    
  })
  
  output$wordcloud1 <- renderPlot({
    
    index <- match(input$Candidate, candidates)
    d <- lst_t[[index]]
    
    set.seed(538)
    wordcloud(words = d$word, freq = d$freq,
              max.words=1000, random.order=FALSE, rot.per=0.25, 
              colors=brewer.pal(8, "Blues"))
    
    
  })
  
  
  
  
}



shinyApp(ui, server)

