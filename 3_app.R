#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(stringr)
library(textclean)
library(igraph)
library(networkD3)
library(utils)


hash_edge_DF <- read.table("edgeList.txt", header = TRUE)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Network of Hashtags"),
    mainPanel("Click and drag the nodes to see a subnetwork. Use your mouse to zoom in and out."),
    simpleNetworkOutput("Network", width = "1000px", height = "1000px")

)

# Define server logic
server <- function(input, output) {
    
    output$Network <- renderSimpleNetwork(
        # visualize the network using an interactive package
        simpleNetwork(hash_edge_DF, height="100px", width="100px", Source = 1, Target = 2, linkDistance = 5,charge = -900, fontSize = 14, fontFamily = "serif", linkColour = "gray", nodeColour = "blue", opacity = 0.9, zoom = T),
        env = parent.frame(), quoted = FALSE
        
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
