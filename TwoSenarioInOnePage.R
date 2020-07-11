library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
library(shinydashboard)
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/PlotMaker.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/SidebaryLayout.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/serverDesign.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/CreateDataFrame.R")


source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/slidebar1For2.R")
dat <- readRDS("05_22.rds")
ui <- fluidPage(
  fluidRow(
    column(3,
           
           # Copy the line below to make a slider bar
           # sliderInput("slider1", label = h3("Slider"), min = 0,
           #             max = 100, value = 50)
           slidebar1For2(ui)
    ),
    column(6,
           
           # Copy the line below to make a slider range 
           plotOutput("plot"),
           plotOutput("plot1")
           
    ), 
    column(3,
           
           # Copy the line below to make a slider range 
           # sliderInput("slider2", label = h3("Slider Range"), min = 0, 
           #             max = 100, value = c(40, 60))
           slidebar1For2(ui)
    )
    
  ),
  
  
  
  hr(),
  
  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("range"))
  )
  
)



server <- function(input, output) {
  
  # You can access the value of the widget with input$slider1, e.g.
  output$value <- renderPrint({ input$day })
  
  # You can access the values of the second widget with input$slider2, e.g.
  output$range <- renderPrint({ input$day })
  
  output$plot <- renderPlot({
    output1 <- serverDesign(dat,"Rt",input)
    output2 <- serverDesign(dat,"n.active",input)
    output1 <- output1+output2
    output
  })
  
  output$plot1 <- renderPlot({
    output <- serverDesign(dat,"n.active",input)
    output
  })
}


shinyApp(ui, server)

