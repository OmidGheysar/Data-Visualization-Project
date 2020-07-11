# install.packages("shinydashboard")
# library(plotly)
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


dat <- readRDS("05_22.rds")

ui <- dashboardPage(
  dashboardHeader(title = "COVID 19 Simulatoin"),
  dashboardSidebar(  
    # =============================================
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
      # =============================================
    )
  ),
  dashboardBody(
    UiDesign(ui)
  ),
  
)


server <- function(input, output) {

  # Show the values in an HTML table ----
  output$values <- renderTable({
    # Reactive expression to create data frame of all input values ----
    sliderValues <- reactive({
      createDataFrame(input)
    })
    sliderValues()
  })
  # action button

  
  observeEvent(input$do, {
    output$plot1 <- renderPlot({
      output <- serverDesign(dat,"Rt",input)
      output

    })
    
    output$plot2 <- renderPlot({
      output <- serverDesign(dat,"n.active",input)
      output
      
    })
    
  })
}

shinyApp(ui, server)