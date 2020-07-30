# library(shiny)
# library("ggplot2")
# library(magrittr) # needs to be run every time you start R and want to use %>%
# library(dplyr)    # alternatively, this also loads %>%
# library("tidyverse")
# library(shinydashboard)
# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/PlotMaker.R")
# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/SidebaryLayout.R")
# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/serverDesign.R")
# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/CreateDataFrame.R")
# dat <- readRDS("05_22.rds")

source("slidebar1For2.R")
source("returnPlotTowScenarions.R")

TwoSenarioInOnePage <- function(){
ui <- fluidPage(
  fluidRow(
    column(3,
           slidebar1For2()
    ),
    column(6,
           plotlyOutput("plotTwoScenarios1"),
           # plotlyOutput("plotTwoScenarios2"),
           h4(code("Compare the Reproductive Number of two scenarios 
                   based upon their simulation parameters"))
           
    ), 
    column(3,
           slidebar1For3()
    )
    
  ),
  
  hr(),

  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("range"))
  )
  
)

return(ui)
}

server <- function(input, output) {

  output$plotTwoScenarios1 <- renderPlot({

  # scenarios<- select100Scenarios(dat, 2,.5,.5,.7,2,1,.3)

    returnPlotTowScenarions(dat,
                            input$R012,
                            input$p.trace12,
                            input$p.trace_app12,
                            input$p.symp12,
                            input$iso_delay_traced_max12,
                            input$iso_delay_untraced_sd_max12,
                            input$sd_contact_rate112,
                            input$R023,
                            input$p.trace23,
                            input$p.trace_app23,
                            input$p.symp23,
                            input$iso_delay_traced_max23,
                            input$iso_delay_untraced_sd_max23,
                            input$sd_contact_rate123
                            )

  })


}
# 
# 
# shinyApp(ui, server)

