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
    
    # Action bar is located here=======================
    

    # =================================================
    
    # App title ----
    # titlePanel("Set the parameters"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar to demonstrate various slider options ----
      sidebarPanel(
        
        # Input: Simple integer interval ----
        sliderInput("R0", "R0",
                    min = 2, max = 3,
                    value = 2.5, step = 0.5),
        
        sliderInput("p.trace", "p.trace",
                    min = 0, max = 1,
                    value = .5, step = 0.25),
        
        sliderInput("p.trace_app", "p.trace_app",
                    min = 0, max = 1,
                    value = .5, step = 0.25),
        
        sliderInput("p.symp", "p.symp",
                    min = 0.6, max = 0.8,
                    value = .7, step = 0.1),
        
        sliderInput("iso_delay_traced_max", "iso_delay_traced_max",
                    min = 1, max = 4,
                    value = 2, step = 1),
        
        sliderInput("iso_delay_untraced_sd_max", "iso_delay_untraced_sd_max",
                    min = 1, max = 5,
                    value = 1, step = 4),
        
        # sliderInput("sd_contact_rate1", "sd_contact_rate1",
        #             min = 0.3, max = 0.8,
        #             value = 0,3, , step = .5),
        shinyWidgets::sliderTextInput("sd_contact_rate1","sd_contact_rate1:",
                                      choices=c(0.3, 0.6, 0.8),
                                      selected=0.3, grid = T),
        
        sliderInput("day", "day",
                    min = 0, max = 31,
                    value = 20, step = 1),
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        actionButton("do", "Run the simulation"),
        plotOutput("plot"),
        # Output: Table summarizing the values entered ----
        # start below Plot==================================================
        fluidRow(
          column(2,
                 # selectInput('x', 'X', c("day")),
                 radioButtons("radioX", label = "X",
                              choices = list("day" = "days"), 
                              selected = "days"),
                 hr(),
                 tableOutput("values"),
          ),
          column(2, offset = 2,
                 # selectInput('y', 'Y', c("Rt","n.active")),
                 radioButtons("radioY", label = "Y",
                              choices = list("Rt" = "Rt", "n.active" = "n.active", "Deafult" = 3), 
                              selected = "Rt" ),
                 selectInput('color', 'Color', c('None'))
          )
        )
        
        # end the below plot================================================
      )
    )
  ),
  
)


server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("R0 value",
               "p.trace value",
               "p.trace_app value",
               "p.symp value",
               "iso_delay_traced_max value",
               "iso_delay_untraced_sd_max value",
               "sd_contact_rate1 value",
               "days"),
      Value = as.character(c(input$R0,
                             input$p.trace,
                             input$p.trace_app,
                             input$p.symp,
                             input$iso_delay_traced_max,
                             input$iso_delay_untraced_sd_max,
                             input$sd_contact_rate1,
                             input$day)),
      stringsAsFactors = FALSE)
    

    
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  # action button =======================
  observeEvent(input$do, {
    
    
    # write your code here:
    # ==========================================
    output$plot <- renderPlot({
      
      # display shape ===========================================================
      outputColumn <- input$radioY
      output<- returnPlot(dat,outputColumn,
                          input$R0,
                          input$p.trace,
                          input$p.trace_app,
                          input$p.symp,
                          input$iso_delay_traced_max,
                          input$iso_delay_untraced_sd_max,
                          input$sd_contact_rate1)
      output
    })
    # ==========================================
    
  })
  # end of action button ================
  
}

shinyApp(ui, server)