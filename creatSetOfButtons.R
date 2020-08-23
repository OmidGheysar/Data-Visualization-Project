

library(shiny)

ui <- fluidPage(
  actionButton("one", "number of active cases"),
  tags$head(
    tags$style(HTML('#run{background-color:orange}'))
  ),
  actionButton("second", "total cases"), 
  actionButton("one", "number of active cases"),
  actionButton("second", "total cases"), 
  actionButton("one", "number of active cases"),
  actionButton("second", "total cases"), 
  actionButton("one", "number of active cases"),
  actionButton("second", "total cases"), 
  
  hr(),
  plotOutput("plot"),
             tabPanel(title = "Panel 1", value = "panel1", 
                      actionButton('jumpToP2', 'Jump to Second Tab')),
             tabPanel(title = "Panel 2", value = "panel2", 
                      actionButton('jumpToP1', 'Jump to First Tab'))
)

server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$one, {
    v$data <- rnorm(100)
  })
  
  observeEvent(input$second, {
    v$data <- runif(100)
  })  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
}

shinyApp(ui, server)

