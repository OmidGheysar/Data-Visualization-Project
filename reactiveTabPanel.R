n <- 200


# Define the UI
ui <- bootstrapPage(
  numericInput('n', 'Number of obs', n),
  plotOutput('plot'),
  navbarPage(id = "sth",inverse=TRUE,"Please",
             tabPanel("Plot"),
             tabPanel("Summary"),
             tabPanel("Table"),
             tabPanel("Plot"),
             tabPanel("Summary"),
             tabPanel("Table"),
             tabPanel("Plot"),
             tabPanel("Summary"),
             tabPanel("Table"),
             tabPanel("Plot"),
             tabPanel("Summary"),
             tabPanel("Table")
             # )
  )
)


# Define the server code
server <- function(input, output) {
  
  observeEvent(input$sth,{
    if(input$sth == "Plot"){
      output$plot <- renderPlot({
        hist(runif(100))
      })
    }else if(input$sth == "Summary") {
      output$plot <- renderPlot({
        hist(rnorm(1000))
      })
    }
  })



}

# Return a Shiny app object
shinyApp(ui = ui, server = server)