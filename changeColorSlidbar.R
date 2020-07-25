library(shiny)

ui <- shinyUI(fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #dec4de;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    sidebarPanel(id="sidebar",
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
})

shinyApp(ui=ui,server=server)