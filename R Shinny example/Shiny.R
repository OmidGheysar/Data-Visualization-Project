

library(shiny)
library("ggplot2")

ui <- fluidPage(
  
  actionButton("do", "Click Me"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  observeEvent(input$do, {
    
    # write your code here:
    # ==========================================
    v <- reactiveValues(data = NULL)
    v$data <- runif(100)
    output$plot <- renderPlot({
      if (is.null(v$data)) return()
      plot(v$data)
      ggplot(as.data.frame(v$data))
      
      # dat <- readRDS("05_22.rds")
      # bigram_files <- readRDS("C:/Users/omidg/OneDrive/Desktop/R shinny app/Working Directory/BCCDC/05_22.rds")
      
      
      x <- runif(100)
      y <- runif(100)
      df <- data.frame(c("X","Y"),x,y)
      ggplot(data=df, aes(x,y))+geom_line()
      hist(x)
      
    })
    # ==========================================
    
  })
  
}

shinyApp(ui, server)
