# install.packages("plotly")
library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  plotlyOutput("distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(iris, aes(Sepal.Width, Petal.Width)) + 
      geom_line() + 
      geom_point()
  })
}

shinyApp(ui = ui, server = server)