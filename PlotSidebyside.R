
library(shiny)

ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do2", "Make 2 plots", value = T)
                ),
                mainPanel("main panel",
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
                          )
                )
  )
)

server <- function(input, output, session) {

  output$plotgraph1 <- renderPlot(hist(rnorm(90)))
  output$plotgraph2 <- renderPlot(hist(rnorm(100)))
}


shinyApp(ui, server)



