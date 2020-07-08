
# install.packages("shinyWidgets")
library(shinyWidgets)
library(shiny)
ui <- fluidPage(
  
  shinyWidgets::sliderTextInput("pvalue2","PValue:",
                                choices=c(0.3, 0.6, 0.8),
                                selected=0.3, grid = T),
  textOutput("selected_var")
)
server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    input$pvalue2
  })
  
  
}
shinyApp(ui, server)