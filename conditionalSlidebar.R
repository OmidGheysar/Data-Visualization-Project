
library(shiny)
myData = c("One", "Two", "Three")

ui <- fluidPage(
  
  checkboxGroupInput("choosemedia", "Choose digital", 
                     choices  = myData,
                     selected = myData),
  textOutput("myText"),
  conditionalPanel(condition = "input.choosemedia.includes('One')", 
                   sliderInput("sliderOne", "Choose your value", min=0, max=100, value=50)
  ),
  conditionalPanel(condition = "input.choosemedia.includes('Two')",
                   sliderInput("sliderTwo", "Choose your other value", 
                               
                               min=0, max=50, value=25))
)

# Define server logic 
server <- function(input, output) {
  output$myText <- renderText({input$choosemedia})
  
}
# Run the application 
shinyApp(ui = ui, server = server)