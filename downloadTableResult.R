

library(shiny)

ui <- fluidPage(
  # This one is linked by the id 'download'
  downloadButton('download',"Download the data for your convenience"),
  fluidRow(column(7,dataTableOutput('dto')))
)

server <- function(input,output){
  # Reactive expression with the data, in this case iris
  thedata <- reactive(iris)
  
  # output$dto <- renderDataTable({thedata()})
  output$download <- downloadHandler(
    filename = function(){"thenameOmid.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
  
}

runApp(list(ui=ui,server=server))