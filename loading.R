# library(shiny)
# install.packages("remotes")
# remotes::install_github("daattali/shinycssloaders")


ui <- fluidPage(
  
  # conditionalPanel(
  #   condition = "output.firstTable",
  #   box(width = 12,
  #       h1("The data is loading...")
  #   )
  # )
  
  actionButton("go", "Go")
  # shinycssloaders::withSpinner(
  #   plotOutput("plot")
  # )
)
server <- function(input, output) {
  
  
  # dat <- readRDS("05_22.rds")
  output$firstTable = reactive({
    return(is.null(dat))
  })
  
  showModal(modalDialog("Doing a function", footer=NULL))
  #Do the stuff here....
  #...
  dat <- readRDS("05_22.rds")
  #...
  #Finish the function
  removeModal()
  
  observeEvent(input$go, {
    showModal(modalDialog("Doing a function", footer=NULL))
    #Do the stuff here....
    #...
    dat <- readRDS("05_22.rds")
    #...
    #Finish the function
    removeModal()
  })
  
  # output$plot <- renderPlot({
  #   input$go
  #   Sys.sleep(20)
  #   plot(runif(10))
  # })
}
shinyApp(ui, server)