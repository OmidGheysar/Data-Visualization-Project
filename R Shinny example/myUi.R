
myUi <- function(){
  ui <- sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("R0", "R0",
                  min = 2, max = 3,
                  value = 2.5, step = 0.5),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
    )
  )
  
  return(ui)
}


ServerRt_for_check <- function(input){
  return(plot(rnorm(input$days),rnorm(input$days)))
}
















