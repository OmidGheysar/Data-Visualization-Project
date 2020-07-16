UiDesign <- function() {

  ui <- sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("R0", "R0",
                  min = 2, max = 3,
                  value = 2.5, step = 0.5),
      
      sliderInput("p.trace", "p.trace",
                  min = 0, max = 1,
                  value = .5, step = 0.25),
      
      sliderInput("p.trace_app", "p.trace_app",
                  min = 0, max = 1,
                  value = .5, step = 0.25),
      
      sliderInput("p.symp", "p.symp",
                  min = 0.6, max = 0.8,
                  value = .7, step = 0.1),
      
      sliderInput("iso_delay_traced_max", "iso_delay_traced_max",
                  min = 1, max = 4,
                  value = 2, step = 1),
      
      sliderInput("iso_delay_untraced_sd_max", "iso_delay_untraced_sd_max",
                  min = 1, max = 5,
                  value = 1, step = 4),
      
      # sliderInput("sd_contact_rate1", "sd_contact_rate1",
      #             min = 0.3, max = 0.8,
      #             value = 0,3, , step = .5),
      shinyWidgets::sliderTextInput("sd_contact_rate1","sd_contact_rate1:",
                                    choices=c(0.3, 0.6, 0.8),
                                    selected=0.3, grid = T),
      
      sliderInput("day", "day",
                  min = 0, max = 31,
                  value = 20, step = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("plotRtTime"),
      plotOutput("plotRtNactive"),
    )
  )
  return(ui)
}

plotRtTime <- function(input){
  outputPlot<- returnPlot(dat,"Rt",
                      input$R0,
                      input$p.trace,
                      input$p.trace_app,
                      input$p.symp,
                      input$iso_delay_traced_max,
                      input$iso_delay_untraced_sd_max,
                      input$sd_contact_rate1)
  
  return(outputPlot)
}
  
plotRtNactive <- function(input){
  outputPlot<- returnPlot(dat,"n.active",
                          input$R0,
                          input$p.trace,
                          input$p.trace_app,
                          input$p.symp,
                          input$iso_delay_traced_max,
                          input$iso_delay_untraced_sd_max,
                          input$sd_contact_rate1)
  
  return(outputPlot)
}

  
  