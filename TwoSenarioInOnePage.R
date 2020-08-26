TwoSenarioInOnePage <- function(){
ui <- fluidPage(
  fluidRow(
    box(
      title = "Scenario with blue color", width = 3, solidHeader = TRUE, status = "primary",
       background = "navy",
      slidebar1For2()
    ),
    
    box(width = 6, solidHeader = FALSE, status = "info",
      color = "black",background = "navy",
    # column(6,
           plotlyOutput("plotTwoScenarios1"),
           br(),
           plotlyOutput("plotTwoScenarios2"),
           h4(code("Compare the Reproductive Number of two scenarios 
                   based upon their simulation parameters"))

           # ),

    ),
    box(
      title = "Scenario with orange color", width = 3, solidHeader = TRUE, status = "warning",
      background = "navy",
      slidebar1For3()
    ),

  ),
  
  hr(),

  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("range"))
  )
  
)

return(ui)
}

server <- function(input, output) {

  output$plotTwoScenarios1 <- renderPlot({

  # scenarios<- select100Scenarios(dat, 2,.5,.5,.7,2,1,.3)

    returnPlotTowScenarions(dat,
                            input$R012,
                            input$p.trace12,
                            input$p.trace_app12,
                            input$p.symp12,
                            input$iso_delay_traced_max12,
                            input$iso_delay_untraced_sd_max12,
                            input$sd_contact_rate112,
                            input$R023,
                            input$p.trace23,
                            input$p.trace_app23,
                            input$p.symp23,
                            input$iso_delay_traced_max23,
                            input$iso_delay_untraced_sd_max23,
                            input$sd_contact_rate123
                            )

  })


}


