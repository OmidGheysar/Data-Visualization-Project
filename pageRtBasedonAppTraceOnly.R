# uploadRequiredLibraries()

ui <- fluidPage(
  # Application title
  titlePanel("Parameters of Scenarios"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      sliderInput("R0",
                  "R0:",
                  min = 2,  max = 3, value = 2, step = .5),
      
      sliderInput("p.sym",
                  "p.sym:",
                  min = .6,  max = .8, value = .6, step = .1),
      sliderInput("iso_delay_untraced",
                  "iso_delay_untraced:",
                  min = 1,  max = 5, value = 1, step = 4),
      
      sliderInput("sd_contact",
                  "sd_contact:",
                  min = .3,  max = .8, value = .3, step = .5),
      
      sliderInput("days",
                  "days:",
                  min = 0,  max = 30,  value = 20),
      
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    myPlot <- RtBasedonAppTrace(dat,
                                   day = input$days,
                                   R = input$R0,
                                   p.tr = 0,
                                   p.trace_ap = 100,
                                   p.sym = input$p.sym,
                                   iso_delay_traced=1,
                                   iso_delay_untraced= input$iso_delay_untraced,
                                   sd_contact = input$sd_contact)
    myPlot
    
  })
}

shinyApp(ui, server)

