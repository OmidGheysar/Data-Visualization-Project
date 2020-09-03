UiDesign <- function() {
  
  ui <- fluidRow(
    box( width = 4, solidHeader = TRUE, color = "black",background = "navy",
      shinyWidgets::sliderTextInput("sd_contact_rate1","Contact rate (proportion of normal)",
                                    choices=c(0.3, 0.6, 0.8),
                                    selected=0.3, grid = T),
      
      sliderInput("p.trace", "Fraction of cases manually traced",
                  min = 0, max = 1,
                  value = .5, step = 0.25),
      
      sliderInput("p.trace_app", "Fraction of people using contact tracing app",
                  min = 0, max = 1,
                  value = .5, step = 0.25),
      
      sliderInput("iso_delay_traced_max", "Delay to isolation for traced cases (days)",
                  min = 1, max = 4,
                  value = 2, step = 1),
      
      
      
      selectInput("selectionMainTime", "Select something", choices = c("Descision Making Parameters", "All Parameters")),
      conditionalPanel(
        "input.selectionMainTime == 'All Parameters'",
        sliderInput("R0", "R0",
                    min = 2, max = 3,
                    value = 2.5, step = 0.5),
        
        
        
        sliderInput("p.symp", "Fraction of cases that are symptomatic",
                    min = 0.6, max = 0.8,
                    value = .7, step = 0.1),
        
        
        sliderInput("iso_delay_untraced_sd_max", "Delay to isolation for untraced & distancing cases",
                    min = 1, max = 5,
                    value = 1, step = 4),
        
        sliderInput("day", "Simulation days",
                    min = 0, max = 31,
                    value = 31, step = 1)
      ),
      hr(),
      h3("Assumptions"),
      tableOutput("TableTime")
    ),
    box(width = 8, solidHeader = TRUE, color = "black",background = "navy",
      plotlyOutput("plotRtTime"),
      br(),
      plotlyOutput("plotRtNactive"),
          navbarPage(id = "sth",inverse=TRUE,"Please choose your plot",
                     tabPanel("Currently active cases"),
                     tabPanel("New cases"),
                     tabPanel("Cumulative new cases"),
                     tabPanel("Isolated cases")
    ),
    h4(code("Given the simulation parameters, plot shows the time series of the progress of epidemiological
            factors of Covid-19 for the duration of 31 days"))
    )

  )
  

  

  return(ui)
}



  
  