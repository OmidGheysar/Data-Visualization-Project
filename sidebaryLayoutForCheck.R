
library(shiny)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    
    shinyWidgets::sliderTextInput("sd_contact_rate1","Strength of physical distancing (contact rate)",
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
      
      sliderInput("day", "day",
                  min = 0, max = 31,
                  value = 20, step = 1),
    ),
    
    hr(),
    h3("Assumpations"),
    tableOutput("values")
    
  ),
  mainPanel(
    plotOutput("plotRtTime"),
    plotOutput("plotRtNactive"),
    br(),
    br(),
    br(),
    br(),
    p("p creates a paragraph of text."),
    p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
    strong("strong() makes bold text."),
    em("em() creates italicized (i.e, emphasized) text."),
    br(),
    code("code displays your text similar to computer code"),
    div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
    br(),
    p("span does the same thing as div, but it works with",
      span("groups of words", style = "color:blue"),
      "that appear inside a paragraph.")
  )
))


server <- function(input, output, session) {
  
  output$plotRtTime <- renderPlot({
    plot(rnorm(20),rnorm(20))
    
  })
  
  output$plotRtNactive <- renderPlot({
    plot(rnorm(20),rnorm(20))
    
  })
  
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "days",
               "Delay to isolation for traced cases (days)",
               "Fraction of people using contact tracing app",
               "Fraction of cases manually traced",
               "Strength of physical distancing (contact rate)"),
      Value = as.character(c(input$R0,
                             input$p.symp,
                             input$iso_delay_untraced_sd_max,
                             input$day,
                             input$iso_delay_traced_max,
                             input$p.trace_app,
                             input$p.trace,
                             input$sd_contact_rate1)),
      stringsAsFactors = FALSE)
  })
  
  
}

shinyApp(ui, server)


