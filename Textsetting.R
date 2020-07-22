ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
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
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)


server <- function(input, output) {
  
  output$caption1 <- renderText({ 
    "You have selected this"
  })
  
  # output$caption <- renderText({
  #   paste("here you can talk about your self regulation
  #         whatever makes you happy or any kind of inner voice will be appreciate
  #         it is our destiny to be successful!!!!", input$range)
  # })
  
}

shinyApp(ui, server)
