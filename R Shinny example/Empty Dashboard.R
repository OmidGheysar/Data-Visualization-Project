# install.packages("shinydashboard")

library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Your Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sbMenu",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)


body <- dashboardBody(
  
  
  tabItems(
    tabItem(tabName = "dashboard",
             UiDesign()
    ),

    tabItem(tabName = "widgets",
            UiRt_Only_Manual()

    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ServerRt_for_check(input)
  })
  
  output$plot1 <- renderPlot({
    plot(rnorm(input$day),rnorm(input$day))
  })
  
  output$plot2 <- renderPlot({
    plot(rnorm(input$day),rnorm(input$day))
  })
  
}
shinyApp(ui, server)







