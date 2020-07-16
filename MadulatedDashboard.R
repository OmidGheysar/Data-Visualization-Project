# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/uploadRequiredLibraries.R")
# uploadRequiredLibraries()

# dat <- readRDS("05_22.rds")

header <- dashboardHeader(title = "COVID-19 Simulation Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sbMenu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem(h5(HTML("Reproductive Number<br/>Time Series")), icon = icon(""), tabName = "second"),
              menuItem(h5(HTML("Reproductive Number<br/>Only App tracing")), tabName = "third"),
              menuItem(h5(HTML("Reproductive Number<br/>Only Manual tracing")), tabName = "fourth"),
              menuItem(h5(HTML("Reproductive Number<br/>App and Manual tracing")), tabName = "fifth")
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Dashboard Main Page")
    ),

    tabItem(tabName = "second",
            ui <- UiDesign()

    ),
    tabItem(tabName = "third",
            ui <- UiRt_Only_App()
            
    ),
    tabItem(tabName = "fourth",
            ui <- UiRt_Only_Manual()
            
    ),
    tabItem(tabName = "fifth",
            ui <- UiRt_App_Manual()
            
    )
  )
)


ui <- dashboardPage(header, sidebar, body)




server <- function (input, output, session){
  
      output$plotRtTime <- renderPlot({
        plotRtTime(input)
        
      })
      
      output$plotRtNactive <- renderPlot({
        plotRtNactive(input)
        
      })
      
      output$plotRt_Only_App <- renderPlot({
        plotRt_Only_App(input)

      })
      
      output$plotRt_Only_Manual <- renderPlot({
        plotRt_Only_Manual(input)
        
      })
      
      output$plotRt_App_Manual <- renderPlot({
        plotRt_App_Manual(input)
        
      })
 
}

# 
# ui <- UiRt_Only_App()
# server <- ServerRt_Only_App()


shinyApp(ui, server)

