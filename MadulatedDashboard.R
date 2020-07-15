# source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/uploadRequiredLibraries.R")
# uploadRequiredLibraries()

# dat <- readRDS("05_22.rds")

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
            h2("Dashboard tab content")
    ),

    tabItem(tabName = "widgets",
            h2("Widgets tab content"),
            ui <- UiDesign()

    )
  )
)


ui <- dashboardPage(header, sidebar, body)




server <- function (input, output, session){
  
      output$plot1 <- renderPlot({
        output<- returnPlot(dat,"Rt",
                            input$R0,
                            input$p.trace,
                            input$p.trace_app,
                            input$p.symp,
                            input$iso_delay_traced_max,
                            input$iso_delay_untraced_sd_max,
                            input$sd_contact_rate1)
        
        output
        
      })
      
      output$plot2 <- renderPlot({
        output<- returnPlot(dat,"n.active",
                            input$R0,
                            input$p.trace,
                            input$p.trace_app,
                            input$p.symp,
                            input$iso_delay_traced_max,
                            input$iso_delay_untraced_sd_max,
                            input$sd_contact_rate1)
        
        output
        
      })
 
}

# 
# ui <- UiRt_Only_App()
# server <- ServerRt_Only_App()


shinyApp(ui, server)

