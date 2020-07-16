source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/uploadRequiredLibraries.R")
uploadRequiredLibraries()


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
  
  dat <- readRDS("05_22.rds")
  
      output$plotRtTime <- renderPlot({
        outputPlot<- returnPlot(dat,"Rt",
                                input$R0,
                                input$p.trace,
                                input$p.trace_app,
                                input$p.symp,
                                input$iso_delay_traced_max,
                                input$iso_delay_untraced_sd_max,
                                input$sd_contact_rate1)
        outputPlot
      })
      
      output$plotRtNactive <- renderPlot({
        outputPlot<- returnPlot(dat,"n.active",
                                input$R0,
                                input$p.trace,
                                input$p.trace_app,
                                input$p.symp,
                                input$iso_delay_traced_max,
                                input$iso_delay_untraced_sd_max,
                                input$sd_contact_rate1)
        outputPlot
      })
      
      output$plotRt_Only_App <- renderPlot({
        
        myPlot <- RtBasedonAppTrace(dat,
                                    day = input$daysforApp,
                                    R = input$R0forApp,
                                    p.tr = 0,
                                    p.trace_ap = 100,
                                    p.sym = input$p.symforApp,
                                    iso_delay_traced=1,
                                    iso_delay_untraced= input$iso_delay_untracedforApp,
                                    sd_contact = input$sd_contactforApp)
        myPlot

      })
      
      output$plotRt_Only_Manual <- renderPlot({
        
        myPlot <- RtBasedonManualTrace(dat,
                                       day = input$daysforManual,
                                       R = input$R0forManual,
                                       p.tr = 100,
                                       p.trace_ap = 0,
                                       p.sym = input$p.symforManual,
                                       iso_delay_traced=100,
                                       iso_delay_untraced= input$iso_delay_untracedforManual,
                                       sd_contact = input$sd_contactforManual)
        myPlot
        
      })
      
      output$plotRt_App_Manual <- renderPlot({
        
        myPlot <- RtBasedonAppAndManual(dat,
                                        day = input$daysforAppManual,
                                        R = input$R0forAppManual,
                                        p.tr = 100,
                                        p.trace_ap = 100,
                                        p.sym = input$p.symforAppManual,
                                        iso_delay_traced=input$iso_delay_tracecedforAppManual,
                                        iso_delay_untraced= input$iso_delay_untracedforAppManaual,
                                        sd_contact = input$sd_contactforAppManual)
        myPlot
        
      })
 
}

# 
# ui <- UiRt_Only_App()
# server <- ServerRt_Only_App()


shinyApp(ui, server)

