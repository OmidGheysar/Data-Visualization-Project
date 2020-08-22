source("uploadRequiredLibraries.R")
uploadRequiredLibraries()
library(shinyWidgets)

header <- dashboardHeader(title = "COVID-19 Simulation Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sbMenu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem(h5(HTML("Reproductive Number<br/>Time Series")), tabName = "second"),
              menuItem(h5(HTML("Reproductive Number<br/>Time Series <br/> Two scenarios")), tabName = "secondOne"),
              menuItem(h5(HTML("Reproductive Number<br/>Only App tracing")), tabName = "third"),
              menuItem(h5(HTML("Reproductive Number<br/>Only Manual tracing")), tabName = "fourth"),
              menuItem(h5(HTML("Reproductive Number<br/>App and Manual tracing")), tabName = "fifth")
  )
)
# here is the body of dashboard
body <- dashboardBody(
  
#   tags$style("
#               body {
#     -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
#     zoom: 0.8; /* Other non-webkit browsers */
#     zoom: 80%; /* Webkit browsers */
# }
#               "),

  tabItems(
    tabItem(tabName = "dashboard",
            h1("Simulation of COVID-19"),
            titlePanel(title=div(img(src="covid4.jpg"))),
            h4(p("Example image from Hellwell et al.",
               tags$a(href="https://doi.org/10.1016/S2214-109X(20)30074-7", 
                      "https://doi.org/10.1016/S2214-109X(20)30074-7"),
                   " - Placeholder Only" ))
    ),
    
    tabItem(tabName = "second",
            ui <- UiDesign()
            
    ),
    
    tabItem(tabName = "secondOne",
            ui <- TwoSenarioInOnePage()
            
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
  
  showModal(modalDialog("Loading the data .....", footer=NULL))
  #Do the stuff here....
  #...
  # dat <- readRDS("05_22.rds")
  dat <- readRDS("Newdata.rds")
  #...
  #Finish the function
  removeModal()
  
  output$plotRtTime <- renderPlotly({
    
    outputPlot<- returnPlot(dat,"Rt",
                            input$R0,
                            input$p.trace,
                            input$p.trace_app,
                            input$p.symp,
                            input$iso_delay_traced_max,
                            input$iso_delay_untraced_sd_max,
                            input$sd_contact_rate1,
                            input$day)
    outputPlot
  })
  
  output$plotRtNactive <- renderPlotly({
    outputPlot<- returnPlot(dat,"n.active",
                            input$R0,
                            input$p.trace,
                            input$p.trace_app,
                            input$p.symp,
                            input$iso_delay_traced_max,
                            input$iso_delay_untraced_sd_max,
                            input$sd_contact_rate1,
                            input$day)
    outputPlot
  })
  
  
  output$TableTime <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "days",
               "Delay to isolation for traced cases (days)",
               "Fraction of people using contact tracing app",
               "Fraction of cases manually traced",
               "Contact rate (proportion of normal)"),
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
  
  
  output$plotTwoScenarios1 <- renderPlotly({
    
    RtBasedonTwoPlots(dat,
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
  
  output$plotRt_Only_App <- renderPlotly({
    
    myPlot <- RtBasedonAppTrace(dat,
                                day = input$daysforApp,
                                R = input$R0forApp,
                                p.sym = input$p.symforApp,
                                sd_contact = input$sd_contactforApp)
    myPlot
    
  })
  
  
  output$tableApp <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "days",
               "Contact rate (proportion of normal)"),
      Value = as.character(c(input$R0forApp,
                             input$p.symforApp,
                             input$daysforApp,
                             input$sd_contactforApp)),
      stringsAsFactors = FALSE)
  })
  
  
  
  output$plotRt_Only_Manual <- renderPlotly({
    
    myPlot <- RtBasedonManualTrace(dat,
                                   day = input$daysforManual,
                                   R = input$R0forManual,
                                   p.sym = input$p.symforManual,
                                   iso_delay_untraced= input$iso_delay_untracedforManual,
                                   sd_contact = input$sd_contactforManual)
    myPlot
    
  })
  
  output$tableManual <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "days",
               "Contact rate (proportion of normal)"),
      Value = as.character(c(input$R0forManual,
                             input$p.symforManual,
                             input$iso_delay_untracedforManual,
                             input$daysforManual,
                             input$sd_contactforManual)),
      stringsAsFactors = FALSE)
  })
  
  
  
  output$plotRt_App_Manual <- renderPlotly({
    
    myPlot <- RtBasedonAppAndManual(dat,
                                    day = input$daysforAppManual,
                                    R = input$R0forAppManual,
                                    p.sym = input$p.symforAppManual,
                                    iso_delay_traced=input$iso_delay_tracecedforAppManual,
                                    iso_delay_untraced= input$iso_delay_untracedforAppManaual,
                                    sd_contact = input$sd_contactforAppManual)
    myPlot
    
  })
  
  output$tableAppManual <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "days",
               "Delay to isolation for traced cases (days)",
               "Contact rate (proportion of normal)"),
      Value = as.character(c(input$R0forAppManual,
                             input$p.symforAppManual,
                             input$iso_delay_untracedforAppManaual,
                             input$daysforAppManual,
                             input$iso_delay_tracecedforAppManual,
                             input$sd_contactforAppManual)),
      stringsAsFactors = FALSE)
  })
  
}

shinyApp(ui, server)