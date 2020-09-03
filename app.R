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
  
  v <- reactiveValues(data = NULL)
  observeEvent(input$sth,{
    if(input$sth == "Currently active cases"){
      v$data <- "n.active"
    }else if(input$sth == "New cases") {
      v$data <- "n.new"
    }else if(input$sth == "Cumulative new cases") {
      v$data <- "n.total"
    }else if(input$sth == "Isolated cases") {
      v$data <- "n.iso"
    }
  })
  
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
    outputPlot<- returnPlot(dat,v$data,
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
               "Simulation days",
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
    
    RtBasedonTwoPlots(dat, "Rt",
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
  
  v <- reactiveValues(data = NULL)
  observeEvent(input$TwoSecnario,{
    if(input$TwoSecnario == "Currently active cases"){
      v$data <- "n.active"
    }else if(input$TwoSecnario == "New cases") {
      v$data <- "n.new"
    }else if(input$TwoSecnario == "Cumulative new cases") {
      v$data <- "n.total"
    }else if(input$TwoSecnario == "Isolated cases") {
      v$data <- "n.iso"
    }
  })
  
  
  output$plotTwoScenarios2 <- renderPlotly({
    
    RtBasedonTwoPlots(dat, v$data,
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
  
  
  v <- reactiveValues(data = NULL)
  

  output$plotRt_Only_App1 <- renderPlotly({
    
    myPlot <- RtBasedonAppTrace(dat,"Rt",
                                day = input$daysforApp,
                                R = input$R0forApp,
                                p.sym = input$p.symforApp,
                                sd_contact = input$sd_contactforApp)
    myPlot
    
    
  })
  
  v <- reactiveValues(data = NULL)
  observeEvent(input$onlyApp,{
    if(input$onlyApp == "Currently active cases"){
      v$data <- "n.active"
    }else if(input$onlyApp == "New cases") {
      v$data <- "n.new"
    }else if(input$onlyApp == "Cumulative new cases") {
      v$data <- "n.total"
    }else if(input$onlyApp == "Isolated cases") {
      v$data <- "n.iso"
    }
  })
  
  output$plotRt_Only_App2 <- renderPlotly({
    
    myPlot <- RtBasedonAppTrace(dat,v$data,
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
               "simulation days",
               "Contact rate (proportion of normal)"),
      Value = as.character(c(input$R0forApp,
                             input$p.symforApp,
                             input$daysforApp,
                             input$sd_contactforApp)),
      stringsAsFactors = FALSE)
  })
  
  
  
  output$plotRt_Only_Manual1 <- renderPlotly({
    
    myPlot <- RtBasedonManualTrace(dat,"Rt",
                                   day = input$daysforManual,
                                   R = input$R0forManual,
                                   p.sym = input$p.symforManual,
                                   iso_delay_untraced= input$iso_delay_untracedforManual,
                                   sd_contact = input$sd_contactforManual)
    myPlot
    
  })
  
  v <- reactiveValues(data = NULL)
  observeEvent(input$onlyManual,{
    if(input$onlyManual == "Currently active cases"){
      v$data <- "n.active"
    }else if(input$onlyManual == "New cases") {
      v$data <- "n.new"
    }else if(input$onlyManual == "Cumulative new cases") {
      v$data <- "n.total"
    }else if(input$onlyManual == "Isolated cases") {
      v$data <- "n.iso"
    }
  })
  
  output$plotRt_Only_Manual2 <- renderPlotly({
    
    myPlot <- RtBasedonManualTrace(dat,v$data,
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
               "Simulation days",
               "Contact rate (proportion of normal)"),
      Value = as.character(c(input$R0forManual,
                             input$p.symforManual,
                             input$iso_delay_untracedforManual,
                             input$daysforManual,
                             input$sd_contactforManual)),
      stringsAsFactors = FALSE)
  })
  
  
  
  output$plotRt_App_Manual1 <- renderPlotly({
    
    myPlot <- RtBasedonAppAndManual(dat,"Rt",
                                    day = input$daysforAppManual,
                                    R = input$R0forAppManual,
                                    p.sym = input$p.symforAppManual,
                                    iso_delay_traced=input$iso_delay_tracecedforAppManual,
                                    iso_delay_untraced= input$iso_delay_untracedforAppManaual,
                                    sd_contact = input$sd_contactforAppManual)
    myPlot
    
  })
  
  v <- reactiveValues(data = NULL)
  observeEvent(input$manualApp,{
    if(input$manualApp == "Currently active cases"){
      v$data <- "n.active"
    }else if(input$manualApp == "New cases") {
      v$data <- "n.new"
    }else if(input$manualApp == "Cumulative new cases") {
      v$data <- "n.total"
    }else if(input$manualApp == "Isolated cases") {
      v$data <- "n.iso"
    }
  })
  
  output$plotRt_App_Manual2 <- renderPlotly({
    
    myPlot <- RtBasedonAppAndManual(dat,v$data,
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
               "Simulation days",
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