source("uploadRequiredLibraries.R")
uploadRequiredLibraries()
library(shinyWidgets)

header <- dashboardHeader(title = "COVID-19 Simulation Dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sbMenu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem(h5(HTML("Time series")), tabName = "second"),
              menuItem(h5(HTML("Time series<br/>Two scenarios")), tabName = "secondOne"),
              menuItem(h5(HTML("Contact tracing with<br/>only App")), tabName = "third"),
              menuItem(h5(HTML("Contact tracing with<br/>Manual methods only")), tabName = "fourth"),
              menuItem(h5(HTML("Contact tracing with<br/>both App and Manual<br/> methods")), tabName = "fifth")
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
            box(
              h1("Simulation of COVID-19"),
              titlePanel(title=div(img(src="covid.jpg"))),
              h4(p("Example image from Hellwell et al.",
                   tags$a(href="https://doi.org/10.1016/S2214-109X(20)30074-7", 
                          "https://doi.org/10.1016/S2214-109X(20)30074-7"),
                   " - Placeholder Only" )),
              width = 12
            ),

            fluidRow(
              box(title = "About the Model",
                  width = 12,solidHeader = TRUE, status = "primary",
                  collapsible = TRUE, collapsed = TRUE,
                  p("This model implements a",tags$a(href="https://en.wikipedia.org/wiki/Branching_process", 
                                                       "stochastic branching process") ," inspired by ",tags$a(href="https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30074-7/fulltext", 
                                                                                                               "Hellewell et al. (2020)")," to simulate the evolution of an infectious disease in a population. This simulation includes factors that add to disease growth (such as importation of cases) or limit disease growth (such as contact tracing and subsequent isolation). Disease and behaviourial parameters for each case are stochastically generated from user-defined distribution when each case is initialized. This includes properties such as time to symptom onset and time of secondary infections (serial intervals). Because our simulation runs forward one time-step at a time, disease milestones such as symptom onset or secondary infections are essentially “pre-loaded” when the case is generated but don’t actually occur in the simulation until their corresponding time-step. This allows for processes to be time-dependent and/or depend on the current state of other cases (e.g. the efficiency of contact tracing could dependon the number of active cases that require tracing) and creates snapshots of the simulation at every timestep.")
                  ),
              box(title = "Case statuses",
                  width = 12,solidHeader = TRUE, status = "primary",
                  collapsible = TRUE, collapsed = TRUE,
                  p("We consider every case to be in one of five disease states:"), 
                  p(strong("Incubation"),": This state describes cases that have been infected and could infect others. It is the initial state for all cases. All four other states are possible after the incubation stage. Typically, the case moves onto the symptomatic or asyptomatic state (based on a stochastically determined value) after the case’s incubation period (also stochastically determined) has passed. However, cases may directly move toisolated or inactive states prior to the end of their incubation period in certain cases. For instance, cases move directly to the isolated state if their isolation time is shorter than the incubation period, which couldhappen when the case is a traced contact of an isolated index case. Alternatively, if the infection duration (a user defined parameter) has passed before the end of the incubation period, then the case moves directly to the inactive state. This pathway is relatively rare, as it only occurs when the stochastic process generates a very long incubation period."),
                  p(strong("Symptomatic"),": This state describes cases determined to be symptomatic and pass their incubation period without being isolated, or exceed their infection duration. Cases in this state can cause secondary infections. For untraced cases, entering this state is an important milestone as it starts the counter for their isolation timeline. Cases in this state will move to the isolated state after their onset-to-isolation delay time has passed. However, if the case reaches the end of their infection duration before isolation occurs, then they will move to the inactive state instead."),
                  p(strong("Asymptomatic:"),"This state describes cases determined to be asymptomatic and pass their incubation period without being isolated or exceed their infection duration. Cases in this state can cause secondary infections. Traced asymptomatic cases may move to the isolated state if their isolation delay is shorterthan their infection duration. Otherwise, traced asymptomatic cases move to the inactive state at the end of their infection. Untraced asymptomatic cases can only move to the inactive state after their infection duration has passed. Note that the infection duration can be set to a very large number, in which case untraced asymptomatic cases may remain in the asymptomatic indefinitely."),
                  p(strong("Isolated"),": This state describes cases that have been isolated from the main population and can no longercause secondary infections. Traced cases can arrive at this state from any of the above states after some stochastically determined delay beyond the index case’s isolation. This delay can be as short as 0 days.Untraced symptomatic cases arrive at this state after a different stochastically determined delay beyondtheir own onset of symptoms. This is one of the two potential end states. All cases that reach this state will remain isolated for the rest of the simulation."),  
                  p(strong("Inactive"),": This state describes cases that are not isolated but are no longer considered to have the disease and are no longer causing secondary infections. Cases can arrive to this state from any of the incubation,symptomatic or asymptomatic states if they reach the end of their stochastically drawn infection duration period before advancing to the isolated state. The use of this state is optional: if the infection duration parameter is set to a very large value (e.g. longer than the total duration of the simulation) then cases will never reach this state. The purpose of this state is to avoid retaining a very large number of untraced asymptomatic cases that are no longer affecting the simulation as well as to avoid the rare situation where the serial interval distribution leads to a stochastic draw of an unrealistic and very large serial interval (in cases where this event is not meant to be included in the simulation)."), 
                  p("Summary of case statuses and pathways is shown in the following graph."),
                  titlePanel(title=div(img(src="test.JPG"))),
              ),
              box(title = "What happens in each timestep",
                  width = 12,solidHeader = TRUE, status = "primary",
                  collapsible = TRUE, collapsed = TRUE,
                  p(strong("1:"),"The overall simulation time counter and each case’s time infected counter is incremented by the time-step."),
                  p(strong("2:"),"Secondary infections that would have occurred since the last timestep are generated with all of their stochastically determined parameters drawn from their respective distributions. This list of secondary cases is kept separate from the list of current active cases in the simulation for now."),
                  p(strong("3:"),"If the time-step is a whole number, then the number of imported cases is determined and these cases aregenerated with all of their stochastically determined parameters drawn from their respective distributions.This list of imported cases is kept separate from the list of current active cases in the simulation for now."),
                  p(strong("4:"),"All cases that have been active for longer than their infection duration since the last time-step are advanced to the inactive state and are not considered for any further case status advancements."),
                  p(strong("5:"),"The remaining active cases are assessed for advancement."),
                  p(strong("*"),": Cases in the incubation state are advanced to symptomatic, asymptomatic or directly to isolated based on the timelines described above"),
                  p(strong("*"),": Then, cases in the symptomatic state are advanced to isolated if they meet the conditions."),
                  p(strong("*"),": Finally, cases in the asymptomatic state are advanced to isolated if they meet the conditions."),
                  p(strong("6:"),"Cases now in the inactive or isolated states are removed from the active cases list."),
                  p(strong("7:"),"New secondary and imported cases generated earlier are added to the bottom of the active cases list, in that order"),
                  p(strong("8:"),"End of time-step."),
                  ),
              # box(title = "About the Model",
              #     "Box content here", br(), "More box content",
              #     width = 12,solidHeader = TRUE, status = "primary",
              #     collapsible = TRUE, collapsed = TRUE,
              #     p("This package implements a stochastic branching process inspired by Hellewell et al. (2020) to simulate theevolution of an infectious disease in a population. This simulation includes factors that add to disease growth(such as importation of cases) or limit disease growth (such as contact tracing and subsequent isolation).Disease and behaviourial parameters for each case are stochastically generated from user-defined distributionwhen each case is initialized. This includes properties such as time to symptom onset and time of secondaryinfections (serial intervals). Because our simulation runs forward one time-step at a time, disease milestonessuch as symptom onset or secondary infections are essentially “pre-loaded” when the case is generated butdon’t actually occur in the simulation until their corresponding time-step. This allows for processes to be time-dependent and/or depend on the current state of other cases (e.g. the efficiency of contact tracing could dependon the number of active cases that require tracing) and creates snapshots of the simulation at every timestep"),
              # )
              ),
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
  
# Page Time series ====================================================================================================
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
  
  plot1 <- reactiveValues(data = NULL)
  observeEvent(input$sth,{
    if(input$sth == "Currently active cases"){
      plot1$data <- "n.active"
    }else if(input$sth == "New cases") {
      plot1$data <- "n.new"
    }else if(input$sth == "Cumulative new cases") {
      plot1$data <- "n.total"
    }else if(input$sth == "Isolated cases") {
      plot1$data <- "n.iso"
    }
  })
  
  output$plotRtNactive <- renderPlotly({
    outputPlot<- returnPlot(dat,plot1$data,
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
               "Contact rate (proportion of normal)",
               "Initial number of infected individuals"),
      Value = as.character(c(input$R0,
                             input$p.symp,
                             input$iso_delay_untraced_sd_max,
                             input$day,
                             input$iso_delay_traced_max,
                             input$p.trace_app,
                             input$p.trace,
                             input$sd_contact_rate1,
                             100)),
      stringsAsFactors = FALSE)
  })
  
  # Add your code -----------------------------------------------------------------------------------------------------
  output$reportForTimeSeries <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reportFotTimeSeries.Rmd")
      file.copy("reportFotTimeSeries.Rmd", tempReport, overwrite = TRUE)
      
      source("RtBasedOnplotProducerForReport.R")
      dat <- readRDS("Newdata.rds")
      filterResult<- select100Scenarios(dat,3,.5,.5,.7,2,1,.3)
      df <- filterResult
      
      dt <- data.frame(titles = c("Omid Gheysar Gharamaki for the ","1:11","1:11","1:11",
                                  "1:11","1:11","1:11","1:11","1:11","1:11","1:11"),
                       values = c(-0.8125594, -0.7590050, -0.7189301, -0.7188391, -0.5047816,
                                  -0.3439579, -0.4376782, -0.1300217, 0.9145718, 2.1844290,
                                  2021)) 
      
      rmarkdown::render(tempReport, output_file = file,
                        params = list(day = df$day,Rt_Q_05 = df$Rt_Q_05, Rt_Q_25 = df$Rt_Q_25, Rt_Q_50=df$Rt_Q_50, Rt_Q_75 = df$Rt_Q_75,Rt_Q_95 = df$Rt_Q_95,
                                      n.active_Q_05 = df$n.active_Q_05, n.active_Q_25 = df$n.active_Q_25, n.active_Q_50=df$n.active_Q_50, n.active_Q_75 = df$n.active_Q_75,n.active_Q_95 = df$n.active_Q_95,
                                      n.new_Q_05 = df$n.new_Q_05, n.new_Q_25 = df$n.new_Q_25, n.new_Q_50=df$n.new_Q_50, n.new_Q_75 = df$n.new_Q_75,n.new_Q_95 = df$n.new_Q_95,
                                      n.total_Q_05 = df$n.total_Q_05, n.total_Q_25 = df$n.total_Q_25, n.total_Q_50=df$n.total_Q_50, n.total_Q_75 = df$n.total_Q_75,n.total_Q_95 = df$n.total_Q_95,
                                      n.iso_Q_05 = df$n.iso_Q_05, n.iso_Q_25 = df$n.iso_Q_25, n.iso_Q_50=df$n.iso_Q_50, n.iso_Q_75 = df$n.iso_Q_75,n.iso_Q_95 = df$n.iso_Q_95,
                                      titles = dt$titles,values = dt$values),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # ------------------------------------------------------

# page time series two senarios ===================================================================================
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
  
  output$T1 <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "Simulation days",
               "Delay to isolation for traced cases (days)",
               "Fraction of people using contact tracing app",
               "Fraction of cases manually traced",
               "Contact rate (proportion of normal)",
               "Initial number of infected individuals"),
      Value = as.character(c(input$R012,
                             input$p.symp12,
                             input$iso_delay_untraced_sd_max12,
                             31,
                             input$iso_delay_traced_max12,
                             input$p.trace_app12,
                             input$p.trace12,
                             input$sd_contact_rate112,
                             100)),
      stringsAsFactors = FALSE)
  })
  
  output$T2 <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "Simulation days",
               "Delay to isolation for traced cases (days)",
               "Fraction of people using contact tracing app",
               "Fraction of cases manually traced",
               "Contact rate (proportion of normal)",
               "Initial number of infected individuals"),
      Value = as.character(c(input$R023,
                             input$p.symp23,
                             input$iso_delay_untraced_sd_max23,
                             31,
                             input$iso_delay_traced_max23,
                             input$p.trace_app23,
                             input$p.trace23,
                             input$sd_contact_rate123,
                             100)),
      stringsAsFactors = FALSE)
  })
  
  
  
  
  plot2 <- reactiveValues(data = NULL)
  observeEvent(input$TwoSecnario,{
    if(input$TwoSecnario == "Currently active cases"){
      plot2$data <- "n.active"
    }else if(input$TwoSecnario == "New cases") {
      plot2$data <- "n.new"
    }else if(input$TwoSecnario == "Cumulative new cases") {
      plot2$data <- "n.total"
    }else if(input$TwoSecnario == "Isolated cases") {
      plot2$data <- "n.iso"
    }
  })
  
  
  output$plotTwoScenarios2 <- renderPlotly({
    
    RtBasedonTwoPlots(dat, plot2$data,
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
  
  
  
  
  
  #===================================================================================================== 
  output$reportForTwoPlots <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "reportForTwoPlot.Rmd")
      file.copy("reportForTwoPlot.Rmd", tempReport, overwrite = TRUE)
      
      source("RtBasedonTwoPlotsForReport.R")
      dat <- readRDS("Newdata.rds")
      scenarios1<- select100Scenarios(dat,2.5,0,0,.8,1,5,.8)
      scenarios2<- select100Scenarios(dat,3,0,0,.8,1,5,.8)
      df1 <- scenarios1
      df2 <- scenarios2
      # params <- as.data.frame(params)
      
      dt1 <- data.frame(titles1 = c("Omid Gheysar Gharamaki for the ","1:11","1:11","1:11",
                                    "1:11","1:11","1:11","1:11","1:11","1:11","1:11"),
                        values1 = c(-0.8125594, -0.7590050, -0.7189301, -0.7188391, -0.5047816,
                                    -0.3439579, -0.4376782, -0.1300217, 0.9145718, 2.1844290,
                                    2021)) 
      
      dt2 <- data.frame(titles2 = c("Omid Gheysar Gharamaki for the ","2","2","2",
                                    "2","2","2","2","2","1:11","1:11"),
                        values2 = c(-0.8125594, -0.7590050, -0.7189301, -0.7188391, -0.5047816,
                                    -0.3439579, -0.4376782, -0.1300217, 0.9145718, 2.1844290,
                                    2021)) 
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = list(day = df1$day,Rt_Q_05_1 = df1$Rt_Q_05, Rt_Q_25_1 = df1$Rt_Q_25, Rt_Q_50_1=df1$Rt_Q_50, Rt_Q_75_1 = df1$Rt_Q_75,Rt_Q_95_1 = df1$Rt_Q_95,
                                      n.active_Q_05_1 = df1$n.active_Q_05, n.active_Q_25_1 = df1$n.active_Q_25, n.active_Q_50_1=df1$n.active_Q_50, n.active_Q_75_1 = df1$n.active_Q_75,n.active_Q_95_1 = df1$n.active_Q_95,
                                      n.new_Q_05_1 = df1$n.new_Q_05, n.new_Q_25_1 = df1$n.new_Q_25, n.new_Q_50_1=df1$n.new_Q_50, n.new_Q_75_1 = df1$n.new_Q_75,n.new_Q_95_1 = df1$n.new_Q_95,
                                      n.total_Q_05_1 = df1$n.total_Q_05, n.total_Q_25_1 = df1$n.total_Q_25, n.total_Q_50_1=df1$n.total_Q_50, n.total_Q_75_1 = df1$n.total_Q_75,n.total_Q_95_1 = df1$n.total_Q_95,
                                      n.iso_Q_05_1 = df1$n.iso_Q_05, n.iso_Q_25_1 = df1$n.iso_Q_25, n.iso_Q_50_1=df1$n.iso_Q_50, n.iso_Q_75_1 = df1$n.iso_Q_75,n.iso_Q_95_1 = df1$n.iso_Q_95,
                                      Rt_Q_05_2 = df2$Rt_Q_05, Rt_Q_25_2 = df2$Rt_Q_25, Rt_Q_50_2=df2$Rt_Q_50, Rt_Q_75_2 = df2$Rt_Q_75,Rt_Q_95_2 = df2$Rt_Q_95,
                                      n.active_Q_05_2 = df2$n.active_Q_05, n.active_Q_25_2 = df2$n.active_Q_25, n.active_Q_50_2=df2$n.active_Q_50, n.active_Q_75_2 = df2$n.active_Q_75,n.active_Q_95_2 = df2$n.active_Q_95,
                                      n.new_Q_05_2 = df2$n.new_Q_05, n.new_Q_25_2 = df2$n.new_Q_25, n.new_Q_50_2=df2$n.new_Q_50, n.new_Q_75_2 = df2$n.new_Q_75,n.new_Q_95_2 = df2$n.new_Q_95,
                                      n.total_Q_05_2 = df2$n.total_Q_05, n.total_Q_25_2 = df2$n.total_Q_25, n.total_Q_50_2=df2$n.total_Q_50, n.total_Q_75_2 = df2$n.total_Q_75,n.total_Q_95_2 = df2$n.total_Q_95,
                                      n.iso_Q_05_2 = df2$n.iso_Q_05, n.iso_Q_25_2 = df2$n.iso_Q_25, n.iso_Q_50_2=df2$n.iso_Q_50, n.iso_Q_75_2 = df2$n.iso_Q_75,n.iso_Q_95_2 = df2$n.iso_Q_95,
                                      titles1 = dt1$titles1,values1 = dt1$values1,titles2 = dt2$titles2,values2 = dt2$values2
                        ),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # ---------------------------------------------------------------------------------------------------
  
  
  v <- reactiveValues(data = NULL)
  
  
# page contact tracing with only app =======================================================================
  output$plotRt_Only_App1 <- renderPlotly({
    
    myPlot <- RtBasedonAppTrace(dat,"Rt",
                                day = input$daysforApp,
                                R = input$R0forApp,
                                p.sym = input$p.symforApp,
                                sd_contact = input$sd_contactforApp)
    myPlot
    
    
  })
  
  plot3 <- reactiveValues(data = NULL)
  observeEvent(input$onlyApp,{
    if(input$onlyApp == "Currently active cases"){
      plot3$data <- "n.active"
    }else if(input$onlyApp == "New cases") {
      plot3$data <- "n.new"
    }else if(input$onlyApp == "Cumulative new cases") {
      plot3$data <- "n.total"
    }else if(input$onlyApp == "Isolated cases") {
      plot3$data <- "n.iso"
    }
  })
  
  output$plotRt_Only_App2 <- renderPlotly({
    
    myPlot <- RtBasedonAppTrace(dat,plot3$data,
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
               "Contact rate (proportion of normal)",
               "Initial number of infected individuals"),
      Value = as.character(c(input$R0forApp,
                             input$p.symforApp,
                             input$daysforApp,
                             input$sd_contactforApp,
                             100)),
      stringsAsFactors = FALSE)
  })
  
  # Add your code -----------------------------------------------------------------------------------------------------
  output$reportForAppTrace <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reportForApp.Rmd")
      file.copy("reportForApp.Rmd", tempReport, overwrite = TRUE)
      
      source("RtBasedonAppTraceForReport.R")
      dat <- readRDS("Newdata.rds")
      myResult <- select64000Scenarios(dat,31 ,2,.7,.3)
      df <- myResult
      # params <- as.data.frame(params)
      
      dt <- data.frame(titles = c("Omid Gheysar Gharamaki for the ","1:11","1:11","1:11",
                                  "1:11","1:11","1:11","1:11","1:11","1:11","1:11"),
                       values = c(-0.8125594, -0.7590050, -0.7189301, -0.7188391, -0.5047816,
                                  -0.3439579, -0.4376782, -0.1300217, 0.9145718, 2.1844290,
                                  2021)) 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = list(iso_delay_untraced_sd_max = df$iso_delay_untraced_sd_max,p.trace_app = df$p.trace_app,Rt_Q_05 = df$Rt_Q_05, Rt_Q_25 = df$Rt_Q_25, Rt_Q_50=df$Rt_Q_50, Rt_Q_75 = df$Rt_Q_75,Rt_Q_95 = df$Rt_Q_95,
                                      n.active_Q_05 = df$n.active_Q_05, n.active_Q_25 = df$n.active_Q_25, n.active_Q_50=df$n.active_Q_50, n.active_Q_75 = df$n.active_Q_75,n.active_Q_95 = df$n.active_Q_95,
                                      n.new_Q_05 = df$n.new_Q_05, n.new_Q_25 = df$n.new_Q_25, n.new_Q_50=df$n.new_Q_50, n.new_Q_75 = df$n.new_Q_75,n.new_Q_95 = df$n.new_Q_95,
                                      n.total_Q_05 = df$n.total_Q_05, n.total_Q_25 = df$n.total_Q_25, n.total_Q_50=df$n.total_Q_50, n.total_Q_75 = df$n.total_Q_75,n.total_Q_95 = df$n.total_Q_95,
                                      n.iso_Q_05 = df$n.iso_Q_05, n.iso_Q_25 = df$n.iso_Q_25, n.iso_Q_50=df$n.iso_Q_50, n.iso_Q_75 = df$n.iso_Q_75,n.iso_Q_95 = df$n.iso_Q_95,
                                      titles = dt$titles,values = dt$values),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # -------------------------------------------------------------------------------------------------------------------
  
  
  # page contact tracing with manual method only ============================================================ 
  output$plotRt_Only_Manual1 <- renderPlotly({
    
    myPlot <- RtBasedonManualTrace(dat,"Rt",
                                   day = input$daysforManual,
                                   R = input$R0forManual,
                                   p.sym = input$p.symforManual,
                                   iso_delay_untraced= input$iso_delay_untracedforManual,
                                   sd_contact = input$sd_contactforManual)
    myPlot
    
  })
  
  plot4 <- reactiveValues(data = NULL)
  observeEvent(input$onlyManual,{
    if(input$onlyManual == "Currently active cases"){
      plot4$data <- "n.active"
    }else if(input$onlyManual == "New cases") {
      plot4$data <- "n.new"
    }else if(input$onlyManual == "Cumulative new cases") {
      plot4$data <- "n.total"
    }else if(input$onlyManual == "Isolated cases") {
      plot4$data <- "n.iso"
    }
  })
  
  output$plotRt_Only_Manual2 <- renderPlotly({
    
    myPlot <- RtBasedonManualTrace(dat,plot4$data,
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
               "Contact rate (proportion of normal)",
               "Initial number of infected individuals"),
      Value = as.character(c(input$R0forManual,
                             input$p.symforManual,
                             input$iso_delay_untracedforManual,
                             input$daysforManual,
                             input$sd_contactforManual,
                             100)),
      stringsAsFactors = FALSE)
  })
  
  # Add your code -----------------------------------------------------------------------------------------------------
  output$reportForManualTrace <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reportForManual.Rmd")
      file.copy("reportForManual.Rmd", tempReport, overwrite = TRUE)
      
      source("RtBasedonManualTraceForReport.R")
      dat <- readRDS("Newdata.rds")
      filterResult<- select64000Scenarios(dat,30 ,3,.7,1,.3)
      df <- filterResult
      # params <- as.data.frame(params)
      
      dt <- data.frame(titles = c("Omid Gheysar Gharamaki for the ","1:11","1:11","1:11",
                                  "1:11","1:11","1:11","1:11","1:11","1:11","1:11"),
                       values = c(-0.8125594, -0.7590050, -0.7189301, -0.7188391, -0.5047816,
                                  -0.3439579, -0.4376782, -0.1300217, 0.9145718, 2.1844290,
                                  2021)) 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = list(iso_delay_traced_max = df$iso_delay_traced_max,p.trace = df$p.trace,Rt_Q_05 = df$Rt_Q_05, Rt_Q_25 = df$Rt_Q_25, Rt_Q_50=df$Rt_Q_50, Rt_Q_75 = df$Rt_Q_75,Rt_Q_95 = df$Rt_Q_95,
                                      n.active_Q_05 = df$n.active_Q_05, n.active_Q_25 = df$n.active_Q_25, n.active_Q_50=df$n.active_Q_50, n.active_Q_75 = df$n.active_Q_75,n.active_Q_95 = df$n.active_Q_95,
                                      n.new_Q_05 = df$n.new_Q_05, n.new_Q_25 = df$n.new_Q_25, n.new_Q_50=df$n.new_Q_50, n.new_Q_75 = df$n.new_Q_75,n.new_Q_95 = df$n.new_Q_95,
                                      n.total_Q_05 = df$n.total_Q_05, n.total_Q_25 = df$n.total_Q_25, n.total_Q_50=df$n.total_Q_50, n.total_Q_75 = df$n.total_Q_75,n.total_Q_95 = df$n.total_Q_95,
                                      n.iso_Q_05 = df$n.iso_Q_05, n.iso_Q_25 = df$n.iso_Q_25, n.iso_Q_50=df$n.iso_Q_50, n.iso_Q_75 = df$n.iso_Q_75,n.iso_Q_95 = df$n.iso_Q_95,
                                      titles = dt$titles,values = dt$values),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # -------------------------------------------------------------------------------------------------------------------
  
  # contact tracing with both manual and app tracing ====================================================== 
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
  
  plot5 <- reactiveValues(data = NULL)
  observeEvent(input$manualApp,{
    if(input$manualApp == "Currently active cases"){
      plot5$data <- "n.active"
    }else if(input$manualApp == "New cases") {
      plot5$data <- "n.new"
    }else if(input$manualApp == "Cumulative new cases") {
      plot5$data <- "n.total"
    }else if(input$manualApp == "Isolated cases") {
      plot5$data <- "n.iso"
    }
  })
  
  output$plotRt_App_Manual2 <- renderPlotly({
    
    myPlot <- RtBasedonAppAndManual(dat,plot5$data,
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
               "Contact rate (proportion of normal)",
               "Initial number of infected individuals"),
      Value = as.character(c(input$R0forAppManual,
                             input$p.symforAppManual,
                             input$iso_delay_untracedforAppManaual,
                             input$daysforAppManual,
                             input$iso_delay_tracecedforAppManual,
                             input$sd_contactforAppManual,
                             100)),
      stringsAsFactors = FALSE)
  })
  
  # Add your code -----------------------------------------------------------------------------------------------------
  output$reportForAppAndManualTrace <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "reportForAppAndManual.Rmd")
      file.copy("reportForAppAndManual.Rmd", tempReport, overwrite = TRUE)
      
      source("RtBasedonAppAndManualForReport.R")
      dat <- readRDS("Newdata.rds")
      myResult <- select64000Scenarios(dat,10 ,3,.7,2,1,.3)
      df <- myResult
      # params <- as.data.frame(params)
      
      dt <- data.frame(titles = c("Omid Gheysar Gharamaki for the ","1:11","1:11","1:11",
                                  "1:11","1:11","1:11","1:11","1:11","1:11","1:11"),
                       values = c(-0.8125594, -0.7590050, -0.7189301, -0.7188391, -0.5047816,
                                  -0.3439579, -0.4376782, -0.1300217, 0.9145718, 2.1844290,
                                  2021)) 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = list(p.trace = df$p.trace,p.trace_app = df$p.trace_app,Rt_Q_05 = df$Rt_Q_05, Rt_Q_25 = df$Rt_Q_25, Rt_Q_50=df$Rt_Q_50, Rt_Q_75 = df$Rt_Q_75,Rt_Q_95 = df$Rt_Q_95,
                                      n.active_Q_05 = df$n.active_Q_05, n.active_Q_25 = df$n.active_Q_25, n.active_Q_50=df$n.active_Q_50, n.active_Q_75 = df$n.active_Q_75,n.active_Q_95 = df$n.active_Q_95,
                                      n.new_Q_05 = df$n.new_Q_05, n.new_Q_25 = df$n.new_Q_25, n.new_Q_50=df$n.new_Q_50, n.new_Q_75 = df$n.new_Q_75,n.new_Q_95 = df$n.new_Q_95,
                                      n.total_Q_05 = df$n.total_Q_05, n.total_Q_25 = df$n.total_Q_25, n.total_Q_50=df$n.total_Q_50, n.total_Q_75 = df$n.total_Q_75,n.total_Q_95 = df$n.total_Q_95,
                                      n.iso_Q_05 = df$n.iso_Q_05, n.iso_Q_25 = df$n.iso_Q_25, n.iso_Q_50=df$n.iso_Q_50, n.iso_Q_75 = df$n.iso_Q_75,n.iso_Q_95 = df$n.iso_Q_95,
                                      titles = dt$titles,values = dt$values),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # -------------------------------------------------------------------------------------------------------------------
  
}

shinyApp(ui, server)