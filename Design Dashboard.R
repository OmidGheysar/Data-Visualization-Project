# install.packages("shinydashboard")
library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
library(shinydashboard)
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
dat <- readRDS("05_22.rds")



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
    # Action bar is located here=======================
    actionButton("do", "Click Me"),
    plotOutput("plot"),
    # =================================================
    
    # App title ----
    # titlePanel("Set the parameters"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar to demonstrate various slider options ----
      sidebarPanel(
        
        # Input: Simple integer interval ----
        sliderInput("R0", "R0",
                    min = 2, max = 3,
                    value = 2.5, step = 0.5),
        
        sliderInput("p.trace", "p.trace",
                    min = 0, max = 1,
                    value = .5, step = 0.25),
        
        sliderInput("p.trace_app", "p.trace_app",
                    min = 0, max = 1,
                    value = .5, step = 0.25),
        
        sliderInput("p.symp", "p.symp",
                    min = 0.6, max = 0.8,
                    value = .7, step = 0.1),
        
        sliderInput("iso_delay_traced_max", "iso_delay_traced_max",
                    min = 1, max = 4,
                    value = 2, step = 1),
        
        sliderInput("iso_delay_untraced_sd_max", "iso_delay_untraced_sd_max",
                    min = 1, max = 5,
                    value = 1, step = 4),
        
        sliderInput("sd_contact_rate1", "sd_contact_rate1",
                    min = 0.3, max = 0.8,
                    value = 0,3, , step = .5),
        
        sliderInput("day", "day",
                    min = 0, max = 31,
                    value = 20, step = 1),
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Table summarizing the values entered ----
        tableOutput("values")
        
      )
    )
  ),
  
)


server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      Name = c("R0 value",
               "p.trace value",
               "p.trace_app value",
               "p.symp value",
               "iso_delay_traced_max value",
               "iso_delay_untraced_sd_max value",
               "sd_contact_rate1 value",
               "days"),
      Value = as.character(c(input$R0,
                             input$p.trace,
                             input$p.trace_app,
                             input$p.symp,
                             input$iso_delay_traced_max,
                             input$iso_delay_untraced_sd_max,
                             input$sd_contact_rate1,
                             input$day)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  # action button =======================
  observeEvent(input$do, {
    
    
    # write your code here:
    # ==========================================
    output$plot <- renderPlot({
      
      # display shape ===========================================================
      
      output<- select100Scenarios(dat,2,0,0,.8,1,5,.8)
    
     output<- select100Scenarios(input$R0,
                                 input$p.trace,
                                 input$p.trace_app,
                                 input$p.symp,
                                 input$iso_delay_traced_max,
                                 input$iso_delay_untraced_sd_max,
                                 input$sd_contact_rate1)
     
     
      
      dB <- data.frame(RQ2=double(),RQ3=double(),RQ4=double(),n.active=double(), stringsAsFactors=FALSE)
      for (i in 0:31){
        result<- output %>% filter(day==i) %>% select("day","Rt","n.active")
        dB[nrow(dB) + 1,] = c(quantile(result$Rt)[c(2,3,4)],mean(result$n.active))
      }
      
      days <- 0:31
      plot(days, dB$RQ3)
      
      quantile(output$Rt)[c(2,3,4)]
      p <- ggplot(data = dB, mapping =  aes(x = days, y = RQ3))
      
      p <- p + geom_point()
      p <- p + geom_line()
      p
      
      myDay <- (0:31)
      Q2 <- dB$RQ2
      Q3 <- dB$RQ3
      Q4 <- dB$RQ4
      kB <- tibble(myDay,Q2, Q3, Q4)
      # kB <- cbind(kB, as.double(0:31) )
      kB <- kB %>% pivot_longer(cols = c(Q2, Q3, Q4),
                                names_to = "Activity")
      
      ggplot(kB,aes(x = myDay,
                    y = value,
                    col = Activity,
                    group = Activity)) +
        geom_line() +
        geom_point() +ylim(0,2.8)
      
      
      

      # 
      # end of ggplot =============================================================
      

      
      # x <- runif(100)
      # y <- runif(100)
      # df <- data.frame(c("X","Y"),x,y)
      # ggplot(data=df, aes(x,y))+geom_line()
      
      
    })
    # ==========================================
    
  })
  # end of action button ================
  
}

shinyApp(ui, server)