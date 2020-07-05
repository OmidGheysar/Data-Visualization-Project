library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
s <- 66



ui <- fluidPage(
  
  actionButton("load", "Fetch the Data"),
  actionButton("do", "Click Me"),
  plotOutput("plotLoad"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  observeEvent(input$load, {
    
    # write your code here:
    # ==========================================
    output$plotLoad <- renderPlot({
      dat <- readRDS("05_22.rds")
      dat <<- dat + 1
      x <- runif(300)
      y <- runif(300)
      df <- data.frame(c("X","Y"),x,y)
      ggplot(data=df, aes(x,y))+geom_line()
      plot(dim(dat))
      
      
    })
    # ==========================================
  })
  
  
  
  
  observeEvent(input$do, {
    
    # write your code here:
    # ==========================================
    output$plotLoad <- renderPlot({
    
      
      # dat <- readRDS("05_22.rds")
      # display shape ===========================================================
      output<- select100Scenarios(dat,2,0,0,.8,1,5,.8)
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
      
      # end of ggplot =============================================================
      
      
      
      
      
      
      
      
      
      
      # x <- runif(100)
      # y <- runif(100)
      # df <- data.frame(c("X","Y"),x,y)
      # ggplot(data=df, aes(x,y))+geom_line()
      
    })
    # ==========================================
    
  })
  
}

shinyApp(ui, server)