library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
library(shinydashboard)
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/PlotMaker.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/SidebaryLayout.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/serverDesign.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/CreateDataFrame.R")


source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/slidebar1For2.R")
# dat <- readRDS("05_22.rds")
ui <- fluidPage(
  fluidRow(
    column(3,
           slidebar1For2()
    ),
    column(6,
           plotOutput("plot1"),
           plotOutput("plot2")
           
    ), 
    column(3,
           slidebar1For3()
    )
    
  ),
  
  
  
  hr(),

  fluidRow(
    column(4, verbatimTextOutput("value")),
    column(4, verbatimTextOutput("range"))
  )
  
)



server <- function(input, output) {
  
  output$plot1 <- renderPlot({
  
  # scenarios<- select100Scenarios(dat, 2,.5,.5,.7,2,1,.3)
  scenarios<- select100Scenarios(dat,
                                 input$R012,
                                 input$p.trace12,
                                 input$p.trace_app12,
                                 input$p.symp12,
                                 input$iso_delay_traced_max12,
                                 input$iso_delay_untraced_sd_max12,
                                 input$sd_contact_rate112)
  yAxsis <- "Rt"


  results1 <- scenarios%>%
    select(day,yAxsis) %>%
    group_by(day) %>%
    summarise(
      Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
      Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
      Rt_median = median(get(yAxsis), na.rm=TRUE),
      Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
      Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
    ) %>% ungroup()

  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  p <- ggplot2::ggplot(results1, ggplot2::aes(x=day)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median,
                   ymin=Q_05,
                   ymax=Q_90),
      fill=paired.cols[7],
      alpha=0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median,
                   ymin=Q_25,
                   ymax=Q_75),
      fill=paired.cols[8],
      alpha=0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=Rt_median),
      color=paired.cols[8],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=Rt_median),
      color=paired.cols[8],
      size=3
    )





  scenarios<- select100Scenarios(dat,
                                 input$R023,
                                 input$p.trace23,
                                 input$p.trace_app23,
                                 input$p.symp23,
                                 input$iso_delay_traced_max23,
                                 input$iso_delay_untraced_sd_max23,
                                 input$sd_contact_rate123)
  yAxsis <- "Rt"


  results2 <- scenarios%>%
    select(day,yAxsis) %>%
    group_by(day) %>%
    summarise(
      Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
      Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
      Rt_median = median(get(yAxsis), na.rm=TRUE),
      Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
      Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
    ) %>% ungroup()

  p <- p+ geom_ribbon(aes(y=results2$Rt_median,
                          ymin=results2$Q_05,
                          ymax=results2$Q_90),
                      fill=paired.cols[2],
                      alpha=0.2
  )+
    ggplot2::geom_ribbon(
      ggplot2::aes(y=results2$Rt_median,
                   ymin=results2$Q_25,
                   ymax=results2$Q_75),
      fill=paired.cols[2],
      alpha=0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=results2$Rt_median),
      color=paired.cols[2],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=results2$Rt_median),
      color=paired.cols[2],
      size=3
    )


    p
  })

  output$plot2 <- renderPlot({
    p
  })
}


shinyApp(ui, server)

