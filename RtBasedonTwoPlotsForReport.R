library(shinyWidgets)
library(plotly)
library(shiny)
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
library(shinydashboard)
library(babynames)
library(ggtext)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
# 
# scenarios1<- select100Scenarios(dat,
#                                 inputR012,
#                                 inputp.trace12,
#                                 inputp.trace_app12,
#                                 inputp.symp12,
#                                 inputiso_delay_traced_max12,
#                                 inputiso_delay_untraced_sd_max12,
#                                 inputsd_contact_rate112)
# 
# scenarios2<- select100Scenarios(dat,
#                                 inputR023,
#                                 inputp.trace23,
#                                 inputp.trace_app23,
#                                 inputp.symp23,
#                                 inputiso_delay_traced_max23,
#                                 inputiso_delay_untraced_sd_max23,
#                                 inputsd_contact_rate123)


RtBasedonTwoPlotsForReport <- function( ouptColumn,
                                       scenarios1,
                                       scenarios2
) {
  

  
  # dat <- readRDS("Newdata.rds")
  # scenarios1<- select100Scenarios(dat,2,0,0,.8,1,5,.8)
  results1 <- scenarios1
  # ouptColumn <- "n.active"
  Q_05_scenarios1  <-  scenarios1[[paste(ouptColumn, "Q_05", sep="_")]]
  Q_25_scenarios1 <-   scenarios1[[paste(ouptColumn, "Q_25", sep="_")]]
  Rt_median_scenarios1 <-  scenarios1[[paste(ouptColumn, "Q_50", sep="_")]]
  Q_75_scenarios1 <-  scenarios1[[paste(ouptColumn, "Q_75", sep="_")]]
  Q_90_scenarios1 <-  scenarios1[[paste(ouptColumn, "Q_95", sep="_")]]
  
  
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  p <- ggplot2::ggplot(results1, ggplot2::aes(x=day)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median_scenarios1,
                   ymin=Q_05_scenarios1,
                   ymax=Q_90_scenarios1),
      fill=paired.cols[7],
      alpha=0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median_scenarios1,
                   ymin=Q_25_scenarios1,
                   ymax=Q_75_scenarios1),
      fill=paired.cols[8],
      alpha=0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=Rt_median_scenarios1),
      color=paired.cols[8],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=Rt_median_scenarios1),
      color=paired.cols[8],
      shape = 21,fill = "white", size = 1, stroke = 2
    )
  
  # scenarios2<- select100Scenarios(dat,3,1,1,.8,1,5,.8)
  # ouptColumn <- "n.active"
  results2 <- scenarios2
  
  Q_05_scenarios2  <-  scenarios2[[paste(ouptColumn, "Q_05", sep="_")]]
  Q_25_scenarios2 <-   scenarios2[[paste(ouptColumn, "Q_25", sep="_")]]
  Rt_median_scenarios2 <-  scenarios2[[paste(ouptColumn, "Q_50", sep="_")]]
  Q_75_scenarios2 <-  scenarios2[[paste(ouptColumn, "Q_75", sep="_")]]
  Q_90_scenarios2 <-  scenarios2[[paste(ouptColumn, "Q_95", sep="_")]]
  
  p <- p+ geom_ribbon(aes(y=Rt_median_scenarios2,
                          ymin=Q_05_scenarios2 ,
                          ymax=Q_90_scenarios2),
                      fill=paired.cols[2],
                      alpha=0.2
  )+
    ggplot2::geom_ribbon(
      ggplot2::aes(y=Rt_median_scenarios2,
                   ymin=Q_25_scenarios2,
                   ymax=Q_75_scenarios2),
      fill=paired.cols[2],
      alpha=0.3
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=Rt_median_scenarios2),
      color=paired.cols[2],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=Rt_median_scenarios2),
      color=paired.cols[2],
      shape = 21,fill = "white", size = 1, stroke = 2
    )
  # p <- p + labs(x="Day", y="Reproductive Number I am here!")
  # p <- p+theme_bw()
  # return(p)
  if(ouptColumn=="Rt"){
    p <- p + geom_hline(yintercept=1,
                        linetype='dotdash',
                        alpha=0.6)
    y <- list(
      title = "Reproductive Number"
    )
  }else if(ouptColumn=="n.active"){
    y <- list(
      title = "Currently Active Cases"
    )
  }else if(ouptColumn=="n.new"){
    y <- list(
      title = "New Cases"
    )
  }else if(ouptColumn=="n.total"){
    y <- list(
      title = "Cumulative New Cases"
    )
  }else if(ouptColumn=="n.iso"){
    y <- list(
      title = "Isolated Cases"
    )
  }
  
  p <- p + labs(y="", x="", 
                color="")
  # p <- p + labs(title="Colors show the level of app tracing")
  # p <- ggplotly(p)
  # x <- list(
  #   title = "Day"
  # )
  # 
  
  # return(ggplotly(p))
  return(p )
  
  
}

# scenarios1<- select100Scenarios(dat,2,0,0,.8,1,5,.8)
# 
# scenarios2<- select100Scenarios(dat,3,0,0,.8,1,5,.8)
# myPlot<- RtBasedonTwoPlotsForReport("Rt",scenarios1, scenarios2)
# myPlot
