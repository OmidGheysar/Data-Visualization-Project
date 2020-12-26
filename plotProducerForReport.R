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

select100Scenarios <- function(dat,
                               R,
                               p.tr,
                               p.trace_ap,
                               p.sym,
                               iso_delay_traced,
                               iso_delay_untraced,
                               sd_contact) {
  
  scenarios100<-dat %>% filter(R0==R &
                                 p.trace==p.tr&
                                 p.trace_app==p.trace_ap&
                                 p.symp== p.sym&
                                 iso_delay_traced_max==iso_delay_traced&
                                 iso_delay_untraced_sd_max==iso_delay_untraced&
                                 sd_contact_rate1==sd_contact)
  return(scenarios100)
}







dat <- readRDS("Newdata.rds")


df<- select100Scenarios(dat,3,.5,.5,.7,2,1,.3)

day<-df$day
Rt_Q_05 <- df$Rt_Q_05
Rt_Q_25 <- df$Rt_Q_25
Rt_Q_50 <- df$Rt_Q_50
Rt_Q_75<- df$Rt_Q_75
Rt_Q_95<- df$Rt_Q_95
filterResult <- (data.frame(day,Rt_Q_05,Rt_Q_25,Rt_Q_50,Rt_Q_75,Rt_Q_95))

plotProducerForReport <- function(filterResult,
                                  ouptColumn,
                                  numberDay) {
  
  
  output <- filterResult
  # dat <- readRDS("Newdata.rds")
  # # output<- select100Scenarios(dat, 2,0,0,.7,2,1,.3)
  # output<- select100Scenarios(dat, 2.5,.5,.5,.7,2,1,.3)
  # ouptColumn <- "Rt"
  # numberDay <- 32
  Q_05  <-  output[[paste(ouptColumn, "Q_05", sep="_")]]
  Q_25 <-   output[[paste(ouptColumn, "Q_25", sep="_")]]
  Rt_median <-  output[[paste(ouptColumn, "Q_50", sep="_")]]
  Q_75 <-  output[[paste(ouptColumn, "Q_75", sep="_")]]
  Q_95 <-  output[[paste(ouptColumn, "Q_95", sep="_")]]
  
  results <- output
  
  # ndays <-  18   
  # numberDay <-  ndays
  ndays <- numberDay+1
  
  caption=NULL
  subtitle=NULL
  ylim=NULL
  
  if(ouptColumn == "Rt"){num <- 7}else {num <- 1}
  
  
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  p <- ggplot2::ggplot(head (results,ndays), ggplot2::aes(head(day,ndays))) +
    ggplot2::geom_ribbon(
      ggplot2::aes(y= head(Rt_median,ndays) ,
                   ymin=head(Q_05,ndays),
                   ymax=head(Q_95,ndays)),
      fill=paired.cols[num],
      alpha=0.8
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin=head(Q_25,ndays),
        ymax=head(Q_75,ndays)),
      fill=paired.cols[num+1],
      alpha=0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y=head(Rt_median,ndays)),
      color=paired.cols[num+1],
      size=1.2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y=head(Rt_median,ndays)),
      color=paired.cols[num+1],
      shape = 21,fill = "white", size = 1, stroke = 2
    ) +
    
    ggplot2::scale_y_continuous(labels = scales::comma) 
  
  # if (ouptColumn=="Rt"){
  #    # p <- p + labs(title='Reproductive number Rt')
  #    p <- p + labs(x="Day", y="Reproductive Number")
  #    # p <- p + ylim(0,2)
  #    p <- p + geom_hline(
  #      yintercept=1,
  #      linetype='dotdash'
  #    ) 
  # }else {
  #    p <- p + labs(title='Number of active cases')
  #    p <- p + labs(x="Day", y="Number of active cases")
  # }
  
  
  
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
  
  p
}


library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")





myPlot<- plotProducerForReport(filterResult,"Rt",7)
myPlot






