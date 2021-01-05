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



select64000Scenarios <- function(dat,
                                 days,
                                 R,
                                 p.sym,
                                 iso_delay_traced,
                                 iso_delay_untraced,
                                 sd_contact) {
  
  scenarios64000<-dat %>% filter(R0==R &
                                   p.symp== p.sym&
                                   iso_delay_traced_max==iso_delay_traced&
                                   iso_delay_untraced_sd_max==iso_delay_untraced&
                                   sd_contact_rate1==sd_contact) %>%filter(day==days)
  return(scenarios64000)
}


RtBasedonAppAndManualForReport <- function(myResult,ouptColumn){
  
  

  
  aes_x <- "p.trace"
  # dat <- readRDS("Newdata.rds")
  # results<- select64000Scenarios(dat,10 ,3,.7,2,1,.3)
  # ouptColumn <- "n.active"
  Q_05  <- paste(ouptColumn, "Q_05", sep="_")
  Q_25 <-   paste(ouptColumn, "Q_25", sep="_")
  Q_50 <-  paste(ouptColumn, "Q_50", sep="_")
  Q_75 <-  paste(ouptColumn, "Q_75", sep="_")
  Q_95 <-  paste(ouptColumn, "Q_95", sep="_")
  
  results<- myResult
  
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  outputs1 <- results %>% filter(p.trace_app==0)
  outputs2 <- results %>% filter(p.trace_app==.25)
  outputs3 <- results %>% filter(p.trace_app==.50)
  outputs4 <- results %>% filter(p.trace_app==.75)
  outputs5 <- results %>% filter(p.trace_app==1)
  
  p <- ggplot(outputs1 ,aes(x=eval(as.name(aes_x)), y=eval(as.name(Q_50))))
  
  p <- p+ geom_ribbon(aes(ymin=outputs1[[Q_25]],ymax=outputs1[[Q_75]] ),fill=paired.cols[8],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs2[[Q_25]],ymax=outputs2[[Q_75]]), fill=paired.cols[2],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs3[[Q_25]],ymax=outputs3[[Q_75]]),fill=paired.cols[6],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs4[[Q_25]],ymax=outputs4[[Q_75]]),fill=paired.cols[4],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs5[[Q_25]],ymax=outputs5[[Q_75]]),fill=paired.cols[5],alpha=0.4)
  
  p <- p+geom_line(size = 2,color = paired.cols[8])
  p <- p+geom_line(aes(x=outputs2$p.trace, y=outputs2[[Q_50]]),size = 2,color = paired.cols[2])
  p <- p+geom_line(aes(x=outputs3$p.trace, y=outputs3[[Q_50]]),size = 2,color = paired.cols[6])
  p <- p+geom_line(aes(x=outputs4$p.trace, y=outputs4[[Q_50]]),size = 2,color = paired.cols[4])
  p <- p+geom_line(aes(x=outputs5$p.trace, y=outputs5[[Q_50]]),size = 2,color = paired.cols[5])
  
  p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs2$p.trace, y=outputs2[[Q_50]]),
                    shape = 21, colour = paired.cols[2], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs3$p.trace, y=outputs3[[Q_50]]),
                    shape = 21, colour = paired.cols[6], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs4$p.trace, y=outputs4[[Q_50]]),
                    shape = 21, colour = paired.cols[4], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs5$p.trace, y=outputs5[[Q_50]]),
                    shape = 21, colour = paired.cols[5], fill = "white", size = 2, stroke = 3)
  # p <- p+theme_bw()
  
  
  # ===========================================


  p <- p + labs(title="Colors show the level of app tracing")
  # p <- ggplotly(p)
  x <- list(
    title = "Fraction of cases manually traced"
  )
  
  if(ouptColumn=="Rt"){
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
  
  
  p <- p+labs(
    title = "<span style='color:#FF7F00'>0%</span>, 
    <span style='color:#1F78B4'>25%</span>,
    <span style='color:#E31A1C'>50%</span>,
    <span style='color:#33A02C'>75%</span>, and
    <span style='color:#FB9A99'>100%</span>,
    of app trace"
  ) +
    theme(
      plot.title = element_markdown()
    )
  
  p <- p + labs(y=y, x="Fraction of cases manualy traced", 
                color="")
  
  return(p)
  
}


# dat <- readRDS("Newdata.rds")
# myResult <- select64000Scenarios(dat,10 ,3,.7,2,1,.3)
# plot <- RtBasedonAppAndManualForReport(myResult,"Rt")
# 
# plot
