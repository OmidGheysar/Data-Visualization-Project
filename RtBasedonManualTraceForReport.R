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
                                 iso_delay_untraced,
                                 sd_contact)  {
  
  scenarios64000<-dat %>% filter(R0==R &
                                   p.trace_app==0&
                                   p.symp== p.sym&
                                   iso_delay_untraced_sd_max==iso_delay_untraced&
                                   sd_contact_rate1==sd_contact) %>% filter(day==days)

  return(scenarios64000)
}




RtBasedonManualTraceForReport <- function(myResult,
                                          ouptColumn){
  results <- myResult
  aes_x <- "p.trace"
  # ouptColumn <- "n.new"
  Q_05  <- paste(ouptColumn, "Q_05", sep="_")
  Q_25 <-   paste(ouptColumn, "Q_25", sep="_")
  Q_50 <-  paste(ouptColumn, "Q_50", sep="_")
  Q_75 <-  paste(ouptColumn, "Q_75", sep="_")
  Q_95 <-  paste(ouptColumn, "Q_95", sep="_")
  
  
  # dat <- readRDS("Newdata.rds")
  # results<- select64000Scenarios(dat,10 ,3,.7,1,.3)

  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  
  outputs1 <- results %>% filter(iso_delay_traced_max==1)
  outputs2 <- results %>% filter(iso_delay_traced_max==2)
  outputs3 <- results %>% filter(iso_delay_traced_max==3)
  outputs4 <- results %>% filter(iso_delay_traced_max==4)
  
  
  p <- ggplot(outputs1 ,aes(x=eval(as.name(aes_x)), y=eval(as.name(Q_50))))
  
  p <- p+ geom_ribbon(aes(ymin=outputs1[[Q_25]],ymax=outputs1[[Q_75]]),fill=paired.cols[8],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs2[[Q_25]],ymax=outputs2[[Q_75]]), fill=paired.cols[2],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs3[[Q_25]],ymax=outputs3[[Q_75]]),fill=paired.cols[6],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs4[[Q_25]],ymax=outputs4[[Q_75]]),fill=paired.cols[4],alpha=0.4)
  
  p <- p+geom_line(size = 1.5,color = paired.cols[8])
  p <- p+geom_line(aes(x=outputs2$p.trace, y=outputs2[[Q_50]]),size = 1.5,color = paired.cols[2])
  p <- p+geom_line(aes(x=outputs3$p.trace, y=outputs3[[Q_50]]),size = 1.5,color = paired.cols[6])
  p <- p+geom_line(aes(x=outputs4$p.trace, y=outputs4[[Q_50]]),size = 1.5,color = paired.cols[4])
  
  p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs2$p.trace, y=outputs2[[Q_50]]),
                    shape = 21, colour = paired.cols[2], fill = "white", size = 2, stroke = 3)
  
  p <- p+geom_point(aes(x=outputs3$p.trace, y=outputs3[[Q_50]]),
                    shape = 21, colour = paired.cols[6], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs4$p.trace, y=outputs4[[Q_50]]),
                    shape = 21, colour = paired.cols[4], fill = "white", size = 2, stroke = 3)
  # p <- p+theme_bw()
  
  
  # =========================================================
  
  p <- p+labs(y="Reproductive Number", x="",
              color="")

  p <- p + labs(title="Colors show delay to isolation for traced cases (days)")
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
  
  return(p)
  
}

# dat <- readRDS("Newdata.rds")
# filterResults<- select64000Scenarios(dat,2 ,3,.7,1,.3)
# # 
# plot<- RtBasedonManualTraceForReport(filterResults,"Rt")
# plot






