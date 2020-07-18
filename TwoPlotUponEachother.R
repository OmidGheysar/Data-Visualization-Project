
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

dat <- readRDS("05_22.rds")


myPlot<- returnPlot(dat,"n.incub" ,2.5,.75,.5,.7,2,1,.3)
myPlot


output<- select100Scenarios(dat, 2.5,.75,.5,.7,2,1,.3)

yAxsis <- "n.incub"



results1 <- output%>% 
  select(day,yAxsis) %>% 
  group_by(day) %>%
  summarise(
    Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
    Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
    Rt_median = median(get(yAxsis), na.rm=TRUE),
    Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
    Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
  ) %>% ungroup()


caption=NULL
subtitle=NULL
ylim=NULL
title='Reproductive number Rt'
paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
p <- ggplot2::ggplot(results1, ggplot2::aes(x=day)) +
  ggplot2::geom_ribbon(
    ggplot2::aes(y=Rt_median,
                 ymin=Q_05,
                 ymax=Q_90),
    fill=paired.cols[7],
    alpha=0.8
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(y=Rt_median,
                 ymin=Q_25,
                 ymax=Q_75),
    fill=paired.cols[8],
    alpha=0.5
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



output<- select100Scenarios(dat, 2,.5,.5,.7,2,1,.3)

yAxsis <- "n.incub"
results2 <- output%>% 
  select(day,yAxsis) %>% 
  group_by(day) %>%
  summarise(
    Q_05 = quantile(get(yAxsis), 0.05, na.rm=TRUE),
    Q_25 = quantile(get(yAxsis), 0.25, na.rm=TRUE),
    Rt_median = median(get(yAxsis), na.rm=TRUE),
    Q_75 = quantile(get(yAxsis), 0.75, na.rm=TRUE),
    Q_90 = quantile(get(yAxsis), 0.90, na.rm=TRUE)
  ) %>% ungroup()


caption=NULL
subtitle=NULL
ylim=NULL
title='Reproductive number Rt'

p <- p+ geom_ribbon(aes(y=results2$Rt_median,
                 ymin=results2$Q_05,
                 ymax=results2$Q_90),
    fill=paired.cols[2],
    alpha=0.3
  )+
  ggplot2::geom_ribbon(
    ggplot2::aes(y=results2$Rt_median,
                 ymin=results2$Q_25,
                 ymax=results2$Q_75),
    fill=paired.cols[2],
    alpha=0.5
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

