
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")

# dat <- readRDS("05_22.rds")
returnPlot <- function(dat,
                       R0,
                       p.trace,
                       p.trace_app,
                       p.symp,
                       iso_delay_traced_max,
                       iso_delay_untraced_sd_max,
                       sd_contact_rate1) {
  
  output<- select100Scenarios(dat,
                              R0,
                              p.trace,
                              p.trace_app,p.symp,
                              iso_delay_traced_max,
                              iso_delay_untraced_sd_max,
                              sd_contact_rate1)
  
  
  dB <- data.frame(RQ2=double(),RQ3=double(),RQ4=double(), stringsAsFactors=FALSE)
  for (i in 0:31){
    myResult<- output %>% filter(day==i) %>% select("day","Rt")
    Q <-  quantile(myResult$Rt,na.rm = TRUE)
    dB[nrow(dB) + 1,] = c(Q[c(2,3,4)])
  }

  myDay <- (0:31)
  Q2 <- dB$RQ2
  Q3 <- dB$RQ3
  Q4 <- dB$RQ4
  kB <- tibble(myDay,Q2, Q3, Q4)
  kB <- kB %>% pivot_longer(cols = c(Q2, Q3, Q4),
                            names_to = "Activity")

  plotOut <- ggplot(kB,aes(x = myDay,
                y = value,
                col = Activity,
                group = Activity)) +
    geom_line() +
    geom_point() +ylim(0,2.8)

  return(plotOut)
}
# myPlot<- returnPlot(dat, 2.5,.5,.5,.7,2,1,.3)
# myPlot




 


