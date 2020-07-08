
library("ggplot2")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library("tidyverse")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/data manipulation.R")
source("C:/Users/omidg/OneDrive/Desktop/BCCCDC R shiny Project/BCCCDC-Project/SpecificPlotSetting.R")
# dat <- readRDS("05_22.rds")
# output<- select100Scenarios(dat, 2.5,.5,.5,.7,2,1,.3)
# saveRDS(output, file = "myPlotScenario.rds")

output <- readRDS(file = "myPlotScenario.rds")
nameOfcolumn <- "n.active"
nameOfcolumn <- "Rt"
dB <- data.frame(RQ2=double(),RQ3=double(),RQ4=double(), stringsAsFactors=FALSE)
for (i in 0:31){
  
  myResult<- output %>% filter(day==i) %>% select("day",nameOfcolumn)
  Q <-  quantile(myResult[,2],na.rm = TRUE)
  dB[nrow(dB) + 1,] = c(Q[c(2,3,4)])
  
}

myDay <- (0:31)
Q2 <- dB$RQ2
Q3 <- dB$RQ3
Q4 <- dB$RQ4
kB <- tibble(myDay,Q2, Q3, Q4)



kB <- kB %>% pivot_longer(cols = c(Q2, Q3, Q4),
                          names_to = "Quantile")

plotOut <- ggplot(kB,aes(x = myDay,
                         y = value,
                         col = Quantile,
                         group = Quantile)) +
  geom_line() +
  geom_point() +
geom_ribbon(aes(x=myDay, ymax=rep(Q4, each = 3), ymin=rep(Q2, each = 3)), fill="pink", alpha=.2)+
geom_point(stroke = 1)

plotOut <- SpecificSettingOfPlotDays(nameOfcolumn,plotOut)
plotOut
