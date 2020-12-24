library(babynames)
library(ggtext)


data(mtcars)
mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("Manual", "Automatic"))
df <- mtcars


ivs <- c("cyl", "disp")
dvs <- c("mpg", "qsec")
sth <- 100
rmarkdown::render("paramPass.Rmd", 
                  params = list(df = df, ivs = ivs, dvs = dvs, sth = sth))


plotMaker <- function() {
  hist(rnorm(1000))
  return(0)
}


library(babynames)
nms <- filter(babynames, name %in% c("Sam", "Alex"))
p <- ggplot(nms) + 
  geom_line(aes(year, prop, color = sex, linetype = name))

ggplotly(p, dynamicTicks = TRUE) %>%
  rangeslider() %>%
  layout(hovermode = "x")

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + geom_smooth()+  annotate(geom="text", x=3, y=30, label="Scatter plot",
                                           color="red")
ggplotly(p)






plotMaker1 <- function() {

  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() + geom_smooth()+   
    theme_minimal() 
    # xlab("x axises") +
    # ylab("y axises") +
    # # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))+ 
    # labs(title = "#python and #rstats: Comparing 1,000 random tweets")
    
    
    p+labs(
      title = "<span style='color:#ff8c00'>#python</span> and 
    <span style='color:#346299'>#rstats</span>: Comparing 1,000 random tweets"
    ) +
    theme(
      plot.title = element_markdown()
    )


}

plotMaker2 <- function() {
  nms <- filter(babynames, name %in% c("Sam", "Alex")) 
  ggplot(nms) + 
    geom_line(aes(year, prop, color = sex, linetype = name))
}


plotMaker3 <- function() {
  aes_x <- "p.trace"
  dat <- readRDS("Newdata.rds")
  results<- select64000Scenarios(dat,10 ,3,.7,2,1,.3)
  
  
  # dim(dat)
  # 
  # datFilter <- dat %>% filter(day==10) %>% filter(R0==3) %>%  filter(p.symp==0.7) %>% filter(iso_delay_traced_max==2) %>% 
  #   filter(iso_delay_untraced_sd_max==1) %>% filter(sd_contact_rate1==.3)
  # 
  # dim(datFilter)
  # 
  # results <- datFilter
  # 

  ouptColumn <- "n.active"
  Q_05  <- paste(ouptColumn, "Q_05", sep="_")
  Q_25 <-   paste(ouptColumn, "Q_25", sep="_")
  Q_50 <-  paste(ouptColumn, "Q_50", sep="_")
  Q_75 <-  paste(ouptColumn, "Q_75", sep="_")
  Q_95 <-  paste(ouptColumn, "Q_95", sep="_")
  
  
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
  p
  
  p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs2$p.trace, y=outputs2[[Q_50]]),
                    shape = 21, colour = paired.cols[2], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs3$p.trace, y=outputs3[[Q_50]]),
                    shape = 21, colour = paired.cols[6], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs4$p.trace, y=outputs4[[Q_50]]),
                    shape = 21, colour = paired.cols[4], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs5$p.trace, y=outputs5[[Q_50]]),
                    shape = 21, colour = paired.cols[5], fill = "white", size = 2, stroke = 3)
  
  p <- p + labs(y="Omid", x="Neo",
                color="")+   
    theme_minimal() 
  
  p+labs(
    title = "<span style='color:#ff8c00'>#python</span> and 
    <span style='color:#346299'>#rstats</span>: Comparing 1,000 random tweets"
  ) +
    theme(
      plot.title = element_markdown()
    )
  
}
# you need this funciton be saved: plotProducerForReport
plotMaker4 <- function() {
  myPlot<- plotProducerForReport(dat,"Rt" ,3,.5,.5,.7,2,1,.3,20)
  myPlot
}



plotMaker5 <- function() {
  myPlot<- RtBasedonTwoPlotsForReport(dat,"Rt",2,0,0,.8,1,5,.8, 3,0,0,.8,1,5,.8)
  myPlot
}

plotMaker6 <- function() {
  p<- RtBasedonAppTraceForReport(dat,"Rt",31 ,2,.7,.3)
  p
}

plotMaker7 <- function() {
  plot<- RtBasedonManualTraceForR(dat,"Rt",10 ,3,.7,1,.3)
  plot
}


