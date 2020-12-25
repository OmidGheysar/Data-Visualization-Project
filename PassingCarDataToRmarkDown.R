library(babynames)
library(ggtext)


data(mtcars)
mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("Manual", "Automatic"))
df <- mtcars


ivs <- c("cyl", "disp")
dvs <- c("mpg", "qsec")
sth <- 100
dp3 <- data.frame(lable = c("dat",
                           "Rt",
                           "days",
                           "R",
                           "p.sym",
                           "iso_delay_traced",
                           "iso_delay_untraced",
                           "sd_contact"),
                 value =c(0,0,30 ,3,.7,2,1,.3))

dp4 <- data.frame(lable = c("dat",
                            "Rt",
                            "R0",
                            "p.trace",
                            "p.trace_app",
                            "p.symp",
                            "iso_delay_traced_max",
                            "iso_delay_untraced_sd_max",
                            "sd_contact_rate1",
                            "numberDay"),
                  value =c(0,0 ,3,.5,.5,.7,2,1,.3,20))


dp5 <- data.frame(lable = c("dat", 
                            "Rt",
                            "inputR012",
                            "inputp.trace12",
                            "inputp.trace_app12",
                            "inputp.symp12",
                            "inputiso_delay_traced_max12",
                            "inputiso_delay_untraced_sd_max12",
                            "inputsd_contact_rate112",
                            "inputR023",
                            "inputp.trace23",
                            "inputp.trace_app23",
                            "inputp.symp23",
                            "inputiso_delay_traced_max23",
                            "inputiso_delay_untraced_sd_max23",
                            "inputsd_contact_rate123"),
                  value =c(0,0,2,0,0,.8,1,5,.8, 2.5,0,0,.8,1,5,.8))

dp6 <- data.frame(lable = c("dat",
                            "Rt",
                            "days",
                            "R",
                            "p.sym",
                            "sd_contact"),
                  value =c(0,0,31 ,3,.7,.3))


dp7 <- data.frame(lable = c("dat",
                            "Rt",
                            "days",
                            "R",
                            "p.sym",
                            "iso_delay_untraced",
                            "sd_contact"),
                  value =c(0,0,31 ,3,.7,1,.3))



rmarkdown::render("paramPass.Rmd", 
                  params = list(df = df,
                                dp3 = dp3,
                                dp4 = dp4,
                                dp5 = dp5,
                                dp6 = dp6,
                                dp7 = dp7,
                                ivs = ivs,
                                dvs = dvs,
                                sth = sth))





# ==========================================================

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

  p <- RtBasedonAppAndManualForReport(dat,"Rt",10 ,3,.7,2,1,.3)


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

# =======================================================================================
