
returnPlot <- function(dat,
                       ouptColumn,
                       R0,
                       p.trace,
                       p.trace_app,
                       p.symp,
                       iso_delay_traced_max,
                       iso_delay_untraced_sd_max,
                       sd_contact_rate1,
                       numberDay) {
  
  output<- select100Scenarios(dat,
                              R0,
                              p.trace,
                              p.trace_app,
                              p.symp,
                              iso_delay_traced_max,
                              iso_delay_untraced_sd_max,
                              sd_contact_rate1)
  
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
   p <- p+theme_bw()
   p <- p + labs(y="", x="", 
                 color="")
   # p <- p + labs(title="Colors show the level of app tracing")
   p <- ggplotly(p)
   x <- list(
      title = "Day"
   )


   # return(ggplotly(p))
   return(p %>% layout(xaxis = x, yaxis = y,  
                       margin = list(l = 50, r = 20, b = 50, t = 20, pad = 4),title=list(x=1)))
}


# myPlot<- returnPlot(dat,"Rt" ,3,.5,.5,.7,2,1,.3)
# myPlot




 


