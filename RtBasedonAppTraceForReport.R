RtBasedonAppTraceForReport <- function(dat,ouptColumn,
                              days,
                              R,
                              p.sym,
                              sd_contact){
  
  
  select64000Scenarios <- function(dat,
                                   days,
                                   R,
                                   p.sym,
                                   sd_contact) {
    
    scenarios<-dat %>% filter(R0==R &
                                p.trace==0&
                                p.symp== p.sym&
                                # why is this ?????????
                                iso_delay_traced_max==2&
                                sd_contact_rate1==sd_contact) %>% filter(day==days) %>%  
      select(p.trace_app,iso_delay_untraced_sd_max,"day":"n.iso_Q_95") 
    return(scenarios)
  }
  
  aes_x <- "p.trace_app"
  
  # ouptColumn <- "n.total"
  Q_25 <-   paste(ouptColumn, "Q_25", sep="_")
  Q_50 <-  paste(ouptColumn, "Q_50", sep="_")
  Q_75 <-  paste(ouptColumn, "Q_75", sep="_")
  
  results<- select64000Scenarios(dat, 
                                 days, 
                                 R,
                                 p.sym,
                                 sd_contact)

  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  
  
  outputs1 <- results %>% filter(iso_delay_untraced_sd_max==1)
  outputs2 <- results %>% filter(iso_delay_untraced_sd_max==5)
  
  
  p <- ggplot(outputs1 ,aes(x=eval(as.name(aes_x)), y=eval(as.name(Q_50))))
  
  p <- p+ geom_ribbon(aes(ymin=outputs1[[Q_25]],ymax=outputs1[[Q_75]]),fill=paired.cols[8],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs2[[Q_25]],ymax=outputs2[[Q_75]]), fill=paired.cols[2],alpha=0.4)
  
  p <- p+geom_line(size = 2,color = paired.cols[8])
  p <- p+geom_line(aes(x=outputs2$p.trace_app, y=outputs2[[Q_50]]),size = 2,color = paired.cols[2])
  
  p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs2$p.trace_app, y=outputs2[[Q_50]]),
                    shape = 21, colour = paired.cols[2], fill = "white", size = 2, stroke = 3)
  
  p <- p + labs(title="Colors show delay to isolation for untraced & distancing cases")
  # p <- ggplotly(p)
  x <- list(
    title = "Fraction of people using contact tracing app"
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
# source("uploadRequiredLibraries.R")
# uploadRequiredLibraries()
# dat <- readRDS("Newdata.rds")
# p<- RtBasedonAppTraceForReport(dat,"Rt",31 ,2,.7,.3)
# p


