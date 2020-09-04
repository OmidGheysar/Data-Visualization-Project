
RtBasedonAppAndManual <- function(dat,ouptColumn,
                                  days,
                                  R,
                                  p.sym,
                                  iso_delay_traced,
                                  iso_delay_untraced,
                                  sd_contact){
  
  
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
                                     sd_contact_rate1==sd_contact) %>% 
      select(p.trace,p.trace_app,"day":"n.iso_Q_95") %>% filter(day==days)
    return(scenarios64000)
  }
  
  aes_x <- "p.trace"
  # dat <- readRDS("Newdata.rds")
  # results<- select64000Scenarios(dat,10 ,3,.7,2,1,.3)
  # ouptColumn <- "n.active"
  Q_05  <- paste(ouptColumn, "Q_05", sep="_")
  Q_25 <-   paste(ouptColumn, "Q_25", sep="_")
  Q_50 <-  paste(ouptColumn, "Q_50", sep="_")
  Q_75 <-  paste(ouptColumn, "Q_75", sep="_")
  Q_95 <-  paste(ouptColumn, "Q_95", sep="_")
  
  results<- select64000Scenarios(dat,
                                 days, 
                                 R, 
                                 p.sym,
                                 iso_delay_traced, 
                                 iso_delay_untraced, 
                                 sd_contact) 

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
  p <- p+theme_bw()

  
  # ===========================================
  
  p <- p + labs(y="", x="", 
         color="")
  # p <- p + geom_hline(yintercept=1,
  #                     linetype='dotdash',
  #                     alpha=0.6)
  if(ouptColumn=="Rt"){
    annotation <- data.frame(
      x= c(0.12,0.12,0.12,0.12,0.12),
      y = c(.07,.0,-.07,-.14,-.21),
      label = c("0.00 app trace","0.25 app trace","0.50 app trace","0.75 app trace","1.00 app trace")
    )
    
    p <- p + geom_text(data=annotation, aes( x=x, y=y, label=label),                 
                       color=c(paired.cols[8],paired.cols[2],paired.cols[6],paired.cols[4],paired.cols[5]), 
                       size=4 , angle=0, fontface="bold" )
    p <- p+annotate("point", x = .0, y = .07, colour = paired.cols[8],size = 3)
    p <- p+annotate("point", x = .0, y = .0, colour = paired.cols[2],size = 3)
    p <- p+annotate("point", x = .0, y = -.07, colour = paired.cols[6],size = 3)
    p <- p+annotate("point", x = .0, y =-.14, colour = paired.cols[4],size = 3)
    p <- p+annotate("point", x = .0, y = -.21, colour = paired.cols[5],size = 3)
    p <- p + geom_hline(yintercept=1,
                        linetype='dotdash',
                        alpha=0.6)
  }
  p <- p + labs(title="Colors show the level of app tracing")
  p <- ggplotly(p)
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
  
  return(p %>% layout(xaxis = x, yaxis = y,  margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),title=list(x=1)))
  
}


UiRt_App_Manual <- function(){
  
  ui <- fluidRow(
    box(title = "Choose which parameters to set",width = 4, solidHeader = TRUE, status = "primary",
      background = "navy",
      shinyWidgets::sliderTextInput("sd_contactforAppManual","
                                      Contact rate (proportion of normal)",
                                    choices=c(0.3, 0.6, 0.8),
                                    selected=0.3, grid = T),
      
      sliderInput("iso_delay_tracecedforAppManual",
                  "Delay to isolation for traced cases (days)",
                  min = 1,  max = 4, value = 1, step = 1),
      
      selectInput("selectionAppManual", "Select something", choices = c("Descision Making Parameters", "All Parameters")),
      conditionalPanel(
        "input.selectionAppManual == 'All Parameters'",
        sliderInput("R0forAppManual",
                    "R0",
                    min = 2,  max = 3, value = 2, step = .5),
        
        sliderInput("p.symforAppManual",
                    "Fraction of cases that are symptomatic",
                    min = .6,  max = .8, value = .6, step = .1),
        
        sliderInput("iso_delay_untracedforAppManaual",
                    "Delay to isolation for untraced & distancing cases",
                    min = 1,  max = 5, value = 1, step = 4),
        
        sliderInput("daysforAppManual",
                    "Simulation days:",
                    min = 0,  max = 31,  value = 31)
      ),
      hr(),
      h3("Assumptions"),
      tableOutput("tableAppManual")
      
    ),
    box(width = 8, solidHeader = TRUE,color = "black",background = "navy",
      plotlyOutput("plotRt_App_Manual1"),
      actionButton("run", "0% app trace", 
                   style="color: #fff; background-color: #FF7F00; border-color: #2e6da4"),
      actionButton("run", "25% app trace", 
                   style="color: #fff; background-color: #1F78B4; border-color: #2e6da4"),
      actionButton("run", "50% app trace", 
                   style="color: #fff; background-color: #E31A1C; border-color: #2e6da4"),
      actionButton("run", "75% app trace", 
                   style="color: #fff; background-color: #33A02C; border-color: #2e6da4"),
      actionButton("run", "100% app trace", 
                   style="color: #fff; background-color: #FB9A99; border-color: #2e6da4"),
      plotlyOutput("plotRt_App_Manual2"),
      navbarPage(id = "manualApp",inverse=TRUE,"Please choose your plot",
                 tabPanel("Currently active cases"),
                 tabPanel("New cases"),
                 tabPanel("Cumulative new cases"),
                 tabPanel("Isolated cases")
      ),
      h4(code("Impact of Manual and digital contact tracing for 
                Reproductive Number of covid-19"))
    ),
  )
  
  
  return (ui)
}

