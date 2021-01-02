RtBasedonAppTrace <- function(dat,ouptColumn,
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
  # dat <- readRDS("Newdata.rds")
  # results<- select64000Scenarios(dat,31 ,3,.7,.3)
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
  
  p <- p+theme_bw()

  if(ouptColumn=="Rt"){
    # p<- p+labs(y="Reproductive Number", x="",
    #            color="")
    annotation <- data.frame(
      x= c(0.12,0.12),
      y = c(.60,.52),
      label = c("1 day delay","5 days delay")
    )
    p <- p + geom_text(data=annotation, aes( x=x, y=y, label=label),                
                       color=c(paired.cols[8],paired.cols[2]), 
                       size=4 , angle=0, fontface="bold" )
    p <- p+annotate("point", x = .0, y = .60, colour = paired.cols[8],size = 3)
    p <- p+annotate("point", x = .0, y = .52, colour = paired.cols[2],size = 3)
    p <- p + geom_hline(yintercept=1,
                        linetype='dotdash',
                        alpha=0.6)
  }

  p <- p + labs(title="Colors show delay to isolation for untraced & distancing cases")
  p <- ggplotly(p)
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
  
  return(p %>% layout(xaxis = x, yaxis = y,  margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),title=list(x=1))) 
  
}

UiRt_Only_App <- function(){
  
  ui <-fluidRow(
    box(title = "Choose which parameters to set", width = 4, solidHeader = TRUE, status = "primary",
      color = "black",background = "navy",
      shinyWidgets::sliderTextInput("sd_contactforApp","
                                      Contact rate (proportion of normal)",
                                    choices=c(0.3, 0.6, 0.8),
                                    selected=0.3, grid = T),
      selectInput("selectionApp", "Select something", choices = c("Descision Making Parameters", "All Parameters")),
      conditionalPanel(
        "input.selectionApp == 'All Parameters'",
        sliderInput("R0forApp",
                    "R0",
                    min = 2,  max = 3, value = 2, step = .5),
        
        sliderInput("p.symforApp",
                    "Fraction of cases that are symptomatic",
                    min = .6,  max = .8, value = .6, step = .1),
        
        sliderInput("daysforApp",
                    "Simulation days",
                    min = 0,  max = 31,  value = 31)
      ),
      hr(),
      h3("Assumptions"),
      tableOutput("tableApp")
      
    ),
    box( width = 8, solidHeader = TRUE,
      color = "black",background = "navy",
      plotlyOutput("plotRt_Only_App1"),
      # fluidRow(
        actionButton("run", "1 day delay", 
                     style="color: #fff; background-color: #FF7F00; border-color: #2e6da4"),
        actionButton("run", "5 days delay", 
                     style="color: #fff; background-color: #1F78B4; border-color: #2e6da4"),
        
      # ),
      plotlyOutput("plotRt_Only_App2"),
      navbarPage(id = "onlyApp",inverse=TRUE,"Please choose your plot",
                 tabPanel("Currently active cases"),
                 tabPanel("New cases"),
                 tabPanel("Cumulative new cases"),
                 tabPanel("Isolated cases")
      ),
      h4(code("Impact of digital contact tracing for 
                Reproductive Number of covid-19")),
      downloadButton("reportForAppTrace", "Generate a report For App Trace"),
      
    ),
  )
  return(ui)
  
}  




