
RtBasedonAppAndManual <- function(dat,
                                  days,
                                  R,
                                  p.tr = 100,
                                  p.trace_ap = 100,
                                  p.sym,
                                  iso_delay_traced,
                                  iso_delay_untraced,
                                  sd_contact){
  
  
  select64000Scenarios <- function(dat,
                                   days,
                                   R,
                                   p.tr,
                                   p.trace_ap,
                                   p.sym,
                                   iso_delay_traced,
                                   iso_delay_untraced,
                                   sd_contact) {
    
    scenarios64000<-dat %>% filter(R0==R &
                                     # p.trace==p.tr&
                                     # p.trace_app==p.trace_ap&
                                     p.symp== p.sym&
                                     iso_delay_traced_max==iso_delay_traced&
                                     iso_delay_untraced_sd_max==iso_delay_untraced&
                                     sd_contact_rate1==sd_contact) %>% 
      select(p.trace,p.trace_app,"day":"n.active_Q_95") %>% filter(day==days)
    return(scenarios64000)
  }
  
  
  aes_x <- "p.trace_app"
  aes_y <- "Revenue"
  aes_col <- "p.trace"
  aes_grp <- "p.trace"
  
  
  results<- select64000Scenarios(dat, days, R, p.tr, p.trace_ap, p.sym,
                                 iso_delay_traced, iso_delay_untraced, sd_contact) 
  
  # # dat <- readRDS("Newdata.rds")
  # # results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)
  # long_results <- results %>% gather(Quarter, Revenue, Rt_Q_05:Rt_Q_95) 
  # p <- ggplot(long_results,
  #             aes(x=eval(as.name(aes_x)),
  #                 y=eval(as.name(aes_y)),
  #                 color=factor(eval(as.name(aes_col))),
  #                 group=eval(as.name(aes_grp)))) +
  #   stat_summary(geom="pointrange",
  #                fun.y  = "median",
  #                fun.ymin = function(x) min(x),
  #                fun.ymax = function(x) max(x),size=2) +
  #   stat_summary(geom="line",
  #                fun.y = "median",size=1)+ylim(0,1.5)+
  #   # labs(y="Reproductive Number", x="Fraction of people using contact tracing app", 
  #   #      color="Fraction of cases manually traced")
  # ==========================================
  
  aes_x <- "p.trace"
  aes_y <- "Rt_Q_50"
  dat <- readRDS("Newdata.rds")
  # I should be careful about which one of select64000Scenarios as there are at least 3 of them
  # results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)
  results<- select64000Scenarios(dat, days, R, p.tr, p.trace_ap, p.sym,
                                 iso_delay_traced, iso_delay_untraced, sd_contact) 
  paired.cols <- RColorBrewer::brewer.pal(12, "Paired")
  outputs1 <- results %>% filter(p.trace_app==0)
  outputs2 <- results %>% filter(p.trace_app==.25)
  outputs3 <- results %>% filter(p.trace_app==.50)
  outputs4 <- results %>% filter(p.trace_app==.75)
  outputs5 <- results %>% filter(p.trace_app==1)

  p <- ggplot(outputs1 ,aes(x=eval(as.name(aes_x)), y=Rt_Q_50))
  
  p <- p+ geom_ribbon(aes(ymin=Rt_Q_25,ymax=Rt_Q_75),fill=paired.cols[8],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs2$Rt_Q_25,ymax=outputs2$Rt_Q_75), fill=paired.cols[2],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs3$Rt_Q_25,ymax=outputs3$Rt_Q_75),fill=paired.cols[6],alpha=0.4)
  p <- p+ geom_ribbon(aes(ymin=outputs4$Rt_Q_25,ymax=outputs4$Rt_Q_75),fill=paired.cols[11],alpha=0.5)
  p <- p+ geom_ribbon(aes(ymin=outputs5$Rt_Q_25,ymax=outputs5$Rt_Q_75),fill=paired.cols[3],alpha=0.4)

  p <- p+geom_line(size = 2,color = paired.cols[8])
  p <- p+geom_line(aes(x=outputs2$p.trace, y=outputs2$Rt_Q_50),size = 2,color = paired.cols[2])
  p <- p+geom_line(aes(x=outputs3$p.trace, y=outputs3$Rt_Q_50),size = 2,color = paired.cols[6])
  p <- p+geom_line(aes(x=outputs4$p.trace, y=outputs4$Rt_Q_50),size = 2,color = paired.cols[11])
  p <- p+geom_line(aes(x=outputs5$p.trace, y=outputs5$Rt_Q_50),size = 2,color = paired.cols[3])
  
  p <- p+geom_point(shape = 21, colour = paired.cols[8], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs2$p.trace, y=outputs2$Rt_Q_50),
                    shape = 21, colour = paired.cols[2], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs3$p.trace, y=outputs3$Rt_Q_50),
                    shape = 21, colour = paired.cols[6], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs4$p.trace, y=outputs4$Rt_Q_50),
                    shape = 21, colour = paired.cols[11], fill = "white", size = 2, stroke = 3)
  p <- p+geom_point(aes(x=outputs5$p.trace, y=outputs5$Rt_Q_50),
                    shape = 21, colour = paired.cols[3], fill = "white", size = 2, stroke = 3)
  p <- p+theme_bw()
  annotation <- data.frame(
    x= c(0.12,0.12,0.12,0.12,0.12),
    y = c(.20,.12,.04,-.04,-.12),
    label = c("0.00 app trace","0.25 app trace","0.50 app trace","0.75 app trace","1.00 app trace")
  )
  
  p <- p + geom_text(data=annotation, aes( x=x, y=y, label=label),                 , 
                     color=c(paired.cols[8],paired.cols[2],paired.cols[6],"yellow",paired.cols[3]), 
                     size=4 , angle=0, fontface="bold" )
  p <- p+annotate("point", x = .0, y = .20, colour = paired.cols[8],size = 3)
  p <- p+annotate("point", x = .0, y = .12, colour = paired.cols[2],size = 3)
  p <- p+annotate("point", x = .0, y = .04, colour = paired.cols[6],size = 3)
  p <- p+annotate("point", x = .0, y =-.04, colour = "yellow",size = 3)
  p <- p+annotate("point", x = .0, y = -.12, colour = paired.cols[3],size = 3)
  p
  
  # ===========================================
  
  p <- p + labs(y="", x="", 
         color="")
  p <- p + geom_hline(yintercept=1,
                      linetype='dotdash',
                      alpha=0.6)
  p <- p + labs(title="Colors show the level of manual tracing")
  p <- ggplotly(p)
  x <- list(
    title = "Fraction of cases manually traced"
  )
  y <- list(
    title = "Reproductive Number"
  )
  
  return(p %>% layout(xaxis = x, yaxis = y,  margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),title=list(x=1)))
  
}


UiRt_App_Manual <- function(){
  ui <- fluidPage(
    # Application title
    titlePanel("Parameters of Scenarios"),
    
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        
        shinyWidgets::sliderTextInput("sd_contactforAppManual","
                                      Strength of physical distancing (contact rate)",
                                      choices=c(0.3, 0.6, 0.8),
                                      selected=0.3, grid = T),
        
        sliderInput("iso_delay_tracecedforAppManual",
                    "Delay to isolation for traced cases (days)",
                    min = 1,  max = 4, value = 1, step = 1),
        
        selectInput("selectionAppManual", "Select something", choices = c("Descision Making Parameters", "All Parameters")),
        conditionalPanel(
          "input.selectionAppManual == 'All Parameters'",
          sliderInput("R0forAppManual",
                      "R0:",
                      min = 2,  max = 3, value = 2, step = .5),
          
          sliderInput("p.symforAppManual",
                      "Fraction of cases that are symptomatic",
                      min = .6,  max = .8, value = .6, step = .1),
          
          sliderInput("iso_delay_untracedforAppManaual",
                      "Delay to isolation for untraced & distancing cases",
                      min = 1,  max = 5, value = 1, step = 4),
          
          sliderInput("daysforAppManual",
                      "days:",
                      min = 0,  max = 30,  value = 20)
        ),
        hr(),
        h3("Assumptions"),
        tableOutput("tableAppManual")
        
      ),
      
      # Show Word Cloud
      mainPanel(
        plotlyOutput("plotRt_App_Manual"),
        h4(code("Impact of Manual and digital contact tracing for 
                Reproductive Number of covid-19"))
      )
    )
  )
  
  return (ui)
}

server <- function(input, output, session) {
  
  output$tableAppManual <- renderTable({
    data.frame(
      Name = c("R0 ",
               "Fraction of cases that are symptomatic",
               "Delay to isolation for untraced & distancing cases",
               "days",
               "Delay to isolation for traced cases (days)",
               "Fraction of people using contact tracing app",
               "Fraction of cases manually traced",
               "Strength of physical distancing (contact rate)"),
      Value = as.character(c(input$R0forAppManual,
                             input$p.symforAppManual,
                             input$iso_delay_untracedforAppManaual,
                             input$daysforAppManual,
                             input$iso_delay_tracecedforAppManual,
                             "None",
                             "None",
                             input$sd_contactforAppManual)),
      stringsAsFactors = FALSE)
  })
}

shinyApp(ui, server)


plotRt_App_Manual <- function(input){
  myPlot <- RtBasedonAppAndManual(dat,
                                  day = input$daysforAppManual,
                                  R = input$R0forAppManual,
                                  p.tr = 100,
                                  p.trace_ap = 100,
                                  p.sym = input$p.symforAppManual,
                                  iso_delay_traced=input$iso_delay_tracecedforAppManual,
                                  iso_delay_untraced= input$iso_delay_untracedforAppManaual,
                                  sd_contact = input$sd_contactforAppManual)
  return(myPlot)
}

# ui <- UiRt_App_Manual()
# server <- ServerRt_App_Manual()



