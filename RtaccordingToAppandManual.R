
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
    select(p.trace,p.trace_app,"day":"Rt") %>% filter(day==days)
  return(scenarios64000)
}


aes_x <- "p.trace_app"
aes_y <- "Rt"
aes_col <- "p.trace"
aes_grp <- "p.trace"

# results<- select64000Scenarios(dat, 1, 2.5,.5,.5,.7,2,5,.3)
results<- select64000Scenarios(dat, days, R, p.tr, p.trace_ap, p.sym,
                               iso_delay_traced, iso_delay_untraced, sd_contact) 

p <- ggplot(results,
            aes(x=eval(as.name(aes_x)),
                y=eval(as.name(aes_y)),
                color=factor(eval(as.name(aes_col))),
                group=eval(as.name(aes_grp)))) +
  stat_summary(geom="pointrange",
               fun.y  = "median",
               fun.ymin = function(x) quantile(x, .25),
               fun.ymax = function(x) quantile(x, .75),size=1) +
  stat_summary(geom="line",
               fun.y = "median",size=1)+ylim(0,1.5)+
  labs(y="y_label", x="x_label", color="c_label")
p <- p + geom_hline(yintercept=1,
                    linetype='dotdash',
                    alpha=0.6)

p

}


UiRt_App_Manual <- function(){
  ui <- fluidPage(
    # Application title
    titlePanel("Parameters of Scenarios"),
    
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        sliderInput("R0",
                    "R0:",
                    min = 2,  max = 3, value = 2, step = .5),
        
        sliderInput("p.sym",
                    "p.sym:",
                    min = .6,  max = .8, value = .6, step = .1),
        
        sliderInput("iso_delay_traceced",
                    "iso_delay_traceced:",
                    min = 1,  max = 4, value = 1, step = 1),
        
        
        sliderInput("iso_delay_untraced",
                    "iso_delay_untraced:",
                    min = 1,  max = 5, value = 1, step = 4),
        
        sliderInput("sd_contact",
                    "sd_contact:",
                    min = .3,  max = .8, value = .3, step = .5),
        
        sliderInput("days",
                    "days:",
                    min = 0,  max = 30,  value = 20),
        
      ),
      
      # Show Word Cloud
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  return (ui)
}



ServerRt_App_Manual <- function(){
  server <- function(input, output, session) {
    output$plot <- renderPlot({
      myPlot <- RtBasedonAppAndManual(dat,
                                      day = input$days,
                                      R = input$R0,
                                      p.tr = 100,
                                      p.trace_ap = 100,
                                      p.sym = input$p.sym,
                                      iso_delay_traced=input$iso_delay_traceced,
                                      iso_delay_untraced= input$iso_delay_untraced,
                                      sd_contact = input$sd_contact)
      myPlot
      
    })
  }
  
  return(server)
}

# ui <- UiRt_App_Manual()
# server <- ServerRt_App_Manual()
  
  
  
