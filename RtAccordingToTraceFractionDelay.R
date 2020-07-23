
RtBasedonManualTrace <- function(dat,
                                 days,
                                 R,
                                 p.tr,
                                 p.trace_ap = 0,
                                 p.sym,
                                 iso_delay_traced,
                                 iso_delay_untraced,
                                 sd_contact){

select64000Scenarios <- function(dat,
                                 days,
                                 R,
                                 p.tr,
                                 p.trace_ap = 0,
                                 p.sym,
                                 iso_delay_traced,
                                 iso_delay_untraced,
                                 sd_contact)  {
  
  scenarios64000<-dat %>% filter(R0==R &
                                 # p.trace==p.tr&
                                 p.trace_app==p.trace_ap&
                                 p.symp== p.sym&
                                 # iso_delay_traced_max==iso_delay_traced&
                                 iso_delay_untraced_sd_max==iso_delay_untraced&
                                 sd_contact_rate1==sd_contact) %>% 
                                 select(p.trace,iso_delay_traced_max,"day":"Rt") %>% filter(day==days)
  return(scenarios64000)
}


aes_x <- "p.trace"
aes_y <- "Rt"
aes_col <- "iso_delay_traced_max"
aes_grp <- "iso_delay_traced_max"
results<- select64000Scenarios(dat, days, R, p.tr, p.trace_ap, p.sym,
                               iso_delay_traced, iso_delay_untraced, sd_contact) 
# results<- select64000Scenarios(dat,10 ,3,.5,.5,.7,2,1,.3)

p <- ggplot(results,
            aes(x=eval(as.name(aes_x)),
                y=eval(as.name(aes_y)),
                color=factor(eval(as.name(aes_col))),
                group=eval(as.name(aes_grp)))) +
  stat_summary(geom="pointrange",
               fun.y  = "median",
               fun.ymin = function(x) quantile(x, .25),
               fun.ymax = function(x) quantile(x, .75),size=2) +
  stat_summary(geom="line",
               fun.y = "median",size=1)+ylim(0,1.5)+
  labs(y="Reproductive Number", x="",
       color="")
  p <- p + geom_hline(yintercept=1,
                    linetype='dotdash',
                    alpha=0.6)
  p <- p + labs(title="Colors show delay to isolation for traced cases (days)")
  p <- ggplotly(p)
  x <- list(
    title = "Fraction of cases manually traced"
  )
  y <- list(
    title = "Reproductive Number"
  )
  
  return(p %>% layout(xaxis = x, yaxis = y,  margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),title=list(x=1)))

}


UiRt_Only_Manual <- function(){
  
  ui <- fluidPage(
    # Application title
    titlePanel("Parameters of Scenarios"),
    
    sidebarLayout(
      # Sidebar with a slider and selection inputs
      sidebarPanel(
        
        shinyWidgets::sliderTextInput("sd_contactforManual","
                                      Strength of physical distancing (contact rate)",
                                      choices=c(0.3, 0.6, 0.8),
                                      selected=0.3, grid = T),
        sliderInput("iso_delay_untracedforManual",
                    "Delay to isolation for untraced & distancing cases",
                    min = 1,  max = 5, value = 1, step = 4),
        selectInput("selection", "Select something", choices = c("Descision Making Parameters", "All Parameters")),
        conditionalPanel(
          "input.selection == 'All Parameters'",
          sliderInput("R0forManual",
                      "R0:",
                      min = 2,  max = 3, value = 2, step = .5),
          
          sliderInput("p.symforManual",
                      "Fraction of cases that are symptomatic",
                      min = .6,  max = .8, value = .6, step = .1),
          
          sliderInput("daysforManual",
                      "days:",
                      min = 0,  max = 30,  value = 20)
        ),
        hr(),
        h3("Assumpations"),
        tableOutput("tableManual")
      ),
      
      # Show Word Cloud
      mainPanel(
        plotlyOutput("plotRt_Only_Manual")
      )
    )
  )
  return(ui)
}

# server <- function(input, output, session) {
# 
#   output$tableManual <- renderTable({
#     data.frame(
#       Name = c("R0 ",
#                "Fraction of cases that are symptomatic",
#                "Delay to isolation for untraced & distancing cases",
#                "days",
#                "Delay to isolation for traced cases (days)",
#                "Fraction of people using contact tracing app",
#                "Fraction of cases manually traced",
#                "Strength of physical distancing (contact rate)"),
#       Value = as.character(c(input$R0forManual,
#                              input$p.symforManual,
#                              input$iso_delay_untracedforManual,
#                              input$daysforManual,
#                              "None",
#                              0,
#                              "None",
#                              input$sd_contactforManual)),
#       stringsAsFactors = FALSE)
#   })
#   
# }

shinyApp(ui, server)


plotRt_Only_Manual <- function(input){
  
  myPlot <- RtBasedonManualTrace(dat,
                                 day = input$daysforManual,
                                 R = input$R0forManual,
                                 p.tr = 100,
                                 p.trace_ap = 0,
                                 p.sym = input$p.symforManual,
                                 iso_delay_traced=100,
                                 iso_delay_untraced= input$iso_delay_untracedforManual,
                                 sd_contact = input$sd_contactforManual)
  return (myPlot)
}

# ui <- UiRt_Only_Manual()
# server <- ServerRt_Only_Manual()




